import ply.yacc as yacc
from lexer import tokens

# =============================================================================
# ESTRUCTURAS DE DATOS SEMÁNTICAS
# =============================================================================

# Tabla de símbolos GLOBAL: { nombre: {'type': str, 'value': any} }
symbol_table = {}

# Pila de scopes locales (parámetros de función activa)
scope_stack = []

# Tabla de registros: { nombre: [{'name': str, 'type': str}, ...] }
record_table = {}

# Tabla de funciones con sobrecarga:
#   { nombre: [ {'params': [...], 'return_type': str}, ... ] }
function_table = {}

# Tipo de retorno de la función que se está parseando (para validar return)
current_return_type = None
pending_function_return_type = None
current_function_has_return = False

# Parámetros pendientes de empujar al scope cuando se abre '{' de función
_pending_params = []

# Lista de cuartetos generados
quartets = []
quartet_buffers = []
emit_enabled_stack = [True]

# Contadores de temporales y etiquetas
_temp_counter  = 0
_label_counter = 0

# Acumulador de errores y flag global
semantic_errors = []
has_errors = False
loop_depth = 0
loop_end_label_stack = []


# =============================================================================
# UTILIDADES DE SCOPES
# =============================================================================

def lookup_symbol(name):
    """Busca un símbolo: scope local primero, luego global."""
    for scope in reversed(scope_stack):
        if name in scope:
            return scope[name]
    return symbol_table.get(name)

def declare_in_current_scope(name, info, lineno=None):
    """Declara en el scope más interno disponible."""
    target = scope_stack[-1] if scope_stack else symbol_table
    if name in target:
        report_error(f"La variable '{name}' ya ha sido declarada.", lineno)
        return False
    if 'quad' not in info:
        info['quad'] = False
    target[name] = info
    return True

def push_scope(params):
    scope = {p['name']: {'type': p['type'], 'value': default_value(p['type'])}
             for p in params}
    scope_stack.append(scope)

def pop_scope():
    if scope_stack:
        scope_stack.pop()


# =============================================================================
# UTILIDADES DE TIPOS
# =============================================================================

BASIC_TYPES = {'int', 'float', 'char', 'boolean'}


def is_known_type(t):
    return t in BASIC_TYPES or t in record_table or t == 'void'


def default_value(t):
    if t == 'int':     return 0
    if t == 'float':   return 0.0
    if t == 'char':    return ''
    if t == 'boolean': return False
    if t in record_table:
        return {f['name']: default_value(f['type']) for f in record_table[t]}
    return None


def can_convert(src, dst):
    """True si src se puede convertir a dst sin pérdida de datos."""
    if src == dst:                                    return True
    if src == 'char'  and dst in ('int', 'float'):   return True
    if src == 'int'   and dst == 'float':             return True
    return False


def unify_types(t1, t2):
    """Tipo común al que pueden convertirse t1 y t2, o None si imposible."""
    if t1 == t2: return t1
    hierarchy = ['char', 'int', 'float']
    if t1 in hierarchy and t2 in hierarchy:
        return hierarchy[max(hierarchy.index(t1), hierarchy.index(t2))]
    return None


# =============================================================================
# UTILIDADES DE ERRORES
# =============================================================================

def report_error(msg, lineno=None):
    global has_errors
    has_errors = True
    prefix = f"[ERROR SEMÁNTICO] Línea {lineno}: " if lineno else "[ERROR SEMÁNTICO] "
    full = prefix + msg
    print(full)
    semantic_errors.append(full)


# =============================================================================
# GENERACIÓN DE CÓDIGO INTERMEDIO (CUARTETOS)
# =============================================================================

def new_temp():
    global _temp_counter
    _temp_counter += 1
    return f"@T{_temp_counter}"


def new_label():
    global _label_counter
    _label_counter += 1
    return f"@L{_label_counter}"


def emit(op, arg1="_", arg2="_", result="_"):
    if not emit_enabled_stack[-1]:
        return
    def fmt(v):
        if v is True:  return 'true'
        if v is False: return 'false'
        return str(v)
    quartet_buffers[-1].append((fmt(op), fmt(arg1), fmt(arg2), fmt(result)))


def push_quartet_buffer():
    quartet_buffers.append([])


def pop_quartet_buffer():
    if len(quartet_buffers) > 1:
        return quartet_buffers.pop()
    return quartet_buffers[0]


def append_quartets(items):
    if not emit_enabled_stack[-1]:
        return
    quartet_buffers[-1].extend(items)


def push_emit_enabled(enabled):
    inherited = emit_enabled_stack[-1]
    emit_enabled_stack.append(inherited and enabled)


def pop_emit_enabled():
    if len(emit_enabled_stack) > 1:
        emit_enabled_stack.pop()


def apply_cast(val, src, dst):
    """
    Emite instrucciones de casting si src != dst.
    Devuelve (nuevo_val, dst).
    """
    if src == dst:
        return val, dst
    if src == 'char' and dst == 'int':
        t = new_temp(); emit('CHAR_TO_INT', val, '_', t); return t, 'int'
    if src == 'char' and dst == 'float':
        t1 = new_temp(); emit('CHAR_TO_INT',   val, '_', t1)
        t2 = new_temp(); emit('INT_TO_FLOAT',  t1,  '_', t2); return t2, 'float'
    if src == 'int' and dst == 'float':
        t = new_temp(); emit('INT_TO_FLOAT', val, '_', t); return t, 'float'
    return val, src   # No debería llegar aquí si se llamó después de can_convert


def _literal(value, vtype):
    if vtype == 'boolean': return 'true' if value else 'false'
    if vtype == 'char':    return repr(value) if value else "''"
    return str(value)


# =============================================================================
# PRECEDENCIA
# =============================================================================

precedence = (
    ('left',  'OR'),
    ('left',  'AND'),
    ('left',  'EQ'),
    ('left',  'GT', 'GE', 'LT', 'LE'),
    ('left',  'PLUS', 'MINUS'),
    ('left',  'MULT', 'DIV'),
    ('right', 'UMINUS', 'UPLUS', 'NOT'),
    ('left',  'DOT'),
)

# =============================================================================
# GRAMÁTICA
# =============================================================================

# ---- Programa ----

def p_program(p):
    '''program : statement_list'''
    pass

def p_statement_list_stmt(p):
    '''statement_list : statement_list statement'''
    pass

def p_statement_list_func(p):
    '''statement_list : statement_list function_def'''
    pass

def p_statement_list_record(p):
    '''statement_list : statement_list record_def'''
    pass

def p_statement_list_if(p):
    '''statement_list : statement_list if_stmt'''
    pass

def p_statement_list_while(p):
    '''statement_list : statement_list while_stmt'''
    pass

def p_statement_list_dowhile(p):
    '''statement_list : statement_list do_while_stmt'''
    pass

def p_statement_list_print(p):
    '''statement_list : statement_list print_stmt'''
    pass

def p_statement_list_empty(p):
    '''statement_list : '''
    pass

# ---- Registro ----

def p_record_def(p):
    '''record_def : RECORD ID LPAREN field_list RPAREN SEMICOLON'''
    name   = p[2]
    fields = p[4]
    if name in record_table:
        report_error(f"El registro '{name}' ya ha sido declarado.", p.lineno(2))
    else:
        record_table[name] = fields

def p_field_list_multi(p):
    '''field_list : field_list COMMA field'''
    p[0] = p[1] + [p[3]]

def p_field_list_single(p):
    '''field_list : field'''
    p[0] = [p[1]]

def p_field_basic(p):
    '''field : type ID'''
    p[0] = {'name': p[2], 'type': p[1]}

def p_field_record(p):
    '''field : ID ID'''
    if p[1] not in record_table:
        report_error(f"El tipo de registro '{p[1]}' no ha sido declarado.", p.lineno(1))
    p[0] = {'name': p[2], 'type': p[1]}

# ---- Función ----

def p_function_def_basic(p):
    '''function_def : type ID LPAREN param_list RPAREN function_prep_basic func_open stmt_block RBRACE'''
    _finalize_function(p[2], p[1], p.lineno(2))
    pop_quartet_buffer()
    pop_emit_enabled()
    pop_scope()
    _register_function(p[1], p[2], p[4], p.lineno(2))

def p_function_def_void(p):
    '''function_def : VOID ID LPAREN param_list RPAREN function_prep_void func_open stmt_block RBRACE'''
    _finalize_function(p[2], 'void', p.lineno(2))
    pop_quartet_buffer()
    pop_emit_enabled()
    pop_scope()
    _register_function('void', p[2], p[4], p.lineno(2))

def p_function_def_record(p):
    '''function_def : ID ID LPAREN param_list RPAREN function_prep_record func_open stmt_block RBRACE'''
    _finalize_function(p[2], p[1], p.lineno(2))
    pop_quartet_buffer()
    pop_emit_enabled()
    pop_scope()
    ret_type = p[1]
    if not is_known_type(ret_type):
        report_error(f"Tipo de retorno desconocido '{ret_type}'.", p.lineno(1))
    _register_function(ret_type, p[2], p[4], p.lineno(2))

def p_function_prep_basic(p):
    '''function_prep_basic : '''
    global pending_function_return_type
    pending_function_return_type = p[-5]

def p_function_prep_void(p):
    '''function_prep_void : '''
    global pending_function_return_type
    pending_function_return_type = 'void'

def p_function_prep_record(p):
    '''function_prep_record : '''
    global pending_function_return_type
    pending_function_return_type = p[-5]

def p_func_open(p):
    '''func_open : LBRACE'''
    # Abrimos el scope con los parámetros pendientes ANTES de parsear el cuerpo
    global _pending_params, current_return_type, pending_function_return_type
    global current_function_has_return
    push_scope(_pending_params)
    push_quartet_buffer()
    push_emit_enabled(False)
    current_return_type = pending_function_return_type
    pending_function_return_type = None
    current_function_has_return = False
    _pending_params = []

def p_block_open(p):
    '''block_open : LBRACE'''
    # Abrimos un scope vacío para bloques if/else/while/do-while
    push_scope([])
    push_quartet_buffer()

def p_param_list_multi(p):
    '''param_list : param_list COMMA param'''
    global _pending_params
    p[0] = p[1] + [p[3]]
    _pending_params = p[0]

def p_param_list_single(p):
    '''param_list : param'''
    global _pending_params
    p[0] = [p[1]]
    _pending_params = p[0]

def p_param_list_empty(p):
    '''param_list : '''
    global _pending_params
    p[0] = []
    _pending_params = []

def p_param_basic(p):
    '''param : type ID'''
    p[0] = {'name': p[2], 'type': p[1]}

def p_param_record(p):
    '''param : ID ID'''
    if p[1] not in record_table:
        report_error(f"El tipo '{p[1]}' no ha sido declarado como registro.", p.lineno(1))
    p[0] = {'name': p[2], 'type': p[1]}

def _register_function(ret_type, name, params, lineno):
    global current_return_type
    current_return_type = None
    if name not in function_table:
        function_table[name] = []
    param_types = [p['type'] for p in (params or [])]
    for sig in function_table[name]:
        if [pp['type'] for pp in sig['params']] == param_types:
            if sig['return_type'] != ret_type:
                report_error(
                    f"Función '{name}' ya declarada con la misma firma pero distinto retorno.", lineno)
            else:
                report_error(f"Función '{name}' ya declarada con la misma firma.", lineno)
            return
    function_table[name].append({'params': params or [], 'return_type': ret_type})

def _finalize_function(name, ret_type, lineno):
    global current_return_type, current_function_has_return
    if ret_type != 'void' and not current_function_has_return:
        report_error(f"La función '{name}' debe incluir una sentencia return de tipo '{ret_type}'.", lineno)
    current_return_type = None
    current_function_has_return = False

# ---- Bloque de sentencias ----

def p_stmt_block_multi(p):
    '''stmt_block : stmt_block inner_statement'''
    pass

def p_stmt_block_empty(p):
    '''stmt_block : '''
    pass

def p_inner_statement(p):
    '''inner_statement : statement
                       | if_stmt
                       | while_stmt
                       | do_while_stmt
                       | break_stmt
                       | return_stmt
                       | print_stmt'''
    pass

# ---- Sentencias ----

def p_statement_decl(p):
    '''statement : decl_stmt SEMICOLON'''
    pass

def p_statement_assign(p):
    '''statement : assign_stmt SEMICOLON'''
    pass

def p_statement_expr(p):
    '''statement : expr SEMICOLON'''
    pass

def p_statement_semi(p):
    '''statement : SEMICOLON'''
    pass

# ---- Declaración ----

def p_decl_type_assign(p):
    '''decl_stmt : type ID ASSIGN expr'''
    vtype, vname = p[1], p[2]
    etype, eval_, equad = p[4]
    if not can_convert(etype, vtype):
        report_error(f"No se puede asignar '{etype}' a variable de tipo '{vtype}'.", p.lineno(3))
        declare_in_current_scope(vname, {'type': vtype, 'value': default_value(vtype)}, p.lineno(2))
        return
    cval, _ = apply_cast(eval_, etype, vtype)
    if equad:
        emit('ASSIGN', cval, '_', vname)
    declare_in_current_scope(vname, {'type': vtype, 'value': cval, 'quad': equad}, p.lineno(2))

def p_decl_type_only(p):
    '''decl_stmt : type ID'''
    vtype, vname = p[1], p[2]
    default = default_value(vtype)
    emit('ASSIGN', _literal(default, vtype), '_', vname)
    declare_in_current_scope(vname, {'type': vtype, 'value': default, 'quad': True}, p.lineno(2))

def p_decl_type_list(p):
    '''decl_stmt : type id_list'''
    vtype  = p[1]
    names  = p[2]
    for name in names:
        default = default_value(vtype)
        emit('ASSIGN', _literal(default, vtype), '_', name)
        declare_in_current_scope(name, {'type': vtype, 'value': default, 'quad': True})

def p_decl_record_assign(p):
    '''decl_stmt : ID ID ASSIGN expr'''
    vtype, vname = p[1], p[2]
    etype, eval_, _ = p[4]
    if vtype not in record_table:
        report_error(f"El tipo '{vtype}' no ha sido declarado.", p.lineno(1))
        return
    if etype != vtype:
        report_error(f"No se puede asignar tipo '{etype}' a variable de tipo registro '{vtype}'.", p.lineno(3))
    declare_in_current_scope(vname, {'type': vtype, 'value': eval_, 'quad': False}, p.lineno(2))

def p_decl_record_only(p):
    '''decl_stmt : ID ID'''
    vtype, vname = p[1], p[2]
    if vtype not in record_table:
        report_error(f"El tipo '{vtype}' no ha sido declarado.", p.lineno(1))
        return
    default = default_value(vtype)
    declare_in_current_scope(vname, {'type': vtype, 'value': default, 'quad': False}, p.lineno(2))

def p_id_list(p):
    '''id_list : id_list COMMA ID
               | ID COMMA ID'''
    if isinstance(p[1], list):
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1], p[3]]

# ---- Asignación ----

def p_assign_stmt(p):
    '''assign_stmt : lvalue ASSIGN expr'''
    lname, ltype, lquad = p[1]
    etype, eval_, equad = p[3]
    if ltype is None:
        return
    if not can_convert(etype, ltype):
        report_error(f"No se puede asignar '{etype}' a '{lname}' de tipo '{ltype}'.", p.lineno(2))
        return
    cval, _ = apply_cast(eval_, etype, ltype)
    if lquad and equad:
        emit('ASSIGN', cval, '_', lname)
    # Actualizar valor si es variable simple en algún scope
    sym = lookup_symbol(lname)
    if sym:
        sym['value'] = cval
        sym['quad'] = lquad and equad

def p_lvalue_id(p):
    '''lvalue : ID'''
    sym = lookup_symbol(p[1])
    if sym is None:
        report_error(f"La variable '{p[1]}' no ha sido declarada.", p.lineno(1))
        p[0] = (p[1], None, False)
    else:
        p[0] = (p[1], sym['type'], sym.get('quad', sym['type'] in BASIC_TYPES))

def p_lvalue_dot(p):
    '''lvalue : lvalue DOT ID'''
    lname, ltype, _ = p[1]
    fname = p[3]
    if ltype is None:
        p[0] = (f"{lname}.{fname}", None, False)
        return
    if ltype not in record_table:
        report_error(f"'{lname}' (tipo '{ltype}') no es un registro.")
        p[0] = (f"{lname}.{fname}", None, False)
        return
    matched = next((f for f in record_table[ltype] if f['name'] == fname), None)
    if matched is None:
        report_error(f"El registro '{ltype}' no tiene el campo '{fname}'.")
        p[0] = (f"{lname}.{fname}", None, False)
    else:
        p[0] = (f"{lname}.{fname}", matched['type'], False)

# ---- Tipos ----

def p_type(p):
    '''type : INT
            | FLOAT
            | CHAR
            | BOOLEAN'''
    p[0] = p[1]

# ---- Control de flujo ----

def p_if_stmt_simple(p):
    '''if_stmt : IF LPAREN cond_open expr RPAREN block_open stmt_block RBRACE'''
    ctype, cval, cquad = p[4]
    label_end = new_label()
    body_quartets = pop_quartet_buffer()
    cond_quartets = pop_quartet_buffer()
    pop_scope()
    if ctype != 'boolean':
        report_error(f"Condición del 'if' debe ser 'boolean', se encontró '{ctype}'.", p.lineno(1))
    if cquad:
        append_quartets(cond_quartets)
        emit('JUMPF', cval, label_end, '_')
        append_quartets(body_quartets)
        emit('LABEL', label_end, '_', '_')

def p_if_stmt_else(p):
    '''if_stmt : IF LPAREN cond_open expr RPAREN block_open stmt_block RBRACE ELSE block_open stmt_block RBRACE'''
    ctype, cval, cquad = p[4]
    label_else = new_label()
    label_end  = new_label()
    else_quartets = pop_quartet_buffer()
    if_quartets = pop_quartet_buffer()
    cond_quartets = pop_quartet_buffer()
    pop_scope()  # scope del else
    pop_scope()  # scope del if
    if ctype != 'boolean':
        report_error(f"Condición del 'if-else' debe ser 'boolean', se encontró '{ctype}'.", p.lineno(1))
    if cquad:
        append_quartets(cond_quartets)
        emit('JUMPF', cval, label_else, '_')
        append_quartets(if_quartets)
        emit('JUMP',  label_end, '_', '_')
        emit('LABEL', label_else, '_', '_')
        append_quartets(else_quartets)
        emit('LABEL', label_end,  '_', '_')

def p_while_stmt(p):
    '''while_stmt : WHILE LPAREN cond_open expr RPAREN loop_enter block_open stmt_block RBRACE loop_exit'''
    ctype, cval, cquad = p[4]
    label_start = new_label()
    label_end   = p[6]
    body_quartets = pop_quartet_buffer()
    cond_quartets = pop_quartet_buffer()
    pop_scope()
    if ctype != 'boolean':
        report_error(f"Condición del 'while' debe ser 'boolean', se encontró '{ctype}'.", p.lineno(1))
    if cquad:
        emit('LABEL', label_start, '_', '_')
        append_quartets(cond_quartets)
        emit('JUMPF', cval, label_end, '_')
        append_quartets(body_quartets)
        emit('JUMP',  label_start, '_', '_')
        emit('LABEL', label_end,   '_', '_')

def p_do_while_stmt(p):
    '''do_while_stmt : DO loop_enter block_open stmt_block RBRACE WHILE LPAREN cond_open expr RPAREN loop_exit'''
    ctype, cval, cquad = p[9]
    label_start = new_label()
    label_end = p[2]
    cond_quartets = pop_quartet_buffer()
    body_quartets = pop_quartet_buffer()
    pop_scope()
    if ctype != 'boolean':
        report_error(f"Condición del 'do-while' debe ser 'boolean', se encontró '{ctype}'.", p.lineno(6))
    if cquad:
        emit('LABEL', label_start, '_', '_')
        append_quartets(body_quartets)
        append_quartets(cond_quartets)
        emit('JUMPT', cval, label_start, '_')
        emit('LABEL', label_end, '_', '_')

def p_break_stmt(p):
    '''break_stmt : BREAK SEMICOLON'''
    if loop_depth <= 0:
        report_error("La sentencia 'break' solo puede aparecer dentro de un bucle.", p.lineno(1))
    elif emit_enabled_stack[-1]:
        emit('JUMP', loop_end_label_stack[-1], '_', '_')

def p_return_stmt(p):
    '''return_stmt : RETURN expr SEMICOLON'''
    global current_function_has_return
    etype, _, _ = p[2]

    if current_return_type is None:
        report_error("La sentencia 'return' solo puede aparecer dentro de una función.", p.lineno(1))
        return

    if current_return_type == 'void':
        report_error("Una función de tipo 'void' no puede devolver un valor.", p.lineno(1))
        return

    if not can_convert(etype, current_return_type):
        report_error(
            f"No se puede devolver '{etype}' en una función de tipo '{current_return_type}'.",
            p.lineno(1))
        return

    current_function_has_return = True

def p_print_stmt(p):
    '''print_stmt : PRINT LPAREN expr RPAREN SEMICOLON'''
    _, val, qok = p[3]
    if qok:
        emit('PRINT', val, '_', '_')

# ---- Expresiones binarias ----

ARITH_OP = {'+': 'ADD', '-': 'SUB', '*': 'MUL', '/': 'DIV'}
COMP_OP  = {'>': 'GT',  '>=': 'GTE', '<': 'LT', '<=': 'LTE', '==': 'EQ'}
LOGIC_OP = {'&&': 'AND', '||': 'OR'}

def p_expr_plus(p):
    '''expr : expr PLUS expr'''
    p[0] = _arith(p[1], '+', p[3], p.lineno(2))

def p_expr_minus(p):
    '''expr : expr MINUS expr'''
    p[0] = _arith(p[1], '-', p[3], p.lineno(2))

def p_expr_mult(p):
    '''expr : expr MULT expr'''
    p[0] = _arith(p[1], '*', p[3], p.lineno(2))

def p_expr_div(p):
    '''expr : expr DIV expr'''
    p[0] = _arith(p[1], '/', p[3], p.lineno(2))

def _arith(left, op, right, lineno):
    t1, v1, q1 = left
    t2, v2, q2 = right
    allowed = ('int', 'float') if op in ('*', '/') else ('int', 'float', 'char')
    common  = unify_types(t1, t2)
    if common is None or common not in allowed:
        report_error(f"Operación '{op}' no permitida entre '{t1}' y '{t2}'.", lineno)
        return ('int', new_temp(), False)
    v1c, _ = apply_cast(v1, t1, common)
    v2c, _ = apply_cast(v2, t2, common)
    t = new_temp()
    qok = q1 and q2
    if qok:
        emit(ARITH_OP[op], v1c, v2c, t)
    return (common, t, qok)

def p_expr_gt(p):
    '''expr : expr GT expr'''
    p[0] = _compare(p[1], '>', p[3], p.lineno(2))

def p_expr_ge(p):
    '''expr : expr GE expr'''
    p[0] = _compare(p[1], '>=', p[3], p.lineno(2))

def p_expr_lt(p):
    '''expr : expr LT expr'''
    p[0] = _compare(p[1], '<', p[3], p.lineno(2))

def p_expr_le(p):
    '''expr : expr LE expr'''
    p[0] = _compare(p[1], '<=', p[3], p.lineno(2))

def p_expr_eq(p):
    '''expr : expr EQ expr'''
    p[0] = _compare(p[1], '==', p[3], p.lineno(2))

def _compare(left, op, right, lineno):
    t1, v1, q1 = left
    t2, v2, q2 = right
    allowed = ('int', 'float', 'char', 'boolean') if op == '==' else ('int', 'float', 'char')
    if t1 == 'boolean' and t2 == 'boolean' and op == '==':
        common = 'boolean'
    else:
        common = unify_types(t1, t2)
    if common is None or common not in allowed:
        report_error(f"Operación '{op}' no permitida entre '{t1}' y '{t2}'.", lineno)
        return ('boolean', new_temp(), False)
    v1c, _ = apply_cast(v1, t1, common)
    v2c, _ = apply_cast(v2, t2, common)
    t = new_temp()
    qok = q1 and q2
    if qok:
        emit(COMP_OP[op], v1c, v2c, t)
    return ('boolean', t, qok)

def p_expr_and(p):
    '''expr : expr AND expr'''
    p[0] = _logic(p[1], '&&', p[3], p.lineno(2))

def p_expr_or(p):
    '''expr : expr OR expr'''
    p[0] = _logic(p[1], '||', p[3], p.lineno(2))

def _logic(left, op, right, lineno):
    t1, v1, q1 = left
    t2, v2, q2 = right
    if t1 != 'boolean' or t2 != 'boolean':
        report_error(f"Operación '{op}' solo entre 'boolean', encontrado '{t1}' y '{t2}'.", lineno)
        return ('boolean', new_temp(), False)
    t = new_temp()
    qok = q1 and q2
    if qok:
        emit(LOGIC_OP[op], v1, v2, t)
    return ('boolean', t, qok)

# ---- Expresiones unarias ----

def p_expr_uminus(p):
    '''expr : MINUS expr %prec UMINUS'''
    etype, eval_, qok = p[2]
    if etype not in ('int', 'float', 'char'):
        report_error(f"Operador '-' unario no permitido para tipo '{etype}'.", p.lineno(1))
        p[0] = (etype, eval_, False)
        return
    t = new_temp()
    if qok:
        emit('UMINUS', eval_, '_', t)
    p[0] = (etype, t, qok)

def p_expr_uplus(p):
    '''expr : PLUS expr %prec UPLUS'''
    etype, eval_, qok = p[2]
    if etype not in ('int', 'float', 'char'):
        report_error(f"Operador '+' unario no permitido para tipo '{etype}'.", p.lineno(1))
        p[0] = (etype, eval_, False)
        return
    t = new_temp()
    if qok:
        emit('UPLUS', eval_, '_', t)
    p[0] = (etype, t, qok)

def p_expr_not(p):
    '''expr : NOT expr'''
    etype, eval_, qok = p[2]
    if etype != 'boolean':
        report_error(f"Operador '!' solo para 'boolean', encontrado '{etype}'.", p.lineno(1))
        p[0] = ('boolean', eval_, False)
        return
    t = new_temp()
    if qok:
        emit('NOT', eval_, '_', t)
    p[0] = ('boolean', t, qok)

# ---- Agrupación ----

def p_expr_paren(p):
    '''expr : LPAREN expr RPAREN'''
    p[0] = p[2]

# ---- new (instanciación de registro) ----

def p_expr_new(p):
    '''expr : NEW ID LPAREN arg_list RPAREN'''
    rname = p[2]
    args  = p[4]
    if rname not in record_table:
        report_error(f"El registro '{rname}' no ha sido declarado.", p.lineno(2))
        p[0] = (rname, {}, False)
        return
    fields = record_table[rname]
    if len(args) != len(fields):
        report_error(
            f"Constructor de '{rname}' espera {len(fields)} argumento(s), se pasaron {len(args)}.",
            p.lineno(2))
    instance = {}
    for i, field in enumerate(fields):
        if i < len(args):
            atype, aval, _ = args[i]
            if not can_convert(atype, field['type']):
                report_error(
                    f"Campo '{field['name']}' de '{rname}' es '{field['type']}', se pasó '{atype}'.",
                    p.lineno(2))
            instance[field['name']] = aval
        else:
            instance[field['name']] = default_value(field['type'])
    p[0] = (rname, instance, False)

# ---- Llamada a función ----

def p_expr_call(p):
    '''expr : ID LPAREN arg_list RPAREN'''
    fname = p[1]
    args  = p[3]
    if fname not in function_table:
        report_error(f"La función '{fname}' no ha sido declarada.", p.lineno(1))
        p[0] = ('int', new_temp(), False)
        return
    arg_types = [a[0] for a in args]
    sig = _resolve_overload(fname, arg_types, p.lineno(1))
    if sig is None:
        p[0] = ('int', new_temp(), False)
        return
    t = new_temp()
    p[0] = (sig['return_type'], t, False)

def _resolve_overload(fname, arg_types, lineno):
    sigs = function_table[fname]
    # Búsqueda exacta (sin conversión)
    for sig in sigs:
        if [p['type'] for p in sig['params']] == arg_types:
            return sig
    # Búsqueda con conversión automática
    candidates = [
        sig for sig in sigs
        if len(sig['params']) == len(arg_types)
        and all(can_convert(at, pt)
                for at, pt in zip(arg_types, [p['type'] for p in sig['params']]))
    ]
    if len(candidates) == 1:
        return candidates[0]
    if len(candidates) > 1:
        report_error(f"Llamada ambigua a '{fname}' con argumentos {arg_types}.", lineno)
        return candidates[0]
    report_error(f"No hay firma de '{fname}' compatible con argumentos {arg_types}.", lineno)
    return None

# ---- lvalue como expresión ----

def p_expr_lvalue(p):
    '''expr : lvalue'''
    lname, ltype, lquad = p[1]
    p[0] = ('int', lname, False) if ltype is None else (ltype, lname, lquad)

# ---- Literales ----

def p_expr_int(p):
    '''expr : INT_VALUE'''
    p[0] = ('int', p[1], True)

def p_expr_float(p):
    '''expr : FLOAT_VALUE'''
    p[0] = ('float', p[1], True)

def p_expr_char(p):
    '''expr : CHAR_VALUE'''
    p[0] = ('char', p[1], True)

def p_expr_true(p):
    '''expr : TRUE'''
    p[0] = ('boolean', True, True)

def p_expr_false(p):
    '''expr : FALSE'''
    p[0] = ('boolean', False, True)

# ---- Lista de argumentos ----

def p_arg_list_multi(p):
    '''arg_list : arg_list COMMA expr'''
    p[0] = p[1] + [p[3]]

def p_arg_list_single(p):
    '''arg_list : expr'''
    p[0] = [p[1]]

def p_arg_list_empty(p):
    '''arg_list : '''
    p[0] = []

# ---- Marcadores auxiliares de cuartetos ----

def p_cond_open(p):
    '''cond_open : '''
    push_quartet_buffer()

# ---- Marcadores auxiliares de contexto ----

def p_loop_enter(p):
    '''loop_enter : '''
    global loop_depth
    loop_depth += 1
    label_end = new_label()
    loop_end_label_stack.append(label_end)
    p[0] = label_end

def p_loop_exit(p):
    '''loop_exit : '''
    global loop_depth
    loop_depth -= 1
    if loop_end_label_stack:
        loop_end_label_stack.pop()

# ---- Error sintáctico ----

def p_error(p):
    global has_errors
    has_errors = True
    if p:
        print(f"[ERROR SINTÁCTICO] Token '{p.type}' inesperado en la línea {p.lineno}")
    else:
        print("[ERROR SINTÁCTICO] Error al final del fichero")

# =============================================================================
# CONSTRUCCIÓN DEL PARSER
# =============================================================================

parser = yacc.yacc()

# =============================================================================
# FUNCIÓN PRINCIPAL DE ANÁLISIS
# =============================================================================

def analyze(source, input_filename):
    """
    Analiza el código fuente completo (léxico + sintáctico + semántico).
    Genera los archivos de salida si no hay errores.
    Devuelve True si el análisis fue correcto, False si hubo errores.
    """
    global symbol_table, scope_stack, record_table, function_table
    global quartets, quartet_buffers, emit_enabled_stack, _temp_counter, _label_counter
    global has_errors, semantic_errors, current_return_type, pending_function_return_type
    global current_function_has_return, _pending_params, loop_depth, loop_end_label_stack

    # Reset completo del estado
    symbol_table      = {}
    scope_stack       = []
    record_table      = {}
    function_table    = {}
    quartets          = []
    quartet_buffers   = [quartets]
    emit_enabled_stack = [True]
    _temp_counter     = 0
    _label_counter    = 0
    has_errors        = False
    semantic_errors   = []
    current_return_type = None
    pending_function_return_type = None
    current_function_has_return = False
    _pending_params   = []
    loop_depth        = 0
    loop_end_label_stack = []

    from lexer import lexer
    lexer.source = source
    lexer.lineno = 1

    parser.parse(source, lexer=lexer)

    if not has_errors:
        _write_symbols(input_filename)
        _write_records(input_filename)
        _write_functions(input_filename)
        _write_quartets(input_filename)

    return not has_errors

# =============================================================================
# ESCRITURA DE ARCHIVOS DE SALIDA
# =============================================================================

def _has_control_or_functions():
    return bool(function_table) or any(
        q[0] in ('JUMP', 'JUMPT', 'JUMPF') for q in quartets
    )

def _format_value(value, vtype):
    if vtype == 'boolean': return 'true' if value else 'false'
    if vtype == 'char':    return repr(value) if value else "''"
    if isinstance(value, dict):
        inner = ','.join(f"{k}:{_format_value(v, '')}" for k, v in value.items())
        return '{' + inner + '}'
    return str(value)

def _write_symbols(filename):
    base = filename.rsplit('.', 1)[0]
    complex_prog = _has_control_or_functions()
    with open(base + '.symbols', 'w', encoding='utf-8') as f:
        for name, info in symbol_table.items():
            if complex_prog:
                f.write(f"{name}:{info['type']}\n")
            else:
                val = _format_value(info['value'], info['type'])
                f.write(f"{name}:{info['type']},{val}\n")

def _write_records(filename):
    base = filename.rsplit('.', 1)[0]
    with open(base + '.records', 'w', encoding='utf-8') as f:
        for rname, fields in record_table.items():
            fstr = ', '.join(f"{fd['name']}:{fd['type']}" for fd in fields)
            f.write(f"{rname}:[{fstr}]\n")

def _write_functions(filename):
    base = filename.rsplit('.', 1)[0]
    with open(base + '.functions', 'w', encoding='utf-8') as f:
        for fname, sigs in function_table.items():
            for sig in sigs:
                pstr = ', '.join(f"{p['name']}:{p['type']}" for p in sig['params'])
                f.write(f"{fname}:[{pstr}],{sig['return_type']}\n")

def _write_quartets(filename):
    base = filename.rsplit('.', 1)[0]
    with open(base + '.quartets', 'w', encoding='utf-8') as f:
        for q in quartets:
            f.write(','.join(q) + '\n')
