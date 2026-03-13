import ply.yacc as yacc
from lexer import tokens

# -----------------------------
# Precedencia de operadores
# -----------------------------
precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQ'),
    ('left', 'GT', 'GE', 'LT', 'LE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIV'),
    ('right', 'UMINUS', 'UPLUS', 'NOT'),
    ('left', 'DOT'),
)

# -----------------------------
# Programa
# -----------------------------
def p_program(p):
    '''program : statement_list'''
    pass

def p_statement_list(p):
    '''statement_list : statement_list statement
                      | statement_list function_def
                      | statement_list record_def
                      | statement_list if_stmt
                      | statement_list while_stmt
                      | statement_list do_while_stmt
                      | statement_list print_stmt
                      | '''
    pass

# -----------------------------
# Definición de registro
# -----------------------------
def p_record_def(p):
    '''record_def : RECORD ID LPAREN field_list RPAREN SEMICOLON'''
    pass

def p_field_list(p):
    '''field_list : field_list COMMA field
                  | field'''
    pass

def p_field(p):
    '''field : type ID
             | ID ID'''
    pass

# -----------------------------
# Definición de función
# -----------------------------
def p_function_def(p):
    '''function_def : type ID LPAREN param_list RPAREN LBRACE stmt_block RBRACE
                    | VOID ID LPAREN param_list RPAREN LBRACE stmt_block RBRACE
                    | ID ID LPAREN param_list RPAREN LBRACE stmt_block RBRACE'''
    pass

def p_param_list(p):
    '''param_list : param_list COMMA param
                  | param
                  | '''
    pass

def p_param(p):
    '''param : type ID
             | ID ID'''
    pass

# -----------------------------
# Bloque de sentencias dentro de llaves
# -----------------------------
def p_stmt_block(p):
    '''stmt_block : stmt_block inner_statement
                  | '''
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

# -----------------------------
# Sentencias
# -----------------------------
def p_statement(p):
    '''statement : decl_stmt SEMICOLON
                 | assign_stmt SEMICOLON
                 | expr SEMICOLON
                 | SEMICOLON'''
    pass

def p_decl_stmt(p):
    '''decl_stmt : type ID ASSIGN expr
                 | type ID
                 | type id_list
                 | ID ID ASSIGN expr
                 | ID ID'''
    pass

def p_id_list(p):
    '''id_list : id_list COMMA ID
               | ID COMMA ID'''
    pass

def p_assign_stmt(p):
    '''assign_stmt : lvalue ASSIGN expr'''
    pass

def p_lvalue(p):
    '''lvalue : ID
              | lvalue DOT ID'''
    pass

# -----------------------------
# Tipos básicos
# -----------------------------
def p_type(p):
    '''type : INT
            | FLOAT
            | CHAR
            | BOOLEAN'''
    pass

# -----------------------------
# Control de flujo
# -----------------------------
def p_if_stmt(p):
    '''if_stmt : IF LPAREN expr RPAREN LBRACE stmt_block RBRACE
               | IF LPAREN expr RPAREN LBRACE stmt_block RBRACE ELSE LBRACE stmt_block RBRACE'''
    pass

def p_while_stmt(p):
    '''while_stmt : WHILE LPAREN expr RPAREN LBRACE stmt_block RBRACE'''
    pass

def p_do_while_stmt(p):
    '''do_while_stmt : DO LBRACE stmt_block RBRACE WHILE LPAREN expr RPAREN'''
    pass

def p_break_stmt(p):
    '''break_stmt : BREAK SEMICOLON'''
    pass

def p_return_stmt(p):
    '''return_stmt : RETURN expr SEMICOLON'''
    pass

def p_print_stmt(p):
    '''print_stmt : PRINT LPAREN expr RPAREN SEMICOLON'''
    pass

# -----------------------------
# Expresiones
# -----------------------------
def p_expr_binary(p):
    '''expr : expr PLUS expr
            | expr MINUS expr
            | expr MULT expr
            | expr DIV expr
            | expr AND expr
            | expr OR expr
            | expr GT expr
            | expr GE expr
            | expr LT expr
            | expr LE expr
            | expr EQ expr'''
    pass

def p_expr_unary(p):
    '''expr : MINUS expr %prec UMINUS
            | PLUS expr %prec UPLUS
            | NOT expr'''
    pass

def p_expr_paren(p):
    '''expr : LPAREN expr RPAREN'''
    pass

def p_expr_new(p):
    '''expr : NEW ID LPAREN arg_list RPAREN'''
    pass

def p_expr_call(p):
    '''expr : ID LPAREN arg_list RPAREN'''
    pass

def p_expr_lvalue(p):
    '''expr : lvalue'''
    pass

def p_expr_value(p):
    '''expr : INT_VALUE
            | FLOAT_VALUE
            | CHAR_VALUE
            | TRUE
            | FALSE'''
    pass

# -----------------------------
# Lista de argumentos
# -----------------------------
def p_arg_list(p):
    '''arg_list : arg_list COMMA expr
                | expr
                | '''
    pass

# -----------------------------
# Error
# -----------------------------
def p_error(p):
    if p:
        print(f"[ERROR] Token '{p.type}' inesperado en la línea {p.lineno}")
    else:
        print("[ERROR] Error sintáctico al final del fichero")

parser = yacc.yacc()