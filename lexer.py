import ply.lex as lex

# -----------------------------
# Palabras reservadas
# -----------------------------
reserved = {
    'true': 'TRUE', 'false': 'FALSE',
    'int': 'INT', 'float': 'FLOAT', 'char': 'CHAR', 'boolean': 'BOOLEAN', 'void': 'VOID',
    'return': 'RETURN', 'if': 'IF', 'else': 'ELSE', 'do': 'DO', 'while': 'WHILE',
    'print': 'PRINT', 'new': 'NEW', 'record': 'RECORD', 'break': 'BREAK'
}

# -----------------------------
# Lista de tokens
# -----------------------------
tokens = [
    'ID',
    'INT_VALUE',
    'FLOAT_VALUE',
    'CHAR_VALUE',
    # Operadores
    'PLUS', 'MINUS', 'MULT', 'DIV',
    'AND', 'OR', 'NOT',
    'GT', 'GE', 'LT', 'LE', 'EQ',
    'ASSIGN',
    # Delimitadores
    'SEMICOLON', 'COMMA', 'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'DOT'
] + list(reserved.values())

# -----------
# Operadores
# -----------
def t_GE(t):
    r'>='
    return t

def t_LE(t):
    r'<='
    return t

def t_EQ(t):
    r'=='
    return t

def t_AND(t):
    r'&&'
    return t

def t_OR(t):
    r'\|\|'
    return t

# -----------------------------
# Expresiones regulares de tokens simples
# -----------------------------
t_PLUS      = r'\+'
t_MINUS     = r'-'
t_MULT      = r'\*'
t_DIV       = r'/'
t_NOT       = r'!'
t_GT        = r'>'
t_LT        = r'<'
t_ASSIGN    = r'='
t_SEMICOLON = r';'
t_COMMA     = r','
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LBRACE    = r'\{'
t_RBRACE    = r'\}'
t_DOT       = r'\.'

# -----------------------------
# Literales e identificadores
# -----------------------------
def t_FLOAT_VALUE(t):
    r'\d+\.\d*([e][+-]?\d*(\.\d+)?)?|\d+[e][+-]?\d*(\.\d+)?|\.\d+'
    lexeme = t.value

    # Error especifico: falta para entera
    if lexeme.startswith('.'):
        print(f"Error léxico: falta parte entera en '{lexeme}' en línea {t.lineno}")
        return None

    # Error especifico: falta para decimal
    if '.' in lexeme:
        parte_decimal = lexeme.split('.')[1]
        # Quitar el exponente si lo hay para verificar solo la parte decimal
        parte_decimal_sin_exp = parte_decimal.split('e')[0]
        if not parte_decimal_sin_exp:
            print(f"Error léxico: falta parte decimal en '{lexeme}' en línea {t.lineno}")
            return None

    # Error especifico: potencia incompleta
    if 'e' in lexeme:
        potencia = lexeme.split('e')[1]
        if not potencia or potencia in ('+', '-'):
            print(f"Error léxico: potencia incompleta en '{lexeme}' en línea {t.lineno}")
            return None
        # Error especifico: potencia con decimales
        potencia_digits = potencia.lstrip('+-')
        if '.' in potencia_digits:
            print(f"Error léxico: potencia debe ser entera en '{lexeme}' en línea {t.lineno}")
            return None

    t.lexeme = lexeme
    t.value = float(lexeme)
    return t

def t_INT_VALUE(t):
    r'0b[01]*[^01\s;,\)\}]?[0-9a-zA-Z]*|0x[0-9A-F]*[^0-9A-F\s;,\)\}]?[0-9a-zA-Z]*|0[0-7]*[89]?[0-9a-zA-Z]*|0|[1-9][0-9]*'
    lexeme = t.value

    # Detectar errores según el tipo
    if lexeme.startswith('0b'):
        digits = lexeme[2:]
        if not digits:
            print(f"Error léxico: literal binario incompleto '0b' en línea {t.lineno}")
            return None
        if not all(c in '01' for c in digits):
            print(f"Error léxico: literal binario inválido '{lexeme}' en línea {t.lineno}")
            return None
        t.value = int(digits, 2)

    elif lexeme.startswith('0x'):
        digits = lexeme[2:]
        if not digits:
            print(f"Error léxico: literal hexadecimal incompleto '0x' en línea {t.lineno}")
            return None
        # Solo mayúsculas según la especificación [0-9A-F]
        if not all(c in '0123456789ABCDEF' for c in digits):
            print(f"Error léxico: literal hexadecimal inválido '{lexeme}' en línea {t.lineno}")
            return None
        t.value = int(digits, 16)

    elif lexeme.startswith('0') and len(lexeme) > 1:
        digits = lexeme[1:]
        # Ceros no significativos: el primer dígito octal no puede ser 0
        if digits[0] == '0':
            print(f"Error léxico: ceros no significativos '{lexeme}' en línea {t.lineno}")
            return None
        if not all(c in '01234567' for c in digits):
            print(f"Error léxico: literal octal inválido '{lexeme}' en línea {t.lineno}")
            return None
        t.value = int(digits, 8)

    else:
        t.value = int(lexeme)

    t.lexeme = lexeme
    return t

def t_CHAR_VALUE(t):
    r"'[^']*'"
    lexeme = t.value
    contenido = lexeme[1:-1]  # quitar comillas

    # Char vacío
    if len(contenido) == 0:
        print(f"Error léxico: char vacío '''' en línea {t.lineno}")
        return None

    # Más de un carácter
    if len(contenido) > 1:
        print(f"Error léxico: char con múltiples caracteres '{lexeme}' en línea {t.lineno}")
        return None

    t.lexeme = lexeme
    t.value = contenido
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    t.lexeme = t.value
    return t

# -----------------------------
# Comentarios
# -----------------------------
def t_COMMENT_MULTILINE(t):
    r'/\*[\s\S]*?\*/'
    t.lexer.lineno += t.value.count('\n')
    pass  # ignorar

def t_COMMENT_SINGLELINE(t):
    r'//[^\n]*'
    pass  # ignorar

# -----------------------------
# Saltos de línea
# -----------------------------
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# -----------------------------
# Espacios, tabulaciones y retorno de carro
# -----------------------------
t_ignore = ' \t\r'

# -----------------------------
# Errores
# -----------------------------
def t_error(t):
    print(f"Caracter ilegal '{t.value[0]}' en la linea {t.lineno}")
    t.lexer.skip(1)

# Construir el lexer
lexer = lex.lex()

# Función auxiliar para encontrar la columna de cada token
def find_column(source, token):
    last_cr = source.rfind('\n', 0, token.lexpos)
    if last_cr < 0:
        last_cr = -1
    return token.lexpos - last_cr - 1

# Función para obtener lexema original
def get_lexeme(tok, source):
    """Devuelve el texto original del token tal como aparece en el fuente."""
    if hasattr(tok, 'lexeme'):
        return tok.lexeme
    return str(tok.value)

# Función para obtener longitud original en el fuente
def get_original_length(tok, source):
    """Calcula la longitud real del token en el texto fuente."""
    col_start = find_column(source, tok)
    # Buscamos el siguiente salto de línea o fin para acotar
    line_end = source.find('\n', tok.lexpos)
    if line_end == -1:
        line_end = len(source)
    segment = source[tok.lexpos:line_end]

    if tok.type == 'INT_VALUE':
        import re
        m = re.match(r'0b[01]+|0x[0-9A-F]+|0[0-7]+|0|[1-9][0-9]*', segment)
        return len(m.group(0)) if m else len(str(tok.value))
    elif tok.type == 'FLOAT_VALUE':
        import re
        m = re.match(r'\d+\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+', segment)
        return len(m.group(0)) if m else len(str(tok.value))
    elif tok.type == 'CHAR_VALUE':
        return 3  # 'x' siempre 3 caracteres
    else:
        return len(str(tok.value))