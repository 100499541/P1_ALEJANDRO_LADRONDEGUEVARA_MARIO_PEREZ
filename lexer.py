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

# -----------------------------
# Expresiones regulares de tokens simples
# -----------------------------
t_GE        = r'>='
t_LE        = r'<='
t_EQ        = r'=='
t_AND       = r'&&'
t_OR        = r'\|\|'
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
# Función auxiliar para calcular columnas
# -----------------------------
def find_column(token, source=None):
    if source is None:
        source = token.lexer.source
    last_cr = source.rfind('\n', 0, token.lexpos)
    if last_cr < 0:
        last_cr = -1
    return token.lexpos - last_cr - 1

def set_columns(t, lexeme=None):
    if lexeme is None:
        lexeme = t.value
    t.col_start = find_column(t)
    t.col_end = t.col_start + len(lexeme)

# -----------------------------
# Literales e identificadores
# -----------------------------
def t_FLOAT_VALUE(t):
    r'\d+\.\d+([e][+-]?\d+)?|\d+[e][+-]?\d+'
    lexeme = t.value
    t.raw_value = lexeme
    set_columns(t, lexeme)
    t.value = float(t.value)
    return t

def t_INT_VALUE(t):
    r'0b[01]+|0x[0-9A-F]+|0[0-7]*|0|[1-9][0-9]*'
    lexeme = t.value
    t.raw_value = lexeme
    set_columns(t, lexeme)
    if lexeme.startswith('0b'):
        t.value = int(lexeme[2:], 2)
    elif lexeme.startswith('0x'):
        t.value = int(lexeme[2:], 16)
    elif lexeme.startswith('0') and len(lexeme) > 1:
        t.value = int(lexeme[1:], 8)
    else:
        t.value = int(lexeme)
    return t

def t_CHAR_VALUE(t):
    r"'[^']'"
    t.raw_value = t.value
    set_columns(t, t.raw_value)
    t.value = t.value[1:-1]
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.raw_value = t.value
    set_columns(t, t.raw_value)
    t.type = reserved.get(t.value, 'ID')
    if t.type == 'TRUE':
        t.value = True
    elif t.type == 'FALSE':
        t.value = False
    return t

# -----------------------------
# Comentarios
# -----------------------------
def t_COMMENT_MULTILINE(t):
    r'/\*[\s\S]*?\*/'
    t.lexer.lineno += t.value.count('\n')
    pass

def t_COMMENT_SINGLELINE(t):
    r'//[^\n]*'
    pass

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
    print(f"Carácter ilegal '{t.value[0]}' en la línea {t.lineno}")
    t.lexer.skip(1)

# Construir el lexer
lexer = lex.lex()
