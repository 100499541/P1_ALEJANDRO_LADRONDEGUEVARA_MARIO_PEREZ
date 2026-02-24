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
t_PLUS       = r'\+'
t_MINUS      = r'-'
t_MULT       = r'\*'
t_DIV        = r'/'
t_AND        = r'&&'
t_OR         = r'\|\|'
t_NOT        = r'!'
t_GT         = r'>'
t_GE         = r'>='
t_LT         = r'<'
t_LE         = r'<='
t_EQ         = r'=='
t_ASSIGN     = r'='
t_SEMICOLON  = r';'
t_COMMA      = r','
t_LPAREN     = r'\('
t_RPAREN     = r'\)'
t_LBRACE     = r'\{'
t_RBRACE     = r'\}'
t_DOT        = r'\.'

# -----------------------------
# Literales y identificadores
# -----------------------------
def t_FLOAT_VALUE(t):
    r'\d+\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+'
    t.value = float(t.value)
    return t

def t_INT_VALUE(t):
    r'0b[01]+|0x[0-9A-Fa-f]+|0[0-7]+|\d+'
    s = t.value
    if s.startswith('0b'):
        t.value = int(s[2:], 2) # Binario
    elif s.startswith('0x'):
        t.value = int(s[2:], 16) # Hexadecimal
    elif s.startswith('0') and len(s) > 1:
        t.value = int(s[1:], 8)  # Octal
    else:
        t.value = int(s)
    return t

def t_CHAR_VALUE(t):
    r"'.'"
    t.value = t.value[1]  # quita las comillas
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')  # Palabra reservada o ID
    return t

# -----------------------------
# Comentarios
# -----------------------------
def t_COMMENT_SINGLELINE(t):
    r'//.*'
    pass  # ignorar

def t_COMMENT_MULTILINE(t):
    r'/\*[\s\S]*?\*/'
    t.lexer.lineno += t.value.count('\n')
    pass  # ignorar

# -----------------------------
# Saltos de línea
# -----------------------------
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# -----------------------------
# Espacios y tabulaciones
# -----------------------------
t_ignore  = ' \t'

# -----------------------------
# Errores
# -----------------------------
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lexer.lineno}")
    t.lexer.skip(1)

# -----------------------------
# Construir el lexer
# -----------------------------
lexer = lex.lex()

# -----------------------------
# Función auxiliar para columna
# -----------------------------
def find_column(input, token):
    last_cr = input.rfind('\n', 0, token.lexpos)
    if last_cr < 0:
        last_cr = -1
    return token.lexpos - last_cr - 1
