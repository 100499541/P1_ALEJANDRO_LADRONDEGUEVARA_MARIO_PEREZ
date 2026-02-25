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
# Operadores ambiguos: definidos como funciones para garantizar orden de matching
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

    # Detectar errores según el tipo
    # Error específico: falta parte entera
    if lexeme.startswith('.'):
        print(f"Error léxico: falta parte entera en '{lexeme}' en línea {t.lineno}")
        return None

    # Error específico: falta parte decimal
    if '.' in lexeme:
        # Separa el número en parte entera y decimal, y solo se queda con la decimal, la de la derecha del punto
        parte_decimal = lexeme.split('.')[1]
        # Separa la parte decimal y la del exponente (si esta existe), y solo se queda con la decimal, la de la izquierda de la e 
        parte_decimal_sin_exp = parte_decimal.split('e')[0]
        # Si no hay parte decimal, salta un error
        if not parte_decimal_sin_exp:
            print(f"Error léxico: falta parte decimal en '{lexeme}' en línea {t.lineno}")
            return None

    # Error específico: potencia incompleta
    if 'e' in lexeme:
        # Separa el número y el exponente, y solo se queda con el exponente, lo de la derecha de la e
        potencia = lexeme.split('e')[1]
        # Si no hay potencia o la potencia es solo un signo, salta un error
        if not potencia or potencia in ('+', '-'):
            print(f"Error léxico: potencia incompleta en '{lexeme}' en línea {t.lineno}")
            return None
        # Quita el signo de la potencia para comprobar que el resto son dígitos y no hay parte decimal
        potencia_digits = potencia.lstrip('+-')
        # Si hay un punto en la parte de la potencia, salta un error porque la potencia debe ser un entero
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
    # Errores para binario, si empieza con '0b'
    if lexeme.startswith('0b'):
        # Quita el prefijo '0b' para analizar solo los dígitos
        digits = lexeme[2:]
        # Si no hay dígitos después del '0b', salta un error porque el literal binario está incompleto
        if not digits:
            print(f"Error léxico: literal binario incompleto '0b' en línea {t.lineno}")
            return None
        # Si hay caracteres que no son '0' o '1' después del '0b', salta un error porque el literal binario es inválido
        if not all(c in '01' for c in digits):
            print(f"Error léxico: literal binario inválido '{lexeme}' en línea {t.lineno}")
            return None
        t.value = int(digits, 2)

    # Errores para hexadecimal, si empieza con '0x'
    elif lexeme.startswith('0x'):
        # Quita el prefijo '0x' para analizar solo los dígitos y letras hexadecimales
        digits = lexeme[2:]
        # Si no hay dígitos ni letras después del '0x', salta un error porque el literal hexadecimal está incompleto
        if not digits:
            print(f"Error léxico: literal hexadecimal incompleto '0x' en línea {t.lineno}")
            return None
        # Si hay caracteres que no son dígitos hexadecimales después del '0x', salta un error porque el literal hexadecimal es inválido
        if not all(c in '0123456789ABCDEF' for c in digits):
            print(f"Error léxico: literal hexadecimal inválido '{lexeme}' en línea {t.lineno}")
            return None
        t.value = int(digits, 16)

    # Errores octal y ceros no significativos, si empieza con '0' pero no es solo '0'
    elif lexeme.startswith('0') and len(lexeme) > 1:
        # Quita el prefijo '0' para analizar solo los dígitos octales
        digits = lexeme[1:]
        # Si el primer dígito después del '0' es otro '0', salta un error porque no se permiten ceros no significativos en los literales octales
        if digits[0] == '0':
            print(f"Error léxico: ceros no significativos '{lexeme}' en línea {t.lineno}")
            return None
        # Si hay caracteres que no son dígitos octales después del '0', salta un error porque el literal octal es inválido
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
    # Quita las comillas para analizar el contenido del char
    contenido = lexeme[1:-1]

    # Detectar errores según el tipo
    # Si el contenido del char está vacío, salta un error porque un char debe contener exactamente un carácter
    if len(contenido) == 0:
        print(f"Error léxico: char vacío '''' en línea {t.lineno}")
        return None

    # Si el contenido del char tiene más de un carácter, salta un error porque un char debe contener exactamente un carácter
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
    pass  # Ignorar

def t_COMMENT_SINGLELINE(t):
    r'//[^\n]*'
    pass  # Ignorar

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

# Función auxiliar para encontrar la columna de cada token
def find_column(source, token):
    # Calcula la columna del token en la línea actual
    last_cr = source.rfind('\n', 0, token.lexpos)
    if last_cr < 0:
        last_cr = -1
    return token.lexpos - last_cr - 1

# Función para obtener lexema original
def get_lexeme(tok, source):
    # Devuelve el texto original del token tal como aparece en el fuente
    # Para poder mostrarlo correctamente en el fichero de salida
    # Si el token tiene lexema original guardado, lo devuelve
    if hasattr(tok, 'lexeme'):
        return tok.lexeme
    # Sino, devuelve directamente el valor del token convertido a string
    return str(tok.value)

# Función para obtener longitud original en el fuente
def get_original_length(tok, source):
    # Buscamos el siguiente salto de línea o fin para acotar
    line_end = source.find('\n', tok.lexpos)
    # Si no hay salto de línea, toma el final del archivo como límite
    if line_end == -1:
        line_end = len(source)
    # Extrae el texto desde la posición del token hasta el fin de su línea
    segment = source[tok.lexpos:line_end]

    # Para token INT_VALUE, aplica un regex estricto para encontrar el lexema original del entero
    if tok.type == 'INT_VALUE':
        import re
        # Busca el lexema original del entero en el segmento
        m = re.match(r'0b[01]+|0x[0-9A-F]+|0[0-7]+|0|[1-9][0-9]*', segment)
        return len(m.group(0)) if m else len(str(tok.value))
    # Para token FLOAT_VALUE, aplica un regex estricto para encontrar el lexema original del float
    elif tok.type == 'FLOAT_VALUE':
        import re
        # Busca el lexema original del float en el segmento
        m = re.match(r'\d+\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+', segment)
        return len(m.group(0)) if m else len(str(tok.value))
    # Para token CHAR_VALUE, siempre devuelve 3, porque el lexema original de un char siempre tiene la forma 'x'
    elif tok.type == 'CHAR_VALUE':
        # Siempre 3 caracteres, por ejemplo 'x'
        return 3
    # Para el resto de tokens, la longitud del valor en texto es la longitud real
    else:
        return len(str(tok.value))