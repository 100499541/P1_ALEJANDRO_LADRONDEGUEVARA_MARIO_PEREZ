import sys
from lexer import lexer, find_column

def run_lexer(filename):
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = f.read()
    except FileNotFoundError:
        print(f"No se encontró el archivo {filename}")
        sys.exit(1)

    lexer.input(data)
    lexer.source = data

    output_file = filename.rsplit('.', 1)[0] + '.token'
    with open(output_file, 'w', encoding='utf-8') as f_out:
        while True:
            tok = lexer.token()
            if not tok:
                break

            if not hasattr(tok, 'col_start'):
                tok.col_start = find_column(tok, data)
                tok.col_end = tok.col_start + len(str(tok.value))

            # Escribe el conjunto { TIPO, VALOR, LÍNEA, COLUMNA-INICIO, COLUMNA-FIN } para el token actual en el archivo de salida
            f_out.write(f"{tok.type}, {tok.value}, {tok.lineno}, {tok.col_start}, {tok.col_end}\n")

    print(f"Archivo de tokens generado: {output_file}")

def run_parser(filename):
    from parser import parser

    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = f.read()
    except FileNotFoundError:
        print(f"No se encontró el archivo {filename}")
        sys.exit(1)

    lexer.source = data
    parser.parse(data, lexer=lexer)

def main():
    if len(sys.argv) == 3 and sys.argv[1] == '--token':
        run_lexer(sys.argv[2])
    elif len(sys.argv) == 2:
        run_parser(sys.argv[1])
    else:
        print("Uso: python main.py <archivo.lava>")
        print("     python main.py --token <archivo.lava>")
        sys.exit(1)

if __name__ == '__main__':
    main()