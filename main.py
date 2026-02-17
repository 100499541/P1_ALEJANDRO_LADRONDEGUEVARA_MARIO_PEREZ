import sys
from lexer import lexer, find_column

def main():
    if len(sys.argv) != 2:
        print("Uso: python main.py <archivo.lava>")
        sys.exit(1)

    filename = sys.argv[1]
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = f.read()
    except FileNotFoundError:
        print(f"No se encontró el archivo {filename}")
        sys.exit(1)

    # Asignar input al lexer
    lexer.input(data)

    # Archivo de salida
    output_file = filename.rsplit('.', 1)[0] + '.token'
    with open(output_file, 'w', encoding='utf-8') as f_out:
        while True:
            tok = lexer.token()
            if not tok:
                break
            start_col = find_column(data, tok)
            end_col = start_col + len(str(tok.value))
            f_out.write(f"{tok.type}, {tok.value}, {tok.lineno}, {start_col}, {end_col}\n")

    print(f"Archivo de tokens generado: {output_file}")

if __name__ == '__main__':
    main()
