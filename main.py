import sys
from lexer import lexer, find_column, get_lexeme, get_original_length

def main():
    # Comprueba que esté bien el comando de uso
    if len(sys.argv) != 2:
        print("Uso: python main.py <archivo.lava>")
        sys.exit(1)

    # Obtiene el nombre del archivo de entrada y lo guarda 
    filename = sys.argv[1]
    # Lee el contenido del archivo de entrada
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = f.read()
    except FileNotFoundError:
        print(f"No se encontró el archivo {filename}")
        sys.exit(1)

    # Asignar input al lexer
    lexer.input(data)

    # Crea y escribe el archivo de salida
    output_file = filename.rsplit('.', 1)[0] + '.token'
    with open(output_file, 'w', encoding='utf-8') as f_out:
        while True:
            tok = lexer.token()
            if not tok:
                break

            # Obtiene los valores de las columnas de inicio y fin del token
            start_col = find_column(data, tok)
            original_len = get_original_length(tok, data)
            end_col = start_col + original_len

            # Obtiene el lexema original del token para mostrarlo en el archivo de salida
            display_value = get_lexeme(tok, data)

            # Escribe el conjunto { TIPO, VALOR, LÍNEA, COLUMNA-INICIO, COLUMNA-FIN } para el token actual en el archivo de salida
            f_out.write(f"{tok.type}, {display_value}, {tok.lineno}, {start_col}, {end_col}\n")

    print(f"Archivo de tokens generado: {output_file}")

if __name__ == '__main__':
    main()