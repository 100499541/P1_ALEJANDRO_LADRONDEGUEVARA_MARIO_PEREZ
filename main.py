import sys

def run_lexer(filename):
    from lexer import lexer, find_column
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = f.read()
    except FileNotFoundError:
        print(f"No se encontró el archivo '{filename}'")
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
            f_out.write(f"{tok.type}, {tok.value}, {tok.lineno}, {tok.col_start}, {tok.col_end}\n")

    print(f"Archivo de tokens generado: {output_file}")

def run_analysis(filename):
    from parser import analyze

    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = f.read()
    except FileNotFoundError:
        print(f"No se encontró el archivo '{filename}'")
        sys.exit(1)

    ok = analyze(data, filename)
    if ok:
        base = filename.rsplit('.', 1)[0]
        print(f"Análisis completado sin errores.")
        print(f"  -> {base}.symbols")
        print(f"  -> {base}.records")
        print(f"  -> {base}.functions")
        print(f"  -> {base}.quartets")

def main():
    if len(sys.argv) == 3 and sys.argv[1] == '--token':
        run_lexer(sys.argv[2])
    elif len(sys.argv) == 2:
        run_analysis(sys.argv[1])
    else:
        print("Uso:")
        print("  python main.py <archivo.lava>          -> análisis completo (léxico + sintáctico + semántico)")
        print("  python main.py --token <archivo.lava>  -> solo análisis léxico (.token)")
        sys.exit(1)

if __name__ == '__main__':
    main()