"""
formatter.py

Generación de ficheros de salida (LaTeX) para las correcciones del uso del Almanaque Náutico.
"""

import calendar


def generate_latex_file(data, ano, output_path):
    """
    Genera el fichero LaTeX a partir de los datos calculados.
    """
    # Matriz de cadenas para la salida LaTeX
    correc = [["    " for _ in range(12)] for _ in range(31)]

    # Rellenar matriz con datos formateados
    for (dia, mes), val in data.items():
        s = f"{val:+4.1f}"
        correc[dia - 1][mes - 1] = s

    with output_path.open("w", encoding="ascii") as f:

        def cell(d, m):
            return correc[d - 1][m - 1]

        for dia in range(1, 32):
            line = ""
            row_cells = [f"${cell(dia, m)}$" if cell(
                dia, m).strip() else "    " for m in range(1, 13)]

            if dia == 1:
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\rule{0pt}{12pt}\\\\\n"

            elif dia in (5, 10, 15, 20, 25):
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\[1.5ex]\n"

            elif dia == 29:
                if calendar.isleap(ano + 1):
                    row_cells[1] = row_cells[1] + "\\rlap{*}"
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\\n"

            elif dia == 30:
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\[1.5ex]\n"

            else:
                content = "&".join([f"{dia:2d}"] + row_cells)
                line = content + "\\\\\n"

            f.write(line)
