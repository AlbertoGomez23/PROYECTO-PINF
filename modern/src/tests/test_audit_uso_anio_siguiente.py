import re
import sys
from pathlib import Path

import click


def parse_dat_file(file_path):
    """
    Parses the .DAT file and returns a dictionary {(day, month): value}.
    Value is float, or None if empty.
    """
    data = {}
    with open(file_path, 'r', encoding='ascii') as f:
        lines = f.readlines()

    for line in lines:
        line = line.strip()
        if not line:
            continue

        # Remove LaTeX row endings
        line = re.sub(r'\\\\.*$', '', line)

        parts = line.split('&')
        if len(parts) < 13:  # Day + 12 months
            continue

        try:
            day = int(parts[0].strip())
        except ValueError:
            continue

        for month in range(1, 13):
            cell_raw = parts[month].strip()

            # Remove LaTeX formatting like $...$, \rlap{*}, etc.
            # Keep only characters that can form a number: digits, ., +, -
            # But be careful not to merge separate numbers if any (shouldn't be).

            # Extract content inside $...$ if present
            m = re.search(r'\$([^$]+)\$', cell_raw)
            if m:
                content = m.group(1)
            else:
                content = cell_raw

            # Clean up content
            # Remove \rlap{*} or similar
            content = re.sub(r'\\.*', '', content)
            content = content.strip()

            if not content:
                data[(day, month)] = None
            else:
                try:
                    val = float(content)
                    data[(day, month)] = val
                except ValueError:
                    # print(f"Warning: Could not parse value '{content}' at Day {day}, Month {month}")
                    data[(day, month)] = None
    return data


def audit_comparison(legacy_path, modern_path, tolerance=0.11):
    """
    Compares two DAT files.
    Returns True if comparison is within tolerance, False otherwise.
    """
    print(f"Auditando:\nLegacy: {legacy_path}\nModerno: {modern_path}\n")

    legacy_data = parse_dat_file(legacy_path)
    modern_data = parse_dat_file(modern_path)

    all_keys = set(legacy_data.keys()) | set(modern_data.keys())

    diffs = []
    missing_in_legacy = []
    missing_in_modern = []
    mismatches = []  # (day, month, legacy_val, modern_val, diff)

    for key in sorted(all_keys):
        day, month = key
        l_val = legacy_data.get(key)
        m_val = modern_data.get(key)

        if l_val is None and m_val is None:
            continue

        if l_val is None:
            missing_in_legacy.append(key)
            continue

        if m_val is None:
            missing_in_modern.append(key)
            continue

        diff = abs(l_val - m_val)
        diffs.append(diff)

        if diff > tolerance:  # Threshold for reporting specific mismatch
            mismatches.append((day, month, l_val, m_val, diff))

    print(f"Total celdas comparadas: {len(diffs)}")
    if diffs:
        print(f"Diferencia máxima: {max(diffs):.4f}")
        print(f"Diferencia media: {sum(diffs)/len(diffs):.4f}")
    else:
        print("No se encontraron celdas numéricas comunes.")

    if missing_in_legacy:
        print(
            f"\nCeldas presentes en Moderno pero FALTAN en Legacy ({len(missing_in_legacy)}):")
        print(missing_in_legacy[:10], "..." if len(
            missing_in_legacy) > 10 else "")

    if missing_in_modern:
        print(
            f"\nCeldas presentes en Legacy pero FALTAN en Moderno ({len(missing_in_modern)}):")
        print(missing_in_modern[:10], "..." if len(
            missing_in_modern) > 10 else "")

    if mismatches:
        print(
            f"\nDiscrepancias significativas (> {tolerance} min arco) ({len(mismatches)}):")
        print(f"{'Día':<4} {'Mes':<6} {'Legacy':<8} {'Moderno':<8} {'Dif':<8}")
        print("-" * 40)
        # Sort by diff descending
        mismatches.sort(key=lambda x: x[4], reverse=True)
        for d, m, l, mod, diff in mismatches[:20]:
            print(f"{d:<4} {m:<6} {l:<8.1f} {mod:<8.1f} {diff:<8.2f}")
        if len(mismatches) > 20:
            print(f"... y {len(mismatches) - 20} más.")

        return False

    if missing_in_legacy or missing_in_modern:
        print("\nADVERTENCIA: Discrepancia estructural (faltan celdas).")
        return False

    print("\nÉXITO: Comparación dentro de la tolerancia.")
    return True


@click.command()
@click.argument('legacy_file', type=click.Path(exists=True, path_type=Path), required=False)
@click.argument('modern_file', type=click.Path(exists=True, path_type=Path), required=False)
@click.option('--tolerance', '-t', default=0.11, help='Tolerancia para la diferencia en minutos de arco. Por defecto: 0.11')
def main(legacy_file, modern_file, tolerance):
    """
    Auditoría de comparación entre ficheros de corrección del Almanaque Náutico (.DAT) Legacy y Moderno.
    """
    # Calculate base directory relative to this script
    base_dir = Path(__file__).resolve().parent.parent.parent.parent
    default_legacy = base_dir / "data" / "almanaque_nautico" / \
        "2012" / "AN2012TUSO2013_legacy.DAT"
    default_modern = base_dir / "data" / \
        "almanaque_nautico" / "2012" / "AN2012TUSO2013.DAT"

    if not legacy_file:
        legacy_file = default_legacy

    if not modern_file:
        modern_file = default_modern

    print(f"--- Configuración de Auditoría ---")
    print(f"Fichero Legacy:  {legacy_file}")
    print(f"Fichero Moderno: {modern_file}")
    print(f"Tolerancia:      {tolerance} min arco")
    print(f"----------------------------------\n")

    if not legacy_file.exists():
        print(f"Error: No se encontró el fichero Legacy en {legacy_file}")
        sys.exit(1)

    if not modern_file.exists():
        print(
            f"Error: No se encontró el fichero Moderno en {modern_file}")
        sys.exit(1)

    success = audit_comparison(
        legacy_file, modern_file, tolerance)
    if not success:
        sys.exit(1)


if __name__ == "__main__":
    main()
