"""
uso_anio_siguiente.py

Script CLI principal para calcular la tabla de correcciones para el uso del Almanaque Náutico
el año siguiente.

Orquesta la lógica de cálculo (core.py) y la generación de salida (formatter.py).
"""

import sys
from pathlib import Path
from typing import cast

import click

# Imports absolutos estándar (Best Practice)
from modern.src.uso_anio_siguiente import core, formatter
from modern.src.utils.coordena import ts

# Asegurar que la raíz del proyecto (/app) está en sys.path para permitir imports absolutos
# Esto es una buena práctica cuando se ejecutan scripts dentro de una estructura de paquetes
project_root = Path(__file__).resolve().parent.parent.parent.parent
if str(project_root) not in sys.path:
    sys.path.append(str(project_root))


def compute_corrections(ano, dt_seconds, base_dir=None):
    """
    Orquesta el cálculo y la generación del fichero.
    """
    can1 = f"{ano:04d}"
    can2 = f"{ano + 1:04d}"

    if base_dir:
        output_dir = Path(base_dir) / can1
    else:
        # Usar ruta relativa al fichero actual para robustez
        workspace_root = Path(__file__).resolve().parent.parent.parent.parent
        output_dir = workspace_root / "data" / "almanaque_nautico" / can1

    if not output_dir.parent.exists():
        output_dir = Path("output") / can1

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"AN{can1}TUSO{can2}.DAT"

    # 1. Calcular datos (Core Logic)
    data = core.calculate_corrections_data(ano, dt_seconds)

    # 2. Generar LaTeX (Formatter Logic)
    formatter.generate_latex_file(data, ano, output_path)

    return output_path


@click.command()
@click.option('--year', type=int, required=True, help='Año del Almanaque actual')
@click.option('--dt', type=float, help='Delta T (TT - UT) en segundos. Si no se especifica, se estima automáticamente.')
def main(year, dt):
    """Generador de Correcciones AN (Moderno)"""
    print("--- Generador de Correcciones AN (Moderno) ---")
    try:
        ano = year

        if dt is None:
            # Obtener Delta T preciso desde Skyfield para el 1 de Enero del año solicitado
            try:
                t_est = ts.utc(ano, 1, 1)
                # t_est.delta_t es un descriptor 'reify', pero devuelve float en runtime
                dt_est = float(cast(float, t_est.delta_t))
                print(
                    f"Estimación Skyfield Delta T = {dt_est:.4f}s para {ano}-01-01")
                dt = dt_est
            except Exception:
                dt = 69.0  # Fallback si falla Skyfield
                print(
                    f"Advertencia: Valor por defecto Delta T = {dt}s (Skyfield no disponible)")

        # Calcular ruta base automáticamente y asegurar que existe
        workspace_root = Path(__file__).resolve().parent.parent.parent.parent
        base_dir = workspace_root / "data" / "almanaque_nautico"
        if not base_dir.exists():
            print(f"Creando directorio base: {base_dir}")
            base_dir.mkdir(parents=True, exist_ok=True)

        ruta = compute_corrections(ano, dt, base_dir=base_dir)
        print(f"Fichero generado exitosamente en:\n{ruta}")

    except Exception as e:
        print(f"Error inesperado: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
