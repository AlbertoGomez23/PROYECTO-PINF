"""
test_uso_anio_siguiente_legacy.py

Tests de regresión que comparan la salida del módulo moderno con los ficheros
legacy almacenados en data/historico/.
"""

import re
import sys
import unittest
from pathlib import Path

from modern.src.uso_anio_siguiente import core
from modern.src.utils.coordena import ts

# Asegurar que la raíz del proyecto (/app) está en sys.path
project_root = Path(__file__).resolve().parent.parent.parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))


def parse_dat_file(file_path: Path) -> dict[tuple[int, int], float | None]:
    """
    CABECERA:       parse_dat_file(file_path)
    DESCRIPCIÓN:    Parsea un fichero .DAT de correcciones del Almanaque Náutico.

    PRECONDICIÓN:   - file_path: Path al fichero .DAT en formato LaTeX legacy.

    POSTCONDICIÓN:  Diccionario {(día, mes): valor_float | None}.
                    None indica celda vacía (día inexistente).
    """
    data: dict[tuple[int, int], float | None] = {}

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

            # Extract content inside $...$ if present
            m = re.search(r'\$([^$]+)\$', cell_raw)
            if m:
                content = m.group(1)
            else:
                content = cell_raw

            # Remove LaTeX commands like \rlap{*}
            content = re.sub(r'\\.*', '', content)
            content = content.strip()

            if not content:
                data[(day, month)] = None
            else:
                try:
                    data[(day, month)] = float(content)
                except ValueError:
                    data[(day, month)] = None

    return data


class TestLegacyComparison(unittest.TestCase):
    """
    Tests de regresión contra ficheros legacy almacenados.
    """

    # Tolerancia máxima en minutos de arco
    TOLERANCE = 0.11
    # La media debe ser significativamente menor que el máximo
    TOLERANCE_MEAN = TOLERANCE / 2

    # Directorio con datos históricos
    HISTORICO_DIR = project_root / "data" / "historico"

    def _compare_year(self, year: int) -> tuple[float, float, int]:
        """
        Compara los cálculos modernos con el fichero legacy para un año dado.

        Retorna: (max_diff, mean_diff, n_days)
        """
        # Buscar fichero TUSO para este año
        year_dir = self.HISTORICO_DIR / str(year)
        if not year_dir.exists():
            self.skipTest(f"No existe directorio para año {year}")

        tuso_files = list(year_dir.glob('*TUSO*.DAT'))
        if not tuso_files:
            self.skipTest(f"No hay fichero TUSO para año {year}")

        legacy_file = tuso_files[0]
        legacy_data = parse_dat_file(legacy_file)

        # Calcular Delta T dinámico
        t = ts.utc(year, 1, 1)
        dt = float(t.delta_t)

        # Calcular con módulo moderno
        modern_data = core.calculate_corrections_data(year, dt)

        # Comparar
        diffs = []
        for key, l_val in legacy_data.items():
            if l_val is None:
                continue
            m_val = modern_data.get(key)
            if m_val is not None:
                diffs.append(abs(l_val - m_val))

        self.assertGreater(
            len(diffs), 0, f"No hay datos comparables para {year}")

        max_diff = max(diffs)
        mean_diff = sum(diffs) / len(diffs)

        return max_diff, mean_diff, len(diffs)

    def test_legacy_2024(self):
        """Comparación con datos legacy del año 2024."""
        max_diff, mean_diff, n_days = self._compare_year(2024)
        self.assertLess(
            max_diff, self.TOLERANCE,
            f"Diferencia máxima {max_diff:.4f}' excede tolerancia {self.TOLERANCE}'"
        )
        self.assertLess(
            mean_diff, self.TOLERANCE_MEAN,
            f"Diferencia media {mean_diff:.4f}' demasiado alta"
        )
        self.assertEqual(n_days, 365, "Faltan días en la comparación")

    def test_legacy_2025(self):
        """Comparación con datos legacy del año 2025."""
        max_diff, mean_diff, n_days = self._compare_year(2025)
        self.assertLess(
            max_diff, self.TOLERANCE,
            f"Diferencia máxima {max_diff:.4f}' excede tolerancia {self.TOLERANCE}'"
        )
        self.assertLess(
            mean_diff, self.TOLERANCE_MEAN,
            f"Diferencia media {mean_diff:.4f}' demasiado alta"
        )
        self.assertEqual(n_days, 365, "Faltan días en la comparación")

    def test_legacy_2026(self):
        """Comparación con datos legacy del año 2026."""
        max_diff, mean_diff, n_days = self._compare_year(2026)
        self.assertLess(
            max_diff, self.TOLERANCE,
            f"Diferencia máxima {max_diff:.4f}' excede tolerancia {self.TOLERANCE}'"
        )
        self.assertLess(
            mean_diff, self.TOLERANCE_MEAN,
            f"Diferencia media {mean_diff:.4f}' demasiado alta"
        )
        self.assertEqual(n_days, 365, "Faltan días en la comparación")

    def test_all_available_years(self):
        """
        Test comprehensivo: compara TODOS los años disponibles en data/historico/.
        """
        if not self.HISTORICO_DIR.exists():
            self.skipTest("No existe directorio data/historico/")

        years_tested = 0
        max_diff_overall = 0.0

        for year_dir in sorted(self.HISTORICO_DIR.iterdir()):
            if not year_dir.is_dir():
                continue
            try:
                year = int(year_dir.name)
            except ValueError:
                continue

            tuso_files = list(year_dir.glob('*TUSO*.DAT'))
            if not tuso_files:
                continue

            max_diff, mean_diff, n_days = self._compare_year(year)
            max_diff_overall = max(max_diff_overall, max_diff)
            years_tested += 1

            # Verificar cada año individualmente
            self.assertLess(
                max_diff, self.TOLERANCE,
                f"Año {year}: Diff máx {max_diff:.4f}' excede tolerancia"
            )
            self.assertLess(
                mean_diff, self.TOLERANCE_MEAN,
                f"Año {year}: Diff media {mean_diff:.4f}' demasiado alta"
            )
            self.assertGreater(
                n_days, 360, f"Año {year}: Faltan días ({n_days})")

        self.assertGreater(
            years_tested, 0, "No se encontraron años para comparar")
        print(
            f"\n✅ Comparados {years_tested} años. Diff máxima global: {max_diff_overall:.4f}'")


class TestKnownValues(unittest.TestCase):
    """
    Tests con valores de referencia conocidos para fechas específicas.
    Estos valores provienen de los ficheros legacy verificados.
    """

    # Valores de referencia extraídos de data/historico/2025/AN2025TUSO2026.DAT
    REFERENCE_VALUES_2025 = {
        # (día, mes): corrección en minutos de arco
        (1, 1): 1.7,    # 1 Enero
        (21, 3): -1.2,  # 21 Marzo (equinoccio primavera)
        (21, 6): 0.6,   # 21 Junio (solsticio verano)
        (22, 9): -1.3,  # 22 Septiembre (equinoccio otoño)
        (21, 12): 1.8,  # 21 Diciembre (solsticio invierno)
    }

    def test_known_values_2025(self):
        """
        Verifica que los cálculos para fechas astronómicas clave
        coinciden con los valores legacy dentro de tolerancia.
        """
        year = 2025
        t = ts.utc(year, 1, 1)
        dt = float(t.delta_t)

        modern_data = core.calculate_corrections_data(year, dt)

        for (day, month), expected in self.REFERENCE_VALUES_2025.items():
            with self.subTest(day=day, month=month):
                actual = modern_data.get((day, month))
                self.assertIsNotNone(actual, f"Falta valor para {day}/{month}")
                assert actual is not None  # Para el type checker

                diff = abs(actual - expected)
                self.assertLess(
                    diff, 0.15,  # Tolerancia ligeramente mayor por redondeo en legacy
                    f"Día {day}/{month}: esperado {expected:.1f}', "
                    f"obtenido {actual:.2f}', diff={diff:.2f}'"
                )

    def test_equinox_solstice_pattern(self):
        """
        Verifica el patrón estacional de las correcciones:
        - Valores positivos cerca de solsticios de invierno/verano
        - Valores negativos cerca de equinoccios
        """
        year = 2025
        t = ts.utc(year, 1, 1)
        dt = float(t.delta_t)

        data = core.calculate_corrections_data(year, dt)

        # Equinoccios: valores típicamente negativos
        self.assertLess(data[(21, 3)], 0,
                        "Equinoccio primavera debería ser negativo")
        self.assertLess(data[(22, 9)], 0,
                        "Equinoccio otoño debería ser negativo")

        # Solsticios/extremos: valores típicamente positivos
        self.assertGreater(data[(1, 1)], 0, "Inicio año debería ser positivo")
        self.assertGreater(data[(21, 12)], 0,
                           "Solsticio invierno debería ser positivo")


if __name__ == '__main__':
    unittest.main(verbosity=2)
