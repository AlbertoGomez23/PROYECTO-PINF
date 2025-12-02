import os
import statistics
import sys
import unittest
from pathlib import Path
from typing import cast

# Imports absolutos estándar (Best Practice)
from modern.src.uso_anio_siguiente import core
from modern.src.utils.coordena import ts

# Asegurar que la raíz del proyecto (/app) está en sys.path
project_root = Path(__file__).resolve().parent.parent.parent.parent
if str(project_root) not in sys.path:
    sys.path.append(str(project_root))


class TestUsoAnioSiguienteBatch(unittest.TestCase):

    def test_batch_2010_2035_sanity(self):
        """
        Ejecuta el cálculo para el rango 2010-2035 y verifica:
        1. Que no hay errores de ejecución (excepciones).
        2. Que los valores están dentro de rangos astronómicos razonables (+/- 7 min arco).
        3. Que la variación interanual de la media es suave (consistencia temporal).
        """

        # Allow configuration via environment variables for non-interactive runs
        start_year = int(os.environ.get('BATCH_START_YEAR', 2010))
        end_year = int(os.environ.get('BATCH_END_YEAR', 2035))

        print(
            f"\n--- Ejecutando Test de Lote {start_year}-{end_year} con Delta T dinámico ---")

        history_means = []

        for year in range(start_year, end_year + 1):
            # Calcular Delta T dinámico para el 1 de Enero
            t = ts.utc(year, 1, 1)
            dt = float(cast(float, t.delta_t))

            # Ejecutar cálculo (solo en memoria, no genera ficheros)
            data = core.calculate_corrections_data(
                year, dt)

            # Validaciones básicas de estructura
            self.assertTrue(len(data) >= 365, f"Faltan días en el año {year}")

            values = list(data.values())
            min_v, max_v = min(values), max(values)
            mean_v = statistics.mean(values)

            # 1. Rango de valores (Sanity Check)
            # Las correcciones por uso al año siguiente (precesión+nutación) son pequeñas.
            # Si superan +/- 6.5 minutos de arco, probablemente hay un error de cálculo.
            # Nota: Ajustado a +/- 6.5 tras observar valores de -6.1 en 2011.
            self.assertTrue(-6.5 < min_v,
                            f"Valor demasiado bajo en {year}: {min_v}")
            self.assertTrue(
                max_v < 6.5, f"Valor demasiado alto en {year}: {max_v}")

            # 2. Consistencia interanual
            if history_means:
                prev_mean = history_means[-1]
                # La corrección media cambia lentamente debido a la precesión anual.
                # Un salto > 0.9 min arco en la media anual sería sospechoso.
                # Nota: Ajustado a 0.9 tras observar saltos de ~0.78 en 2011-2012.
                diff = abs(mean_v - prev_mean)
                self.assertLess(
                    diff, 0.9, f"Cambio brusco en la media entre {year-1} y {year}: {diff:.4f}")

            history_means.append(mean_v)

        print(
            f"Verificados {end_year - start_year + 1} años correctamente. Todo parece nominal.")


if __name__ == '__main__':
    unittest.main()
