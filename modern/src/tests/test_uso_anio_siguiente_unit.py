import sys
import tempfile
import unittest
from pathlib import Path

# Imports absolutos estándar (Best Practice)
from modern.src.uso_anio_siguiente import core
from modern.src.uso_anio_siguiente import formatter

# Asegurar que la raíz del proyecto (/app) está en sys.path
project_root = Path(__file__).resolve().parent.parent.parent.parent
if str(project_root) not in sys.path:
    sys.path.append(str(project_root))


class TestUsoAnioSiguiente(unittest.TestCase):

    def test_calculate_corrections_structure(self):
        """Test that the calculation returns a dictionary with correct keys."""
        ano = 2023  # Non-leap year
        dt = 69.0
        data = core.calculate_corrections_data(ano, dt)

        self.assertIsInstance(data, dict)
        # Check a random valid date
        self.assertIn((1, 1), data)
        self.assertIsInstance(data[(1, 1)], float)

        # Check that Feb 30 is NOT in data
        self.assertNotIn((30, 2), data)

    def test_leap_year_handling(self):
        """
        Test handling of Feb 29.

        Since corrections are calculated between Year N and N+1, and consecutive years
        cannot both be leap years, Feb 29 will always be invalid in at least one of the years.
        Therefore, Feb 29 should never be included in the output dictionary.
        """
        ano_leap = 2012  # Leap
        dt = 66.0
        data = core.calculate_corrections_data(ano_leap, dt)

        # Feb 29 exists in 2012 but not 2013. Should raise ValueError for 2013 and be skipped.
        self.assertNotIn((29, 2), data)

    def test_generate_latex_file(self):
        """Test that the LaTeX file is generated with correct content and column alignment."""

        # Create dummy data
        # Day 1, Month 1 (Jan) -> +1.2
        # Day 2, Month 1 (Jan) -> -0.5
        data = {(1, 1): 1.2, (2, 1): -0.5}
        ano = 2023

        with tempfile.TemporaryDirectory() as tmpdirname:
            output_path = Path(tmpdirname) / "test_output.dat"
            formatter.generate_latex_file(data, ano, output_path)

            self.assertTrue(output_path.exists())

            content = output_path.read_text(encoding='ascii')
            lines = content.splitlines()

            # Check Day 1 line
            # Format: " 1& $+1.2$&    &    &..."
            day1_line = next(l for l in lines if l.strip().startswith("1&"))
            parts = day1_line.split('&')
            self.assertEqual(parts[0].strip(), "1")
            # Month 1 is column index 1 (0-based index 1)
            self.assertIn("$+1.2$", parts[1])

            # Check Day 2 line
            day2_line = next(l for l in lines if l.strip().startswith("2&"))
            parts = day2_line.split('&')
            self.assertEqual(parts[0].strip(), "2")
            self.assertIn("$-0.5$", parts[1])

    def test_leap_year_marker(self):
        """
        Test that the leap year marker \rlap{*} is added for Feb 29 
        when the NEXT year (ano+1) is a leap year.
        """
        from modern.src.uso_anio_siguiente import formatter
        ano = 2023  # 2024 is leap
        data = {}  # Empty data

        with tempfile.TemporaryDirectory() as tmpdirname:
            output_path = Path(tmpdirname) / "test_marker.dat"
            formatter.generate_latex_file(data, ano, output_path)

            content = output_path.read_text(encoding='ascii')

            # Find the line for Day 29
            # It should contain the marker in the 2nd column (February)
            # Since data is empty, cells are empty, but marker should be appended.
            self.assertIn(r"\rlap{*}", content)

    def test_compute_corrections_integration(self):
        """Test the full flow with temporary directory."""
        from modern.src.uso_anio_siguiente import uso_anio_siguiente
        ano = 2023
        dt = 69.0

        with tempfile.TemporaryDirectory() as tmpdirname:
            # compute_corrections expects a base_dir
            base_dir = Path(tmpdirname)

            output_path = uso_anio_siguiente.compute_corrections(
                ano, dt, base_dir=base_dir)

            self.assertTrue(output_path.exists())
            self.assertEqual(output_path.name, f"AN{ano}TUSO{ano+1}.DAT")

            # Verify it's not empty
            self.assertGreater(output_path.stat().st_size, 0)


if __name__ == '__main__':
    unittest.main()
