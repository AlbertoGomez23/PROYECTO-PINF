"""
core.py

Lógica de cálculo astronómico para las correcciones del uso del Almanaque Náutico
el año siguiente.
"""

import math
from datetime import date
from typing import cast

from utils import coordena as coordena_moderno
from utils.coordena import ts

# Constantes
PI = math.pi
DPI = 2.0 * PI
DEGREE = PI / 180.0


def calculate_corrections_data(ano, dt_seconds):
    """
    Calcula los datos de corrección basándose en la diferencia de GHA del Sol.

    Fórmulas:
      1. GAST (Greenwich Apparent Sidereal Time): theta
      2. AR (Ascensión Recta Aparente del Sol): alpha
      3. GHA (Greenwich Hour Angle): GHA = theta - alpha
      4. Corrección = GHA(año+1) - GHA(año)

    Retorna:
    - data: Diccionario {(dia, mes): valor_float}
            Si el día no existe, no estará en el diccionario.
    """
    data = {}

    print(
        f"Calculando correcciones para {ano}-{ano+1} con DT={dt_seconds}s...")

    for mes in range(1, 13):
        for dia in range(1, 32):
            try:
                # Usamos datetime.date para validación estricta.
                _ = date(ano, mes, dia)
                _ = date(ano + 1, mes, dia)

                t_base_1 = ts.utc(ano, mes, dia)
                t_base_2 = ts.utc(ano + 1, mes, dia)
            except ValueError:
                continue

            # --- AÑO ACTUAL (1) ---
            # Forzamos 0h UT1 para coincidir con la definición clásica del Almanaque
            # ts.utc(ano, mes, dia) devuelve 0h UTC.
            # Obtenemos el número de día juliano entero y sumamos 0.5 para tener 0h UT1 exactas.
            jd_ut1_val_1 = int(t_base_1.ut1) + 0.5
            jd_tt1 = jd_ut1_val_1 + dt_seconds / 86400.0
            t1 = ts.tt_jd(jd_tt1)

            # GAST (theta) en radianes
            # t1.gast devuelve horas siderales, convertimos a radianes:
            # theta = gast * 15 * (pi/180)
            ts1 = (cast(float, t1.gast) * 15.0 * DEGREE) % DPI

            # AR (alpha) del Sol (cuerpo 11) en radianes
            ar1, _, _ = coordena_moderno.equatorial_apparent(11, t1)

            # --- AÑO SIGUIENTE (2) ---
            jd_ut1_val_2 = int(t_base_2.ut1) + 0.5
            jd_tt2 = jd_ut1_val_2 + dt_seconds / 86400.0
            t2 = ts.tt_jd(jd_tt2)

            # GAST (theta) en radianes
            ts2 = (cast(float, t2.gast) * 15.0 * DEGREE) % DPI

            # AR (alpha) del Sol (cuerpo 11) en radianes
            ar2, _, _ = coordena_moderno.equatorial_apparent(11, t2)

            # --- CÁLCULO ---
            # GHA = theta - alpha
            gha1 = (ts1 - ar1) % DPI
            gha2 = (ts2 - ar2) % DPI

            # Delta = GHA(N+1) - GHA(N)
            diff = gha2 - gha1

            # Normalización a [-PI, PI] para obtener el camino más corto
            if diff > PI:
                diff -= DPI
            elif diff < -PI:
                diff += DPI

            # Conversión a minutos de arco
            # Corrección = diff_rad * (180/pi) * 60
            corr_arc_minutes = (diff / DEGREE) * 60.0
            data[(dia, mes)] = corr_arc_minutes

    return data
