"""
core.py

Lógica de cálculo astronómico para las correcciones del uso del Almanaque Náutico
el año siguiente.
"""

import logging
import math
from datetime import date
from typing import cast

logger = logging.getLogger(__name__)

# Imports compatibles con web_app.py y CLI
try:
    # Intento 1: Import relativo (cuando se ejecuta desde web_app.py)
    from ..utils import coordena as coordena_moderno
    from ..utils.coordena import ts
except ImportError:
    # Intento 2: Import desde src (cuando src está en sys.path)
    from utils import coordena as coordena_moderno
    from utils.coordena import ts

# Constantes
PI = math.pi
DPI = 2.0 * PI
DEGREE = PI / 180.0


def calculate_corrections_data(ano: int, dt_seconds: float) -> dict[tuple[int, int], float]:
    """
    CABECERA:       calculate_corrections_data(ano, dt_seconds)
    DESCRIPCIÓN:    Calcula las correcciones de GHA del Sol para uso del Almanaque
                    Náutico el año siguiente.

    PRECONDICIÓN:   - ano: Año del Almanaque actual (int, ej: 2025).
                    - dt_seconds: ΔT = TT - UT1 en segundos (float, ~69s en 2025).
                      Diferencia entre Tiempo Terrestre y Tiempo Universal 1.

    POSTCONDICIÓN:  Diccionario {(dia, mes): corrección_arcmin}.
                    - Claves: tuplas (día, mes) con día ∈ [1,31], mes ∈ [1,12].
                    - Valores: corrección en minutos de arco (float).
                    - Los días inexistentes (ej: 30 Feb) se omiten.

                    ALGORITMO:
                    1. GAST (θ): Greenwich Apparent Sidereal Time [calculado en UT1]
                    2. AR (α): Ascensión Recta Aparente del Sol [calculado en TT]
                    3. GHA = θ - α
                    4. Corrección = GHA(año+1) - GHA(año), normalizado a [-π, π]
    """
    data: dict[tuple[int, int], float] = {}

    logger.info(
        f"Calculando correcciones para {ano}-{ano+1} con DT={dt_seconds}s...")

    for mes in range(1, 13):
        for dia in range(1, 32):
            try:
                # Validación estricta: date() lanza ValueError si la fecha no existe
                date(ano, mes, dia)
                date(ano + 1, mes, dia)

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
