# Guardar como: coordena.py
# Traducción funcional del módulo de coordenadas astronómicas
# Adaptado para usar el archivo de efemérides JPL de440.bsp
# Compatible con FaseLuna.py

import numpy as np
import math
import os
from jplephem.spk import SPK
import constants
import subAN

# ============================================================
# --- Cargar efemérides DE440 ---
# ============================================================

try:
    jpl = SPK.open('de440.bsp')
    print("coordena.py: Efemérides 'de440.bsp' cargadas correctamente.")
    print(f"📘 Pares disponibles: {list(jpl.pairs.keys())}")
except Exception as e:
    print(f"❌ ERROR al cargar 'de440.bsp': {e}")
    jpl = None

# ============================================================
# --- Códigos de cuerpos ---
# ============================================================

SOL = 10
TIERRA = 399
LUNA = 301
BARY_TM = 3  # Tierra-Luna barycenter

# ============================================================
# --- Funciones matemáticas básicas ---
# ============================================================

def CART2EQU(x, y, z):
    """Convierte coordenadas cartesianas en ecuatoriales."""
    a = math.atan2(y, x)
    if a < 0.0:
        a += constants.dpi
    dist = math.sqrt(x*x + y*y + z*z)
    d = math.asin(z / dist) if dist != 0.0 else 0.0
    return a, d


def EQU2ECLI(e, a, d):
    """Convierte coordenadas ecuatoriales en eclípticas."""
    ce = math.cos(e)
    se = math.sin(e)
    sd = math.sin(d)
    cd = math.cos(d)
    au = cd * math.sin(a)
    lo = math.atan2(sd*se + au*ce, math.cos(a)*cd)
    if lo < 0.0:
        lo += constants.dpi
    la = math.asin(sd*ce - au*se)
    return lo, la

# ============================================================
# --- APARENTE: Posición aparente de un cuerpo ---
# ============================================================

def APARENTE(qal, tt):
    """
    Devuelve (x, y, z, d, de) en km y radianes.
    Traducción directa del subprograma APARENTE del FORTRAN.
    """

    if jpl is None:
        raise RuntimeError("Archivo de efemérides no cargado.")

    jd_tdb = subAN.TDBTDT(tt)

    # Nutaciones (simplificadas, solo corrección de oblicuidad)
    dps, dep = 0.0, 0.0
    de = dep

    # Matriz de precesión/nutación
    pn = subAN.PRENUT(tt, dps, dep)

    # ============================================================
    # --- Obtener posición real según el cuerpo ---
    # ============================================================

    if qal == LUNA:
        # Luna = (Baricentro→Tierra-Luna) + (Tierra-Luna→Luna)
        r_bary = jpl[0, BARY_TM].compute(jd_tdb)
        r_rel = jpl[BARY_TM, LUNA].compute(jd_tdb)
        r = r_bary + r_rel

    elif qal == TIERRA:
        # Tierra = (Baricentro→Tierra-Luna) + (Tierra-Luna→Tierra)
        r_bary = jpl[0, BARY_TM].compute(jd_tdb)
        r_rel = jpl[BARY_TM, TIERRA].compute(jd_tdb)
        r = r_bary + r_rel

    elif qal == SOL:
        # Sol directo
        r = jpl[0, SOL].compute(jd_tdb)

    else:
        raise ValueError(f"⚠️ Código de cuerpo no reconocido: {qal}")

    # ============================================================
    # --- Calcular distancia ---
    # ============================================================

    d = math.sqrt(r[0]**2 + r[1]**2 + r[2]**2)

    # ============================================================
    # --- Correcciones físicas ---
    # ============================================================

    # Deflexión de luz (no se aplica al Sol)
    if qal != SOL:
        r_pos = subAN.DEFLELUZ(r, jpl[0, SOL].compute(jd_tdb))
    else:
        r_pos = r

    # Aberración planetaria
    r_pos_ab = subAN.PLABER(r_pos[0], r_pos[1], r_pos[2], 0, 0, 0)

    # Aplicar matriz de precesión/nutación
    x, y, z = subAN.PNESTADO(r_pos_ab[0], r_pos_ab[1], r_pos_ab[2], pn)

    return x, y, z, d, de

# ============================================================
# --- ECLIPTIC: Convierte a coordenadas eclípticas ---
# ============================================================

def ECLIPTIC(qal, tt):
    """Devuelve longitud, latitud eclíptica y distancia (r)."""
    x, y, z, r, de = APARENTE(qal, tt)
    a, d = CART2EQU(x, y, z)
    e = subAN.OBLECL(tt) + de
    lo, la = EQU2ECLI(e, a, d)
    return lo, la, r
