# ============================================================
# faseLuna.py
# Migración completa desde FORTRAN (faseLuna.f + FASENEWT)
# Compatible con coordena.py (usa de440.bsp) y comunesAN.py
# ============================================================

import math
import comunesAN as cm
import constants as const
cpi = const.dpi


# ============================================================
# --- FASENEWT: método iterativo para hallar fase ---
# ============================================================

def FASENEWT(t, dt, fi, dif_out=None):
    """
    Traducción directa del subprograma FASENEWT de FORTRAN.
    Calcula una corrección iterativa de la fase lunar.
    """
    cpi = cm.dpi
    lun = 301  # Luna
    sol = 10   # Sol

    # Derivada
    t0 = t - dt
    t1 = t + dt

    # Posiciones eclípticas para t - dt
    lel, la_l, r_l = co.ECLIPTIC(lun, t0)
    les, la_s, r_s = co.ECLIPTIC(sol, t0)
    dif = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif > cpi:
        dif -= 2.0 * cpi

    # Posiciones para t + dt
    lel, la_l, r_l = co.ECLIPTIC(lun, t1)
    les, la_s, r_s = co.ECLIPTIC(sol, t1)
    r = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if r > cpi:
        r -= 2.0 * cpi

    # Velocidad de variación (diferencia angular / tiempo)
    v = (r - dif) / (2.0 * dt)
    if v == 0:
        v = 1e-12  # Evitar división por cero

    # Evaluación en t
    lel, la_l, r_l = co.ECLIPTIC(lun, t)
    les, la_s, r_s = co.ECLIPTIC(sol, t)
    dif = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif > cpi:
        dif -= 2.0 * cpi

    # Aproximación tangente (Newton-Raphson)
    t = t - dif / v

    # Evaluar el error en la nueva t
    lel, la_l, r_l = co.ECLIPTIC(lun, t)
    les, la_s, r_s = co.ECLIPTIC(sol, t)
    dif = (2.0 * cpi + les - lel + fi) % (2.0 * cpi)
    if dif > cpi:
        dif -= 2.0 * cpi

    dt = abs(dif / v) / 2.0
    dif = abs(dif)

    if dif_out is not None:
        dif_out = dif

    return t, dt, dif


# ============================================================
# --- FASESLUNA: cálculo de las fases principales ---
# ============================================================

def FasesDeLaLUNA(ano0, dt0):
    """
    Calcula las fechas aproximadas de las fases principales de la Luna
    a partir de un año y un valor inicial de paso dt0.
    """
    print("🌙 Cálculo de fases principales de la Luna...")
    cpi = cm.dpi

    # Fases: 0, π/2, π, 3π/2
    fases = [
        ("Luna Nueva", 0.0),
        ("Cuarto Creciente", 0.5 * cpi),
        ("Luna Llena", cpi),
        ("Cuarto Menguante", 1.5 * cpi)
    ]

    resultados = []

    for nombre, fi in fases:
        tt = ano0
        dt = dt0
        dif = 1.0
        iteracion = 0

        # Iteración hasta converger (Newton-Raphson)
        while abs(dif) > 1e-5 and iteracion < 10:
            tt, dt, dif = FASENEWT(tt, dt, fi)
            iteracion += 1

        resultados.append((nombre, tt))
        print(f"✅ {nombre:20s} -> JD {tt:.6f}  (iter {iteracion})")

    return resultados


# ============================================================
# --- CUALFASE: determina la fase actual ---
# ============================================================

def CUALFASE(tt, fi):
    """
    Determina la fase actual de la Luna comparando posiciones del Sol y la Luna.
    """
    cpi = cm.dpi
    lel, la_l, r_l = co.ECLIPTIC(301, tt)  # Luna
    les, la_s, r_s = co.ECLIPTIC(10, tt)   # Sol

    dif = (les - lel + fi) % (2.0 * cpi)
    if dif > cpi:
        dif -= 2.0 * cpi

    return dif


# ============================================================
# --- EJECUCIÓN DE PRUEBA DIRECTA ---
# ============================================================

if __name__ == "__main__":
    # Día juliano aproximado para 1 enero 2025
    ano0 = 2460680.5
    dt0 = 0.5  # paso inicial

    fases = FasesDeLaLUNA(ano0, dt0)

    print("\n📅 RESULTADOS:")
    for f in fases:
        print(f"{f[0]:20s}  JD = {f[1]:.6f}")
