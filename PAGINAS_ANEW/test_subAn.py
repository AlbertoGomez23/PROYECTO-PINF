# Guardar como: test_subAN_completo.py
#
# Este script prueba TODAS las funciones de subAN.py,
# incluyendo las funciones de física y matrices.

import constants
import subAN as suban
import numpy as np
import math

print("--- INICIANDO PRUEBAS COMPLETAS DE 'subAN.py' (v10) ---")

# --- 1. PRUEBAS DE TRADUCCIÓN LITERAL (Utilidades y Formato) ---
print("\n## 1. Probando Funciones de Formato (Traducción Literal)")

# Prueba de FUNCTION ROUND(r)
print("\nProbando ROUND(r)...")
print(f"  ROUND(5.5)   -> Esperado: 6, Obtenido: {suban.ROUND(5.5)}")
print(f"  ROUND(-5.5)  -> Esperado: -5, Obtenido: {suban.ROUND(-5.5)}")
print(f"  ROUND(-5.6)  -> Esperado: -6, Obtenido: {suban.ROUND(-5.6)}")

# Prueba de SUBROUTINE HOMIEN(hor,h,mi)
print("\nProbando HOMIEN(hor)...")
h, m = suban.HOMIEN(12.75)
print(f"  HOMIEN(12.75) -> Esperado: (12, 45), Obtenido: ({h}, {m})")

# --- 2. PRUEBAS DE FUNCIONES ASTRONÓMICAS (Traducción Literal) ---
print("\n## 2. Probando Funciones Astronómicas (Fórmulas Literales)")

jd_j2000_tt = constants.j2000 
jd_j2000_ut = 2451545.0 

# Prueba de FUNCTION TDBTDT(tt)
print("\nProbando TDBTDT(tt)...")
esperado_tdb = jd_j2000_tt + (0.001658 * math.sin(math.radians(357.53)) + 0.000014 * math.sin(2.0 * math.radians(357.53))) / constants.di2s
obtenido_tdb = suban.TDBTDT(jd_j2000_tt)
print(f"  TDBTDT(J2000) -> Coinciden: {np.isclose(esperado_tdb, obtenido_tdb)}")

# Prueba de FUNCTION OBLECL(tt)
print("\nProbando OBLECL(tt)...")
# En J2000, x = TOCENT(tt) es casi 0, por lo que la fórmula da:
esperado_obl = constants.sa2r * 84381.448
obtenido_obl = suban.OBLECL(jd_j2000_tt)
print(f"  OBLECL(J2000) -> Coinciden: {np.isclose(esperado_obl, obtenido_obl)}")

# Prueba de FUNCTION TSMUT(jd)
print("\nProbando TSMUT(jd)...")
obtenido_tsmut = suban.TSMUT(jd_j2000_ut)
print(f"  TSMUT(J2000 UT1) -> Obtenido: {obtenido_tsmut:.8f} rad") # (Solo comprobamos que corre)

# --- 3. PRUEBAS DE FÍSICA Y MATRICES (Traducción Literal) ---
print("\n## 3. Probando Funciones de Física y Matrices")

# Prueba de PRECEANG(tt)
print("\nProbando PRECEANG(tt)...")
s, z, t = suban.PRECEANG(constants.j2000)
print(f"  PRECEANG(J2000) -> Esperado: (0, 0, 0), Obtenido: ({s}, {z}, {t})")

# Prueba de PRENUT(tt, dps, dep)
print("\nProbando PRENUT(tt, dps, dep)...")
# En J2000, con nutación 0, la matriz debe ser la Identidad
matriz_identidad = np.identity(3)
matriz_pn = suban.PRENUT(constants.j2000, 0.0, 0.0)
print(f"  PRENUT(J2000, 0, 0) -> Coincide con Matriz Identidad: {np.allclose(matriz_pn, matriz_identidad)}")

# Prueba de PNESTADO(x,y,z, pn)
print("\nProbando PNESTADO(x,y,z, pn)...")
# Aplicar la matriz identidad a un vector no debe cambiarlo
vector_in = (1.0, 2.0, 3.0)
x_out, y_out, z_out = suban.PNESTADO(vector_in[0], vector_in[1], vector_in[2], matriz_identidad)
print(f"  PNESTADO( (1,2,3), Identidad ) -> Obtenido: ({x_out}, {y_out}, {z_out})")

# Prueba de PLABER(x,y,z, xp,yp,zp)
print("\nProbando PLABER(x,y,z, xp,yp,zp)...")
# Si la velocidad (xp,yp,zp) es 0, el vector no debe cambiar
x_p, y_p, z_p = suban.PLABER(1.0, 2.0, 3.0, 0.0, 0.0, 0.0)
print(f"  PLABER( (1,2,3), Vel=0 ) -> Obtenido: ({x_p}, {y_p}, {z_p})")

# Prueba de DEFLELUZ(p, s)
print("\nProbando DEFLELUZ(p, s)...")
# Esta es una "smoke test": solo comprueba que corre sin crashear
p_in = [100.0, 200.0, 50.0]
s_in = [1.0, 1.0, -1.0] # Un vector Sol de prueba
x_d, y_d, z_d = suban.DEFLELUZ(p_in, s_in)
print(f"  DEFLELUZ(p, s) -> Obtenido: ({x_d:.8f}, {y_d:.8f}, {z_d:.8f})")
print(f"  (Debe ser ligeramente diferente de {p_in})")


print("\n--- PRUEBAS COMPLETAS FINALIZADAS ---")