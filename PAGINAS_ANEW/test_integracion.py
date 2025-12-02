# Guardar como: test_integracion.py
# (Versión final, 100% revisada)

import constants
import subAN as suban  # 1. Importa tu "calculadora" (subAN.py)
from jplephem.spk import SPK
import os
import math

print("--- INICIANDO PRUEBA DIRECTA (subAN + de440.bsp) ---")

# --- 1. Cargar la "Base de Datos" (de440.bsp) ---
try:
    jpl = SPK.open('de440.bsp')
    print(f"  [Paso 1] ÉXITO: 'de440.bsp' cargado.")
except Exception as e:
    print(f"  [Paso 1] ERROR: No se pudo cargar 'de440.bsp'. {e}")
    exit()

# --- 2. Definir Datos de Prueba Reales ---
jd_tt_test = constants.j2000 # 2451545.0
print(f"  [Paso 2] Fecha de prueba (TT): {jd_tt_test} JD")

# --- 3. Usar la "Calculadora" (subAN.py) ---
try:
    jd_tdb_calculado = suban.TDBTDT(jd_tt_test)
    print(f"  [Paso 3] ÉXITO: 'subAN.TDBTDT()' funcionó.")
    print(f"     -> TDB calculado: {jd_tdb_calculado:.10f}")
except Exception as e:
    print(f"  [Paso 3] ERROR: Falló la función TDBTDT de subAN.py. {e}")
    exit()

# --- 4. Usar la "Base de Datos" (de440.bsp) con el TDB de subAN ---
print(f"\n--- Probando de440.bsp con el TDB de subAN ---")
try:
    # Sintaxis correcta de jplephem: jpl[target, center](time)
    
    # Pedimos target=301 (Luna), center=3 (Baricentro Tierra-Luna)
    pos_luna_desde_bcm = jpl[3, 301](jd_tdb_calculado)
    
    # Pedimos target=399 (Tierra), center=3 (Baricentro Tierra-Luna)
    pos_tierra_desde_bcm = jpl[3, 399](jd_tdb_calculado)
    
    # La posición de la Luna relativa a la Tierra es la resta:
    pos_luna_xyz_km = pos_luna_desde_bcm - pos_tierra_desde_bcm
    
    print(f"  [Paso 4] ÉXITO: 'de440.bsp' devolvió una posición.")
    print(f"     -> Posición Ecuatorial de la Luna [X,Y,Z] (km):")
    print(f"     -> {pos_luna_xyz_km}")

except KeyError as e:
    print(f"  [Paso 4] ERROR: KeyError de jplephem. {e}")
    print("     Significa que tu .bsp no tiene los segmentos (3, 301) o (3, 399).")
except Exception as e:
    print(f"  [Paso 4] ERROR: Falló la consulta a jplephem. {e}")
    exit()

print("\n--- PRUEBA DE INTEGRACIÓN DIRECTA COMPLETADA CON ÉXITO ---")