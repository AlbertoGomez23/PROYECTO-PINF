import numpy as np
from math import sin, cos, acos, atan, asin, radians
import sys
from pathlib import Path

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Propósito: Asegurar que el script tenga acceso a los módulos de la biblioteca
# 'utils' y 'subAN' independientemente del directorio de ejecución.
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

try:
    # 'read_de440': Maneja la lectura optimizada de efemérides (Skyfield wrapper)
    # 'coordena': Realiza transformaciones de coordenadas (Ecuatoriales -> Horizontales)
    from utils import read_de440 as read
    from utils import coordena as coor
except ImportError as e:
    print(f"Error al importar dependencias críticas: {e}")

# =============================================================================
# CONSTANTES FÍSICAS Y RATIOS (PRE-CALCULADOS)
# =============================================================================
# Se utilizan valores IAU (Unión Astronómica Internacional) para coherencia con DE440.
RET_KM = 6378.137      # Radio ecuatorial terrestre en km
REL_KM = 1737.4        # Radio medio lunar en km
AU_KM = 149597870.7    # Unidad Astronómica en km

# Ratios UA: Optimizan el rendimiento eliminando divisiones repetitivas en bucles.
# Se usan para calcular el paralaje (pi) y el semidiámetro (SD) rápidamente.
# pi = asin(R_tierra / Distancia)
# SD = asin(R_luna / Distancia)
RET_AU_RATIO = RET_KM / AU_KM
REL_AU_RATIO = REL_KM / AU_KM

# =============================================================================
# FUNCIONES AUXILIARES DE CÁLCULO
# =============================================================================

def get_moon_zenith_target_opt(ra_rad, de_rad, dist_au, fi_rad, jd_ut1, lon_deg):
    """
    Calcula la distancia cenital geométrica y las correcciones del fenómeno lunar.
    Resuelve el Triángulo de Posición Astronómico para un instante dado.

    Args:
        ra_rad (float): Ascensión Recta aparente en radianes.
        de_rad (float): Declinación aparente en radianes.
        dist_au (float): Distancia Tierra-Luna en Unidades Astronómicas.
        fi_rad (float): Latitud del observador en radianes.
        jd_ut1 (float): Fecha Juliana UT1 para el cálculo del Tiempo Sideral.
        lon_deg (float): Longitud del observador en grados.

    Returns:
        tuple: (a0, sd_luna, pi_luna)
               - a0: Distancia cenital calculada (topocéntrica) en radianes.
               - sd_luna: Semidiámetro angular de la Luna en radianes.
               - pi_luna: Paralaje horizontal ecuatorial en radianes.
    """
    
    

    # 1. Obtención del Tiempo Sideral Local y Ángulo Horario (HA)
    # LST = Tiempo Sideral Greenwich + Longitud
    lst_hours = read.sidereal_time(jd_ut1, lon_deg)
    
    # Ángulo Horario (t o H) = LST - Ascensión Recta (convertido a radianes)
    ha_rad = radians(lst_hours * 15.0) - ra_rad
    
    # 2. Resolución del triángulo de posición (Distancia cenital a0)
    # Fórmula fundamental de la navegación astronómica (Teorema del coseno esférico):
    # cos(z) = sin(lat)*sin(dec) + cos(lat)*cos(dec)*cos(H)
    cos_zenit = (sin(fi_rad) * sin(de_rad) + 
                 cos(fi_rad) * cos(de_rad) * cos(ha_rad))
    
    # Acotamos el valor entre -1 y 1 para evitar errores numéricos en acos
    a0 = acos(max(-1.0, min(1.0, cos_zenit)))
    
    # 3. Cálculo de Semidiámetro y Paralaje
    # La Luna está tan cerca que su tamaño aparente (SD) y el desplazamiento 
    # por la posición del observador (Paralaje) son significativos.
    # SD = atan(Radio_Luna / Distancia)
    sd_luna = atan(REL_AU_RATIO / dist_au)
    # HP = asin(Radio_Tierra / Distancia)
    pi_luna = asin(RET_AU_RATIO / dist_au)
    
    return a0, sd_luna, pi_luna

def itera_luna_final(t_aprox, dj_base, fi_rad, lon_deg, dz0):
    """
    Refina la hora aproximada de un fenómeno lunar mediante el método de la secante.
    
    Este método toma el "bracket" de tiempo encontrado en la búsqueda vectorizada
    y reduce el error hasta la precisión de segundos requerida por el Almanaque.

    Args:
        t_aprox (float): Fracción de día estimada del fenómeno (obtenida de la malla).
        dj_base (float): Fecha Juliana (00h) del día de cálculo.
        fi_rad (float): Latitud en radianes.
        lon_deg (float): Longitud en grados.
        dz0 (float): Distancia cenital objetivo teórica (90° + refracción).

    Returns:
        float: Hora decimal (0-24h) corregida del fenómeno.
    """
    # Definición de límites iniciales para la secante
    # u1 es nuestra mejor estimación actual.
    u1 = dj_base + t_aprox
    # u0 es un punto de apoyo ligeramente anterior (1 min = 0.0006944 días)
    u0 = u1 - 0.0006944 
    
    eps = 0.0001388      # Umbral de precisión (~12 segundos) para detener iteración
    
    # --- Paso 0: Evaluar la función en el punto inicial u0 ---
    t0_obj = read.get_time_obj(u0, scale='ut1')
    ra0, de0, dist0 = coor.equatorial_apparent(10, t0_obj) # 10 es el ID SPICE de la Luna
    a0, _sd0, _pi0 = get_moon_zenith_target_opt(ra0, de0, dist0, fi_rad, u0, lon_deg)

    # --- Bucle de refinamiento (Método de la Secante) ---
    # Buscamos 'u' tal que: DistanciaCenitalCalculada(u) - DistanciaCenitalObjetivo(u) = 0
    for _ in range(10): # Máximo 10 iteraciones (suelen bastar 3 o 4)
        
        # Evaluamos en u1
        t1_obj = read.get_time_obj(u1, scale='ut1')
        ra1, de1, dist1 = coor.equatorial_apparent(10, t1_obj)
        a1, sd1, pi1 = get_moon_zenith_target_opt(ra1, de1, dist1, fi_rad, u1, lon_deg)
        
        

        # Calculamos el objetivo REAL en este instante (dz_target).
        # El objetivo cambia ligeramente en cada iteración porque la distancia a la Luna
        # (y por tanto su SD y Paralaje) cambia minuto a minuto.
        # dz_target = 90° 34' + Semidiametro - Paralaje
        dz_target = dz0 + sd1 - pi1
        
        # Denominador de la secante (pendiente de la recta)
        denom = a1 - a0
        
        # Evitamos división por cero (si la altura no cambia, no hay solución)
        if abs(denom) < 1e-12: break
        
        # Fórmula de la secante: x2 = x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))
        # Aquí resolvemos para 'u' (tiempo)
        u2 = u0 + (u1 - u0) * (dz_target - a0) / denom
        
        # Comprobación de convergencia
        if abs(u2 - u1) < eps:
            return (u2 - dj_base) * 24.0
        
        # Desplazamiento de variables para la siguiente iteración
        u0, u1 = u1, u2
        a0 = a1
        
    return (u1 - dj_base) * 24.0

# =============================================================================
# IMPLEMENTACIÓN VECTORIZADA (ALTO RENDIMIENTO)
# =============================================================================

def fenoluna(dj, lat_deg, fen='ort', lon_deg=0.0):
    """
    Calcula la hora del orto u ocaso lunar para un día y posición geográfica.
    
    Esta función reemplaza los bucles de tiempo tradicionales de Fortran con
    operaciones vectorizadas de NumPy, reduciendo cientos de llamadas individuales
    a las efemérides a una sola llamada masiva.

    Args:
        dj (float): Fecha Juliana a las 00:00 UTC.
        lat_deg (float): Latitud decimal (+N / -S).
        fen (str): 'ort' para orto (salida), 'oca' para ocaso (puesta).
        lon_deg (float): Longitud decimal (+E / -W).

    Returns:
        float: Hora UTC decimal (0-24h). Retorna 9999.0 si no ocurre el fenómeno.
    """
    fi_rad = radians(lat_deg)
    
    # dz0 = 90° 34' en radianes
    # Esto incluye el radio geométrico (90) + refracción atmosférica estándar (34')
    dz0 = 1.580686525889531153 
    
    # Dirección del fenómeno para detectar cruces:
    # Orto (-1): Altura aumenta, distancia cenital disminuye.
    # Ocaso (1): Altura disminuye, distancia cenital aumenta.
    sgn = -1 if fen == 'ort' else 1

    # 1. GENERACIÓN VECTORIZADA DE DATOS
    # Creamos un array de 52 puntos temporales (aprox cada 30 min).
    # El rango va de -0.02 (23:30 del día anterior) a 1.05 (01:12 del día siguiente)
    # para asegurar que capturamos eventos cerca de la medianoche.
    pasos = np.linspace(-0.02, 1.05, 52) 
    tiempos_jd = dj + pasos
    
    # Obtención masiva de efemérides (Skyfield optimizado)
    # Esta es la operación más costosa, se hace UNA sola vez para todo el día.
    t_objs = read.get_time_obj(tiempos_jd, scale='ut1')
    ras, des, dists = coor.equatorial_apparent(10, t_objs) # 10 = ID Luna
    
    # 2. PROCESAMIENTO MATEMÁTICO VECTORIZADO (NumPy)
    # Calculamos Tiempo Sideral para todo el array simultáneamente.
    # 't_objs.gast' devuelve un vector de tiempos siderales de Greenwich.
    lst_hours = t_objs.gast + lon_deg / 15.0  
    ha_rads = np.radians(lst_hours * 15.0) - ras
    
    # Cálculo masivo de distancias cenitales (a_puntos)
    # Aplicamos la fórmula del coseno esférico a los vectores completos.
    cos_zenits = (sin(fi_rad) * np.sin(des) + 
                  cos(fi_rad) * np.cos(des) * np.cos(ha_rads))
    
    # np.clip protege contra errores de coma flotante (ej: 1.00000000001)
    a_puntos = np.arccos(np.clip(cos_zenits, -1.0, 1.0))
    
    # Correcciones vectorizadas para cada punto
    sds = np.arctan(REL_AU_RATIO / dists)
    pis = np.asin(RET_AU_RATIO / dists)
    
    # 'difs' calcula la "distancia al horizonte efectivo".
    # difs = (Objetivo) - (Calculado). 
    # Cuando difs cambia de signo, hemos cruzado el horizonte.
    difs = sgn * ((dz0 + sds - pis) - a_puntos)

    # 3. IDENTIFICACIÓN DEL CRUCE DE HORIZONTE
    # np.where busca índices donde el valor pasa de positivo a negativo (o viceversa)
    # entre el punto i y el punto i+1.
    cruces = np.where((difs[:-1] >= 0) & (difs[1:] <= 0))[0]

    if len(cruces) == 0:
        return 9999.0

    # LÓGICA DE SELECCIÓN DE EVENTO:
    # La Luna tiene un movimiento propio rápido (~50 min de retraso diario).
    # Es posible encontrar un orto/ocaso del día ANTERIOR muy cerca del inicio (t < 0)
    # o del día SIGUIENTE muy cerca del final.
    t_aprox = None
    for idx in cruces:
        t_posible = pasos[idx+1] # Tomamos el tiempo del segundo punto del intervalo
        
        # Filtramos eventos espurios muy negativos.
        # Umbral: -0.5 minutos (-0.00833 horas) de tolerancia.
        if t_posible * 24.0 > -0.00833:
            t_aprox = t_posible
            break # Nos quedamos con el primer evento válido del día
    
    if t_aprox is None:
        return 9999.0

    # 4. REFINAMIENTO FINO
    # Una vez sabemos que el orto ocurre, por ejemplo, entre las 14:00 y las 14:30,
    # llamamos a la función iterativa para hallar el segundo exacto.
    return itera_luna_final(t_aprox, dj, fi_rad, lon_deg, dz0)