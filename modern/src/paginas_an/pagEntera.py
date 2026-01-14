import numpy as np
import sys
from pathlib import Path
from skyfield.api import load
from skyfield.magnitudelib import planetary_magnitude
from skyfield.searchlib import find_discrete

# =============================================================================
# CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Bloque de inicialización de rutas relativas para garantizar que el script
# funcione independientemente de dónde esté alojado el proyecto.

# Obtenemos la ruta absoluta de ESTE fichero
ruta_pagEntera = Path(__file__).resolve()

# Obtenemos la ruta de la carpeta paginas_an
ruta_paginas_an = ruta_pagEntera.parent

# Obtenemos la ruta padre tanto de paginas_an como de utils, es decir, src
ruta_Padre = ruta_paginas_an.parent

# Ruta al archivo de efemérides planetarias (DE440 es el estándar moderno de la NASA/JPL)
ruta_DE440 = ruta_Padre / 'data' / 'de440.bsp'

ruta_data = ruta_Padre.parent.parent

# Este bloque asegura que Python pueda encontrar los módulos propios del proyecto
# aunque el script se ejecute desde una carpeta distinta.
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    # Fallback para entornos interactivos (Jupyter, REPL)
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

try:
    # Importación de librerías utilitarias propias del proyecto
    from utils import funciones 
except ImportError as e:
    # Si faltan las librerías, el programa fallará al llamar a las funciones de cálculo,
    # pero permite cargar el script para revisión de código estático.
    pass


# Importamos módulos específicos del Almanaque
from subAN import *
from constants import *
from ortoocasoluna import fenoluna #, retardo_lunar_R (comentado en original)
from ortoocasol import fenosol

"""""
Por último, importamos la carpeta padre en el sys.path.
Esto hará que Python, al buscar módulos, también busque en 'src'.
Se transforma a string ya que sys.path espera una lista de strings, no objetos Path.
"""""
if str(ruta_Padre) not in sys.path:
    sys.path.append(str(ruta_Padre))


# =============================================================================
# CARGA DE EFEMÉRIDES Y CONSTANTES
# =============================================================================

# Cargamos el kernel DE440.bsp (posiciones planetarias de alta precisión 1550-2650)
print("Cargando efemérides DE440...")
eph = load(str(ruta_DE440))

# Obtenemos los objetos Skyfield para los cuerpos principales
sol = eph['sun']
luna = eph['moon']
tierra = eph['earth']

# Diccionario para mapear nombres internos a objetos Skyfield de planetas
plan_dic ={
    'ven': eph['venus'],
    'mar': eph['mars barycenter'], # Usamos baricentro para planetas externos
    'jup': eph['jupiter barycenter'],
    'sat': eph['saturn barycenter'],
    'ari': None     # Aries es un punto geométrico ficticio, no un cuerpo físico
}

# Obtenemos la escala de tiempo (necesaria para conversiones TT, UT1, TDB)
ts = load.timescale()

# Definición de latitudes estándar para el cálculo de fenómenos (ortos/ocasos)
# Orden descendente desde 60N a 60S
LATITUDES_VAL = [60, 58, 56, 54, 52, 50, 45, 40, 35, 30, 20, 10, 0, 
                 -10, -20, -30, -35, -40, -45, -50, -52, -54, -56, -58, -60]

LATITUDES_STR = ['60 N', '58  ', '56  ', '54  ', '52  ', '50  ',
                 '45  ', '40  ', '35  ', '30  ', '20  ', '10 N',
                 ' 0  ', '10 S', '20  ', '30  ', '35  ', '40  ',
                 '45  ', '50  ', '52  ', '54  ', '56  ', '58  ',
                 '60 S']


# Mapeo de índices a nombres de días
num_a_dia ={
    0: "Lunes", 1: "Martes", 2: "Miércoles", 3: "Jueves",
    4: "Viernes", 5: "Sábado", 6: "Domingo"
}

# Constantes físicas (Radios en Unidades Astronómicas) para cálculos de semidiámetro
RAD_SOL_UA    = 4.65247e-3
RAD_LUNA_UA   = 1.16178e-5
RAD_TIERRA_UA = 4.26352e-5

# =============================================================================
# FUNCIONES AUXILIARES DE FECHA Y FORMATO
# =============================================================================

def DiaSem(dJul):
    """
    Calcula el día de la semana a partir del Día Juliano.
    
    Args:
        dJul (float): Día Juliano.
        
    Returns:
        int: 0 para Lunes, 1 para Martes, ..., 6 para Domingo.
    """
    return int((dJul - 2442915.5) % 7)

# Mapeo de número de mes a nombre en español
num_a_mes = {
    1: "Enero", 2: "Febrero", 3: "Marzo", 4: "Abril", 5:"Mayo",
    6: "Junio", 7: "Julio", 8: "Agosto", 9: "Septiembre",
    10: "Octubre", 11: "Noviembre", 12: "Diciembre"
}

def MesANom(mes):
    """Devuelve el nombre del mes dado su número (1-12)."""
    return num_a_mes[mes]

def grads_A_sexagesimal(grados):
    """
    Convierte grados decimales a formato sexagesimal (Grados, Minutos).
    
    Args:
        grados (float): Ángulo en grados decimales.
        
    Returns:
        tuple: (signo, grados_entero, minutos_decimales)
    """
    # Determinación del signo
    if grados >= 0:
        signo = '+'
    else:
        signo = '-'

    grad = abs(grados)      # Trabajamos con valor absoluto
    g = int(grad)
    minutos = (grad - g) * 60.0
    minutos = round(minutos, 1)

    # Ajuste por desbordamiento de minutos (ej: 59.95 -> 60.0 -> 0.0 y +1 grado)
    if minutos >= 60.0:
        g += 1
        minutos = 0.0

    return signo, g, minutos

def formato_grado_minuto(grad, err=0.05):
    """
    Formatea grados y minutos con una lógica de redondeo específica 
    para mantener consistencia con tablas náuticas antiguas (tipo Fortran).
    
    Args:
        grad (float): Grados decimales.
        err (float): Tolerancia para el redondeo al entero superior.
        
    Returns:
        tuple: (grados, minutos) con el signo aplicado a los grados.
    """
    sgn = np.sign(grad)
    gra_abs = abs(grad)
    
    gr = int(gra_abs)
    mi = (gra_abs - gr) * 60.0
    
    # Lógica crítica de redondeo: si estamos muy cerca de 60, pasamos al siguiente grado
    if (60.0 - mi) <= err:
        gr = gr + 1
        mi = 0.0
        
    # Aplicamos signo al grado resultante
    gr = int(gr * sgn)
    return gr, mi

def formato_signo_grado_minuto(grad, err=0.05):
    """
    Similar a formato_grado_minuto, pero devuelve el signo como string separado.
    Útil para declinaciones (+/-).
    """
    if grad >= 0:
        sgn_str = '+'
    else:
        sgn_str = '-'
        
    gra_abs = abs(grad)
    gr = int(gra_abs)
    mi = (gra_abs - gr) * 60.0
    
    if (60.0 - mi) <= err:
        gr = gr + 1
        mi = 0.0
        
    return sgn_str, gr, mi

# =============================================================================
# FUNCIONES DE CÁLCULO ASTRONÓMICO
# =============================================================================



def cal_coord_ap(cuerpo, t):
    """
    Calcula las coordenadas aparentes (GHA, Declinación) de un cuerpo celeste.
    Es el núcleo de cálculo del script.

    Args:
        cuerpo: Objeto Skyfield (eph['sun']) o string 'aries'/'ari'.
        t: Objeto Time de Skyfield (incluye delta T y UT1).

    Returns:
        tuple: (gha, dec, dist)
            - gha: Ángulo Horario de Greenwich (0-360).
            - dec: Declinación (-90 a +90).
            - dist: Distancia en UA.
    """

    if cuerpo == 'aries' or cuerpo == 'ari':
        # Para Aries, el GHA es el Ángulo Sidéreo de Greenwich (GAST) convertido a grados
        gha  = (t.gast * 15.0) % 360.0
        dec = 0.0
        dist = 0.0
    else:
        # Lógica para cuerpos físicos
        if isinstance(cuerpo, str):
            # Resolución de nombres si se pasa un string en lugar de un objeto
            nombre_original = cuerpo
            cuerpo = plan_dic.get(nombre_original)
            
            if cuerpo is None: # Fallback manual
                if nombre_original == 'sol': cuerpo = sol
                elif nombre_original == 'lun': cuerpo = luna
        
        # Obtenemos la posición astrométrica desde la Tierra
        astronomic = tierra.at(t).observe(cuerpo)

        # Calculamos posición aparente (aplica precesión, nutación y aberración de luz)
        app = astronomic.apparent()

        # Obtenemos coordenadas ecuatoriales (Ascensión Recta y Declinación)
        ra, dec_obj, distancia = app.radec(epoch='date')

        # Conversión fundamental: GHA = GAST - RA
        ra_deg = ra.hours * 15.0
        gst_deg = t.gast * 15.0
        gha  = (gst_deg - ra_deg) % 360.0
        
        dec  = dec_obj.degrees
        dist = distancia.au

    return gha, dec, dist


def calc_sd_sol(dist_ua):
    """Calcula el Semidiámetro (SD) del Sol basado en su distancia."""
    if dist_ua != 0:
        return np.degrees(np.arcsin(RAD_SOL_UA / dist_ua)) * 60.0
    else:
        return 0

def calc_sd_luna(dist_ua):
    """Calcula el Semidiámetro (SD) de la Luna basado en su distancia."""
    if dist_ua !=0:
        return np.degrees(np.arcsin(RAD_LUNA_UA / dist_ua)) * 60.0
    else:
        return 0

def calc_phe_luna(dist_ua):
    """Calcula el Paralaje Horizontal Ecuatorial (PHE) de la Luna."""
    if dist_ua != 0:   
        return np.degrees(np.arcsin(RAD_TIERRA_UA / dist_ua)) * 60.0
    else:
        return 0



def Paso_Mer(jdInicio, cuerpo, dt):
    """
    Calcula el momento exacto del paso por el meridiano superior de Greenwich (Tránsito).
    
    Args:
        jdInicio (float): Día Juliano de inicio.
        cuerpo (obj): Cuerpo celeste.
        dt (float): Delta T.
        
    Returns:
        float: Hora UT del paso (decimal). Si no cruza, devuelve 12.0 por defecto.
    """
    t0 = ts.ut1_jd(jdInicio)
    t1 = ts.ut1_jd(jdInicio + 1.0)

    # Definimos la condición de cruce: cuando el GHA pasa de 360 a 0 (cruce del meridiano)
    # Matemáticamente, buscamos cuando el ángulo normalizado cruza cero.
    def meridian_condition(t):
        gha, _, _ = cal_coord_ap(cuerpo, t)
        
        # Normalizamos: si gha > 180, restamos 360 para tener rango [-180, 180]
        # El cruce ocurre cuando pasa de positivo a negativo (o viceversa según la convención)
        gha = np.where(gha > 180, gha - 360, gha)
        
        return gha < 0      # Booleano que cambia de estado en el cruce

    meridian_condition.step_days = 0.04 # Paso de búsqueda para find_discrete

    tiempos, _valores = find_discrete(t0, t1, meridian_condition)

    # Si encontramos eventos de cruce
    if len(tiempos) > 0:
        for t_event in tiempos:
            # Verificación de seguridad
            gha_check, _, _ = cal_coord_ap(cuerpo, t_event)
            if gha_check > 180: gha_check -= 360
            
            # Si estamos realmente cerca del meridiano (margen de error < 90 grados)
            if abs(gha_check) < 90: 
                 return (t_event.ut1 - jdInicio) * 24.0
        
    return 12.0 # Valor por defecto si falla la búsqueda

def Mag_visual(jd_tt, cuerpo):
    """Calcula la magnitud visual aparente de un planeta."""
    if cuerpo == 'ari':
        return 0.0      # Aries no brilla
    else:
        t = ts.tt_jd(jd_tt)     # Usamos Tiempo Terrestre para efemérides
        objetivo = plan_dic.get(cuerpo)
        
        if not objetivo:        # Cuerpo no encontrado en el diccionario
            return -99.9
        else:
            obs = tierra.at(t).observe(objetivo)
            try:
                mag = planetary_magnitude(obs)      # Función interna de Skyfield
                return float(mag)
            except:
                return -99.9

# =============================================================================
# FUNCIÓN PRINCIPAL DE GENERACIÓN DE PÁGINA
# =============================================================================

def UNAPAG(da, annio, dt, return_content=False):
    """
    Genera una página completa del Almanaque Náutico (fichero .dat formateado).
    
    Args:
        da (int): Día del Año (1-365/366).
        annio (int): Año.
        dt (float): Delta T (diferencia TT - UT1).
        return_content (bool): Si es True, devuelve el string en lugar de escribir a disco.
    """
    import io
    print(f"Generando página para el día {da} de {annio} (Delta: {dt})...")

    # Cálculo del Día Juliano base
    jd0_annio = funciones.DiaJul(1,1,annio,0.0)
    jd  = jd0_annio + (da - 1)

    can = f"{annio:04d}" # Cadena año

    # --- Cabecera ---
    dia, mes, anomas, _ = funciones.DJADia(jd + 1)
    nombre_mes = MesANom(mes)
    nombre_dia_sem = num_a_dia[DiaSem(jd)]

    # Ruta de salida
    fichero_salida = ruta_data / "data" / "almanaque_nautico" / f"{annio}" / "PAG.dat"
    fichero_salida.parent.mkdir(parents=True, exist_ok=True)

    # Variables de estado para interpolación en el formato final
    org = [0]*6; orm = [0]*6
    err = 0.05      # Tolerancia estándar

    # --- Manejo de Contexto de Salida (Archivo vs Memoria) ---
    class OutputContext:
        """Context Manager para manejar escritura en archivo o en buffer de memoria StringIO."""
        def __init__(self, path, return_content):
            self.path = path
            self.return_content = return_content
            self.f = None

        def __enter__(self):
            if self.return_content:
                self.f = io.StringIO()
            else:
                self.f = open(self.path, 'w', encoding='utf-8')
            return self.f

        def __exit__(self, exc_type, exc_val, exc_tb):
            if not self.return_content:
                self.f.close()
            pass

    # Inicio del bloque de escritura
    with OutputContext(fichero_salida, return_content) as f23:
        # Escritura de Título
        f23.write(f" {nombre_dia_sem:>9}   {dia}  de  {nombre_mes}  de  {anomas}\n")

        # --- Datos Diarios del SOL ---
        pmg_sol = Paso_Mer(jd, 'sol', dt)
        org[1], mie_sol = HOMI(pmg_sol)

        # Ajuste visual si el minuto se redondea a 60
        if mie_sol >= 59.95:
            mie_sol = 0.0
            org[1] += 1

        t_mediodia = ts.tt_jd(jd + 0.5 + dt/86400.0)
        _, _, dist_sol = cal_coord_ap('sol', t_mediodia)

        sd_sol = calc_sd_sol(dist_sol)

        f23.write(f"S D : {sd_sol:4.1f}\n")
        f23.write(f"PMG : {org[1]:2d} {mie_sol:4.1f}\n")       

        # --- Datos Diarios de la LUNA ---
        _, _, dist_lun = cal_coord_ap('lun', t_mediodia)
        sd_lun = calc_sd_luna(dist_lun)
        f23.write(f"S D : {sd_lun:4.1f}\n")

        # Cálculo de la Edad de la Luna (días desde Luna Nueva)
        fichero_fases = ruta_data / "data" / "almanaque_nautico" /  f"{can}" / f"Fases{can}.dat"
        edad_luna = 0.0

        try:
            if fichero_fases.exists():
                # Leemos las fases pre-calculadas
                vals = [float(x) for x in fichero_fases.read_text().split()]
                ult_fase = 0.0
                
                for idx, v in enumerate(vals):
                    # Buscamos la última fase (Luna Nueva) ocurrida antes o en el día actual
                    if v > 0 and v > jd:
                        break # Paramos al encontrar una fecha futura
                    
                    if v > 0 and (idx % 4 == 0): # idx % 4 == 0 implica Luna Nueva
                        ult_fase = v
                
                if ult_fase > 0:
                    edad_luna = jd - ult_fase
                else:
                    edad_luna = 0.0
            else:
                print("Aviso: no existe fichero Fases, edad_luna = 0")
        except:
            edad_luna = 0.0

        f23.write(f"Edad : {edad_luna:4.1f}\n")

        # Índice de la imagen de la fase lunar (0-11)
        nfl = int((edad_luna * 10 - 13) / 24) + 1
        nfl = nfl % 12

        # PMG de la Luna
        pmg_lun = Paso_Mer(jd, 'lun', dt)
        org[2], orm[2] = HOMIEN(pmg_lun)
        f23.write(f"PMG : {org[2]:2d} {orm[2]:2d}\n")

        # PHE de la Luna (Paralaje Horizontal Ecuatorial) - calculado cada 8 horas
        for i in range(4, 21, 8):
            t_phe = ts.tt_jd(jd + i/24.0 + dt/86400.0)
            _, _, d_phe = cal_coord_ap('lun', t_phe)
            phe = calc_phe_luna(d_phe)
            f23.write(f"PHE : {i:2d} {phe:4.1f}\n")

        # Retardo del paso de la Luna (diferencia con el día siguiente)
        pmg_lun_sig = Paso_Mer(jd + 1.0, 'lun', dt)
        ret_pmg = ROUND(60.0 * pmg_lun_sig) - ROUND(60.0 * pmg_lun)
        f23.write(f"Rº PMG {ret_pmg:3d}\n")
 
        # Definición de latitudes para visualización
        LAT_VALS = [60, 58, 56, 54, 52, 50, 45, 40, 35, 30, 20, 10, 0,
                -10, -20, -30, -35, -40, -45, -50, -52, -54, -56, -58, -60]
        
        LAT_STRS = ['60 N', '58  ', '56  ', '54  ', '52  ', '50  ',
                    '45  ', '40  ', '35  ', '30  ', '20  ', '10 N',
                    ' 0  ', '10 S', '20  ', '30  ', '35  ', '40  ',
                    '45  ', '50  ', '52  ', '54  ', '56  ', '58  ',
                    '60 S']

        # ----------------------------------------------------------------------------------
        # SECCIÓN VECTORIZADA (SOL Y LUNA)
        # ----------------------------------------------------------------------------------
        # Aquí optimizamos el rendimiento calculando las 24 horas (indices 0-24) de una sola vez
        # usando arrays de NumPy en lugar de un bucle "for" convencional para las llamadas a Skyfield.
        
        # 1. Crear vector de tiempos (0..24 horas)
        horas_vec = np.arange(25)
        t_vec_main = ts.tt_jd(jd + horas_vec/24.0)

        # 2. Calcular SOL para las 25 horas
        ast_sol_vec = tierra.at(t_vec_main).observe(sol).apparent()
        ra_sol_vec, dec_sol_vec, _ = ast_sol_vec.radec(epoch='date')
        
        # Cálculo vectorizado del GHA: (GAST - RA) % 360
        gh_sol_deg_arr = (t_vec_main.gast * 15.0 - ra_sol_vec.hours * 15.0) % 360.0
        dec_sol_deg_arr = dec_sol_vec.degrees

        # 3. Calcular LUNA para las 25 horas
        ast_lun_vec = tierra.at(t_vec_main).observe(luna).apparent()
        ra_lun_vec, dec_lun_vec, _ = ast_lun_vec.radec(epoch='date')

        gh_lun_deg_arr = (t_vec_main.gast * 15.0 - ra_lun_vec.hours * 15.0) % 360.0
        dec_lun_deg_arr = dec_lun_vec.degrees

        CONST_MOV_MEDIO_LUNA_MIN = 859.0 # Valor constante para interpolación "v"

        # ----------------------------------------------------------------------------------
        # OPTIMIZACIÓN: CACHE DE FENÓMENOS (ORTOS/OCASOS)
        # ----------------------------------------------------------------------------------
        # Pre-calculamos todos los eventos (salidas, puestas, crepúsculos) antes de imprimir
        # las filas. Esto evita recalcular la geometría solar/lunar cientos de veces.
        
        # Determinamos si es página par (0) o impar (1) para saber qué eventos mostrar
        pn_calc = (da + 1) % 2
        if pn_calc == 0:
            eventos_sol = ['pcn', 'pcc', 'ort'] # Principio Crepúsculo Nautico/Civil, Orto
        else:
            eventos_sol = ['oca', 'fcc', 'fcn'] # Ocaso, Fin Crepúsculo Civil/Nautico
        
        # Cache Fenómenos SOLARES
        cache_fenosol = {}
        for lat_val in LAT_VALS:
            for evt in eventos_sol:
                cache_fenosol[(lat_val, evt)] = fenosol(jd, lat_val, evt)
        
        # Cache Fenómenos LUNARES (Hoy y Mañana para cálculo de retraso)
        cache_fenoluna_hoy = {}
        cache_fenoluna_maniana = {}
        for lat_val in LAT_VALS:
            for evt in ['ort', 'oca']:
                cache_fenosol_hoy = fenoluna(jd, lat_val, evt)
                cache_fenoluna_hoy[(lat_val, evt)] = cache_fenosol_hoy
                cache_fenoluna_maniana[(lat_val, evt)] = fenoluna(jd + 1, lat_val, evt)

        # Bucle de impresión de filas (0 a 24 horas)
        for i in range(25):
            # Extraemos datos pre-calculados de los arrays
            gh_sol_deg = gh_sol_deg_arr[i]
            dec_sol_deg = dec_sol_deg_arr[i]
            
            gh_lun_deg = gh_lun_deg_arr[i]
            dec_lun_deg = dec_lun_deg_arr[i]

            # Formateo
            hgg_sol, hgm_sol = formato_grado_minuto(gh_sol_deg, 0.05) 
            sgn_sol, deg_sol, dem_sol = formato_signo_grado_minuto(dec_sol_deg, 0.05)

            hgg_lun, hgm_lun = formato_grado_minuto(gh_lun_deg, 0.05)
            sgn_lun, deg_lun, dem_lun = formato_signo_grado_minuto(dec_lun_deg, 0.05)

            # Cálculo de "v" y "d" (variaciones horarias)
            if i > 0:
                prev_gha_lun = gh_lun_deg_arr[i-1]
                prev_dec_lun = dec_lun_deg_arr[i-1]

                # Cálculo de 'v' (variación del GHA)
                diff_gha = gh_lun_deg - prev_gha_lun
                if diff_gha < -180.0: diff_gha += 360.0 # Corrección por salto de día (360 -> 0)
                
                diff_mins = diff_gha * 60.0
                v_float = (diff_mins - CONST_MOV_MEDIO_LUNA_MIN) * 10.0
                v_final = int(round(v_float))

                # Cálculo de 'd' (variación de declinación)
                diff_dec = abs(dec_lun_deg - prev_dec_lun)
                d_float = diff_dec * 60.0 * 10.0
                d_final = int(round(d_float))


            # --- Recuperación de Fenómenos desde Cache ---
            lat_act_val = LAT_VALS[i]
            lat_act_str = LAT_STRS[i]

            # Sol
            vals_sol = []
            for evt in eventos_sol:
                hora_raw = cache_fenosol[(lat_act_val, evt)]
                h_entera, m_entera = HOMIEN(hora_raw)    
                vals_sol.extend([h_entera, m_entera])

            # Luna
            vals_lun = []
            for evt in ['ort', 'oca']:
                hora_raw = cache_fenoluna_hoy[(lat_act_val, evt)]
                h_entera, m_entera = HOMIEN(hora_raw)
                
                maniana = cache_fenoluna_maniana[(lat_act_val, evt)]
                today = hora_raw  
                
                # Cálculo del retardo lunar (diferencia entre mañana y hoy)
                if maniana is None or today is None:
                    ret_val = 9999
                else:
                    ret_val = ROUND(maniana * 60) - ROUND(today * 60)
                vals_lun.extend([h_entera, m_entera, int(ret_val)])  

            # --- Construcción de la línea de texto ---
            s_sol = f"{hgg_sol:3d} {hgm_sol:4.1f} {sgn_sol} {deg_sol:2d} {dem_sol:4.1f}"
            s_lun = f"{hgg_lun:3d} {hgm_lun:4.1f}"
            s_lun_dec = f"{sgn_lun} {deg_lun:2d} {dem_lun:4.1f}"
            
            s_fen_sol = f"{vals_sol[0]:2d} {vals_sol[1]:2d}  {vals_sol[2]:2d} {vals_sol[3]:2d}  {vals_sol[4]:2d} {vals_sol[5]:2d}"
            s_fen_lun = f"{vals_lun[0]:2d} {vals_lun[1]:2d} {vals_lun[2]:3d}  {vals_lun[3]:2d} {vals_lun[4]:2d} {vals_lun[5]:3d}"

            # Escritura condicional (la fila 0 no lleva 'v' ni 'd')
            if i == 0:
                linea = f"&{i:2d}  {s_sol}  {s_lun}     {s_lun_dec}      {lat_act_str}  {s_fen_sol}  {s_fen_lun}"
            else:
                linea = f"&{i:2d}  {s_sol}  {s_lun} {v_final:3d} {s_lun_dec} {d_final:3d}  {lat_act_str}  {s_fen_sol}  {s_fen_lun}"            
            f23.write(linea + "\n")


        # --- PIE DE PÁGINA (Planetas y Aries) ---
        pmg_ari = Paso_Mer(jd, 'ari', dt)
        h_ari, m_ari = HOMI(pmg_ari)
        f23.write(f"PMG Aries : {h_ari:2d} {m_ari:4.1f}\n")

        cuerpos_orden  = ['ven', 'mar', 'jup', 'sat']
        for k in cuerpos_orden: 
            h, m = HOMIEN(Paso_Mer(jd, k, dt))
            
            # Magnitud
            val_mag = Mag_visual(jd + 0.5, k)
            sig = '+' if val_mag > 0 else '-'

            f23.write(f"PMG : {h:2d} {m:2d}\nMag. : {sig}{abs(val_mag):4.1f}\n")

        # ----------------------------------------------------------------------------------
        # SECCIÓN VECTORIZADA (PLANETAS)
        # ----------------------------------------------------------------------------------
        # Similar a la sección Sol/Luna, pero usando UT1 para precisión
        t_vec_planets = ts.ut1_jd(jd + horas_vec/24.0)

        # 1. Calcular Aries Vectorizado
        gh_ari_arr = (t_vec_planets.gast * 15.0) % 360.0

        # 2. Pre-calcular posiciones de planetas
        planets_data = {}
        for k in cuerpos_orden:
            obj_planet = plan_dic[k]
            
            ast_p = tierra.at(t_vec_planets).observe(obj_planet).apparent()
            ra_p, dec_p, _ = ast_p.radec(epoch='date')
            
            # GHA = GAST - RA
            gh_p_arr = (t_vec_planets.gast * 15.0 - ra_p.hours * 15.0) % 360.0
            dec_p_arr = dec_p.degrees
            
            planets_data[k] = (gh_p_arr, dec_p_arr)

        # Impresión de la tabla inferior (Planetas)
        for i in range(25):
            # Aries
            gha_ari = gh_ari_arr[i]
            tsg, tsm = formato_grado_minuto(gha_ari, err)

            linea = f"&{i:2d} {tsg:3d} {tsm:4.1f} "
            
            # Iteramos planetas
            for k in cuerpos_orden:
                gh_arr, dec_arr = planets_data[k]
                
                gha = gh_arr[i]
                dec = dec_arr[i]

                hg, hm = formato_grado_minuto(gha, err)
                sg, dg, dm = formato_signo_grado_minuto(dec, err)

                linea += f"{hg:3d} {hm:4.1f} {sg} {dg:2d} {dm:4.1f}  "
            
            f23.write(linea + "\n")

        # --- Bloque de Diferencias ---
        # Calcula cuánto varía el GHA y la Dec en 24 horas para dar ayudas de interpolación
        dif_str = "DIF         "
        for k in cuerpos_orden:
            t0, t1 = ts.ut1_jd(jd), ts.ut1_jd(jd + 1.0)
            gha0, dec0, _ = cal_coord_ap(k, t0)
            gha1, dec1, _ = cal_coord_ap(k, t1)
            
            d_gha = gha1 - gha0
            if d_gha < -180:
                d_gha += 360 # Normalización del giro

            val_h = (d_gha * 60.0 * 10.0) /24.0
            if abs(val_h) > 4500:
                val_h -= np.sign(val_h) * 9000
            
            sh, ah = SIGENT(ROUND(val_h))

            d_dec = dec1 - dec0
            val_d = (d_dec * 60.0 * 10.0) / 24.0
            sd, ad = SIGENT(ROUND(val_d))

            dif_str += f"      {sh} {ah:2d}      {sd} {ad:2d}"

        f23.write(dif_str + "\n")
        
        # Referencia al archivo gráfico de la fase lunar
        f23.write(f"\\def\\figlun{{FigLuna{nfl+1:02d}.epsf scaled 120}}\n")
        
        if return_content:
            content = f23.getvalue()
            # f23 se cierra automáticamente por el context manager al salir, 
            # pero para StringIO a veces queremos el valor antes de cerrar.
            return content

    print(f"Fichero generado: {fichero_salida}")

# Bloque de prueba (comentado en original)
"""""
if __name__ == "__main__":
    iniCrono = time.perf_counter()
    UNAPAG(1,2012,69)
    finCrono = time.perf_counter()
    tiempoTotal = finCrono - iniCrono

    print(f"\nProceso completado.\n")
    print(f"Tiempo en segundos = {tiempoTotal}")
"""""