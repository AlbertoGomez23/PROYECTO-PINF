import sys
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor

# =============================================================================
# 1. CONFIGURACIÓN DE RUTAS E IMPORTACIONES
# =============================================================================
# Propósito: Este bloque es fundamental para que Python encuentre los módulos
# hermanos (utils, paginas_an, src) sin importar desde dónde se ejecute el script.

# Obtenemos la ruta absoluta de ESTE fichero
ruta_fichDatAN = Path(__file__).resolve()

# Obtenemos la ruta de la carpeta contenedora 'paginas_an'
ruta_paginas_an = ruta_fichDatAN.parent

# Obtenemos la ruta 'src' (padre de paginas_an y utils)
ruta_Padre = ruta_paginas_an.parent

# Añadimos las rutas al PATH del sistema si no están ya
if str(ruta_paginas_an) not in sys.path:
    sys.path.append(str(ruta_paginas_an))

if str(ruta_Padre) not in sys.path:
    sys.path.append(str(ruta_Padre))

# Bloque de seguridad adicional para rutas relativas
try:
    ruta_base = Path(__file__).resolve().parent.parent
except NameError:
    ruta_base = Path.cwd().parent

ruta_str = str(ruta_base)
if ruta_str not in sys.path:
    sys.path.append(ruta_str)

# =============================================================================
# 2. IMPORTACIÓN DE MÓDULOS DEL PROYECTO
# =============================================================================
try:
    # UNAPAG: Genera los datos numéricos de un día (Ascensión Recta, Declinación, etc.)
    # PagTexProcessor: Toma esos datos y escribe el código LaTeX para el PDF.
    from pagEntera import UNAPAG
    from pagLatex import PagTexProcessor
except ImportError:
    # Fallback: Intento de importación absoluta si la relativa falla
    from src.paginas_an.pagEntera import UNAPAG
    from src.paginas_an.pagLatex import PagTexProcessor

try:
    # 'funciones': Utilidades generales (conversión de fechas, Julian Day).
    # 'faseLuna': Módulo específico para calcular fases lunares del año.
    from utils import funciones 
    from fase_luna import faseLuna
except ImportError as e:
    # Se permite continuar, pero algunas funcionalidades podrían fallar
    print(f"Advertencia: No se pudieron cargar utilidades externas: {e}")

# =============================================================================
# 3. FUNCIONES AUXILIARES
# =============================================================================

def IDIAAN(dia: int, mes: int, anio: int) -> int:
    """
    Calcula el número de día dentro del año (Day of Year, DOY).
    Ejemplo: 1 de Enero -> 1, 31 de Diciembre -> 365 (o 366).
    
    Args:
        dia, mes, anio: Fecha calendario.
    
    Returns:
        int: Número de día (1-366).
    """
    # Lógica: (JD de la fecha actual) - (JD del 0 de Enero del mismo año)
    return funciones.DiaJul(dia, mes, anio, 0) - funciones.DiaJul(0, 1, anio, 0)     

def process_day(args):
    """
    Función 'Worker' para el procesamiento en paralelo.
    Esta función se ejecuta de forma aislada en un núcleo del procesador.
    
    Args:
        args (tuple): (dia, anio, dt, ruta_latex)
    
    Returns:
        tuple: (dia, pag_content) -> Devuelve el día y el string de datos generado.
    """
    dia, anio, dt, ruta_latex = args
    
    # 1. CÁLCULO DE DATOS ASTRONÓMICOS
    # Llamamos a UNAPAG con return_content=True para obtener el string en memoria
    # en lugar de escribirlo a disco inmediatamente (evita bloqueos de I/O).
    pag_content = UNAPAG(dia, anio, dt, return_content=True)
    
    # 2. GENERACIÓN DE LATEX INDIVIDUAL
    # Definimos el nombre del archivo .dat intermedio para LaTeX
    if dia < 100: 
        nombre_fich = f"AN{anio}{dia:02d}.dat"
    else:       
        nombre_fich = f"AN{anio}{dia:03d}.dat"
    
    path_latex_out = ruta_latex / nombre_fich
    
    # Instanciamos un procesador LaTeX propio para este hilo/proceso
    procLatex = PagTexProcessor()
    procLatex.pagtex_bis(dia, anio, input_content=pag_content, output_path=path_latex_out)
    
    return dia, pag_content

# =============================================================================
# 4. FUNCIÓN PRINCIPAL (ORQUESTADOR)
# =============================================================================

def generarFichero(anio: int, dt: float, opcion: int = 1):
    """
    Controlador principal para la generación del Almanaque Náutico.
    
    Args:
        anio (int): Año a calcular.
        dt (float): Delta T (Diferencia entre Tiempo Terrestre y Universal) en segundos.
        opcion (int): 
            1 -> Generar AÑO COMPLETO (Multiproceso).
            2 -> Generar un solo DÍA (Interactivo).
            3 -> Generar un INTERVALO de días (Interactivo).
            
    Returns:
        str: Ruta del directorio donde se generaron los archivos.
    """
    
    ruta_final = Path("")

    # Validación de entrada de usuario
    while True:
        try:
            if opcion in [1, 2, 3]:
                break
            else:
                print("Error, opción inválida.\n")
                opcion = int(input("Introduzca su opción (1, 2 o 3): "))
        except ValueError:
            print("Error, escriba un valor válido.\n")

    # Definición de la ruta de salida de datos
    ruta_datos = ruta_Padre.parent.parent / "data" / "almanaque_nautico"

    match opcion:
        # ---------------------------------------------------------------------
        # CASO 1: GENERACIÓN MASIVA (AÑO COMPLETO)
        # ---------------------------------------------------------------------
        case 1: 
            print(f"--- Iniciando generación completa para el año {anio} ---")
            
            # Preparación de directorios
            ruta_final = ruta_datos / str(anio)
            ruta_final.mkdir(parents=True, exist_ok=True)
            
            ruta_latex = ruta_final / "latex"
            ruta_latex.mkdir(parents=True, exist_ok=True)

            # 1. Pre-cálculo de fases lunares (global para el año)
            print("Calculando fases lunares...")
            faseLuna.FasesDeLaLunaDatos(anio, dt)

            # 2. Configuración del archivo maestro
            canio = f"{anio:04d}"
            ComDat = ruta_final / f"AN{canio}COM.dat"

            try:
                # Determinamos si es bisiesto implícitamente (366 días por seguridad)
                # (Nota: Sería ideal una función is_leap(anio), aquí asumimos 366 
                # y el código interno manejará si el día existe o no).
                num_dias = 366 
                
                # --- PREPARACIÓN DEL MULTIPROCESAMIENTO ---
                
                # Creamos una lista de tareas (argumentos) para enviar a los trabajadores.
                day_args = [(i, anio, dt, ruta_latex) for i in range(1, num_dias + 1)]
                
                # Diccionario para almacenar resultados desordenados y reordenarlos después
                results_dict = {}
                
                print(f"Lanzando hilos de procesamiento para {num_dias} días...")
                
                # Executor gestiona el pool de procesos (usa todos los núcleos de la CPU)
                with ProcessPoolExecutor() as executor:
                    # 'submit' envía la tarea inmediatamente y devuelve un objeto 'Future'
                    futures = {executor.submit(process_day, arg): arg[0] for arg in day_args}
                    
                    # Recogemos resultados conforme terminan (asynchronously)
                    for future in futures:
                        try:
                            dia, content = future.result()
                            results_dict[dia] = content
                            # Feedback visual simple
                            if dia % 50 == 0: print(f"... procesado día {dia}")
                        except Exception as exc:
                            print(f"EXCEPCIÓN CRÍTICA en día {futures[future]}: {exc}")

                # 3. ESCRITURA SECUENCIAL (MASTER DATA FILE)
                # Escribimos los resultados en el orden correcto (1 al 366)
                print("Escribiendo archivo maestro de datos...")
                with open(ComDat, "w", encoding="utf-8") as f_out:
                    for i in range(1, num_dias + 1):
                        if i in results_dict:
                            f_out.write(results_dict[i])
                        else:
                            # Puede pasar si el día 366 no existe en un año no bisiesto
                            # Depende de cómo UNAPAG maneje días inválidos.
                            pass 
                
                # 4. FUSIÓN DE ARCHIVOS LATEX
                # Concatenamos todos los pequeños archivos .dat de LaTeX en uno solo
                # para facilitar la compilación del PDF final.
                print("Generando archivo maestro LaTeX...")
                latex_completo = ruta_final / f"AN{anio}COMLatex.dat"
                
                with open(latex_completo, 'w', encoding='latin-1') as f_latex:
                    for j in range(1, num_dias + 1):
                        if j < 100: nombre_latex = f"AN{anio}{j:02d}.dat"
                        else:       nombre_latex = f"AN{anio}{j:03d}.dat"
                        
                        latex_file = ruta_latex / nombre_latex
                        if latex_file.exists():
                            f_latex.write(latex_file.read_text(encoding='latin-1'))
                            f_latex.write("\n")
                
                print(f"¡Éxito! Archivos generados en: {ruta_final}")

            except IOError as e:
                print(f"Error fatal de E/S: {e}")
                return

        # ---------------------------------------------------------------------
        # CASO 2: UN SOLO DÍA (MODO INTERACTIVO)
        # ---------------------------------------------------------------------
        case 2: 
            try:
                fecha = input("Escriba día (dd), mes (mm) y año(aaaa) [separado por espacios]: ")
                datos = fecha.replace(',',' ').split()

                dia = int(datos[0])
                mes = int(datos[1])
                anio = int(datos[2])

                dt = int(input("Introduzca dt (dt = TT - UT) aprox 69s: "))
            except (ValueError, IndexError):
                print("Error: formato de fecha o dt incorrecto")
                return

            faseLuna.FasesDeLaLunaDatos(anio, dt)
            diaAnio = IDIAAN(dia, mes, anio)
            
            # Genera solo PAG.DAT (comportamiento por defecto de UNAPAG)
            UNAPAG(diaAnio, anio, dt)          

        # ---------------------------------------------------------------------
        # CASO 3: INTERVALO (MODO INTERACTIVO)
        # ---------------------------------------------------------------------
        case 3:
            try:
                fecha = input("Fecha inicial (dd,mm,aaaa): ")
                datos = fecha.replace(',',' ').split()

                diaIni = int(datos[0])
                mesIni = int(datos[1])
                anioIni = int(datos[2])

                nDias = int(input("Número de días a calcular: "))
                dt = int(input("Introduzca dt: "))
            except (ValueError, IndexError):
                print("Error en los datos de entrada.")
                return

            faseLuna.FasesDeLaLunaDatos(anioIni, dt)
            diaAnIni = IDIAAN(diaIni, mesIni, anioIni)

            print("Procesando intervalo...")
            for i in range(nDias):
                diaActual = diaAnIni + i
                UNAPAG(diaActual, anioIni, dt)
                
    return str(ruta_final)

# Punto de entrada para ejecución directa
if __name__ == "__main__": 
    # Ejemplo de uso: Año 2015, DeltaT 68 segundos
    generarFichero(2015, 68)