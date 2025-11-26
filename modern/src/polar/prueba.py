import math
import numpy as np
import os
import sys
from pathlib import Path

# Obtener el directorio actual del script
current_dir = Path(__file__).resolve()
# Obtener el directorio src
ruta_src = current_dir.parent.parent

# Añadir el directorio padre al path
if str(ruta_src) not in sys.path:
    sys.path.append(str(ruta_src))


from estrellas.LeeDE440 import *
from estrellas.SubrEstr import *
from estrellas.ReduEstr import *
from estrellas.prepaes import *
from estrellas.PasoMeGr import *

# Constantes y datos de la Estrella Polar
sti2dia = 0.1157407407407407e-04  # 1/60/60/24
pa = 0.662403647456573  # 9108.704(s) 1/60/60 (2 Pi)/24
pap = 1.445496230912137e-03  # 19.877(s/SJ) 1/60/60 (2 Pi)/24
pd = 1.55795225490400  # 321350.72(") 1/60/60 (2 Pi)/360
pdp = -7.369167952864947e-06  # -1.52("/SJ) 1/60/60 (2 Pi)/360
ppi = 0.0  # 0.003(") 1/60/60 (2 Pi)/360
pvr = 0.0  # -17.4(km/s) 60 60 24 36525/1.4959787066e+8
deg = 0.174532925199433e-01
PI = 4.0 * math.atan(1.0)


def crear_carpeta_resultados(ano):
    """
    Crea la carpeta resultados si no existe y devuelve la ruta
    resultados_dir = os.path.join(current_dir, 'resultados', str(ano))
    os.makedirs(resultados_dir, exist_ok=True)
    return resultados_dir
    """

    can = str(ano)
    
    # 1. Obtener la ruta base del proyecto
    # Path(__file__) es el archivo actual. .parent es su carpeta.
    # Subimos los niveles necesarios segun tu logica original:
    ruta_actual = Path(__file__).resolve().parent
    ruta_proyecto = ruta_actual.parent.parent.parent 
    # NOTA: Ajusta el número de .parent según dónde esté tu script exactamente. 
    # Si tu script está en: proyecto/src/algo/script.py, usar 3 .parent suele llevar a 'proyecto'.

    # 2. Definir la ruta destino: proyecto/data/almanaque_nautico/2024
    # Usamos el operador '/' que une rutas limpiamente en pathlib
    ano_dir = ruta_proyecto / "data" / "almanaque_nautico" / can
    
    # 3. Crear la carpeta si no existe
    # parents=True crea las carpetas padres si faltan. exist_ok=True evita error si ya existe.
    ano_dir.mkdir(parents=True, exist_ok=True)
    return ano_dir


def prueba():
    """Programa principal - migración fiel del código Fortran"""
    global PI, sti2dia, pa, pap, pd, pdp, ppi, pvr, deg
    
    # Variables locales
    can = ""
    u = [""] * 53  # 0:52
    ano = 0
    j = 0
    k = 0
    m = 0
    dT = 0
    dj = 0.0
    al = [0.0] * 13
    de = [0.0] * 13
    a0 = 0.0
    d0 = 0.0
    cd = 0.0
    ha = 0.0
    t_val = 0.0
    sol = [0.0] * 3
    b_e = [0.0] * 7
    dpsi = 0.0
    deps = 0.0
    ct = 0.0
    pn = np.zeros((3, 3))
    alt = 0.0
    agr = 0
    ami = 0
    dgr = 0
    dmi = 0
    ase = 0.0
    dse = 0.0
    
    # Arrays para cálculos
    h = [0.0] * 7  # 0:6
    co = [0.0] * 15  # 2:14
    
    print(" introduzca año (XXXX)")
    ano = int(input())
    can = str(ano)
    print(" introduzca ΔT = TT - UT")
    dT = int(input())
    
    # Crear carpeta de resultados para este año
    resultados_dir = crear_carpeta_resultados(ano)
    print(f"Los archivos se guardarán en: {resultados_dir}")
    
    # Cálculo de valores medios de (AR,δ)
    a0 = 0.0
    d0 = 0.0
    
    # Abrir efemérides
    ABRDE200()  # 12 == OBLIGATORIO
    
    for k in range(1, 14):  # 1 a 13
        dj = DIAJUL(1, k, ano, 0.0)
        BARNULEE(dj, dT, sol, b_e, dpsi, deps)
        ct = TOCENT(dj + dT * sti2dia)  # intervalo en centurias
        pn = PRENUT(ct, dpsi, deps)  # Corregimos por precesión y nutación
        
        al[k-1] = pa
        de[k-1] = pd
        
        # Aplicar reducción astrométrica
        arp_temp, dep_temp = REDUESTR(al[k-1], de[k-1], pap, pdp, ppi, pvr, pn, b_e, sol, ct)
        al[k-1] = arp_temp
        de[k-1] = dep_temp
        
        # Convertir a grados, minutos, segundos
        ase = al[k-1] / deg
        agr = int(ase)
        ase = 60.0 * (ase - agr)
        ami = int(ase)
        ase = 60.0 * (ase - ami)
        
        dse = de[k-1] / deg
        dgr = int(dse)
        dse = 60.0 * (dse - dgr)
        dmi = int(dse)
        dse = 60.0 * (dse - dmi)
        
        print(f"{k:2d} {agr:3d} {ami:2d} {ase:5.2f} {dgr:2d} {dmi:2d} {dse:5.2f}")
        
        a0 = a0 + al[k-1]
        d0 = d0 + de[k-1]
    
    # Cerrar efemérides
    CIERRADE()  # CLOSES u = 'JPLEPH'
    
    a0 = a0 / 13.0
    d0 = d0 / 13.0
    cd = PI / 2.0 - d0
    
    # Cálculo de la TABLA I - CORRECCIÓN FINAL DEL FORMATO
    print(' TABLA I')

    # Inicializar el array u correctamente
    u = [""] * 53  # 0:52

    for k in range(0, 53):  # 0 a 52 (26*2 + 1)
        for j in range(0, 7):  # 0 a 6
            h[j] = (0.5 * k + 26 * j) * deg
            t_val = h[j] - a0
            co[j+2] = -cd * math.cos(t_val) * 60.0 / deg
        
        # Formatear línea como en Fortran (7(F6.1,F3.0,F5.1)) - CORREGIDO
        line_parts = []
        for j in range(7):
            # Formato: F6.1 para el ángulo, F3.0 para el signo, F5.1 para el valor
            angle = h[j] / deg
            sign_char = '+' if co[j+2] >= 0 else '-'
            value = abs(co[j+2])
            
            # CORRECCIÓN: Convertir el ángulo a entero (sin decimales)
            angle_int = int(round(angle))
            
            # CORRECCIÓN: Asegurar que cada segmento tenga exactamente 14 caracteres
            # F6.1 = 6 caracteres para ángulo entero: "   0" o "  26"  
            # F3.0 = 3 caracteres para signo: "  -" o "  +"
            # F5.1 = 5 caracteres para valor: " 30.4" o " 39.3"
            
            angle_str = f"{angle_int:6d}"      # 6 caracteres: "     0" o "    26"  
            sign_str = f"{sign_char:3s}"       # 3 caracteres: "  -" o "  +"
            value_str = f"{value:5.1f}"        # 5 caracteres: " 30.4" o " 39.3"
            
            # Combinar en un segmento de 14 caracteres
            segment = f"{angle_str}{sign_str}{value_str}"
            line_parts.append(segment)
        
        u[k] = ''.join(line_parts)
        
        # Ajustes de formato como en Fortran
        if len(u[k]) >= 98:
            u_list = list(u[k])
            
            # Esto para que ponga el signo + donde sea espacio
            positions = [6, 20, 34, 48, 62, 76, 90]
            for pos in positions:
                if pos < len(u_list) and u_list[pos] == ' ':
                    u_list[pos] = '+'
            
            # Esto para que no ponga signo si es 0.0
            zero_positions = [(10, 14), (24, 28), (38, 42), (52, 56), (66, 70), (80, 84), (94, 98)]
            zero_indicators = [6, 20, 34, 48, 62, 76, 90]
            
            for (start, end), indicator_pos in zip(zero_positions, zero_indicators):
                if start < len(u[k]) and end <= len(u[k]):
                    if u[k][start:end] == ' 0.0':
                        u_list[indicator_pos] = ' '
            
            u[k] = ''.join(u_list)

    # Escribir archivo AN{ano}382.DAT - CORREGIDO EL FORMATO DE ESPACIOS
    try:
        archivo_382 = os.path.join(resultados_dir, f'AN{can}382.DAT')
        with open(archivo_382, 'w') as f22:
            for k in range(0, 53):  # 0 a 52
                if len(u[k]) != 98:
                    continue
                    
                # Parsear correctamente cada segmento de 14 caracteres
                segments = []
                for i in range(7):
                    start = i * 14
                    end = start + 14
                    segment = u[k][start:end]
                    segments.append(segment)
                
                # CORRECCIÓN: Calcular horas y minutos desde k (una vez por fila)
                hours = k // 2
                minutes = "00" if k % 2 == 0 else "30"
                
                # Construir la línea correctamente - CORREGIDO FORMATO DE ESPACIOS
                line_parts = []
                for i, segment in enumerate(segments):
                    if len(segment) == 14:
                        # Extraer componentes - NO HACER STRIP EN value_part
                        angle_part = segment[0:6].strip()
                        sign_part = segment[6:9].strip()
                        value_part = segment[9:14]  # ¡SIN .strip()!
                        
                        # CORRECCIÓN: Formatear con espacios exactos como en Fortran
                        # Ángulo: 3 caracteres con espacios a la izquierda
                        angle_formatted = f"{int(angle_part):3d}"
                        
                        # Valor: usar directamente value_part que ya tiene los espacios correctos
                        value_formatted = value_part
                        
                        col = f"{angle_formatted}&{minutes}&${sign_part}$&{value_formatted}"
                        line_parts.append(col)
                
                if len(line_parts) == 7:
                    line = "&".join(line_parts) + "\\\\"
                    
                    # Añadir [1ex] cada 6 líneas para líneas impares (k impar)
                    if k % 2 == 1 and (k // 2) % 3 == 2:  # k=5,11,17,23,etc.
                        line = line.replace("\\\\", "\\\\[1ex]")
                    
                    f22.write(line + '\n')
        
        print(f"Archivo creado: {archivo_382}")
        
    except Exception as e:
        print(f"Error escribiendo archivo AN{can}382.DAT: {e}")
    


    # Segunda parte de la TABLA I - CON EL MISMO FORMATO QUE LA PRIMERA PARTE
    for k in range(182*2, 26*2 + 182*2 + 1):
        m = k - 364
        for j in range(0, 7):  # 0 a 6
            h[j] = (0.5 * k + 26 * j) * deg
            t_val = h[j] - a0
            co[j+2] = -cd * math.cos(t_val) * 60.0 / deg
        
        # Formatear línea con el MISMO formato que la primera parte
        line_parts = []
        for j in range(7):
            # Formato: F6.1 para el ángulo, F3.0 para el signo, F5.1 para el valor
            angle = h[j] / deg
            sign_char = '+' if co[j+2] >= 0 else '-'
            value = abs(co[j+2])
            
            # Convertir el ángulo a entero (sin decimales)
            angle_int = int(round(angle))
            
            # Asegurar que cada segmento tenga exactamente 14 caracteres
            angle_str = f"{angle_int:6d}"      # 6 caracteres: "   182" o "   208"  
            sign_str = f"{sign_char:3s}"       # 3 caracteres: "  -" o "  +"
            value_str = f"{value:5.1f}"        # 5 caracteres: " 31.4" o " 39.7"
            
            # Combinar en un segmento de 14 caracteres
            segment = f"{angle_str}{sign_str}{value_str}"
            line_parts.append(segment)
        
        u[m] = ''.join(line_parts)
        
        # Ajustes de formato como en Fortran - IGUAL QUE LA PRIMERA PARTE
        if len(u[m]) >= 98:
            u_list = list(u[m])
            
            # Esto para que ponga el signo + donde sea espacio
            positions = [6, 20, 34, 48, 62, 76, 90]
            for pos in positions:
                if pos < len(u_list) and u_list[pos] == ' ':
                    u_list[pos] = '+'
            
            # Esto para que no ponga signo si es 0.0
            zero_positions = [(10, 14), (24, 28), (38, 42), (52, 56), (66, 70), (80, 84), (94, 98)]
            zero_indicators = [6, 20, 34, 48, 62, 76, 90]
            
            for (start, end), indicator_pos in zip(zero_positions, zero_indicators):
                if start < len(u[m]) and end <= len(u[m]):
                    if u[m][start:end] == ' 0.0':
                        u_list[indicator_pos] = ' '
            
            # Para las últimas filas (k > 44), dejar vacías las últimas columnas
            if m > 44 and len(u_list) >= 98:
                u_list[84:98] = list('              ')  # 14 espacios
            
            u[m] = ''.join(u_list)

    # Escribir archivo AN{ano}383.DAT en la carpeta resultados - CON EL MISMO FORMATO
    # Escribir archivo AN{ano}383.DAT en la carpeta resultados - CORREGIDO
    try:
        archivo_383 = os.path.join(resultados_dir, f'AN{can}383.DAT')
        with open(archivo_383, 'w') as f22:
            for k in range(0, 53):  # 0 a 52
                if len(u[k]) < 98:
                    continue
                    
                # Parsear correctamente cada segmento de 14 caracteres
                segments = []
                for i in range(7):
                    start = i * 14
                    end = start + 14
                    segment = u[k][start:end]
                    segments.append(segment)
                
                # Calcular horas y minutos desde k
                hours = k // 2
                minutes = "00" if k % 2 == 0 else "30"
                
                # Construir la línea correctamente
                line_parts = []
                for i, segment in enumerate(segments):
                    if len(segment) == 14:
                        # Extraer componentes
                        angle_part = segment[0:6].strip()
                        sign_part = segment[6:9].strip()
                        value_part = segment[9:14]  # SIN .strip()
                        
                        # Verificar si este segmento tiene datos válidos (no solo espacios)
                        has_data = value_part.strip() != ""
                        
                        # Solo dejar vacío si realmente no hay datos
                        if not has_data:
                            col = "   &  &   &     "
                        else:
                            # Columnas normales
                            angle_formatted = f"{int(angle_part):3d}"
                            value_formatted = value_part
                            col = f"{angle_formatted}&{minutes}&${sign_part}$&{value_formatted}"
                        
                        line_parts.append(col)
                
                if len(line_parts) == 7:
                    line = "&".join(line_parts) + "\\\\"
                    
                    # Añadir [1ex] cada 6 líneas para líneas impares (k impar)
                    if k % 2 == 1 and (k // 2) % 3 == 2:  # k=5,11,17,23,etc.
                        line = line.replace("\\\\", "\\\\[1ex]")
                    
                    f22.write(line + '\n')
        
        print(f"Archivo creado: {archivo_383}")
    except Exception as e:
        print(f"Error escribiendo archivo AN{can}383.DAT: {e}")
    
    # Cálculo de la TABLA II
    print(' TABLA II')
    
    try:
        archivo_384A = os.path.join(resultados_dir, f'AN{can}384A.DAT')
        with open(archivo_384A, 'w') as f22:
            for j in range(0, 19):  # 0 a 18
                ha = 20.0 * j * deg
                t_val = ha - a0
                for k in range(2, 15):  # 2 a 14
                    alt = 5.0 * k * deg
                    co[k] = 0.5 * cd**2 * math.sin(t_val)**2 * math.tan(alt) * 60.0 / deg
                
                # Formatear según el ejemplo proporcionado
                if (j != 0) and ((j % 5 == 0) or (j == 18)):
                    line = (f"{int(ha/deg+0.5):3d}&{co[2]:3.1f}&{co[3]:3.1f}&{co[4]:3.1f}&{co[5]:3.1f}&"
                           f"{co[6]:3.1f}&{co[7]:3.1f}&{co[8]:3.1f}&{co[9]:3.1f}&{co[10]:3.1f}&"
                           f"{co[11]:3.1f}&{co[12]:3.1f}&{co[13]:3.1f}\\\\[2ex]")
                else:
                    line = (f"{int(ha/deg+0.5):3d}&{co[2]:3.1f}&{co[3]:3.1f}&{co[4]:3.1f}&{co[5]:3.1f}&"
                           f"{co[6]:3.1f}&{co[7]:3.1f}&{co[8]:3.1f}&{co[9]:3.1f}&{co[10]:3.1f}&"
                           f"{co[11]:3.1f}&{co[12]:3.1f}&{co[13]:3.1f}\\\\")
                f22.write(line + '\n')
        print(f"Archivo creado: {archivo_384A}")
    except Exception as e:
        print(f"Error escribiendo archivo AN{can}384A.DAT: {e}")
    
    
    # Cálculo de la TABLA III
    # --- Cálculo de la TABLA III - CON ORDEN DE TÉRMINOS INVERTIDO
    print(' TABLA III')

    try:
        archivo_384B = os.path.join(resultados_dir, f'AN{can}384B.DAT')
        with open(archivo_384B, 'w') as f22:
            for j in range(0, 19):  # 0 a 18
                ha = 20.0 * j * deg
                t_val = ha - a0
                co = [0.0] * 15
                
                for k in range(1, 14):  # 1 a 13
                    
                    term1 = (de[k-1] - d0) * math.cos(t_val)
                    term2 = cd * math.sin(al[k-1] - a0) * math.sin(t_val)
                    
                    co[k+1] = (term1 - term2) * 60.0 / deg
                    
                # Formatear 
                angle_part = f"{int(ha/deg+0.5):3d}"
                
                parts = []
                for k in range(2, 15):  # 2 a 14 (13 valores)
                    valor = co[k]
                    
                    if abs(valor) < 0.05:
                        sign_char = ' '
                        valor_str = "0.0"
                    else:
                        sign_char = '+' if valor >= 0 else '-'
                        valor_str = f"{abs(valor):.1f}"
                    
                    parts.append(f"${sign_char}$&{valor_str}")
                
                line = f"{angle_part}&" + "&".join(parts) + "\\\\"
                
                if (j % 5 == 0) and (j != 0):
                    line = line.replace("\\\\", "\\\\[2ex]")
                
                f22.write(line + '\n')
        
        print(f"Archivo creado: {archivo_384B}")
        
    except Exception as e:
        print(f"Error al escribir archivo 384B: {e}")
        
    
    # Cálculo de AZIMUTES - CORREGIDO
    print(' AZIMUTES')

    for j in range(0, 37):  # 0 a 36
        ha = 10.0 * j * deg
        t_val = ha - a0
        
        # CÁLCULO CORREGIDO - igual que en Fortran
        co_az = [0.0] * 15
        for k in range(2, 14):  # 2 a 13 (12 columnas)
            alt = 5.0 * k * deg  # altura en radianes
            # Fórmula original del Fortran: ATAN(-SIN(t)/TAN(d0)/COS(alt))/deg
            co_az[k] = math.atan(-math.sin(t_val) / math.tan(d0) / math.cos(alt)) / deg
        
        # Formatear línea
        ha_val = 10 * j
        line_parts = [f"\\bf {ha_val:3d}"]
        
        for k in range(2, 14):  # 2 a 13 (12 columnas)
            valor = co_az[k]
            
            # Determinar signo y formato
            if abs(valor) < 0.05:  # Cercano a cero
                signo = ' '
                num_str = f"0.0"
            else:
                signo = '+' if valor >= 0 else '-'
                num_str = f"{abs(valor):3.1f}"
            
            line_parts.append(f"&${signo}${num_str}")
        
        line_parts.append(f"&\\bf {ha_val:3d}")
        
        if j % 4 == 3:
            u[j] = ''.join(line_parts) + '\\\\[2ex]'
        else:
            u[j] = ''.join(line_parts) + '\\\\'

    try:
        archivo_385 = os.path.join(resultados_dir, f'AN{can}385.DAT')
        with open(archivo_385, 'w') as f22:
            for j in range(0, 37):  # 0 a 36
                f22.write(u[j] + '\n')
        print(f"Archivo creado: {archivo_385}")
    except Exception as e:
        print(f"Error escribiendo archivo AN{can}385.DAT: {e}")

if __name__ == "__main__":
    prueba()