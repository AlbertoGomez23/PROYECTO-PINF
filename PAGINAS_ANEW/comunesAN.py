# Guardar como: comunesAN.py
#
# Migración literal de las funciones de fecha de comunesAN.f [cite: 787-801]
# (Código proporcionado por ti)

import math

def INTLO(a):
    """
    TRADUCCIÓN: FUNCTION INTLO(a) [cite: 802-805]
    Lógica de FORTRAN para truncar números, 
    manejando negativos correctamente.
    """
    i = int(a)
    ai = float(i)
    if ai > a:
        i = i - 1
    return i

def DiaJul(dia, mes, annio, hora):
    """
    TRADUCCIÓN: FUNCTION DIAJUL(giorno,mese,anno,ora) [cite: 787-791]
    Calcula el Día Juliano a partir de una fecha gregoriana.
    """
    if mes <= 2:
        iy = annio - 1
        im = mes + 12
    else:
        iy = annio
        im = mes
    
    ib = 0 # Valor por defecto
    
    if annio > 1582:
        ib = iy//400 - iy//100
    else:
        ib = -2
        if annio == 1582:
            if mes > 10:
                ib = iy//400 - iy//100
            elif mes == 10 and dia >= 15:
                ib = iy//400 - iy//100
    k1 = int(365.25 * iy)
    k2 = int(30.6001 * (im + 1))
    k3 = k1 + k2 + ib - 679004 + dia
    diaJul = 2400000.5 + k3 + hora/24
    
    return diaJul

def DJADia(dj) -> tuple:
    """
    TRADUCCIÓN: SUBROUTINE DJADIA(dj,dia,mes,anno,hora) [cite: 792-801]
    Usa la lógica de INTLO() del FORTRAN [cite: 802-805]
    """
    a = dj + 0.5
    ia = int(a)
    hora = (a - ia) * 24
    if ia < 2299161:
        ic = ia + 1524
    else:
        # Usamos math.floor() que es el equivalente a INTLO para esta lógica
        ib = math.floor((ia - 1867216.25)/36524.25)
        ic = ia + ib - math.floor(ib/4) + 1525
    
    id_ = math.floor((ic - 122.1)/365.25) # renombrado id a id_
    ie = math.floor(365.25 * id_)
    ef = math.floor((ic - ie)/30.6001)
    dia = ic - ie - math.floor(30.6001*ef)
    mes = ef - 1 - 12 * math.floor(ef/14)
    annio = id_ - 4715 - math.floor((7 + mes)/10)

    return dia, mes, annio, hora