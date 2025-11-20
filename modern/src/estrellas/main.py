import os
import sys
from pathlib import Path

# Obtenemos la ruta absoluta de este fichero
ruta_main = Path(__file__).resolve()

# Obtenemos la ruta de estrellas 
ruta_estrellas = ruta_main.parent

# Obtenemos la ruta de src
ruta_scr = ruta_estrellas.parent

if str(ruta_scr) not in sys.path:
    sys.path.append(str(ruta_scr))

# Importar en el orden correcto para evitar dependencias circulares
from SubrEstr import *
from LeeDE440 import *
from ReduEstr import *
from PasoMeGr import *
from prepaes import *
from prueba import main as prueba_main

if __name__ == "__main__":

    # Ejecutar el programa principal
    prueba_main()