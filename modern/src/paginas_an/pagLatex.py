import os

class PagTexProcessor:
    def __init__(self):
        # Variables para manejar la ventana deslizante de 3 líneas (anterior, actual, siguiente)
        self.fil1 = ""
        self.fil2 = ""
        self.fil3 = ""
        # Buffer que almacena las 24 horas del día más el encabezado
        self.v = [""] * 25  
        
        # Objetos de archivo para lectura y escritura
        self.f_in = None
        self.f_out = None

    # --- Utilitarios ---
    def read_line(self, length=110):
        """Lee una línea del archivo de entrada y asegura una longitud fija de 110 caracteres."""
        line = self.f_in.readline()
        if not line:
            return " " * length
        return line.replace('\n', '').ljust(length)

    def write_line(self, text):
        """Escribe una cadena seguida de un salto de línea en el archivo de salida."""
        self.f_out.write(text + '\n')

    def modify_str(self, s, start_1_based, end_1_based, value):
        """
        Emula la función de subcadenas de Fortran (s(inicio:fin) = valor).
        Permite modificar partes de una cadena manteniendo su longitud fija.
        """
        start = start_1_based - 1
        end = end_1_based
        s_list = list(s)
        if len(s_list) < 110:
             s_list.extend([' '] * (110 - len(s_list)))
        val_list = list(value)
        
        # Ajusta el valor al tamaño del segmento especificado
        slice_len = end - start
        if len(val_list) != slice_len:
            val_list = val_list[:slice_len] + [' '] * (slice_len - len(val_list))
            
        s_list[start:end] = val_list
        return "".join(s_list)

    # --- Lógica PAGTEXBIS ---
    def pagtex_bis(self, da, ano, input_path=None, output_path=None):
        """Función principal que procesa los datos y genera el formato LaTeX."""
        self.v = [" " * 110] * 25 # Inicializa el buffer con espacios
        ida = da + 9
        
        # Generación de la ruta del archivo de salida según el año y el día ajustado
        if ida < 100:
            path = f"./Almanaque Nautico/DATOS/{ano}/AN{ano}{ida:02d}.dat"
        else:
            path = f"./Almanaque Nautico/DATOS/{ano}/AN{ano}{ida:03d}.dat"

        # Asegura que la estructura de carpetas de destino exista
        os.makedirs(os.path.dirname(path), exist_ok=True)

        try:
            # Apertura de archivos con codificación latin-1 para compatibilidad con datos heredados
            self.f_in = open(input_path or './Datos/pag.dat', 'r', encoding='latin-1')
            self.f_out = open(output_path or path, 'w', encoding='latin-1')
        except FileNotFoundError as e:
            print(f"Error: {e}")
            return

        # 1. Definiciones iniciales de macros LaTeX (\def) extraídas de las primeras líneas
        
        # Extrae la fecha
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\fecha{{{self.fil1[:58]}}}")

        # Extrae Semidiámetro del Sol
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\sdsol{{{self.fil1[6:8]}.{self.fil1[9]}\\Min}}")

        # Extrae Paso por el Meridiano de Greenwich del Sol
        self.fil1 = self.read_line()
        if self.fil1[9] == ' ': self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
        self.write_line(f"\\def\\pmgsol{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}.{self.fil1[12]}\\Mint}}")

        # Extrae Semidiámetro de la Luna
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\sdluna{{{self.fil1[6:8]}.{self.fil1[9]}\\Min}}")

        # Extrae Edad de la Luna
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\edad{{{self.fil1[7:9]}.{self.fil1[10]}\\Diap}}")

        # Extrae Paso por el Meridiano de Greenwich de la Luna
        self.fil1 = self.read_line()
        if self.fil1[9] == ' ': self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
        self.write_line(f"\\def\\pmgluna{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}\\Mint}}")

        # Extrae Fenómenos (Salida, Puesta, Crepúsculo)
        for name in ["pheu", "phed", "phet"]:
            self.fil1 = self.read_line()
            self.write_line(f"\\def\\{name}{{{self.fil1[9:11]}.{self.fil1[12]}\\Min}}")

        # Extrae datos de rotación/paso
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\ropmg{{{self.fil1[7:10]}\\Mint}}")

        self.write_line(r"\def\arriba{")

        # 2. Carga del Buffer V (Parte Superior - Lógica ARREGU)
        # Se leen las 3 primeras líneas para iniciar la comparación
        self.fil1 = self.read_line()
        self.fil2 = self.read_line()
        self.fil3 = self.read_line()
        
        self.v[0] = self.fil1
        
        # Itera sobre las 24 horas del día
        for i in range(1, 24):
            fil4 = self.fil2
            # Procesa la línea actual comparándola con la anterior y siguiente para evitar redundancia
            self.fil2 = self.arregu_bis(i, self.fil1, self.fil2, self.fil3)
            self.v[i] = self.fil2
            
            # Desplaza las líneas para la siguiente iteración
            self.fil1 = fil4
            self.fil2 = self.fil3
            
            if i < 23:
                self.fil3 = self.read_line()

        self.v[24] = self.fil2

        # Limpieza: Asegura que haya ceros en posiciones clave si están vacías (formato fijo)
        for i in range(25):
            line = self.v[i]
            indices = [9, 19, 29, 43, 62, 69, 76, 83, 95]
            for idx in indices:
                if line[idx] == ' ':
                    line = self.modify_str(line, idx+1, idx+1, '0')
            self.v[i] = line

        # 3. Escritura del Buffer V procesado al archivo de salida con formatos LaTeX
        self.write_line(self.format_211(self.v[0])) # Primera fila

        for i in range(1, 25):
            # Asigna formatos específicos de LaTeX según la fila para controlar el espaciado vertical
            if i in [5, 17]:
                self.write_line(self.format_213(self.v[i]))
            elif i == 11:
                self.write_line(self.format_214(self.v[i]))
            elif i in [13, 24]:
                self.write_line(self.format_215(self.v[i]))
            else:
                self.write_line(self.format_212(self.v[i]))
        
        self.write_line("}") # Cierra macro \arriba

        # 4. Procesamiento de Planetas y Punto Aries
        self.fil1 = self.read_line()
        if self.fil1[15] == ' ': self.fil1 = self.modify_str(self.fil1, 16, 16, '0')
        self.write_line(f"\\def\\pmgaries{{{self.fil1[12:14]}\\Hora\\ {self.fil1[15:17]}.{self.fil1[18]}\\Mint}}")

        planetas = ["venus", "marte", "jupiter", "saturno"]
        for p in planetas:
            # Magnitud
            self.fil1 = self.read_line()
            self.write_line(f"\\def\\mag{p}{{{self.fil1[6]}{self.fil1[8:11]}}}")
            # Paso por el Meridiano
            self.fil1 = self.read_line()
            if self.fil1[9] == ' ': self.fil1 = self.modify_str(self.fil1, 10, 10, '0')
            self.write_line(f"\\def\\pmg{p}{{{self.fil1[6:8]}\\Hora\\ {self.fil1[9:11]}\\Mint}}")

        self.write_line(r"\def\abajo{")

        # 5. Carga del Buffer V (Parte Inferior - Lógica ARREGD)
        self.fil1 = self.read_line()
        self.fil2 = self.read_line()
        self.fil3 = self.read_line()
        
        self.v[0] = self.fil1[:97].ljust(110)
        
        for i in range(1, 24):
            fil4 = self.fil2
            self.fil2 = self.arregd_bis(i, self.fil1, self.fil2, self.fil3)
            self.v[i] = self.fil2[:97].ljust(110)
            
            self.fil1 = fil4
            self.fil2 = self.fil3
            
            if i < 23:
                self.fil3 = self.read_line()
        
        self.v[24] = self.fil2[:97].ljust(110)

        # Limpieza de ceros en el buffer inferior
        for i in range(25):
            line = self.v[i]
            indices = [9, 19, 29, 39, 49, 59, 69, 79, 89]
            for idx in indices:
                if line[idx] == ' ': line = self.modify_str(line, idx+1, idx+1, '0')
            self.v[i] = line

        # 6. Escritura del Buffer Inferior
        for i in range(25):
            if i in [5, 11, 17]:
                self.write_line(self.format_232(self.v[i]))
            else:
                self.write_line(self.format_231(self.v[i]))
                
        self.write_line("}") # Cierra macro \abajo

        # 7. Diferencias y Pie de página
        self.fil1 = self.read_line()
        self.fil1 = self.arredif(self.fil1)
        
        def get_diff(start):
            idx = start - 1
            return f"{self.fil1[idx]}{self.fil1[idx+1:idx+3]}"
            
        vals = [get_diff(x) for x in [20, 30, 40, 50, 60, 70, 80, 90]]
        diff_str = f"\\def\\dif{{${vals[0]}$ &&& ${vals[1]}$&& ${vals[2]}$ &&& ${vals[3]}$&& ${vals[4]}$ &&& ${vals[5]}$&& ${vals[6]}$ &&& ${vals[7]}$}}"
        self.write_line(diff_str)

        # Ruta a la figura de la fase lunar
        self.fil1 = self.read_line()
        self.write_line(f"\\def\\figlun{{{self.fil1[12:21]}.pdf}}")

        self.f_in.close()
        self.f_out.close()

    # --- Lógica de Negocio Específica ---

    def arregu_bis(self, i, f1, f2, f3):
        """Compara columnas de la parte superior para omitir valores repetidos entre filas."""
        if i not in [5, 6, 11, 12, 17, 18]:
            f2 = self.compa4(15, 18, f1, f2, f3)
            f2 = self.compa4(39, 42, f1, f2, f3)
            f2 = self.compa2(60, 61, f1, f2, f3)
            f2 = self.compa2(67, 68, f1, f2, f3)
            f2 = self.compa2(74, 75, f1, f2, f3)
            f2 = self.compa2(81, 82, f1, f2, f3)
            f2 = self.compa2(93, 94, f1, f2, f3)
        return f2

    def arregd_bis(self, i, f1, f2, f3):
        """Compara columnas de la parte inferior para omitir valores repetidos."""
        if i in [6, 12, 18]:
            f2 = self.modify_str(f2, 97, 97, 'c')
        elif i not in [5, 11, 17]:
            f2 = self.compa4(25, 28, f1, f2, f3)
            f2 = self.compa4(45, 48, f1, f2, f3)
            f2 = self.compa4(65, 68, f1, f2, f3)
            f2 = self.compa4(85, 88, f1, f2, f3)
        return f2
    
    def arredif(self, f1):
        """Ajusta el formato de las diferencias para la visualización final."""
        indices = [22, 32, 42, 52, 62, 72, 82, 92]
        for idx in indices:
            if f1[idx-1:idx+1] == ' 0': 
                 f1 = self.modify_str(f1, idx-2, idx-2, ' ')
        return f1

    def compa4(self, i1, i2, f1, f2, f3):
        """Si un bloque de 4 caracteres es idéntico al anterior y al posterior, lo oculta."""
        start = i1 - 1
        end = i2
        if (f1[start:end] == f2[start:end]) and (f2[start:end] == f3[start:end]):
            f2 = self.modify_str(f2, i1, i2, ' &  ')
        return f2

    def compa2(self, i1, i2, f1, f2, f3):
        """Si un bloque de 2 caracteres es idéntico al anterior y al posterior, lo limpia."""
        start = i1 - 1
        end = i2
        if (f1[start:end] == f2[start:end]) and (f2[start:end] == f3[start:end]) and (f2[start:end] != '**'):
            f2 = self.modify_str(f2, i1, i2, '  ')
        return f2

    # --- Formateadores de filas para LaTeX ---

    def get_parts_upper(self, line):
        """Fracciona la línea superior en campos individuales."""
        return [
            line[1:3], line[5:8], line[9:13], line[14], line[16:18], # 0-4
            line[19:23], line[25:28], line[29:33], line[34:37], line[38], # 5-9
            line[40:42], line[43:47], line[48:51], line[53:55], line[56], # 10-14
            line[59:61], line[62:64], line[66:68], line[69:71], line[73:75], # 15-19
            line[76:78], line[80:82], line[83:85], line[87:90], line[92:94], # 20-24
            line[95:97], line[99:102] # 25-26
        ]

    def format_211(self, line):
        """Formato de encabezado con negrita."""
        p = self.get_parts_upper(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&${p[3]}$&{p[4]}&{p[5]}&{p[6]}&{p[7]}&"
                f" &                              {p[8]} &${p[9]}$&{p[10]}&"
                f"{p[11]}&                              {p[12]} & \\bf {p[13]} &\\bf "
                f"{p[14]}&{p[15]}&{p[16]}&{p[17]}&{p[18]}&{p[19]}&{p[20]}&{p[21]}&{p[22]}& "
                f"{p[23]}&{p[24]}&{p[25]}& {p[26]} \\\\")

    def format_212(self, line):
        """Formato estándar para las filas de datos."""
        p = self.get_parts_upper(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&${p[3]}$&{p[4]}&{p[5]}&{p[6]}&{p[7]}&"
                f"\\raisebox{{1ex}}[0pt]{{\\scriptsize {p[8]}}}&${p[9]}$&{p[10]}&"
                f"{p[11]}&\\raisebox{{1ex}}[0pt]{{\\scriptsize {p[12]}}}& \\bf {p[13]} &    "
                f"{p[14]}&{p[15]}&{p[16]}&{p[17]}&{p[18]}&{p[19]}&{p[20]}&{p[21]}&{p[22]}& "
                f"{p[23]}&{p[24]}&{p[25]}& {p[26]} \\\\")

    def format_213(self, line):
        """Formato con espacio extra al final."""
        return self.format_212(line).replace(r" \\", r" \\[1.0ex]")

    def format_214(self, line):
        """Formato con negrita y espacio extra."""
        base = self.format_212(line)
        base = base.replace(r"&    ", r"&\bf ") 
        base = base.replace(r" \\", r" \\[1.0ex]")
        return base

    def format_215(self, line):
        """Formato con negrita en campos específicos."""
        base = self.format_212(line)
        base = base.replace(r"&    ", r"&\bf ")
        return base

    def get_parts_lower(self, line):
        """Fracciona la línea inferior en campos individuales."""
        return [
            line[1:3], line[5:8], line[9:13], line[15:18], line[19:23], # 0-4
            line[24], line[26:28], line[29:33], line[35:38], line[39:43], # 5-9
            line[44], line[46:48], line[49:53], line[55:58], line[59:63], # 10-14
            line[64], line[66:68], line[69:73], line[75:78], line[79:83], # 15-19
            line[84], line[86:88], line[89:93] # 20-22
        ]

    def format_231(self, line):
        """Formato de tabla para la sección inferior (planetas)."""
        p = self.get_parts_lower(line)
        return (f"\\bf {p[0]}&{p[1]}&{p[2]}&{p[3]}&{p[4]}&${p[5]}$&{p[6]}&"
                f"{p[7]}&{p[8]}&{p[9]}&${p[10]}$&{p[11]}&{p[12]}&{p[13]}&{p[14]}&${p[15]}"
                f"$&{p[16]}&{p[17]}&{p[18]}&{p[19]}&${p[20]}$&{p[21]}&{p[22]}\\\\")

    def format_232(self, line):
        """Formato inferior con espacio extra."""
        return self.format_231(line).replace(r"\\", r"\\[1.0ex]")

# --- Ejecución ---
if __name__ == "__main__":
    proc = PagTexProcessor()
    print("Procesando PAGTEXBIS...")
    proc.pagtex_bis(1, 2024)
    print("Hecho.")