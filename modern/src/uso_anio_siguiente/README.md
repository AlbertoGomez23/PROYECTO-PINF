# Migración del Módulo: Uso del Almanaque el Año Siguiente

Este documento detalla el proceso de migración, arquitectura y validación del módulo encargado de calcular la tabla de correcciones para el uso del Almanaque Náutico en el año siguiente.

## 1. Descripción General

El objetivo de este módulo es calcular la corrección en minutos de arco que debe aplicarse a las coordenadas de los astros cuando se utiliza el Almanaque Náutico de un año determinado para observaciones realizadas en el año siguiente. Esta corrección se deriva principalmente de la precesión y nutación de la Tierra.

### Comparativa: Legacy vs. Modern

| Característica | Legacy (Fortran 77) | Modern (Python 3.12+) |
| :--- | :--- | :--- |
| **Archivo Principal** | `UsoAnoSig.f` | `uso_anio_siguiente.py` (CLI) |
| **Lógica Astronómica** | Implementación manual (`PLEPH`, `TSMUT`, `EQATORIA`) | Librería `skyfield` (NASA JPL DE440) |
| **Entrada de Datos** | Interactiva (`READ(*,*)`) | CLI (`click`) y Variables de Entorno |
| **Salida** | Fichero LaTeX (`.DAT`) con ruta hardcodeada | Fichero LaTeX (`.DAT`) en ruta configurable |
| **Arquitectura** | Monolítica (Cálculo + Formateo mezclados) | Modular (`core.py`, `formatter.py`, `cli`) |
| **Precisión** | Limitada por algoritmos simplificados (VSOP87/IAU1980) | Alta precisión (IAU 2000A/2006 via Skyfield) |

## 2. Arquitectura de la Solución Moderna

La solución se ha diseñado siguiendo el principio de separación de responsabilidades:

* **`core.py`**: Contiene la lógica astronómica pura.
  * Utiliza `skyfield` para obtener tiempos siderales (GAST) y posiciones aparentes.
  * Calcula el Ángulo Horario en Greenwich (GHA) para el mismo día en el año $N$ y $N+1$.
  * Determina la diferencia $\Delta GHA$ y la convierte a minutos de arco.
* **`formatter.py`**: Se encarga exclusivamente de la generación del fichero de salida.
  * Replica el formato exacto de la tabla LaTeX del sistema legacy.
  * Maneja la lógica visual para años bisiestos (marca `*` en febrero).
* **`uso_anio_siguiente.py`**: Punto de entrada CLI.
  * Orquesta la llamada a `core` y `formatter`.
  * Gestiona argumentos de línea de comandos (`--year`, `--dt`).
  * Maneja la inyección de dependencias (rutas de salida).

## 3. Estrategia de Pruebas y Resultados

Se ha implementado una suite de pruebas exhaustiva para garantizar la fiabilidad de los cálculos sin depender de una comparación bit a bit con el legacy (dado que los algoritmos de fondo han cambiado a versiones más precisas).

### 3.1 Tests Unitarios (`test_uso_anio_siguiente_unit.py`)

Verifican la corrección estructural y lógica del código:

* **Estructura de Datos**: Asegura que `core.calculate_corrections_data` devuelve un diccionario válido.
* **Años Bisiestos**: Valida que el 29 de febrero se maneja correctamente (se ignora si el año siguiente no es bisiesto).
* **Formato LaTeX**: Comprueba que el generador produce líneas con la alineación y sintaxis LaTeX esperada.
* **Integración**: Prueba el flujo completo de creación de archivos y directorios.

### 3.2 Tests de Lote / Sanity Checks (`test_uso_anio_siguiente_batch.py`)

Dado que la "verdad absoluta" astronómica es compleja, se utilizan "Sanity Checks" sobre un rango amplio de años (2010-2035):

* **Rango de Valores**: Se verifica que ninguna corrección exceda $\pm 6.5$ minutos de arco. (Valores típicos oscilan entre $\pm 6.0'$).
* **Consistencia Temporal**: Se verifica que la media de las correcciones no varíe bruscamente de un año a otro (umbral $< 0.9'$). Esto valida que la precesión se está aplicando de forma continua.

### 3.3 Resultados de la Validación

La ejecución de los tests de lote para el periodo 2010-2035 ha sido **EXITOSA**.

```text
--- Ejecutando Test de Lote 2010-2035 con Delta T dinámico ---
Calculando correcciones para 2010-2011... OK
...
Calculando correcciones para 2034-2035... OK
Verificados 26 años correctamente. Todo parece nominal.
```

## 4. Instrucciones de Uso

### Ejecución Manual

Para generar la tabla para un año específico (ej. 2025):

```bash
python -m modern.src.uso_anio_siguiente.uso_anio_siguiente --year 2025
```

El sistema estimará automáticamente el valor de $\Delta T$ (Delta T) usando `skyfield`, o se puede proporcionar manualmente:

```bash
python -m modern.src.uso_anio_siguiente.uso_anio_siguiente --year 2025 --dt 69.2
```

### Ejecución de Tests

Para validar la instalación y la lógica:

```bash
# Tests unitarios
python -m modern.src.tests.test_uso_anio_siguiente_unit

# Tests de lote (Sanity Check)
export PYTHONPATH=$PYTHONPATH:.
python -m modern.src.tests.test_uso_anio_siguiente_batch
```
