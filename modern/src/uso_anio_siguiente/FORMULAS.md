# Fórmulas Astronómicas: Uso del Almanaque el Año Siguiente

Este documento describe las fórmulas matemáticas y astronómicas utilizadas en el módulo `uso_anio_siguiente` para calcular las correcciones necesarias al utilizar el Almanaque Náutico de un año $N$ en el año $N+1$.

## 1. Fundamento Teórico

La corrección se basa en la diferencia del Ángulo Horario en Greenwich (GHA) del Sol (o Aries, conceptualmente similar para efectos de rotación) entre el mismo día y hora de dos años consecutivos. Esta diferencia surge principalmente debido a:

1. **Precesión de los Equinoccios**: El movimiento secular del eje de la Tierra.
2. **Nutación**: Las oscilaciones periódicas del eje de la Tierra.
3. **Diferencia en Tiempo Sideral**: La relación entre el tiempo solar medio y el tiempo sideral cambia ligeramente de año en año.

## 2. Algoritmo de Cálculo

El cálculo se realiza para cada día del año, comparando el instante $t_1$ (año $N$) con el instante $t_2$ (año $N+1$).

### 2.1. Definición de Tiempo

Para un día $D$ y mes $M$, definimos los instantes en Tiempo Terrestre (TT):

$$
t_1 = \text{Fecha}(N, M, D, 0h) + \Delta T
$$
$$
t_2 = \text{Fecha}(N+1, M, D, 0h) + \Delta T
$$

Donde $\Delta T$ es la diferencia entre Tiempo Terrestre y Tiempo Universal (TT - UT).

### 2.2. Tiempo Sideral Aparente en Greenwich (GAST)

Para cada instante, se calcula el GAST ($\theta$), que representa el ángulo horario del Punto Aries verdadero:

$$
\theta_1 = \text{GAST}(t_1)
$$
$$
\theta_2 = \text{GAST}(t_2)
$$

En el código, esto se obtiene mediante `skyfield` y se convierte a radianes.

### 2.3. Ascensión Recta Aparente del Sol ($\alpha$)

Se calcula la posición aparente del Sol (cuerpo 11 en JPL DE440) para obtener su Ascensión Recta ($\alpha$), corregida por nutación y aberración anual:

$$
\alpha_1 = \text{AR}_{\odot}(t_1)
$$
$$
\alpha_2 = \text{AR}_{\odot}(t_2)
$$

### 2.4. Ángulo Horario en Greenwich (GHA)

El Ángulo Horario en Greenwich del Sol se define como la diferencia entre el GAST y la Ascensión Recta:

$$
\text{GHA}_1 = (\theta_1 - \alpha_1) \pmod{2\pi}
$$
$$
\text{GHA}_2 = (\theta_2 - \alpha_2) \pmod{2\pi}
$$

### 2.5. Cálculo de la Corrección ($\Delta$)

La corrección es la diferencia entre los ángulos horarios del año siguiente y el año actual:

$$
\Delta = \text{GHA}_2 - \text{GHA}_1
$$

Esta diferencia se normaliza al rango $[-\pi, \pi]$ para obtener el camino más corto:

$$
\text{Si } \Delta > \pi \implies \Delta = \Delta - 2\pi
$$
$$
\text{Si } \Delta < -\pi \implies \Delta = \Delta + 2\pi
$$

Finalmente, el resultado se convierte de radianes a minutos de arco para su publicación en el Almanaque:

$$
\text{Corrección (min)} = \Delta_{\text{rad}} \times \frac{180}{\pi} \times 60
$$

## 3. Implementación en Código

La implementación se encuentra en `modern/src/uso_anio_siguiente/core.py`. Utiliza la librería `skyfield` para obtener valores de alta precisión (IAU 2000A/2006) en lugar de las aproximaciones de series de Fourier utilizadas en la versión Legacy (Fortran).
