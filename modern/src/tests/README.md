# Pruebas unitarias e integración para Almanaque Nautico ROA

Aquí irán los tests del proyecto: pruebas unitarias y de integración para validar funcionalidades, detectar regresiones y asegurar la calidad del código.

## Contenido

### Uso Año Siguiente

Pruebas relacionadas con el cálculo y generación de datos para el año siguiente.

- **`test_uso_anio_siguiente_unit.py`**:  
    Pruebas unitarias para validar la lógica interna del módulo.

- **`test_uso_anio_siguiente_batch.py`**:  
    Pruebas de lote (*sanity checks*) para verificar la estabilidad de los resultados en un rango amplio de años.

- **`test_audit_uso_anio_siguiente.py`**:  
    Auditoría de los ficheros de salida generados para asegurar que cumplen con el formato y contenido esperado.

> ℹ️ **Más información**: Consulta la documentación detallada en [`/modern/src/uso_anio_siguiente/README.md`](../uso_anio_siguiente/README.md).
