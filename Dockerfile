FROM python:3.12.12-slim-bookworm

# Metadata
LABEL maintainer="https://github.com/AlbertoGomez23/PROYECTO-PINF"

WORKDIR /app

# Variables de entorno optimizadas
ENV PYTHONPATH=/app/modern/src \
    PATH="${PATH}:/home/appuser/.local/bin" \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

# Crear usuario no privilegiado con grupo
RUN groupadd -r appuser && useradd -r -g appuser -u 1000 appuser

# Copiar solo requirements primero (mejor aprovechamiento de caché)
COPY requirements.txt .

# Instalar dependencias con limpieza de caché
RUN pip install --no-cache-dir -r requirements.txt && \
    find /usr/local -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true && \
    find /usr/local -name "*.pyc" -delete && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Copiar código fuente
COPY --chown=appuser:appuser . .

# Cambiar al usuario no privilegiado
USER appuser

# Exponer puerto Streamlit
EXPOSE 8501

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD python -c "import requests; requests.get('http://localhost:8501/_stcore/health')" || exit 1

# Comando de arranque
CMD ["streamlit", "run", "modern/web_app.py", "--server.port=8501", "--server.address=0.0.0.0"]