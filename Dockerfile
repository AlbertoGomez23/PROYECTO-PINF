FROM python:3.12.12-slim-bookworm

# Crear un usuario no privilegiado (coincidiendo con devcontainer)
RUN useradd -m -u 1000 vscode

WORKDIR /app
# Establecer PYTHONPATH sin referencia circular vacía
ENV PYTHONPATH=/app/modern/src
# Seguridad Defensiva: Añadir binarios de usuario al FINAL del PATH.
# Esto evita el "PATH Hijacking": los comandos del sistema siempre tienen prioridad.
ENV PATH="${PATH}:/home/vscode/.local/bin"

COPY requirements.txt .
# Usar caché de pip para acelerar reconstrucciones
RUN --mount=type=cache,target=/root/.cache/pip \
    pip install -r requirements.txt
COPY --chown=vscode:vscode . .

# Cambiar al usuario no privilegiado
USER vscode

CMD ["python", "modern/app.py"]