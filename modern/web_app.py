from datetime import datetime

import streamlit as st

st.set_page_config(
    page_title="Almanaque Náutico ROA",
    page_icon="⚓",
    layout="wide"
)

st.title("⚓ Almanaque Náutico - Real Instituto y Observatorio de la Armada")
st.markdown("---")

# Sidebar for configuration
with st.sidebar:
    st.header("Configuración de Generación")

    year = st.number_input("Año del Almanaque", min_value=1900,
                           max_value=2100, value=datetime.now().year + 1)

    st.subheader("Opciones de Cálculo")
    calc_stars = st.checkbox("Calcular Posiciones de Estrellas", value=True)
    calc_moon = st.checkbox("Calcular Fases de la Luna", value=True)
    calc_sun = st.checkbox("Calcular Semidiámetro del Sol", value=True)

    st.markdown("---")
    st.caption("Versión del Sistema: Modern (Python 3.12)")

# Main content area
col1, col2 = st.columns([2, 1])

with col1:
    st.subheader("Estado del Sistema")
    st.info("Sistema listo para generar efemérides.")

    # Mockup of a log or output area
    st.text_area(
        "Log de Salida (Simulado)",
        value="Esperando inicio del proceso...\nCargando kernels SPICE (de440.bsp)...\nListo.",
        height=300,
        disabled=True
    )

with col2:
    st.subheader("Acciones")

    if st.button("Generar Almanaque Completo", type="primary", use_container_width=True):
        st.toast(f"Iniciando generación para el año {year}...", icon="🚀")
        with st.spinner('Calculando efemérides...'):
            # This is non-functional, so we just simulate a delay or action
            import time
            time.sleep(2)
        st.success("¡Generación completada! (Simulación)")
        st.balloons()

    st.markdown("### Descargas")
    st.button("Descargar PDF (Mock)", disabled=True, use_container_width=True)
    st.button("Descargar LaTeX (Mock)",
              disabled=True, use_container_width=True)

st.markdown("---")
st.markdown(
    """
    <div style='text-align: center; color: grey;'>
        <small>Proyecto de Modernización del Almanaque Náutico | ROA</small>
    </div>
    """,
    unsafe_allow_html=True
)
