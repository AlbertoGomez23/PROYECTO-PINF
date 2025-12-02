from jplephem.spk import SPK

try:
    # 1️⃣ Cargar el archivo
    jpl = SPK.open('de440.bsp')
    print("✅ de440.bsp cargado correctamente.")
except Exception as e:
    print("❌ Error al abrir de440.bsp:", e)
    raise SystemExit

# 2️⃣ Listar los pares de cuerpos disponibles
print("\n📘 Pares disponibles en el archivo:")
for key in jpl.pairs.keys():
    print(" ", key)

# 3️⃣ Probar un cálculo sencillo: posición Tierra-Sol
try:
    jd = 2451545.0  # J2000
    r = jpl[0, 10].compute(jd)  # posición del Sol respecto al baricentro
    print("\n🌞 Ejemplo: posición del Sol (barycenter → 10) en J2000:")
    print("x,y,z =", r)
except KeyError as e:
    print("\n⚠️ El par (0,10) no existe en este BSP:", e)
except Exception as e:
    print("\n❌ Error al calcular:", e)
