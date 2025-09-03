############################################################################
# TALLER PRÁCTICO SESIÓN 6: ESTADÍSTICA ECOLÓGICA EN R
# Asignatura: Bioestadística Aplicada a las Ciencias Ambientales y Pesqueras
# Instructor: Mg. Ing. Raul Lima Coasaca
############################################################################

#--------------------------------------------------------------------------#
# PASO 0: PREPARACIÓN DEL ENTORNO
#--------------------------------------------------------------------------#

# Si no tienes instalado el paquete 'vegan', ejecuta la siguiente línea una vez:
# install.packages("vegan")

# Cargamos la librería 'vegan', nuestra herramienta principal para este taller.
library(vegan)

#--------------------------------------------------------------------------#
# PASO 1: CARGA Y EXPLORACIÓN DE DATOS
#--------------------------------------------------------------------------#

# Para este ejercicio, usaremos el conjunto de datos 'BCI' que viene con vegan.
# Imaginemos que esta es una matriz de datos de IMARPE donde:
# - Las FILAS son diferentes estaciones de muestreo (ej. arrastres de fondo).
# - Las COLUMNAS son diferentes especies de invertebrados bentónicos.
# - Los VALORES son las abundancias (conteo de individuos) de cada especie.
data(BCI)

# Echemos un vistazo a nuestros datos
# Dimensión de la matriz (sitios x especies)
dim(BCI)

# Primeras 6 filas y primeras 6 columnas
head(BCI[, 1:6])

# Vamos a simular un archivo con variables ambientales para nuestras estaciones.
# Esto es muy común en los estudios de IMARPE.
set.seed(123) # Para que los resultados aleatorios sean reproducibles
bci_env <- data.frame(
  Profundidad_m = round(runif(nrow(BCI), min = 20, max = 200)),
  Salinidad_ups = round(runif(nrow(BCI), min = 33.5, max = 35.0), 1),
  Temperatura_C = round(runif(nrow(BCI), min = 15, max = 22), 1)
)

# Vemos nuestras variables ambientales
head(bci_env)


#--------------------------------------------------------------------------#
# PASO 2: CÁLCULO DE ÍNDICES DE DIVERSIDAD
#--------------------------------------------------------------------------#

# Objetivo: Cuantificar la diversidad en cada una de nuestras estaciones.

# 2.1 Riqueza de especies (S)
# Es simplemente el número de especies presentes en cada sitio.
riqueza <- specnumber(BCI)

# 2.2 Índice de Shannon (H')
# Considera la abundancia y el número de especies. Sensible a especies raras.
shannon <- diversity(BCI, index = "shannon")

# 2.3 Índice de Simpson (1-D)
# Mide la probabilidad de que dos individuos seleccionados al azar NO sean
# de la misma especie. Se enfoca en la dominancia.
simpson <- diversity(BCI, index = "simpson")

# 2.4 Unimos todo en un solo data frame para una fácil visualización
indices_diversidad <- data.frame(
  Riqueza = riqueza,
  Shannon = shannon,
  Simpson = simpson
)

# Mostramos los resultados para las primeras estaciones
head(indices_diversidad)


#--------------------------------------------------------------------------#
# PASO 3: CURVAS DE RAREFACCIÓN
#--------------------------------------------------------------------------#

# Objetivo: Comparar la riqueza de especies entre sitios, estandarizando por
# el esfuerzo de muestreo (número de individuos).

# Calculamos el número mínimo de individuos en una estación.
# Este será nuestro punto de comparación estandarizado.
min_individuos <- min(rowSums(BCI))
cat("El tamaño de muestra mínimo para la estandarización es:", min_individuos, "individuos.\n")

# Generamos la curva de rarefacción
# La función 'rarecurve' grafica la riqueza esperada para cada tamaño de muestra.
rarecurve(BCI, 
          step = 20,                  # Calcula un punto cada 20 individuos
          sample = min_individuos,    # Estandariza la curva hasta el mínimo común
          xlab = "Nº de Individuos (Esfuerzo de Muestreo)", 
          ylab = "Nº de Especies (Riqueza Esperada)",
          main = "Curva de Rarefacción para Estaciones de Muestreo",
          col = "darkcyan",           # Color de las curvas
          label = FALSE)              # Ocultamos las etiquetas para mayor claridad

# La curva que llega más alto a un mismo nivel de esfuerzo (eje X)
# corresponde a la comunidad con mayor riqueza esperada.


#--------------------------------------------------------------------------#
# PASO 4: RELACIONANDO LA DIVERSIDAD CON VARIABLES AMBIENTALES
#--------------------------------------------------------------------------#

# Objetivo: Explorar si existe una relación entre la diversidad y el ambiente.
# Esto es una introducción a los análisis que veremos en la próxima sesión.

# Unimos nuestros índices de diversidad con las variables ambientales
datos_completos <- cbind(indices_diversidad, bci_env)
head(datos_completos)

# Pregunta: ¿La diversidad de Shannon se relaciona con la profundidad?
# Usaremos un modelo lineal simple (visto en sesiones anteriores) para explorarlo.
modelo_shannon_prof <- lm(Shannon ~ Profundidad_m, data = datos_completos)

# Vemos el resumen del modelo
summary(modelo_shannon_prof)
# El p-value (Pr(>|t|)) nos indica si la relación es estadísticamente significativa.

# Visualicemos esta relación
plot(datos_completos$Profundidad_m, datos_completos$Shannon,
     xlab = "Profundidad (m)",
     ylab = "Índice de Diversidad de Shannon (H')",
     main = "Relación entre Diversidad y Profundidad",
     pch = 16, col = "steelblue")
abline(modelo_shannon_prof, col = "red", lwd = 2) # Agregamos la línea de tendencia

# --- Fin del Taller ---