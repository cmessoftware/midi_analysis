# ============================================
# ANÁLISIS COMPLETO DE MIDI CON EDA Y ML
# ============================================
# Este script ejecuta todo el análisis sin necesidad de Jupyter

cat("=== INICIANDO ANÁLISIS COMPLETO DE MIDI ===\n\n")

# 1. CARGAR Y VERIFICAR PAQUETES
cat("1. Verificando e instalando paquetes...\n")

# Lista de paquetes requeridos
paquetes <- c("dplyr", "ggplot2", "randomForest", "caret", "pROC", "corrplot")

for(paquete in paquetes) {
  if (!require(paquete, character.only = TRUE, quietly = TRUE)) {
    cat("   Instalando:", paquete, "\n")
    install.packages(paquete, repos = "https://cran.rstudio.com/")
    library(paquete, character.only = TRUE)
  }
}

# Instalar fluidsynth si no existe
if (!require("fluidsynth", quietly = TRUE)) {
  cat("   Instalando fluidsynth desde r-universe...\n")
  install.packages("fluidsynth", repos = "https://ropensci.r-universe.dev")
  library(fluidsynth)
}

cat("   ✅ Todos los paquetes cargados\n\n")

# 2. VERIFICAR ARCHIVOS DE DATOS
cat("2. Verificando archivos de datos...\n")

if (!file.exists("analisis_completo_notas.csv")) {
  cat("   ❌ Archivos de análisis no encontrados\n")
  cat("   🔄 Ejecutando proto2.R para generar datos...\n")
  source("proto2.R")
  cat("   ✅ Datos generados\n")
} else {
  cat("   ✅ Archivos de datos encontrados\n")
}

# 3. CARGAR DATOS
cat("\n3. Cargando datos para análisis...\n")

notas_completas <- read.csv("analisis_completo_notas.csv")
notas_sospechosas <- read.csv("notas_desafinadas_detectadas.csv")

cat("   - Dataset completo:", nrow(notas_completas), "notas\n")
cat("   - Dataset sospechosas:", nrow(notas_sospechosas), "notas\n")

# 4. EDA - ESTADÍSTICAS DESCRIPTIVAS
cat("\n4. Realizando Análisis Exploratorio de Datos...\n")

# Crear variable target
notas_completas$es_sospechosa <- ifelse(
  notas_completas$fuera_escala_mayor & notas_completas$fuera_escala_menor |
  notas_completas$duracion_muy_corta |
  notas_completas$velocidad_anomala |
  notas_completas$intervalo_disonante, 1, 0
)

cat("   - Total de notas:", nrow(notas_completas), "\n")
cat("   - Notas sospechosas:", sum(notas_completas$es_sospechosa), "\n")
cat("   - Porcentaje sospechosas:", round(mean(notas_completas$es_sospechosa) * 100, 2), "%\n")

# 5. PREPARAR DATOS PARA ML
cat("\n5. Preparando datos para Machine Learning...\n")

notas_ml <- notas_completas %>%
  mutate(
    posicion_relativa = (tick_on - min(tick_on)) / (max(tick_on) - min(tick_on)),
    octava = floor(pitch / 12),
    nota_en_octava = pitch %% 12,
    velocidad_norm = scale(velocity)[,1],
    duracion_norm = scale(dur_ticks)[,1],
    target = factor(es_sospechosa, levels = c(0, 1), labels = c("Normal", "Sospechosa"))
  ) %>%
  select(pitch, dur_ticks, velocity, tick_on, channel, clase_pitch,
         posicion_relativa, octava, nota_en_octava, velocidad_norm, duracion_norm, target) %>%
  na.omit()

cat("   - Dimensiones dataset ML:", dim(notas_ml), "\n")

# 6. MODELO - PARTICIÓN TEMPORAL
cat("\n6. Entrenando modelo Random Forest (Partición Temporal)...\n")

set.seed(123)
notas_ordenadas <- notas_ml %>% arrange(tick_on)
punto_corte <- floor(nrow(notas_ordenadas) * 0.7)

train_temporal <- notas_ordenadas[1:punto_corte, ]
test_temporal <- notas_ordenadas[(punto_corte+1):nrow(notas_ordenadas), ]

rf_temporal <- randomForest(
  target ~ . - tick_on,
  data = train_temporal,
  ntree = 500,
  importance = TRUE
)

pred_temporal <- predict(rf_temporal, test_temporal)
conf_temporal <- confusionMatrix(pred_temporal, test_temporal$target, positive = "Sospechosa")

cat("   - Accuracy:", round(conf_temporal$overall['Accuracy'], 4), "\n")
cat("   - Sensitivity:", round(conf_temporal$byClass['Sensitivity'], 4), "\n")
cat("   - Specificity:", round(conf_temporal$byClass['Specificity'], 4), "\n")

# 7. MODELO - PARTICIÓN ALEATORIA
cat("\n7. Entrenando modelo Random Forest (Partición Aleatoria)...\n")

train_indices <- createDataPartition(notas_ml$target, p = 0.7, list = FALSE)
train_aleatorio <- notas_ml[train_indices, ]
test_aleatorio <- notas_ml[-train_indices, ]

rf_aleatorio <- randomForest(
  target ~ .,
  data = train_aleatorio,
  ntree = 500,
  importance = TRUE
)

pred_aleatorio <- predict(rf_aleatorio, test_aleatorio)
conf_aleatorio <- confusionMatrix(pred_aleatorio, test_aleatorio$target, positive = "Sospechosa")

cat("   - Accuracy:", round(conf_aleatorio$overall['Accuracy'], 4), "\n")
cat("   - Sensitivity:", round(conf_aleatorio$byClass['Sensitivity'], 4), "\n")
cat("   - Specificity:", round(conf_aleatorio$byClass['Specificity'], 4), "\n")

# 8. GUARDAR RESULTADOS
cat("\n8. Guardando resultados...\n")

# Guardar modelos
save(rf_temporal, rf_aleatorio, file = "modelos_random_forest.RData")

# Importancia de variables
importance_data <- importance(rf_aleatorio)
write.csv(importance_data, "importancia_variables.csv")

# Predicciones
prob_temporal <- predict(rf_temporal, test_temporal, type = "prob")
prob_aleatorio <- predict(rf_aleatorio, test_aleatorio, type = "prob")

resultados_ml <- data.frame(
  estrategia = rep(c("temporal", "aleatorio"), 
                   c(nrow(test_temporal), nrow(test_aleatorio))),
  real = c(as.character(test_temporal$target), as.character(test_aleatorio$target)),
  prediccion = c(as.character(pred_temporal), as.character(pred_aleatorio)),
  probabilidad_sospechosa = c(prob_temporal[,2], prob_aleatorio[,2])
)

write.csv(resultados_ml, "resultados_modelos_ml.csv", row.names = FALSE)

# 9. GENERAR VISUALIZACIONES
cat("\n9. Generando visualizaciones...\n")

# Crear directorio para gráficos si no existe
if (!dir.exists("graficos")) dir.create("graficos")

# Gráfico 1: Distribución de pitch
png("graficos/distribucion_pitch.png", width = 800, height = 600)
p1 <- ggplot(notas_completas, aes(x = pitch, fill = factor(es_sospechosa))) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "salmon"), 
                    labels = c("Normal", "Sospechosa")) +
  labs(title = "Distribución de Pitch por Tipo de Nota",
       x = "Pitch (MIDI)", y = "Frecuencia", fill = "Tipo") +
  theme_minimal()
print(p1)
dev.off()

# Gráfico 2: Importancia de variables
png("graficos/importancia_variables.png", width = 800, height = 600)
varImpPlot(rf_aleatorio, main = "Importancia de Variables - Random Forest")
dev.off()

cat("   ✅ Gráficos guardados en carpeta 'graficos/'\n")

# 10. RESUMEN FINAL
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("🎯 ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("📊 ARCHIVOS GENERADOS:\n")
cat("   • modelos_random_forest.RData - Modelos entrenados\n")
cat("   • resultados_modelos_ml.csv - Predicciones y evaluaciones\n")
cat("   • importancia_variables.csv - Importancia de features\n")
cat("   • graficos/ - Visualizaciones generadas\n\n")

cat("🎼 RESULTADOS CLAVE:\n")
cat("   • Total notas analizadas:", nrow(notas_completas), "\n")
cat("   • Notas sospechosas detectadas:", sum(notas_completas$es_sospechosa), "\n")
cat("   • Accuracy modelo temporal:", round(conf_temporal$overall['Accuracy'], 4), "\n")
cat("   • Accuracy modelo aleatorio:", round(conf_aleatorio$overall['Accuracy'], 4), "\n")

cat("\nAnálisis completo finalizado! 🎉\n")