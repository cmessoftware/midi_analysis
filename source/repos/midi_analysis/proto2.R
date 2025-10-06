# Instalar y cargar los paquetes

install.packages("fluidsynth", repos = "https://ropensci.r-universe.dev")
install.packages("dplyr", repos = "https://cran.rstudio.com/")

library(fluidsynth)
library(dplyr)

# Leer el archivo MIDI y convertirlo en dataframe
midi_df <- midi_read("mz_311_1.mid")

# Filtrar el dataframe y quedarse sólo con los eventos de notas
ev_notes <- midi_df %>%
  filter(event %in% c("NOTE_ON", "NOTE_OFF")) %>%
  mutate(
    is_on  = (event == "NOTE_ON"  & param2 > 0),
    is_off = (event == "NOTE_OFF" | (event == "NOTE_ON" & param2 == 0))
  ) %>%
  arrange(channel, param1, tick)   # ordenar por canal, pitch y tiempo

# 3) Reconstruir las notas: para cada ON buscar el primer OFF posterior
notes <- ev_notes %>%
  group_by(channel, pitch = param1) %>%
  arrange(tick, .by_group = TRUE) %>%
  reframe({
    on_idx  <- which(is_on)   # posiciones de filas donde la nota se prendió
    off_idx <- which(is_off)  # posiciones de filas donde la nota se apagó
    
    if (length(on_idx) == 0) return(NULL)
    
    rows <- lapply(on_idx, function(i){
      # buscar el primer "apagado" después de este "encendido"
      k_rel <- which(off_idx > i)[1]   # índice relativo dentro de off_idx
      if (is.na(k_rel)) return(NULL)   # no hay OFF posterior, se descarta esa nota
      j <- off_idx[k_rel]              # índice absoluto de ese OFF en el dataframe del grupo
      
      tibble(
        tick_on  = tick[i],
        tick_off = tick[j],
        dur_ticks = tick[j] - tick[i],
        velocity  = param2[i]
      )
    })
    bind_rows(rows)
  }) %>%
  ungroup()

head(notes)

# Resumir la pieza en una sola fila para entrenar al modelo

features <- notes %>%
  summarise(
    pitch_mean = mean(pitch, na.rm = TRUE),  # promedio de altura
    pitch_sd   = sd(pitch, na.rm = TRUE),    # cuánto varían las alturas
    vel_mean   = mean(velocity, na.rm = TRUE),  # intensidad media
    vel_sd     = sd(velocity, na.rm = TRUE),    # variación de intensidades
    dur_mean   = mean(dur_ticks, na.rm = TRUE), # duración promedio de las notas
    dur_sd     = sd(dur_ticks, na.rm = TRUE),   # variación de duraciones
    n_notes    = n()                             # cantidad total de notas reconstruidas
  )

features

# Guardar los resultados en archivos CSV

# 1. Guardar todas las notas procesadas
write.csv(notes, "notas_midi_procesadas.csv", row.names = FALSE)
cat("✓ Archivo 'notas_midi_procesadas.csv' guardado con", nrow(notes), "notas\n")

# 2. Guardar las características resumidas
write.csv(features, "caracteristicas_midi.csv", row.names = FALSE)
cat("✓ Archivo 'caracteristicas_midi.csv' guardado con el resumen del análisis\n")

# =====================================
# FUNCIÓN PARA DETECTAR NOTAS DESAFINADAS
# =====================================

detectar_notas_desafinadas <- function(notes_df, metodo = "todos") {
  
  # Función auxiliar: convertir MIDI pitch a nombre de nota
  pitch_to_note <- function(pitch) {
    note_names <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
    octave <- floor(pitch / 12) - 1
    note <- note_names[(pitch %% 12) + 1]
    paste0(note, octave)
  }
  
  # Escalas musicales comunes (en semitonos desde C)
  escala_mayor <- c(0, 2, 4, 5, 7, 9, 11)      # Do mayor
  escala_menor <- c(0, 2, 3, 5, 7, 8, 10)      # Do menor natural
  escala_cromatica <- 0:11                       # Todas las notas
  
  # Intervalos disonantes (diferencias en semitonos)
  intervalos_disonantes <- c(1, 6, 10, 11)      # 2ª menor, tritono, 7ª mayor, 7ª mayor
  
  # Análisis estadístico de la pieza
  duracion_media <- mean(notes_df$dur_ticks, na.rm = TRUE)
  duracion_sd <- sd(notes_df$dur_ticks, na.rm = TRUE)
  velocidad_media <- mean(notes_df$velocity, na.rm = TRUE)
  velocidad_sd <- sd(notes_df$velocity, na.rm = TRUE)
  
  # Umbrales para detección
  umbral_duracion_corta <- duracion_media - 2 * duracion_sd
  umbral_velocidad_baja <- velocidad_media - 2 * velocidad_sd
  umbral_velocidad_alta <- velocidad_media + 2 * velocidad_sd
  
  # Detectar tónica más probable (nota más frecuente)
  tonica_midi <- as.numeric(names(sort(table(notes_df$pitch), decreasing = TRUE))[1])
  tonica_clase <- tonica_midi %% 12
  
  # Aplicar detección según el método
  notas_problematicas <- notes_df %>%
    mutate(
      nota_nombre = sapply(pitch, pitch_to_note),
      clase_pitch = pitch %% 12,
      
      # Criterio 1: Fuera de escala mayor/menor desde la tónica
      fuera_escala_mayor = !((clase_pitch - tonica_clase) %% 12 %in% escala_mayor),
      fuera_escala_menor = !((clase_pitch - tonica_clase) %% 12 %in% escala_menor),
      
      # Criterio 2: Duración sospechosamente corta
      duracion_muy_corta = dur_ticks < max(10, umbral_duracion_corta),
      
      # Criterio 3: Velocidad anómala
      velocidad_anomala = velocity < umbral_velocidad_baja | velocity > umbral_velocidad_alta,
      
      # Criterio 4: Intervalos disonantes con notas vecinas
      intervalo_disonante = FALSE  # Se calculará después
    ) %>%
    arrange(tick_on)
  
  # Calcular intervalos disonantes con notas vecinas
  for(i in 2:(nrow(notas_problematicas)-1)) {
    pitch_actual <- notas_problematicas$pitch[i]
    pitch_anterior <- notas_problematicas$pitch[i-1]
    pitch_siguiente <- notas_problematicas$pitch[i+1]
    
    intervalo_ant <- abs(pitch_actual - pitch_anterior) %% 12
    intervalo_sig <- abs(pitch_actual - pitch_siguiente) %% 12
    
    if(intervalo_ant %in% intervalos_disonantes | intervalo_sig %in% intervalos_disonantes) {
      notas_problematicas$intervalo_disonante[i] <- TRUE
    }
  }
  
  # Filtrar según el método seleccionado
  if(metodo == "escala") {
    sospechosas <- notas_problematicas %>%
      filter(fuera_escala_mayor & fuera_escala_menor)
  } else if(metodo == "duracion") {
    sospechosas <- notas_problematicas %>%
      filter(duracion_muy_corta)
  } else if(metodo == "velocidad") {
    sospechosas <- notas_problematicas %>%
      filter(velocidad_anomala)
  } else if(metodo == "intervalos") {
    sospechosas <- notas_problematicas %>%
      filter(intervalo_disonante)
  } else { # "todos"
    sospechosas <- notas_problematicas %>%
      filter(fuera_escala_mayor & fuera_escala_menor | 
             duracion_muy_corta | 
             velocidad_anomala | 
             intervalo_disonante)
  }
  
  # Crear resumen
  resumen <- list(
    total_notas = nrow(notes_df),
    notas_sospechosas = nrow(sospechosas),
    porcentaje_sospechosas = round(nrow(sospechosas) / nrow(notes_df) * 100, 2),
    tonica_detectada = pitch_to_note(tonica_midi),
    criterios_usados = metodo,
    umbrales = list(
      duracion_minima = round(umbral_duracion_corta, 1),
      velocidad_baja = round(umbral_velocidad_baja, 1),
      velocidad_alta = round(umbral_velocidad_alta, 1)
    )
  )
  
  return(list(
    notas_sospechosas = sospechosas,
    resumen = resumen,
    todas_las_notas = notas_problematicas
  ))
}

# =====================================
# APLICAR DETECCIÓN DE NOTAS DESAFINADAS
# =====================================

cat("\n=== ANÁLISIS DE NOTAS DESAFINADAS ===\n")

# Detectar con todos los criterios
resultado_completo <- detectar_notas_desafinadas(notes, "todos")

# Mostrar resumen
cat("📊 RESUMEN DEL ANÁLISIS:\n")
cat("• Total de notas:", resultado_completo$resumen$total_notas, "\n")
cat("• Notas sospechosas:", resultado_completo$resumen$notas_sospechosas, "\n")
cat("• Porcentaje sospechoso:", resultado_completo$resumen$porcentaje_sospechosas, "%\n")
cat("• Tónica detectada:", resultado_completo$resumen$tonica_detectada, "\n")

# Análisis por criterios individuales
cat("\n📋 ANÁLISIS POR CRITERIOS:\n")
fuera_escala <- detectar_notas_desafinadas(notes, "escala")
cat("• Fuera de escala:", fuera_escala$resumen$notas_sospechosas, "notas\n")

duracion_corta <- detectar_notas_desafinadas(notes, "duracion")
cat("• Duración muy corta:", duracion_corta$resumen$notas_sospechosas, "notas\n")

velocidad_anomala <- detectar_notas_desafinadas(notes, "velocidad")
cat("• Velocidad anómala:", velocidad_anomala$resumen$notas_sospechosas, "notas\n")

intervalos_disonantes <- detectar_notas_desafinadas(notes, "intervalos")
cat("• Intervalos disonantes:", intervalos_disonantes$resumen$notas_sospechosas, "notas\n")

# Guardar resultados detallados
write.csv(resultado_completo$notas_sospechosas, "notas_desafinadas_detectadas.csv", row.names = FALSE)
write.csv(resultado_completo$todas_las_notas, "analisis_completo_notas.csv", row.names = FALSE)

cat("\n✅ Archivos guardados:\n")
cat("• 'notas_desafinadas_detectadas.csv' - Solo notas sospechosas\n")
cat("• 'analisis_completo_notas.csv' - Todas las notas con análisis\n")