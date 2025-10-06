# Instalar y cargar los paquetes

install.packages("fluidsynth", repos = "https://ropensci.r-universe.dev")
install.packages("dplyr")

library(fluidsynth)
library(dplyr)

# Leer el archivo MIDI y convertirlo en dataframe
midi_df <- midi_read("tu_archivo.mid")

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