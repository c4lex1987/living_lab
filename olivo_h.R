# Acceso a CRAN
options(download.file.method = "wininet")

# Cargar librerías necesarias
library(readr)   # Para leer CSVs
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Creación de gráficos
library(ggthemes)
library(lubridate) # Parseo fechas
library(ggfortify)
library(rlang)
library(cli)
library(vars)
library(zoo)
library(graphics)
library(tidyverse)
library(datos)
library(mgcv)
library(patchwork)
library(RColorBrewer)
library(paletteer)
library(colorBlindness)
library(pracma)
#setwd("D:\Perfiles\rac82\Desktop\cosas\PROYECTOS\SOCIAL-AGRI\datos_living_lab_20250221\analisis_living_lab") #Ajustar directorio de trabajo

locale <- Sys.getlocale(category = "LC_TIME") #Obtener y ajustar la localización actual
#parse_datetime(olivo1$`Fecha/Hora`, format = "%d-%m-%Y %H:%M:%S", locale = default_locale(), trim_ws = TRUE)

# Leer el archivo CSV de C1N1
olivo1 <- read_csv("C1N1.csv")
olivo1$`Fecha/Hora`<-as_datetime(olivo1$`Fecha/Hora`,format="%d-%m-%Y %H:%M:%S")
olivo1_filtrado <- filter(olivo1, `Humedad (%)` < 100, `Humedad (%)` > 2)
# Ver las primeras filas del dataset
head(olivo1_filtrado)

# Leer el archivo CSV de C1N2
olivo2 <- read_csv("C1N2.csv")
olivo2$`Fecha/Hora`<-as_datetime(olivo2$`Fecha/Hora`,format="%d-%m-%Y %H:%M:%S")
olivo2_filtrado <- filter(olivo2, `Humedad (%)` < 100, `Humedad (%)` > 2)
# Ver las primeras filas del dataset
head(olivo2_filtrado)

# Leer el archivo CSV de C1N3
olivo3 <- read_csv("C1N3.csv")
olivo3$`Fecha/Hora`<-as_datetime(olivo3$`Fecha/Hora`,format="%d-%m-%Y %H:%M:%S")
olivo3_filtrado <- filter(olivo3, `Humedad (%)` < 100, `Humedad (%)` > 2)
# Ver las primeras filas del dataset
head(olivo3_filtrado)


# Agrupar datos por mes (o por día u hora si quieres menos suavización)
olivo1_suavizado <- olivo1_filtrado %>%
  mutate(Fecha = floor_date(`Fecha/Hora`, "hour")) %>%  # Cambia "month" por "day" si quieres diario
  group_by(Fecha) %>%
  summarise(Humedad = mean(`Humedad (%)`, na.rm = TRUE))

olivo2_suavizado <- olivo2_filtrado %>%
  mutate(Fecha = floor_date(`Fecha/Hora`, "hour")) %>%  # Cambia "month" por "day" si quieres diario
  group_by(Fecha) %>%
  summarise(Humedad = mean(`Humedad (%)`, na.rm = TRUE))

olivo3_suavizado <- olivo3_filtrado %>%
  mutate(Fecha = floor_date(`Fecha/Hora`, "hour")) %>%  # Cambia "month" por "day" si quieres diario
  group_by(Fecha) %>%
  summarise(Humedad = mean(`Humedad (%)`, na.rm = TRUE))


# Plot

p1 <- ggplot(olivo1_suavizado, aes(x = Fecha, y= Humedad)) +
   geom_line(color="blue", alpha=0.7) +
   #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "lightblue", se = FALSE) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "") + # Muestra cada mes
   scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +  # Establece los límites del eje Y 
  #scale_x_continuous(breaks = seq(0,50, by = 1)) +
  labs(title = "            h_C1N1",  
        x = "", 
        y = "") +
   #theme(plot.title = element_text(hjust = 0.5)) +
   theme_minimal() +
   theme(axis.text.x=element_blank(),
         axis.ticks.x=element_blank())
 
p2 <- ggplot(olivo2_suavizado, aes(x = Fecha, y= Humedad)) +
   geom_line(color="blue", alpha=0.7) +
   #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "lightblue", se = FALSE) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "") + # Muestra cada mes
   scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +  # Establece los límites del eje Y
   labs(title = "            h_C1N2",  
        x = "", 
        y = "") +
   #theme(plot.title = element_text(hjust = 0.5)) +
   theme_minimal() +
  theme(axis.text.x=element_blank(),
           axis.ticks.x=element_blank())
 
p3 <- ggplot(olivo3_suavizado, aes(x = Fecha, y= Humedad)) +
   geom_line(color="blue", alpha=0.7) +
   #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "lightblue", se = FALSE) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") + # Muestra cada mes
   scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +  # Establece los límites del eje Y
  labs(title = "            h_C1N3",  
        x = "Fecha", 
        y = "") +
   #theme(plot.title = element_text(hjust = 0.5)) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 # Renombrar las columnas de Humedad antes de unir
 olivo1_suavizado <- olivo1_suavizado %>% rename(Humedad_1 = Humedad)
 olivo2_suavizado <- olivo2_suavizado %>% rename(Humedad_2 = Humedad)
 olivo3_suavizado <- olivo3_suavizado %>% rename(Humedad_3 = Humedad)
 
 # Unir los tres datasets por la columna "Fecha"
 olivos_promedio <- full_join(olivo1_suavizado, olivo2_suavizado, by = "Fecha") %>%
   full_join(olivo3_suavizado, by = "Fecha") %>%
   mutate(Humedad_Promedio = rowMeans(across(starts_with("Humedad")), na.rm = TRUE))
 
 # Ver los primeros registros
 head(olivos_promedio)

 # Agrupar datos por día
 olivos_promedio_suavizado <- olivos_promedio %>%
   mutate(Fecha = floor_date(Fecha, "day")) %>%
   group_by(Fecha) %>%
   summarise(Humedad = round(mean(Humedad_Promedio, na.rm = TRUE), 1)) %>%
   ungroup()
 
 # Asegurar formato POSIXct en Fecha
 olivos_promedio_suavizado$Fecha <- as.POSIXct(olivos_promedio_suavizado$Fecha)
 
 # Aplicar suavizado utilizando media móvil
 olivos_promedio_suavizado$Humedad_suavizada <- zoo::rollapply(olivos_promedio_suavizado$Humedad, 
                                                               width = 2,  # Tamaño de la ventana para media móvil
                                                               FUN = mean, 
                                                               fill = NA, 
                                                               align = "center")
 
 # Detectar picos máximos y mínimos locales sobre los datos suavizados
 picos_max <- findpeaks(olivos_promedio_suavizado$Humedad_suavizada, nups = 1, npeaks = 20, minpeakdistance = 20)
 picos_min <- findpeaks(-olivos_promedio_suavizado$Humedad_suavizada, nups = 1, npeaks = 25, minpeakdistance = 15)
 
 # Verificar si se detectaron picos antes de continuar
 if (length(picos_max) == 0 | length(picos_min) == 0) {
   stop("No se han detectado picos. Verifique los parámetros de suavizado o la resolución de los datos.")
 }
 
 # Extraer índices de los picos
 idx_max <- picos_max[, 2]
 idx_min <- picos_min[, 2]
 
 # Crear dataframe con los picos detectados
 picos_df <- data.frame(
   Tipo = c(rep("Máximo", length(idx_max)), rep("Mínimo", length(idx_min))),
   Fecha = c(olivos_promedio_suavizado$Fecha[idx_max], olivos_promedio_suavizado$Fecha[idx_min]),
   Humedad = c(olivos_promedio_suavizado$Humedad_suavizada[idx_max], olivos_promedio_suavizado$Humedad_suavizada[idx_min])
 ) %>%
   arrange(Fecha)  # Ordenar por fecha
 
 # Filtrar solo pares alternados (Máximo → Mínimo → Máximo...)
 picos_ordenados <- picos_df %>%
   filter(
     (lag(Tipo) == "Máximo" & Tipo == "Mínimo") |
       (lag(Tipo) == "Mínimo" & Tipo == "Máximo")
   ) %>%
   mutate(
     Fecha_anterior = lag(Fecha),
     Humedad_anterior = lag(Humedad),
     Pendiente = (Humedad - Humedad_anterior) / as.numeric(difftime(Fecha, Fecha_anterior, units = "days"))
   ) %>%
   filter(!is.na(Pendiente))  # Eliminar filas sin pendiente
 
 # Graficar los datos con picos detectados
 pt <- ggplot(olivos_promedio_suavizado, aes(x = Fecha, y = Humedad)) +
   geom_line(color = "blue") +
   geom_point(data = picos_df, aes(x = Fecha, y = Humedad, color = Tipo), size = 2) +
   scale_color_manual(values = c("Máximo" = "red", "Mínimo" = "green")) +  # Cambio aquí
   labs(title = "Cont. de agua en suelo del olivar", x = "Fecha", y = "Humedad (%)") +
   theme_minimal() +
   #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "black", se = FALSE, size = 1) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
   scale_y_continuous(limits = c(0, 40)) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme(legend.position = "none")  # Eliminar la leyenda
 
 # Mostrar la tabla con las pendientes
 print(picos_ordenados)
 
 # Exportar tabla a CSV si lo necesitas
 write.csv(picos_ordenados, "pendientes_min_max.csv", row.names = FALSE)
 
#Gráfico combinado 
pt|p1/p2/p3
