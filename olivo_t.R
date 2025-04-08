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
#setwd("D:\Perfiles\rac82\Desktop\cosas\PROYECTOS\SOCIAL-AGRI\datos_living_lab_20250221\analisis_living_lab") #Ajustar directorio de trabajo

locale <- Sys.getlocale(category = "LC_TIME") #Obtener y ajustar la localización actual
#parse_datetime(olivo1$`Fecha/Hora`, format = "%d-%m-%Y %H:%M:%S", locale = default_locale(), trim_ws = TRUE)

# Leer el archivo CSV de C1N1
olivo1 <- read_csv("C1N1.csv")
olivo1$`Fecha/Hora`<-as_datetime(olivo1$`Fecha/Hora`,format="%d-%m-%Y %H:%M:%S")
olivo1_filtrado <- filter(olivo1, `Temperatura (ºC)` < 70)
# Ver las primeras filas del dataset
head(olivo1_filtrado)

# Leer el archivo CSV de C1N2
olivo2 <- read_csv("C1N2.csv")
olivo2$`Fecha/Hora`<-as_datetime(olivo2$`Fecha/Hora`,format="%d-%m-%Y %H:%M:%S")
olivo2_filtrado <- filter(olivo2, `Temperatura (ºC)` < 70)
# Ver las primeras filas del dataset
head(olivo2_filtrado)

# Leer el archivo CSV de C1N3
olivo3 <- read_csv("C1N3.csv")
olivo3$`Fecha/Hora`<-as_datetime(olivo3$`Fecha/Hora`,format="%d-%m-%Y %H:%M:%S")
olivo3_filtrado <- filter(olivo3, `Temperatura (ºC)` < 70)
# Ver las primeras filas del dataset
head(olivo3_filtrado)


# Agrupar datos por mes (o por día u hora si quieres menos suavización)
olivo1_suavizado <- olivo1_filtrado %>%
  mutate(Fecha = floor_date(`Fecha/Hora`, "hour")) %>%  # Cambia "month" por "day" si quieres diario
  group_by(Fecha) %>%
  summarise(Temperatura = mean(`Temperatura (ºC)`, na.rm = TRUE))

olivo2_suavizado <- olivo2_filtrado %>%
  mutate(Fecha = floor_date(`Fecha/Hora`, "hour")) %>%  # Cambia "month" por "day" si quieres diario
  group_by(Fecha) %>%
  summarise(Temperatura = mean(`Temperatura (ºC)`, na.rm = TRUE))

olivo3_suavizado <- olivo3_filtrado %>%
  mutate(Fecha = floor_date(`Fecha/Hora`, "hour")) %>%  # Cambia "month" por "day" si quieres diario
  group_by(Fecha) %>%
  summarise(Temperatura = mean(`Temperatura (ºC)`, na.rm = TRUE))


# Plot

p1 <- ggplot(olivo1_suavizado, aes(x = Fecha, y= Temperatura)) +
   geom_line(color="black", alpha=0.7) +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "#FF8C00", se = FALSE) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "") + # Muestra cada mes
   scale_y_continuous(limits = c(0, 50), breaks = seq(0, 55, by = 10)) +  # Establece los límites del eje Y 
  #scale_x_continuous(breaks = seq(0,50, by = 1)) +
  labs(title = "            t_C1N1",  
        x = "", 
        y = "") +
   #theme(plot.title = element_text(hjust = 0.5)) +
   theme_minimal() +
   theme(axis.text.x=element_blank(),
         axis.ticks.x=element_blank())
 
p2 <- ggplot(olivo2_suavizado, aes(x = Fecha, y= Temperatura)) +
   geom_line(color="black", alpha=0.7) +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "#FF8C00", se = FALSE) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "") + # Muestra cada mes
   scale_y_continuous(limits = c(0, 50), breaks = seq(0, 55, by = 10)) +  # Establece los límites del eje Y
   labs(title = "            t_C1N2",  
        x = "", 
        y = "") +
   #theme(plot.title = element_text(hjust = 0.5)) +
   theme_minimal() +
  theme(axis.text.x=element_blank(),
           axis.ticks.x=element_blank())
 
p3 <- ggplot(olivo3_suavizado, aes(x = Fecha, y= Temperatura)) +
   geom_line(color="black", alpha=0.7) +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "darkorange", se = FALSE) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") + # Muestra cada mes
   scale_y_continuous(limits = c(0, 50), breaks = seq(0, 55, by = 10)) +  # Establece los límites del eje Y
  labs(title = "            t_C1N3",  
        x = "Fecha", 
        y = "") +
   #theme(plot.title = element_text(hjust = 0.5)) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 # Renombrar las columnas de temperatura antes de unir
 olivo1_suavizado <- olivo1_suavizado %>% rename(Temperatura_1 = Temperatura)
 olivo2_suavizado <- olivo2_suavizado %>% rename(Temperatura_2 = Temperatura)
 olivo3_suavizado <- olivo3_suavizado %>% rename(Temperatura_3 = Temperatura)
 
 # Unir los tres datasets por la columna "Fecha"
 olivos_promedio <- full_join(olivo1_suavizado, olivo2_suavizado, by = "Fecha") %>%
   full_join(olivo3_suavizado, by = "Fecha") %>%
   mutate(Temperatura_Promedio = rowMeans(across(starts_with("Temperatura")), na.rm = TRUE))
 
 # Ver los primeros registros
 head(olivos_promedio)
 
 # Agrupar datos por mes (o por día u hora si quieres menos suavización)
 olivos_promedio_suavizado <- olivos_promedio %>%
   mutate(Fecha = floor_date(`Fecha`, "hour")) %>%  # Cambia "month" por "day" si quieres diario
   group_by(Fecha) %>%
   summarise(Temperatura = round(mean(`Temperatura_Promedio`, na.rm = TRUE), 1))  # Ajusta a 1 decimal
 head(olivos_promedio_suavizado)
 
 # Plot
pt <-  ggplot(olivos_promedio_suavizado, aes(x = Fecha, y= Temperatura, color = Temperatura)) +
   geom_line(alpha=1) +
   geom_hline(yintercept = max(olivos_promedio_suavizado$Temperatura, na.rm = TRUE), color = "darkorange", alpha(0.5)) +
   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "black", se = FALSE, size = 1) +
   scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") + # Muestra cada mes
   scale_y_continuous(limits = c(0, 50)) +  # Establece los límites del eje Y
   labs(title = "Ev. de la temperatura en suelo del olivar",  
        x = "Fecha", 
        y = "Temperatura (ºC)") +
   #scale_color_gradient(low = "brown", high = "red") +  # Cambia la paleta de color
   scale_color_paletteer_c("ggthemes::Classic Orange-White-Blue", direction = -1) +  # Usando la paleta de colores
   #theme(plot.title = element_text(hjust = 0.5)) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme(legend.position = "null")

#Gráfico combinado 
pt|p1/p2/p3

olivos_promedio_it <- olivos_promedio %>%
  # Promedio de las dos temperaturas por hora
  mutate(Temp_Promedio_Hora = rowMeans(select(., Temperatura_1, Temperatura_2), na.rm = TRUE),
         Fecha = floor_date(Fecha, "day")) %>%
  # Agrupamos por día y sacamos la media diaria
  group_by(Fecha) %>%
  summarise(Temp_Diaria = mean(Temp_Promedio_Hora, na.rm = TRUE)) %>%
  # Calculamos los grados >7, y los acumulamos por año
  mutate(it_diaria = pmax(Temp_Diaria - 7, 0),
         Anio = year(Fecha)) %>%
  group_by(Anio) %>%
  arrange(Fecha) %>%
  mutate(it_acumulada = cumsum(it_diaria))

head(olivos_promedio_it)
  cat("Integral térmica total:", total_it, "\n")
