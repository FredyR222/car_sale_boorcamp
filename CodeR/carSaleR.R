library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)
library(GGally)
library(dplyr)


options(scipen = 999) 

df<-read.csv('colocar ruta', header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

#Descripcion de La informacion que contiene el archivo .csv
str(df)

#Vector de muestra de la informacion
head(df)

#/*** parseo de datos **/
df$Date<- as.Date(df$Date,format = '%m/%d/%Y')
df$Phone<-as.character(df$Phone)
str(df)

#Revision de Data
distinct_counts <- sapply(df, function(x) length(unique(x)))

print(distinct_counts)

#Valores null
null_counts <- colSums(is.na(df))
print(null_counts)

null_rows <- df[!complete.cases(df), ]
print(null_rows)

#Datos duplicados
duplicates <- df[duplicated(df), ]
print(head(duplicates))

# Grafico de distribucion de conteo de ventas por categorias

body_style_count <- as.data.frame(table(df$Body_Style))
colnames(body_style_count) <- c('Body_Style', 'Count')


print(body_style_count)

pie_chart <- ggplot(body_style_count, aes(x = '', y = Count, fill = Body_Style)) + 
  geom_bar(width = 1, stat = 'identity') + 
  coord_polar('y') + 
  labs(title = 'Conteo por Body Style Vendidos') + 
  theme_void()

# Mostrar el gráfico
print(pie_chart)


#/***** Grafica de conteo de ventas por marca ****/
sales_by_brand <- as.data.frame(table(df$Company))
colnames(sales_by_brand) <- c('Brand', 'Sales')

plot <- ggplot(sales_by_brand, aes(x = Brand, y = Sales)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  theme_minimal() + 
  labs(title = 'Cantidad de Ventas por Marca', x = 'Marca', y = 'Cantidad de Ventas') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)


#/*********Grafico de marca y modelos*************/
top_45 <- df %>%
  count(Company, Model,sort= TRUE) %>%
  slice_head(n = 45)
print(top_45)

# Create the bar plot
sales_plot <- ggplot(top_45, aes(x = Model, y = n, fill = Company)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  labs(title = 'Cantidad de Ventas por Modelo y Marca', x = 'Modelo', y = 'Cantidad de Ventas') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(sales_plot)

#Pair Plot
ggpairs(df, 
        columns = c("Annual_Income", "Price"),  # Solo columnas numéricas
        aes(color = Gender)) 

# distribuciuon de ventas por genero
conteo_Gender <- df %>%
  count(Gender) %>%
  mutate(porcentaje = n / sum(n) * 100)  # Calcular porcentaje

# Crear gráfico de pastel
ggplot(conteo_Gender, aes(x = "", y = porcentaje, fill = Gender)) +
  geom_col() +
  coord_polar(theta = "y") +  # Convertir a gráfico circular
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Balance de Ventas por Género") +
  theme_minimal()

# Calcular el total de ingresos por género
ingresos_por_genero <- df %>%
  group_by(Gender) %>%
  summarise(total_ingreso = sum(Price))

# Crear el gráfico de barras

ggplot(ingresos_por_genero, aes(x = Gender, y = total_ingreso, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("pink", "blue")) +  # Colores personalizados
  labs(title = "Monto de Ventas por Género",
       x = "Género",
       y = "Precio de ventas totales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0)) +  # Mantener etiquetas horizontales
  geom_text(aes(label = total_ingreso), vjust = -0.5)  # Agregar etiquetas con valores


#/***************** Calcular el promedio de ingresos por género ******************/
ingresos_por_genero <- df %>%
  group_by(Gender) %>%
  summarise(ingreso_promedio = mean(Price))

# Crear el gráfico de barras
ggplot(ingresos_por_genero, aes(x = Gender, y = ingreso_promedio, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("pink", "blue")) +  # Colores personalizados
  labs(title = "Promedio del monto por Género",
       x = "Género",
       y = "Ingreso Promedio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0)) +  # Mantener etiquetas horizontales
  geom_text(aes(label = round(ingreso_promedio, 2)), vjust = -0.5)  # Agregar etiquetas con valores

#/**** ingresos_por_genero

df <- df %>%
  mutate(Mes_year = floor_date(as.Date(Date), "month"))

# Obtener géneros únicos
generos <- unique(df$Gender)

# Loop por género
for (genero in generos) {
  
  df_genero <- df %>%
    filter(Gender == genero) %>%
    group_by(Gender, Mes_year) %>%
    summarise(Precio_Promedio = mean(Price, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(df_genero, aes(x = Mes_year, y = Precio_Promedio)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(title = "Precio Promedio de Vehículos por Género y Mes",
         subtitle = paste("Género:", genero),
         x = "Mes y Año",
         y = "Precio Promedio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)  # <- Esto asegura que se muestre el gráfico dentro del loop
}

#/****** boxplot *****/

ggplot(df, aes(x = factor(1), y = Annual_Income)) +
  geom_boxplot(fill = "salmon", color = "grey") +
  labs(title = "Boxplot Ingreso Anual", x = "", y = "Ingreso anual") +
  coord_flip() +
  theme_minimal()

ggplot(df, aes(x = factor(1), y = Price)) +
  geom_boxplot(fill = "salmon", color = "grey") +
  labs(title = "Boxplot Precio", x = "", y = "Precio") +
  coord_flip() +
  theme_minimal()


#/**** Aplicacion de Logaritmo Natural ******
df$Annual_Income_ln <- log(df$Annual_Income)
df$Price_ln <- log(df$Price)

ggplot(df, aes(x = factor(1), y = Annual_Income_ln)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot Ingreso Anual", x = "", y = "Ingreso anual") +
  coord_flip() +
  theme_minimal()

ggplot(df, aes(x = factor(1), y = Price_ln)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot Precio", x = "", y = "Precio") +
  coord_flip() +
  theme_minimal()

#/************** Crear el gráfico Dispercion ***/////
ggplot(df, aes(x = Price, y = Annual_Income)) +
  geom_point(color = "blue", alpha = 0.6) +  # Ajusta color y transparencia
  labs(title = "Relación entre Ingresos Anuales y Costo de Vehículo",
       x = "Precio Vehículo",
       y = "Ingresos Anuales") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))  # Añade líneas de la cuadrícula

#/*** grafico dispercion en logaritmo natural
ggplot(df, aes(x = Price_ln, y = Annual_Income_ln)) +
  geom_point(color = "blue", alpha = 0.6) +  # Ajusta color y transparencia
  labs(title = "Relación entre Ingresos Anuales y Costo de Vehículo con logaritmo natural",
       x = "Precio Vehículo",
       y = "Ingresos Anuales") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))  # Añade líneas de la cuadrícula

#/******** Montos por Region  ****/
ingresos_por_region_promedio <- df %>%
  group_by(Dealer_Region) %>%
  summarise(Price = sum(Price))

# Crear el gráfico de barras
ggplot(ingresos_por_region_promedio, aes(x = Dealer_Region, y = Price, fill = Dealer_Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Monto por Región",
       x = "Dealer Region",
       y = "Ingreso Promedio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0)) +  # Mantener etiquetas horizontales
  geom_text(aes(label = Price), vjust = -0.5, size = 4)  # Agregar etiquetas con valores


# Calcular el total por región y año

df$year<- year(df$Date)#extraemos solo el dato del year de date

ingresos_por_region <- df %>%
  group_by(Dealer_Region, year) %>%
  summarise(Total = sum(Price))

# Crear el gráfico de barras
ggplot(ingresos_por_region, aes(x = interaction(Dealer_Region, year), y = Total, fill = Dealer_Region)) +
  geom_bar(stat = "identity", position = "dodge") +  # Agrupar por año
  labs(title = "Total del monto por Región y Año",
       x = "Dealer Región y Año",
       y = "Ingreso Promedio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotar etiquetas del eje X
  scale_fill_brewer(palette = "Set3")  # Colores automáticos para distintas regiones



#/**** Comparativas por genero ***/
# Filtrar datos por género
dfGeneroM <- df %>% filter(Gender == "Male")
dfGeneroF <- df %>% filter(Gender == "Female")

# Contar ocurrencias de Body_Style por género
conteo_fem <- dfGeneroF %>% count(Body_Style)
conteo_mas <- dfGeneroM %>% count(Body_Style)

# Crear el gráfico de líneas para la comparación
ggplot() +
  geom_line(data = conteo_fem, aes(x = Body_Style, y = n, group = 1), 
            color = "pink", linewidth = 1.2) +
  geom_point(data = conteo_fem, aes(x = Body_Style, y = n), color = "pink", size = 3) +
  geom_line(data = conteo_mas, aes(x = Body_Style, y = n, group = 1), 
            color = "blue", linewidth = 1.2) +
  geom_point(data = conteo_mas, aes(x = Body_Style, y = n), color = "blue", size = 3) +
  labs(title = "Comparación de Tendencia por Género",
       x = "Body Style",
       y = "Cantidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotación de etiquetas para mejor legibilidad
  scale_x_discrete(limits = sort(unique(df$Body_Style))) +  # Ordenar categorías en el eje X
  geom_text(data = conteo_fem, aes(x = Body_Style, y = n, label = n), vjust = -0.5, color = "pink") +
  geom_text(data = conteo_mas, aes(x = Body_Style, y = n, label = n), vjust = -0.5, color = "blue")

#/***promedio*****/#

promedio_fem <- df %>%
  filter(Gender == "Female") %>%
  count(Body_Style) %>%
  mutate(percentage = n / sum(n))

promedio_mas <- df %>%
  filter(Gender == "Male") %>%
  count(Body_Style) %>%
  mutate(percentage = n / sum(n))

# Crear el gráfico de líneas
ggplot() +
  geom_line(data = promedio_fem, aes(x = Body_Style, y = percentage, group = 1), 
            color = "pink", linewidth = 1.2) +
  geom_point(data = promedio_fem, aes(x = Body_Style, y = percentage), color = "pink", size = 3) +
  geom_line(data = promedio_mas, aes(x = Body_Style, y = percentage, group = 1), 
            color = "blue", linewidth = 1.2) +
  geom_point(data = promedio_mas, aes(x = Body_Style, y = percentage), color = "blue", size = 3) +
  labs(title = "Promedio de Preferencias por Género",
       x = "Body Style",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje X
  scale_x_discrete(limits = sort(unique(df$Body_Style))) +  # Ordenar categorías en el eje X
  geom_text(data = promedio_fem, aes(x = Body_Style, y = percentage, label = scales::percent(percentage)), vjust = -0.5, color = "pink") +
  geom_text(data = promedio_mas, aes(x = Body_Style, y = percentage, label = scales::percent(percentage)), vjust = -0.5, color = "blue")


#/*********Analisis *****//
