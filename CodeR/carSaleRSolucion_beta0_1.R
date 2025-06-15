library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)
library(GGally)
library(dplyr)

library(randomForest)
library(dplyr)
library(shiny)

options(scipen = 999) 

df<-read.csv('colocar ruta', header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

df$Date<- as.Date(df$Date,format = '%m/%d/%Y')
df$Phone<-as.character(df$Phone)
#/**** Aplicacion de Logaritmo Natural ******
df$Annual_Income_ln <- log(df$Annual_Income)
df$Price_ln <- log(df$Price)

set.seed(1000)  # Fijar semilla para reproducibilidad
indices <- sample(1:nrow(df), size = 0.8 * nrow(df))  # Selección aleatoria (80% de los datos)
df_train <- df[indices, ]  # Data de entrenamiento
df_test <- df[-indices, ]  # Data de prueba

df_model <- df_train %>%
  select(Gender, Annual_Income_ln, Price_ln,Body_Style) %>%
  mutate(Gender = as.factor(Gender),Body_Style= as.factor(Body_Style))

modelo_rf <- randomForest(Body_Style ~ Gender + Annual_Income_ln, data = df_model, ntree = 100)

ui <- fluidPage(
  titlePanel("Predicción de Precio de Vehículo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Género:", choices = c("Male", "Female")),
      numericInput("income", "Salario Anual:", 50000, min = 10000, max = 200000),
      actionButton("predict", "Predecir")
    ),
    mainPanel(
      textOutput("resultado")
    )
  )
)

# Server

server <- function(input, output) {
  observeEvent(input$predict, {
    nueva_persona <- data.frame(Gender = as.factor(input$gender), 
                                Annual_Income_ln = log(input$income))
    
    nueva_persona$Gender <- factor(nueva_persona$Gender, levels = levels(df_model$Gender))  # Asegurar niveles de factor
    
    prediccion <- predict(modelo_rf, nueva_persona)  # Predicción
    
    output$resultado <- renderText(paste("Modelo recomendado:", prediccion))
  })
}
# Ejecutar la app
shinyApp(ui = ui, server = server)


