library(shiny)
library(wordcloud2)

# Lista reactiva para almacenar las palabras ingresadas
palabras <- reactiveVal(data.frame(word = character(0), freq = numeric(0)))

# Interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Generador de WordCloud interactivo"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("nueva_palabra", "Ingrese palabras separadas por espacios:", ""),
      actionButton("agregar", "Agregar palabras"),
      actionButton("borrar", "Borrar todas las palabras"), # Botón para borrar
      hr(),
      h5("Palabras agregadas:"),
      tableOutput("tabla_palabras")
    ),
    
    mainPanel(
      wordcloud2Output("wordcloud")
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  
  # Agregar palabras cuando se presiona el botón "Agregar"
  observeEvent(input$agregar, {
    # Si hay espacios, separar las palabras para tomarlas individualmente
    nuevas_palabras <- strsplit(input$nueva_palabra, "\\s+")[[1]]
    
    if (length(nuevas_palabras) > 0) {
      palabras_anteriores <- palabras()
      
      # Recorrer las palabras ingresadas
      for (palabra in nuevas_palabras) {
        if (palabra != "") {
          # Verificar si la palabra ya existe
          if (palabra %in% palabras_anteriores$word) {
            #Agregar palabra existente a la lista
            palabras_anteriores$freq[palabras_anteriores$word == palabra] <- 
              palabras_anteriores$freq[palabras_anteriores$word == palabra] + 1
          } else {
            #Agregar palabra nueva a la lista
            palabras_anteriores <- rbind(palabras_anteriores, data.frame(word = palabra, freq = 1))
          }
        }
      }
      
      palabras(palabras_anteriores) # Actualizar la lista de palabras
      updateTextInput(session, "nueva_palabra", value = "") # Resetear el campo de entrada
    }
  })
  
  # Borrar todas las palabras cuando se presiona el botón "Borrar"
  observeEvent(input$borrar, {
    palabras(data.frame(word = character(0), freq = numeric(0)))  # Reiniciar la lista de palabras
  })
  
  # Renderizar el WordCloud
  output$wordcloud <- renderWordcloud2({
    wordcloud2(palabras())
  })
  
  # Mostrar la tabla de palabras agregadas
  output$tabla_palabras <- renderTable({
    palabras()
  })
}

# Ejecutar la app
shinyApp(ui = ui, server = server)
