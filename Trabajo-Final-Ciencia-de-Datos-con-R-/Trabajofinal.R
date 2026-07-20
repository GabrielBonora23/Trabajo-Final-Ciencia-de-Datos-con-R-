
#Cargo librerías
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)




#Aplicación Shiny

ui <- navbarPage(
  
  "Análisis de Países",
  
  tabPanel(
    "Tabla",
    
    fluidPage(
      
      titlePanel("Países del mundo : Tabla"),
      
      fluidRow(
        column(3,
               selectInput("reg",
                           "Region:",
                           c("Todas",
                             unique(as.character(paises$Region))))
        ),   #Visualización de columnas
        column(3,
               selectInput("pbi",
                           "Nivel de PBI:",
                           c("Todos", unique(as.character(
                             paises$Categoria_PBI[!is.na(paises$Categoria_PBI)]))))
        )
      ),
      
dataTableOutput("tabla")
      
    )
    
  ),
  
  tabPanel(
    "Mapa de calor",
    
    fluidPage(
      
      titlePanel("Mapa de Calor, Comparativo de Indicadores"),
      
      sidebarLayout(
        
        sidebarPanel(width = 2, #Tamaño de la barra de control.
                     
                     checkboxGroupInput( #Permite seleccionar variables
                       "variable_calor",
                       "Seleccionar variable:",
                       choices = c(
                         "Poblacion", "Area", "Densidad_pob", "Proporcion_costa",
                         "Migracion", "Mort_infantil", "Mortalidad", "Natalidad",
                         "PBI_percapita", "Alfabetizacion", "Telefonos",
                         "Agricultura", "Servicios", "Industria"
                       )
                     )
                     
        ),
        
        mainPanel(
          width = 10,
          plotOutput("Grafico", height = "600px")
        )
        
      )
      
    )
    
  ),
  
  tabPanel(
    "Relación entre variables y PBI",
    fluidPage(
      titlePanel("Relación entre variables y PBI"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "variable_pbi",
            "Seleccionar variable:",
            choices = c(
              "Natalidad", "Mortalidad", "Agricultura", "Industria",
              "Servicios", "Telefonos", "Migracion",
              "Mort_infantil")
          ),
          
          checkboxGroupInput(
            "region",
            "Seleccionar regiones:", 
            choices = unique(paises$Region), #Elimina valores repetidos
            selected = unique(paises$Region)
          )
        ),
        mainPanel(
          plotlyOutput("grafico"),
          wellPanel(
            verbatimTextOutput("correlacion") 
          )
          
        )
        
      )
      
    )
    
  ),
 #Distribución del PBI por región (en barras)
  
   tabPanel("Distribución del PBI",
           sidebarLayout(
             sidebarPanel(
               sliderInput("rango_pbi", 
                           "Rango del PBI pér cápita :",
                           min = floor(min(paises$PBI_percapita, na.rm = T)),
                           max = ceiling(max(paises$PBI_percapita, na.rm = T)),
                           value = c(
                             floor(min(paises$PBI_percapita, na.rm = T)),
                                     ceiling(max(paises$PBI_percapita, na.rm = T))
                                     ), #Redondea los valores al número entero más cercano.
                           
                           step = 1000) #Muestra los valores de la barra cada 100
               ),
     mainPanel(
       plotOutput("barras")
     )                                       
                                           
             )
           ))
           
  
server <- function(input, output) {
  
#Tabla básica de los datos.

  
  output$tabla <- renderDataTable(
    
    datatable({
      
      datos <- paises
      
      if(input$reg != "Todas") {
        datos <- datos[datos$Region == input$reg,]
      }
      
      if(input$pbi != "Todos") {
        datos <- datos[datos$Categoria_PBI == input$pbi,] #Permite elegir pbi y región
      }
      
      datos
      
    },
    
    options = list(scrollX = T) #Añade barra deslizadora a la tabla
    
    )
    
  )
  
#Mapa de calor
  
  output$Grafico <- renderPlot({
    
    req(input$variable_calor)
    
    datos <- paises_prom
    
    datos <- datos |>
      select(Region,all_of(input$variable_calor))
    
    datos <- datos |>
      mutate(across(-Region, scale)) #Estandarizo las variables con Scale
    
    datos <- datos |>
      pivot_longer(
        cols = -Region,
        names_to = "Variable",
        values_to = "Valor"
      )
    
    ggplot(datos, aes(x = Variable, y = Region, fill = Valor)) +
      geom_tile() +
      geom_text(aes(label = round(Valor, 2))) + #Muestra el número sobre el "cuadro"
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        name = "Posición respecto\nal promedio",
        breaks = c(-1.75, -1, 0, 1, 1.75),
        labels = c("Muy inferior", "Inferior", "Promedio", "Superior", "Muy superior")
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold")
      )
  })
  
#PBI pér capita
  
  output$correlacion <- renderText({
    
    datos_filtrados <- paises |>
      filter(Region %in% input$region)
    
    correlacion <- cor(
      datos_filtrados[[input$variable_pbi]],
      datos_filtrados$PBI_percapita,
      use = "complete.obs" #Elimina NAs
    )
    paste(
      "Índice de Correlación Lineal:",
      round(correlacion, 3)
    )
  })
  
  output$grafico <- renderPlotly({
    
    datos_filtrados <- paises |>
      filter(Region %in% input$region)
    
    g <- ggplot(
      datos_filtrados,
      aes(
        x = .data[[input$variable_pbi]],
        y = PBI_percapita,
        colour = Region,
        text = paste(
          "País:", Pais
        )
      )
    ) +
      geom_point(alpha = 0.6, size = 2.5) +
      scale_colour_manual(values = c(
        "red","blue", "green", "orange", "purple",
        "brown", "black", "cyan", "magenta",
        "yellow", "darkgreen"
      )) +
      labs(
        x = input$variable_pbi,
        y = "PBI per cápita",
        title = paste("PBI vs", input$variable_pbi)
      ) +
      theme_minimal()
    
    ggplotly(g)
  })
  
#Distribución del PBI per cápita
  output$barras <- renderPlot({
    datos <- paises |> 
      filter(PBI_percapita >= input$rango_pbi[1], #Valor mínimo
                    PBI_percapita <= input$rango_pbi [2]) #Valor máximo
    datos |> 
      count(Region) |> 
      ggplot(aes(x = Region, y = n, fill = Region)) + 
      geom_col() + coord_flip()
      
  })
}

shinyApp(ui, server)



