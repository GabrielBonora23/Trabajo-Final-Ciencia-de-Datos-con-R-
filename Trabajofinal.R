
#Cargo librerías
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)


#Cargo los datos

paises <- read.csv("countries of the world.csv")

#Renombro las variables del dataset original

paises <- paises |> 
  rename(Pais = Country,
         Poblacion = Population,
         Area = Area..sq..mi..,
         Densidad_pob = Pop..Density..per.sq..mi..,
         Proporcion_costa = Coastline..coast.area.ratio.,
         Migracion = Net.migration,
         Mort_infantil = Infant.mortality..per.1000.births.,
         PBI_percapita = GDP....per.capita.,
         Alfabetizacion = Literacy....,
         Telefonos = Phones..per.1000.,
         Tierra_fertil = Arable....,
         Cultivos = Crops....,
         Natalidad = Birthrate,
         Mortalidad = Deathrate,
         Agricultura = Agriculture,
         Industria = Industry,
         Servicios = Service)
         

#Elimino las dos columnas que no usaremos.
paises <- paises |> select(-Other...., -Climate)

#Cambio de coma a punto las columnas con valores numéricos

paises[] <- lapply(paises, function(x) {
  if (is.character(x)) {
    y <- gsub(",", ".", trimws(x))
    suppressWarnings({ #Oculta advertencias al encontrar texto
      z <- as.numeric(y)
    })
    if (all(is.na(z) == is.na(y))) z else x
  } else {
    x
  } #Si la conversión es correcta, sustituye la columna, de lo contrario la deja igual
})

columnas <- c("Migracion", "Mort_infantil", "Alfabetizacion", "Telefonos", 
              "Tierra_fertil", "Cultivos", "Natalidad", "Mortalidad", "Agricultura",
              "Industria", "Servicios")

paises[columnas] <- lapply(paises[columnas], function(x) {
  x <- trimws(x) #Elimina espacios, ej: " 0,36 " a "0,36"
  x[x==""] <- NA #Sustituye columnas vacías por NA
  x <- gsub(",", ".", x) #Sustituye comas por puntos
  as.numeric(x) #Convierte los valores de caracter a numéricos
})



#Cantidad de países por región
paises |> count(Region)


#Pbi per cápita promedio según región.

paises |> group_by(Region) |> 
  summarise(PBI = mean(PBI_percapita, na.rm = T)) |> 
  ggplot(aes(x = reorder(Region, PBI), y = PBI, fill = Region)) +
  geom_col() + 
  coord_flip() + labs(title = "PBI per cápita según región", 
                      x = "PBI per cápita", y = "Región") + 
  theme(legend.position = "none")


#Región de los 10 países con mayor PBI per cápita
paises |> slice_max(PBI_percapita, n = 10) |> 
  select(Region, PBI_percapita)

#Los 10 con menos PBI per cápita
paises |> slice_min(PBI_percapita, n =10) |> 
  select(Region, PBI_percapita)
           

#Desvío y media del PBI per cápita por región
paises |> group_by(Region) |>
  summarise(prom_pbi = mean(PBI_percapita, na.rm = T),
            desv_pbi = sd(PBI_percapita, na.rm = T))
                            


#Natalidad por región
paises |> group_by(Region) |> 
  summarise(media_natalidad = mean(Natalidad, na.rm = T),
            desvio_natalidad = sd(Natalidad, na.rm = T)) 
 
  
paises |> ggplot(aes(x = Natalidad, y =  Pais, colour  = Region
                                            )) + 
  geom_point(alpha = 0.7, size = 2) + labs(
    title = "Natalidad por Región") + facet_wrap(~Region)+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )


#¿Mayor dispersión en natalidad implica mayor dispersión en PBI pc?

#Mortalidad por región
paises |> group_by(Region) |> 
  summarise(media_mortalidad = mean(Mortalidad, na.rm = T),
            desvio_mortalidad = sd(Mortalidad, na.rm = T)) 


paises |> ggplot(aes(x = Mortalidad, y =  Pais, colour  = Region
)) + 
  geom_point(alpha = 0.7, size = 2) + labs(
    title = "Mortalidad por Región") + facet_wrap(~Region)+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

#Relación entre Mortalidad y Mortalidad infantil

paises |> ggplot(aes(x = Mortalidad, y = Mort_infantil, colour = Region)) +
  geom_point(size=2, alpha = 0.7) + facet_wrap(~Region)
 
#Esto muestra que, a pesar de que la mortalidad es mayor en los países del oeste de Europa, esto se debe a que la población está mas envejecida, y no necesariamene implica "falta de recursos"


#Alfabetización por región


paises |> group_by(Region) |> 
  summarise(media_alfabetizacion = mean(Alfabetizacion, na.rm = T),
            desvio_alfabetizacion = sd(Alfabetizacion, na.rm = T)) 


#Busco distribuir "equitativamente" las categorías de PBI per cápita

quantile(paises$PBI_percapita, probs = c(0.333, 0.667), na.rm = T)

#Determino las categorías
paises <- paises|>
  mutate(Categoria_PBI = case_when(PBI_percapita < 2885 ~ "Bajo",
                                   PBI_percapita >= 2885 & PBI_percapita<= 10230 ~ "Medio",
                                   PBI_percapita > 10230 ~ "Alto"))


paises |> ggplot(aes(x = Alfabetizacion, y =  Pais, colour  = Region
)) + 
  geom_point(alpha = 0.7, size = 2) + labs(
    title = "Alfabetización por Región") + facet_wrap(~Region)+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),  
    legend.position = "none"
  ) #Elimina los textos del gráfico




#Shiny

#Tabla "básica" del dataframe

ui <- fluidPage(
  titlePanel("Países del mundo : Tabla"),
  
  fluidRow(
    column(3, 
           selectInput("reg",
                       "Region:",
                       c("Todas",
                         unique(as.character(paises$Region))))
    ),
    column(3,
           selectInput("pbi",
                       "Nivel de PBI:",
                       c("Todos", unique(as.character(
                         paises$Categoria_PBI[!is.na(paises$Categoria_PBI)]))))
    )),
  DT::dataTableOutput("tabla"))


server <- function(input,output) {
  output$tabla <- DT::renderDataTable(DT::datatable({
    datos <- paises
    if(input$reg !="Todas") {
      datos <- datos[datos$Region == input$reg,]
    }
    if(input$pbi != "Todos") {
      datos <- datos[datos$Categoria_PBI == input$pbi,]
    }
    datos
    
  }, options = list(scrollX = T) #Añade una barra deslizadora para las columnas
  ))
}

shinyApp(ui, server)


#Relación de las distintas variables con el PBI per cápita.

ui <- fluidPage(
  
  titlePanel("Relación entre variables y PBI"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "variable",
        "Seleccionar variable:",
        choices = c("Natalidad", "Mortalidad", "Agricultura", "Industria",
                    "Servicios", "Telefonos", "Migracion",
                    "Mort_infantil"
        )
      ), checkboxGroupInput(
        "region",
        "Seleccionar regiones:",
        choices = unique(paises$Region),
        selected = unique(paises$Region)
      )
    ),
    
    mainPanel(
      plotlyOutput("grafico")
    )
  )
)

server <- function(input, output) {
  
  output$grafico <- renderPlotly({
    datos_filtrados <- paises |>
      dplyr::filter(Region %in% input$region)
    
  g <- ggplot(datos_filtrados,
           aes(x = .data[[input$variable]], y = PBI_percapita, colour = Region,
               text = paste(
                 "País:", Pais #Añade el nombre del país en el plotly
               ))) +
      geom_point(alpha = 0.6, size = 2.5) + 
      scale_colour_manual(values = c(
        "red","blue", "green", "orange", "purple", "brown", "black", "cyan",
        "magenta", "yellow", "darkgreen")) +
      labs(
        x = input$variable,
        y = "PBI per cápita",
        title = paste("PBI vs", input$variable)
      ) +
      theme_minimal()
 ggplotly(g)   
  })
  
}

shinyApp(ui, server)

s


#Promedio de cada variable por región.

paises_prom <- paises |> group_by(Region) |> 
  summarise(across(c(Poblacion, Area, Densidad_pob,Proporcion_costa, Migracion,
                     Mort_infantil, Mortalidad, Natalidad,PBI_percapita, Alfabetizacion,
                     Telefonos, Agricultura, Servicios, Industria), mean,
                   na.rm = T))
          
#Shiny de los promedios por región 

ui <- fluidPage(
  titlePanel("Promedios por región : Mapa de calor"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(   #Permite seleccionar varias variables
        "variable", "Seleccionar variable:",
        choices = c("Poblacion", "Area", "Densidad_pob", "Proporcion_costa",
                    "Migracion", "Mort_infantil", "Mortalidad", "Natalidad",
                    "PBI_percapita", "Alfabetizacion", "Telefonos",
                    "Agricultura", "Servicios", "Industria")
      )
      
    ), mainPanel(plotOutput("Grafico")
      
    )))
  
server <- function(input, output) {
  output$Grafico <- renderPlot({
    datos <- paises_prom
    datos <- datos |> 
      dplyr:: select(Region, dplyr::all_of(input$variable)) #Selecciona región y las variables elegidas
    
    datos <- datos |> mutate(dplyr::across(-Region,
                                           scale)) #Elimina región y estandariza los valores
    
    datos <- datos |> tidyr::pivot_longer(
      cols = -Region,
      names_to = "Variable",
      values_to = "Valor"
    ) #"Alarga" la tabla
    
    ggplot(datos, aes(x = Variable, y = Region, fill = Valor)) +
      geom_tile() +  #Mapa de calor
      geom_text(aes(label = round(Valor,2))) +  #Añade el número redondeando el valor
      scale_fill_gradient(low = "white", high = "red")
  })
}

shinyApp(ui, server)



