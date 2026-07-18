
#Cargo librerías
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(dplyr)
library(plotly)


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
           

#Busco distribuir "equitativamente" las categorías

quantile(paises$PBI_percapita, probs = c(0.333, 0.667), na.rm = T)

#Determino las categorías
paises <- paises|>
  mutate(Categoria_PBI = case_when(PBI_percapita < 2885 ~ "Bajo",
                    PBI_percapita >= 2885 & PBI_percapita<= 10230 ~ "Medio",
                    PBI_percapita > 10230 ~ "Alto"))

#Ordeno las categorías para mostrar en el gráfico
paises <- paises |> mutate(Categoria_PBI = factor(
  Categoria_PBI, levels = c("Bajo", "Medio", "Alto")
)) 



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
 

#Alfabetización por región


paises |> group_by(Region) |> 
  summarise(media_alfabetizacion = mean(Alfabetizacion, na.rm = T),
            desvio_alfabetizacion = sd(Alfabetizacion, na.rm = T)) 


paises |> ggplot(aes(x = Alfabetizacion, y =  Pais, colour  = Region
)) + 
  geom_point(alpha = 0.7, size = 2) + labs(
    title = "Alfabetización por Región") + facet_wrap(~Region)+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )




#Shiny

library(shiny)

ui <- fluidPage(
  titlePanel("Hola"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Cantidad de datos", min = 10, max = 50, value = 20)
    ),
    mainPanel(
      plotOutput("histograma"))
  )
) 

server <- function(input, output) {
  output$histograma <- renderPlot({
    hist(rnorm(input$n))
  })
}

shinyApp(ui, server) 


library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Relación entre variables y PBI"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "variable",
        "Seleccione una variable:",
        choices = c(
          "Natalidad",
          "Mortalidad",
          "Agricultura",
          "Industria",
          "Servicios",
          "Alfabetizacion",
          "Telefonos",
          "Migracion",
          "Mort_infantil"
          
        )
      ), checkboxGroupInput(
        "region",
        "Seleccione las regiones:",
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
                 "País:", Pais
               ))) +
      geom_point(alpha = 0.6, size = 3) + 
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
