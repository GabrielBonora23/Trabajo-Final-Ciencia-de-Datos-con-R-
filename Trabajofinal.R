
#Cargo librerías
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(dplyr)
library(viridis)
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

#Gráfico interactivo de alfabetización según PBI per cápita, coloreado por región


grafico <- paises |> ggplot(aes(x = PBI_percapita, y = Alfabetizacion, colour = Region)) +
  geom_point(alpha = 0.4, size = 3
             ) + labs(
    title = "Alfabetización según PBI per cápita") + 
  scale_colour_manual(values = c(
    "red","blue", "green", "orange", "purple", "brown", "black", "cyan",
    "magenta", "yellow", "darkgreen"))
   
ggplotly(grafico)             

#Relación entre natalidad y PBI per cápita

paises |> ggplot(aes(x = Natalidad, y =PBI_percapita,
                     colour = "red")) + 
  geom_point(alpha = 0.7, size = 3) + labs(
    title = "Relación entre Natalidad y PBI per cápita")

#Distribución de la mortalidad infantil según categoría del PBI per cápita

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

#Gráfico de mortalidad infantil según PBI per cápita
paises |> filter(!is.na(Categoria_PBI)) |> 
  ggplot(aes(x = Mort_infantil)) +
  geom_histogram(bins = 20, colour = "white", fill = "purple") +
  facet_wrap(~Categoria_PBI) + labs(
    title = "Mortalidad infantil según PBI per cápita",
    x = "Mortalidad infantil")




