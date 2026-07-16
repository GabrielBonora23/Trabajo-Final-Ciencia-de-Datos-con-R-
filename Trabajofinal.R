
#Cargo librerías
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(dplyr)


#Cargo los datos

paises <- read.csv("countries of the world.csv")

#Renombro las variables

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
 

#Pbi per cápita según región      