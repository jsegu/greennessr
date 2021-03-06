% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcular_urban_green.R
\encoding{UTF-8}
\name{area_urban}
\alias{area_urban}
\title{Calculo de la superficie de areas verdes urbanas}
\usage{
area_urban(
  sc,
  id_sc,
  urban,
  categorias = c(14100),
  puntos = FALSE,
  radio = 300
)
}
\arguments{
\item{sc}{Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales
de las secciones censales.}

\item{id_sc}{Carácter: nombre de la columna que contiene el código identificador único de cada
sección censal.}

\item{urban}{Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales
del mapa urban atlas.}

\item{categorias}{Vector: Correspondiente a las categorías de espacios verdes de la base de datos
del urban atlas.}

\item{puntos}{Boleano \code{TRUE o FALSE:} Por defecto es \code{FALSE}, poner a \code{TRUE} si la base
de datos \code{sc} es de puntos, dónde la propia función creará un buffer para convertir el dato
puntual a polígono.}

\item{radio}{Numérico: Se utiliza si el parámetro \code{puntos = TRUE}. Distancia en metros del radio
del buffer, por defecto 300m.}
}
\value{
El resultado que devuelve es el mismo objeto espacial \code{(SpatialPolygonsDataFrame / sf)}
introducido en el parámetro \code{sc} de la función, con una columna nueva llamada area_urban que
tiene el resultado para cada polígono, expresado en porcentaje.
}
\description{
Extrae el porcentaje de áreas verdes urbanas correspondiente a cada sección censal.
Para ello utiliza los datos del Urban Atlas i el mapa de secciones censales. Por defecto la función
tiene definidas las categorías de espacios verdes correspondientes a 'Green urban areas' y
'Sports and leisure facilities', pero estas se pueden modificar con el parámetro \code{categorias}.
}
\details{
La función empieza calculando el área de cada sección censal (sc). Luego intersecta las sc
con las áreas verdes definidas por el mapa urban atlas, calcula el área de las intersecciones y
finalmente calcula el porcentaje de área verde urbana ubicado en cada sección censal.

Para el uso de esta función es imprescindible descargar los datos del Urban atlas. Este es un mapa
de cubiertas y usos del suelo de las principales ciudades europeas creado por el proyecto Copernicus,
consta de dos ediciones 2006 y 2012. Para el primer año solo están disponibles las principales
ciudades españolas, en la versión de 2012 aumenta considerablemente el número de ciudades.
\href{https://land.copernicus.eu/local/urban-atlas}{Clic para más información y descargas}.

No hace falta tener en cuenta los sistemas de referencia de los datos geográficos, la propia función
los cambiará si fuese necesario.
}
\examples{
library(greennessr)

# carga de datos de ejemplo
data("secciones_castellon")
data("urban_atlas_castellon")

# comprobar que son obejetos espaciales
class(secciones_castellon)
class(urban_atlas_castellon)

head(secciones_castellon) # 'secccion'

castellon_area_urban <- area_urban(
sc         = secciones_castellon,
id_sc      = 'seccion',
urban      = urban_atlas_castellon,
categorias = c(14100)
)

summary(castellon_area_urban$area_urban)


}
