#' @title Calculo del NDVI para cada seccion censal
#'
#' @description  Extrae el valor del índice de vegetación normalizado (NDVI) para cada sección censal.
#' Para ello utiliza los datos de una imagen del satelite Landsat, a la que se le realiza el cálculo del
#' NDVI y una capa de polígonos correspondiente a las secciones censales. Por defecto el método de
#' extracción es la media de píxeles de cada polígono, este método se puede modificar con el parámetro
#' \code{calculo}.
#'
#' @param dir_img Carácter: Directorio donde se encuentra la imagen Landsat, tiene que estar descomprimida.
#' @param sc Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales de
#' las secciones censales.
#' @param calculo Función para resumir los valores NDVI en la capa de polígonos, por defecto \code{mean}.
#' Se puede modificar (ej. median, min o max).
#' @param dev_raster Lógico: Si es TRUE en el resultado se incluye la capa ráster con los valores del NDVI.
#' @param landsat8 Lógico: Por defecto se calculan de manera automática las imagenes de landsat-5
#' y landsat-7. En caso de querer calcular una imagen landsat-8, cambiar el parámetro a \code{TRUE}.
#'
#' @usage calcular_ndvi(dir_img, sc, calculo = mean, dev_raster = FALSE, landsat8 = FALSE)
#'
#' @details El índice de vegetación normalizado (NDVI) se utiliza para estimar la cantidad, calidad y el
#' desarrollo de la vegetación. Este estimador se calcula mediante los valores de intensidad de la radiación
#' que las plantas emiten o reflejan en ciertos rangos del espectro electromagnético. Concretamente en las
#' longitudes de onda del rojo y del infrarrojo cercano (nir). El cálculo es el siguiente:
#'
#' NDVI = (nir - rojo) / (nir + rojo)
#'
#' Para que la función pueda calcularlo es necesario una imagen satélite multiespectral, en este caso se
#' utilizan las de los satélites Landsat-5, Landsat-7 y Landsat-8 (en caso de utilizar Landsat-8 cambiar
#' el parámetro \code{landsat8 = TRUE}). Para descargar las imágenes es recomendable usar la página web
#' \href{https://earthexplorer.usgs.gov/}{earth explorer de la USGS.} Es muy importante que al realizar
#' la descarga se tenga en cuenta lo siguiente:
#'
#' \itemize{
#' \item Que las imágenes seleccionadas sean de la colección 'Landsat Collection 1 Level-1 (L1TP)'
#' ya que están radiometricamente calibradas y ortorectificadas.
#'
#' \item Que la zona de estudio no este cubierta por nubes, en la página web de
#' \href{https://earthexplorer.usgs.gov/}{earth explorer} podrá visualizar la imagen antes de descargarla.
#'
#' \item Que la imagen cubra toda la superficie de la zona de estudio. En el caso de tener que descargar
#' más de una imagen para cubrir su zona de estudio tendrá que juntar las imágenes previamente antes
#' de introducirlas en la función. Esto lo puede realizar usando la función \code{\link[raster]{merge}}.
#'}
#' En este \href{https://earthexplorer.usgs.gov/documents/helptutorial.pdf}{link} encontrará más detalles a cerca
#' de la descarga y los distintos niveles de procesamiento de las imágenes.
#'
#' No hace falta tener en cuenta los sistemas de referencia de los datos geográficos, la propia función los
#' cambiará si fuese necesario.
#'
#' @return El resultado que devuelve es el mismo objeto espacial \code{(SpatialPolygonsDataFrame / sf)}
#' introducido en el parámetro \code{sc} de la función, con una columna nueva llamada ndvi que tiene el
#' resultado para cada polígono.
#'
#' Si también se quiere obtener el mapa ráster con el resultado del ndvi, se tiene que cambiar el
#' parámetro \code{dev_raster = TRUE}. Entonces el resultado devuelto es una lista con dos elementos:
#' El primero es el objeto espacial \code{(SpatialPolygonsDataFrame / sf)} comentado anteriormente y
#' el segundo una \code{RasterLayer} con el resultado del ndvi en forma de mapa de píxeles.
#'
#' @examples
#' \dontrun{
#' library(greennessr)
#' sc_valencia <- calcular_ndvi(
#'   dir_img    = "~01_data/LE07_L1TP_199033_20000621_20170211_01_T1",
#'   sc         = sc_valencia,
#'   calculo    = median,
#'   dev_raster = FALSE,
#'   landsat8   = FALSE
#' )
#' sc_valencia}
#'
#' @encoding UTF-8
#'
#' @export
calcular_ndvi <- function(dir_img, sc, calculo = mean, dev_raster = FALSE, landsat8 = FALSE){

  # Para cambiar patern en funcion del satelite
  if (isFALSE(landsat8)){
    satelite <- 'B3|B4'
  }
  else{
    satelite <- 'B4|B5'
  }

  # Lectura y apilado de las bandas rojo y nir
  dir <- list.files(path = dir_img, pattern = satelite, full.names = TRUE)
  bandas <- raster::stack(dir)
  names(bandas)<- c("red", "nir")

  # implementar la sf
  a_sf <- constancia(sc)
  if (class(sc)[1] == "sf"){
    sc <- sp::SpatialPolygonsDataFrame(
      sf::as_Spatial(sc),
      data = as.data.frame(sc)[, setdiff(colnames(sc), attr(sc, "sf_column"))]
    )
  }

  # Comprobar EPSG sc
  sc_aux <- epsg_igual(bandas, sc)

  # Cortar bandas con sc
  bandas <- raster::crop(bandas, sc_aux)

  # Calculo de el NDVI
  ndvi <- (bandas$nir-bandas$red)/(bandas$nir+bandas$red) # NDVI
  ndvi[ndvi>1] <- 1; ndvi[ndvi< (-1)] <- -1 #Reescalado para evitar outliers

  # Extraer datos a nivel de sección censal
  sc$ndvi <- raster::extract(ndvi, sc_aux, fun=calculo, na.rm=TRUE)[,1]

  # pasar a sf si conviene
  if (a_sf == FALSE){
    sc <- sf::st_as_sf(sc)
  }
  res <- sc

  # Devolver raster?
  if (isTRUE(dev_raster)){
    res <- list(sc = res, ndvi = ndvi)
    message("Resultado compuesto por una lista de dos elementos:

            [1] Objeto espacial con la columna 'ndvi'
            [2] RasterLayer resultado calculo ndvi
            ")
  }
  return(res)
}

#' @title Calculo del NDVI para cada seccion censal ponderado
#'
#' @encoding UTF-8
#' @export
ndvi_ponderado <- function(dir_img, sc, id_sc, catastro, radio =300, calculo = mean, dev_raster = FALSE, landsat8 = FALSE){
  # comprobar que el id_sc existe y cambiar por seccion


  # comprobar que la capa de catastro sea la adecuada
  if (length(grep('numberOfDwellings', colnames(catastro))) == 0 |
      length(grep('gml_id', colnames(catastro))) == 0){
    stop('Objeto catastro no valido: Asegurese que ha seleccionado la capa "BUILDING"
         y introduzcala a la funcion sin eliminar ni modificar ninguna variable')
  }
  a_sp <- constancia(sc)
  sc <- transformar(sc)
  catastro <- transformar(catastro)
  sc <- epsg_igual(catastro, sc)
  sc <- id_existe(sc, id_sc)
  crs_sc <- sf::st_crs(sc)
  # filtros a los datos
  catastro <- catastro[catastro$currentUse %in% '1_residential', c('gml_id','numberOfDwellings')]
  catastro$gml_id <- as.character(catastro$gml_id)

  # Para cambiar patern en funcion del satelite
  if (isFALSE(landsat8)){
    satelite <- 'B3|B4'
  } else{
    satelite <- 'B4|B5'
  }

  # Lectura y apilado de las bandas rojo y nir
  dir <- list.files(path = dir_img, pattern = satelite, full.names = TRUE)
  bandas <- raster::stack(dir)
  names(bandas)<- c("red", "nir")
  # Cortar bandas con sc
  bandas <- raster::crop(bandas, epsg_igual(bandas, sc))
  # Calculo de el NDVI
  ndvi <- (bandas$nir-bandas$red)/(bandas$nir+bandas$red) # NDVI
  ndvi[ndvi>1] <- 1; ndvi[ndvi< (-1)] <- -1 #Reescalado para evitar outliers
  rm(bandas)

  # extraer centroides
  catastro <- sf::st_centroid(catastro)
  # buffer a lo urbano
  catastro <- sf::st_buffer(catastro, radio)
  # Extraer datos a nivel de cada parcela edificio
  catastro$ndvi <- raster::extract(ndvi,
                                   epsg_igual(ndvi,
                                              transformar(catastro, a_sp = TRUE)),
                                   fun=calculo,
                                   na.rm=TRUE)[,1]

  # intersectar edificios con las secciones censales
  aux <- transformar(sc)
  aux <- sf::st_intersection(epsg_igual(catastro, aux), sf::st_centroid(catastro))

  # calculo de la ponderación
  ponderado <- sapply(
    unique(aux$seccion),
    function(x) {
      stats::weighted.mean(
        aux$ndvi[aux$seccion == x],
        aux$numberOfDwellings[aux$seccion == x]
      )
    }
  )
  sc$ndvi_pond<- ponderado[match(sc$seccion, names(ponderado))]

  # transformaciones para devolver mismo objeto introducido por usuario
  sc <- sf::st_transform(sc, crs_sc)
  colnames(sc)[colnames(sc)=='seccion'] <- id_sc
  sc <- transformar(sc, a_sp)

  # Devolver raster?
  if (isTRUE(dev_raster)){
    sc <- list(sc = sc, ndvi = ndvi)
    message("Resultado compuesto por una lista de dos elementos:

            [1] Objeto espacial con la columna 'ndvi'
            [2] RasterLayer resultado calculo ndvi
            ")
  }
  return(sc)
}
