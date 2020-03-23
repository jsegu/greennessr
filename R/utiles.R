
# Devuelve mismo EPSG en las dos capas en funcion cogiendo el de la x
epsg_igual <- function(x, y){
  if (is.na(raster::crs(y)) | is.na(raster::crs(x))){
    stop("Objeto no valido: Solo son validos para el calculo los objetos espaciales ej: SpatialPolygons o sf. En
         caso de que este introduciendo uno de los objetos espaciales mencionados,compruebe si la capa esta proyectada.")
  }
  else if (as.character(raster::crs(x)) != as.character(raster::crs(y))){
    if (class(x)[1] == "sf" & class(y)[1] == "sf"){
      y <- sf::st_transform(y, raster::crs(x))
    }
    else if (class(x)[1] != "sf" & class(y)[1] != "sf"){
      y <- sp::spTransform(y, raster::crs(x))
    }
    else{
      stop("Fallo de clases: ponga los dos objetos con la misma clase espacial, ya sea 'sp' o 'raster' y sino 'sf'")
    }
  }
  return(y)
}

# Comprobar el año del mapa urban atlas
ano_urban <- function(x, categorias){
  if (length((grep('CODE2006', names(x)))) == 1){
    x <- x[x$CODE2006 %in% categorias, c(5)]
    x
  } else if (length((grep('CODE2012', names(x)))) == 1){
    x <- x[x$CODE2012 %in% categorias, c(5)]
    x
  } else{
    stop('Fichero "urban" desconocido. Busque en la ayuda de la funcion el link para descargarlo.')
  }
}

# comprobar que el id_sc existe y cambiar por seccion
id_existe <- function(x, nombre){
  if (length(grep(nombre, colnames(x))) != 0){
    colnames(x)[colnames(x)==nombre] <- "seccion"
    x
  } else {
    stop(sprintf('id_sc no encontrado, asegurese que la columna %s es su identificador de seccion censal', nombre))
  }
}

transformar <- function(x, a_sp = FALSE){
  if (is.na(sf::st_crs(x))){
    stop("Objeto no valido: Solo son validos para el calculo los objetos espaciales ej: SpatialPolygons o sf. En
         caso de que este introduciendo uno de los objetos espaciales mencionados,compruebe si la capa esta proyectada.")
  }
  if (class(x)[1] != "sf"){
    x <- sf::st_as_sf(x)
  }
  if (a_sp == TRUE){
    x <- sp::SpatialPolygonsDataFrame(
      sf::as_Spatial(x),
      data = as.data.frame(x)[, setdiff(colnames(x), attr(x, "sf_column"))]
      )
  }
  return(x)
}

constancia <- function(x){
  if (is.na(sf::st_crs(x))){
    stop("Objeto no valido: Solo son validos para el calculo los objetos espaciales ej: SpatialPolygons o sf. En
         caso de que este introduciendo uno de los objetos espaciales mencionados,compruebe si la capa esta proyectada.")
  }
  if (class(x)[1] != "sf"){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' @title Extracción índice greenness
#'
#' @description A partir de las variables calculadas con las funciones de extracción de la superficie
#' verde urbana \link[greennessr]{area_urban} y \link[greennessr]{acceso_urban} y de las funciones
#' de extracción del NDVI \link[greennessr]{ndvi} y \link[greennessr]{acceso_ndvi}. Se construye
#' un índice combinado llamado “greenness” calculado con una media ponderada entre el área verde
#' urbana y el NDVI dando mayor peso al área verde urbana “urban atlas”.
#'
#' @param x DataFrame con las columnas de entrada.
#'
#' @param urban Vector: Con la columna que contiene el parámetro 'urban'.
#'
#' @param ndvi Vector: Con la columna que contiene el parámetro 'ndvi'.
#'
#' @details Función para crear un índice ponderado que resuma la exposición a espacios verdes
#' urbanos. Para ello es necesario introducir las variables de 'urban atlas' y NDVI como datos de
#' entrada, teniendo en cuenta el nivel de desagregación con el que han sido calculados: acceso (
#' donde se tiene en cuenta la ponderación de la capa de catastro) o area (sin esa ponderación).
#'
#' El cálculo consiste en una índice ponderado que da mayor peso a la capa 'urban atlas',
#' concretamente un 80% del peso, mientras que el ndvi tiene un 20% de peso. Para ello, en primer
#' lugar se entanderiza el NDVI para que su rango vaya de 0-100. Luego se realiza una media
#' ponderada con los valores comentados anteriormente.
#'
#' @return Devuelve el mismo DataFrame de entrada con dos columnas añadidas:
#' \itemize{
#' \item greenness: Columna con el valor absoluto del índice greenness.
#' \item green_cat: Columna con el valor categorizado por cuartiles.
#' }
#'
#' @examples
#' \dontrun{
#' library(greennessr)
#'
#' # carga de datos de ejemplo
#' data("catastro_castellon")
#' data("secciones_castellon")
#' data("urban_atlas_castellon")
#'
#' # comprobar que son obejetos espaciales
#' class(catastro_castellon)
#' class(secciones_castellon)
#' class(urban_atlas_castellon)
#'
#  # ver nombre columna id secciones
#' head(secciones_castellon) # 'secccion'
#'
#' # calculo de la variable 'urban atlas'
#' castellon <- acceso_urban(
#'   sc         = secciones_castellon,
#'   id_sc      = 'seccion',
#'   urban      = urban_atlas_castellon,
#'   catastro   = catastro_castellon,
#'   radio      = 300,
#'   categorias = c(14100, 14200)
#' )
#'
#' # calculo de la variable NDVI
#' castellon <- acceso_ndvi(
#'   dir_img    = '~01_data/landsat/castellon',
#'   sc         = secciones_castellon,
#'   id_sc      = "seccion",
#'   catastro   = catastro_castellon,
#'   radio      = 300,
#'   calculo    = mean,
#'   dev_raster = TRUE,
#'   landsat8   = FALSE
#' )
#'
#' # calculo del índice ponderado greenness
#' castellon <- greenness(
#'    x     = castellon,
#'    urban = castellon$accs_urban,
#'    ndvi  = castellon$accs_ndvi
#' )
#'
#' # estadísticas del resultado
#' summary(castellon$greenness)
#' }
#'
#' @encoding UTF-8
#'
#' @export
greenness <- function(x, urban, ndvi){
  res <- x
  x <- as.data.frame(x)
  a <- function(x){
    ifelse(x<stats::quantile(x,0.25), '1',
           ifelse(x<stats::quantile(x,0.5),'2',
                  ifelse(x<(stats::quantile(x,0.75)),'3','4'
                  )))}
  #indice ponderado
  res$greenness <- as.numeric(urban)*0.8+(as.numeric(ndvi)+1)*50*0.2

  ## crear categorias
  res$green_cat <- a(res$greenness)

  return(res)
}
