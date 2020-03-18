#' @title Calculo de la superficie de areas verdes urbanas
#'
#' @description Extrae el porcentaje de áreas verdes urbanas correspondiente a cada sección censal.
#' Para ello utiliza los datos del Urban Atlas i el mapa de secciones censales. Por defecto la función
#' tiene definidas las categorías de espacios verdes correspondientes a 'Green urban areas' y
#' 'Sports and leisure facilities', pero estas se pueden modificar con el parámetro \code{categorias}.
#'
#' @param sc Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales
#' de las secciones censales.
#' @param id_sc Carácter: nombre de la columna que contiene el código identificador único de cada
#' sección censal.
#' @param urban Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales
#' del mapa urban atlas.
#' @param categorias \code{Vector:} Correspondiente a las categorías de espacios verdes de la base de datos
#' del urban atlas.
#'
#' @usage area_urban(sc, id_sc, urban, categorias = c(14100, 14200))
#'
#' @details La función empieza calculando el área de cada sección censal (sc). Luego intersecta las sc
#' con las áreas verdes definidas por el mapa urban atlas, calcula el área de las intersecciones y
#' finalmente calcula el porcentaje de área verde urbana ubicado en cada sección censal.
#'
#' Para el uso de esta función es imprescindible descargar los datos del Urban atlas. Este es un mapa
#' de cubiertas y usos del suelo de las principales ciudades europeas creado por el proyecto Copernicus,
#' consta de dos ediciones 2006 y 2012. Para el primer año solo están disponibles las principales
#' ciudades españolas, en la versión de 2012 aumenta considerablemente el número de ciudades.
#' \href{https://land.copernicus.eu/local/urban-atlas}{Clic para más información y descargas}.
#'
#' No hace falta tener en cuenta los sistemas de referencia de los datos geográficos, la propia función
#' los cambiará si fuese necesario.
#'
#' @return El resultado que devuelve es el mismo objeto espacial \code{(SpatialPolygonsDataFrame / sf)}
#' introducido en el parámetro \code{sc} de la función, con una columna nueva llamada area_urban que
#' tiene el resultado para cada polígono, expresado en porcentaje.
#'
#' @examples
#' library(greennessr)
#'
#' # carga de datos de ejemplo
#' data("secciones_castellon")
#' data("urban_atlas_castellon")
#'
#' # comprobar que son obejetos espaciales
#' class(secciones_castellon)
#' class(urban_atlas_castellon)
#'
#  # ver nombre columna id secciones
#' head(secciones_castellon) # 'secccion'
#'
#' castellon_area_urban <- area_urban(
#' sc         = secciones_castellon,
#' id_sc      = 'seccion',
#' urban      = urban_atlas_castellon,
#' categorias = c(14100, 14200)
#' )
#'
#' summary(castellon_area_urban$area_urban)
#'
#'
#' @encoding UTF-8
#'
#' @export
area_urban <- function(sc, id_sc, urban, categorias = c(14100,14200)){
  # Comprobar el año del mapa urban atlas y Subset por categorias: por defecto 'Green urban areas' y 'Sports and leisure facilities'
  urban <- ano_urban(urban, categorias)

  # comprobar y transformar dejando constancia de si era o no era sp
  a_sp <- constancia(sc)
  urban <- transformar(urban)
  sc <- transformar(sc)

  # comprobar que el id_sc existe y cambiar por seccion
  sc <- id_existe(sc, id_sc)

  # comprobar epsg guardando orginal para cambiar al final
  epsg_sc <- sf::st_crs(sc)
  sc <- epsg_igual(urban, sc)

  # calculo del area de cada sc y de su interseccion
  sc$area_sc <- sf::st_area(sc)
  aux <- sf::st_intersection(sc, urban)
  aux$urban <- sf::st_area(aux)
  aux <- as.data.frame(aux)

  # group by seccion sumando superficies
  tmp <- sapply(
    X   = by(
      data     = aux,
      INDICES  = aux$seccion,
      FUN      = function(x) sum(x$urban, na.rm = TRUE) / x$area_sc * 100
    ),
    FUN = unique
  )
  aux <- data.frame(
    seccion          = names(tmp),
    urban      = tmp,
    stringsAsFactors = FALSE
  )

  # juntar resultados y volver a cambiar nombre id_sc
  sc <- sc[, -which(names(sc) %in% c('area_sc'))]
  sc$urban <- aux$urban[match(sc$seccion, aux$seccion)]
  sc[is.na(sc$urban),'urban'] <- 0
  colnames(sc)[colnames(sc)=='seccion'] <- id_sc

  # transformar coordenadas y formato sp si es necesario
  sc <- sf::st_transform(sc, epsg_sc)
  sc <- transformar(sc, a_sp)

  return(sc)
}

#' @title Accesibilidad a areas verdes
#'
#' @description Extrae el índice de accesibilidad ponderado de áreas verdes urbanas correspondiente a cada
#' sección censal. Para ello utiliza los datos del Urban Atlas, el mapa de secciones censales y la base de
#' datos del catastro. Por defecto la función tiene definidas las categorías de espacios verdes
#' correspondientes a 'Green urban areas' y Sports and leisure facilities', pero estas se pueden modificar
#' con el parámetro \code{categorias}.
#'
#' @details Esta función es una variante más compleja de \link[greennessr]{area_urban}, que tiene en cuenta
#' la accesibilidad a espacios verdes urbanos de los habitantes de una sección censal en función de la
#' ubicación de su edificio. En primer lugar la se extrae el centroide de cada edificio del mapa catastral,
#' seguidamente se genera un buffer de 300 metros (modificable con el parámetro \code{radio}) que
#' intersecta con las áreas verdes urbanas y se calcula la superficie que ocupa está área respecto el
#' buffer. Finalmente se calcula la media de las áreas de los edificio de cada sección censal ponderando
#' por el número de viviendas que se albergan en cada edificio.
#'
#' Para el uso de la función se necesita descargar los edificios de la base de datos catastral (capa
#' BUILDING). Estos están disponible a través del servicio de
#' \href{http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.BU.atom.xml}{descargas ATOM} del
#' servicio INSPIRE de la página del catastro. Además también existe un plugin para QGIS, llamado
#' 'Spanish Insipire Catastral Downloader', que proporciona una interfaz más amable para la descarga
#' de los datos. Es recomendable cargar la capa con extensión '.gml' porque su tiempo de lectura de datos
#' es menor.
#'
#' También es imprescindible descargar los datos del Urban atlas. Este es un mapa
#' de cubiertas y usos del suelo de las principales ciudades europeas creado por el proyecto Copernicus,
#' consta de dos ediciones 2006 y 2012. Para el primer año solo están disponibles las principales
#' ciudades españolas, en la versión de 2012 aumenta considerablemente el número de ciudades.
#' \href{https://land.copernicus.eu/local/urban-atlas}{Clic para más información y descargas}.
#'
#' No hace falta tener en cuenta los sistemas de referencia de los datos geográficos, la propia función
#' los cambiará si fuese necesario.
#'
#' @param sc Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales
#' de las secciones censales.
#' @param id_sc Carácter: nombre de la columna que contiene el código identificador único de cada
#' sección censal.
#' @param urban Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales
#' del mapa urban atlas.
#' @param catastro Objeto espacial \code{(SpatialPolygonsDataFrame / sf):} con las geometrías poligonales
#' de la capa 'building' de la base de datos espacial del catastro.
#' @param radio \code{Numérico:} Valor del radio del buffer para la intersección entre los centroides
#' de los polígonos catastrales y capa urban atlas, expresado en metros. Por defecto toma valor de 300m.
#' @param categorias \code{Vector:} Correspondiente a las categorías de espacios verdes de la base de datos
#' del urban atlas.
#'
#' @return El resultado que devuelve es el mismo objeto espacial \code{(SpatialPolygonsDataFrame / sf)}
#' introducido en el parámetro \code{sc} de la función, con una columna nueva llamada accs_verde que
#' tiene el resultado para cada polígono. Este valor está comprendido entre 0 y 1. Un valor 1 quiere decir
#' que la media de los buffers alrededor de los centroides de los edificios de cada sección censal están
#' integramente cubiertos por espacios verdes y un valor 0 que no lo están. Se tiene que tener en cuenta
#' que en los edificios que comprenden un número alto de viviendas obtienen un mayor peso en la media.
#'
#' @examples
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
#' accs_verde <- acceso_urban(
#'   sc         = secciones_castellon,
#'   id_sc      = 'seccion',
#'   urban      = urban_atlas_castellon,
#'   catastro   = catastro_castellon,
#'   radio      = 300,
#'   categorias = c(14100, 14200)
#' )
#'
#' summary(accs_verde$accs_verde)
#'
#' @encoding UTF-8
#'
#' @export
acceso_urban <- function(sc, id_sc, urban, catastro, radio = 300, categorias = c(14100,14200)){
  crs_sc <- sf::st_crs(sc)

  # siempre a sf
  catastro <- sf::st_as_sf(catastro)

  # comprobar que la capa de catastro sea la adecuada
  if (length(grep('numberOfDwellings', colnames(catastro))) == 0 |
      length(grep('gml_id', colnames(catastro))) == 0){
    stop('Objeto catastro no valido: Asegurese que ha seleccionado la capa "BUILDING"
         y introduzcala a la funcion sin eliminar ni modificar ninguna variable')
  }

  # filtros a los datos
  urban <- ano_urban(urban, categorias)
  catastro <- catastro[catastro$currentUse %in% '1_residential', c('gml_id','numberOfDwellings')]
  catastro$gml_id <- as.character(catastro$gml_id)

  # cambiar epsg todos a metros y pasar a sf
  a_sp <- constancia(sc)
  urban <- transformar(urban)
  sc <- transformar(sc)
  catastro <- transformar(catastro)
  sc <- epsg_igual(catastro, sc)
  urban <- epsg_igual(catastro, urban)

  # comprobar que el id_sc existe y cambiar por seccion
  sc <- id_existe(sc, id_sc)

  # extraer centroides
  catastro <- sf::st_centroid(catastro)

  # buffer a lo urbano
  urban_bff <- sf::st_buffer(urban, radio)

  # determinar si los centroides están dentro o fuera de la influencia green urban
  catastro$dentro <- lengths(sf::st_intersects(catastro, urban_bff, sparse = TRUE)) > 0
  rm(urban_bff)

  # buffer con los centroides dentro de urban
  aux <- sf::st_buffer(catastro[catastro$dentro, ], radio)
  aux$area_bff <- sf::st_area(aux)

  # intersection
  aux <- sf::st_intersection(aux, urban)
  aux$area_i <- sf::st_area(aux)
  aux <- as.data.frame(aux)

  # group by seccion sumando superficies
  tmp <- sapply(
    X   = by(
      data     = aux,
      INDICES  = aux$gml_id,
      FUN      = function(x) sum(x$area_i, na.rm = TRUE) / x$area_bff
    ),
    FUN = unique
  )

  aux <- data.frame(
    gml_id      = names(tmp),
    area_i      = tmp,
    stringsAsFactors = FALSE
  )

  # union del resultado intersecciones
  catastro$area_inter <- aux$area_i[match(catastro$gml_id, aux$gml_id)]
  rm(aux, tmp, urban)

  # pegar numero seccion censal i elimanar nulos
  catastro <- sf::st_join(catastro, sc[, c('seccion')])
  catastro <- catastro[!is.na(catastro$seccion),]

  # calculo de la ponderación
  catastro$area_inter <- ifelse(is.na(catastro$area_inter), 0, catastro$area_inter)
  prop_ponderada <- sapply(
    unique(catastro$seccion),
    function(x) {
      stats::weighted.mean(
        catastro$area_inter[catastro$seccion == x],
        catastro$numberOfDwellings[catastro$seccion == x]
      )
    }
  )
  # juntar resultado a un nuevo campo
  sc$accs_verde <- prop_ponderada[match(sc$seccion, names(prop_ponderada))]

  # transformaciones para devolver mismo objeto introducido por usuario
  sc <- sf::st_transform(sc, crs_sc)
  colnames(sc)[colnames(sc)=='seccion'] <- id_sc
  sc <- transformar(sc, a_sp)

  return(sc)
}
