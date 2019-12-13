#' @title Urban Atlas de Castello de la Plana
#'
#' @description Cartografía de la base de datos Urban Atlas en el municipio de Castelló
#' de la Plana para ser usada como ejemplo en las funciones de
#' \code{acceso_verde y area_verde}.
#'
#' @details El urban atlas es un mapa de cubiertas y usos del suelo de las principales
#' ciudades europeas creado por el proyecto Copernicus, consta de dos ediciones 2006
#' y 2012. Para el primer año solo están disponibles las principales ciudades españolas,
#' en la versión de 2012 aumenta considerablemente el número de ciudades.
#' \href{https://land.copernicus.eu/local/urban-atlas}{Clic para más información y descargas}
#'
#' @format Objeto data.frame con extensión espacial sf.
#'
#' @name urban_atlas_castellon
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @examples
#' library(greennessr)
#' data(urban_atlas_castellon)
#'
#' @encoding UTF-8
"urban_atlas_castellon"

#' @title Edificios de Castello de la Plana
#'
#' @description Cartografía de edificios de la base de datos del catastro en el
#' municipio de Castelló de la Plana para ser usada como ejemplo en las
#' funciones de \code{acceso_verde y area_verde}.
#'
#'
#' @details Los datos están disponible a través del servicio de
#' \href{http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.BU.atom.xml}{descargas ATOM} del
#' servicio INSPIRE de la página del catastro. Además también existe un plugin para QGIS, llamado
#' 'Spanish Insipire Catastral Downloader', que proporciona una interfaz más amable para la descarga
#' de los datos. Es recomendable cargar la capa con extensión '.gml' porque su tiempo de lectura de datos
#' es menor.
#'
#' @name catastro_castellon
#'
#' @docType data
#'
#' @format Objeto data.frame con extensión espacial sf.
#'
#' @keywords datasets
#'
#' @examples
#' library(greennessr)
#' data(catastro_castellon)
#'
#' @encoding UTF-8
"catastro_castellon"

#' @title Secciones censales Castello de la Plana
#'
#' @description Cartografía de las secciones censales del municipio de Castelló de la Plana
#' para ser usada como ejemplo en las funciones de \code{acceso_verde y area_verde}
#'
#' @details Datos extraidos de la función \code{carto_medea3} de la librería \code{medear},
#' está cartografía tiene la particularidad que esta unificada acorde para el desarrollo del
#' proyecto MEDEA3, para la concordancia de las variaciones del seccionado en el período de
#' estudio (1996-2015).
#'
#' @name secciones_castellon
#'
#' @docType data
#'
#' @format Objeto data.frame con extensión espacial sf.
#'
#' @keywords datasets
#'
#' @examples
#' library(greennessr)
#' data(secciones_castellon)
#'
#' @encoding UTF-8
"secciones_castellon"
