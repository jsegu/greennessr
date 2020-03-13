# indice ponderado secciones censales

# funcion greenness para secciones censales
library(medear)
library(sp)
library(raster)
library(sf)

# data("cartografia")
#
# #capa de secciones censales
# sc <- cartografia[cartografia$NMUN == 'Oviedo',]
# rm(cartografia)
#
# # capa de catastro
# catastro <- sf::st_read('c:/proyectos/_teletrabajo_coronavirus/_oviedo/33900/A.ES.SDGC.BU.33900.building.gml')
#
# # raster ndvi
# ndvi <- res_Oviedo[[2]]
#
# # capa urban atlas
# urban <- sf::st_read('c:/proyectos/_teletrabajo_coronavirus/greenness_ciudades/01_data/urban_atlas/Oviedo/Shapefiles/ES013L2_OVIEDO_UA2006_Revised.shp')
#
# #guardarlo
# save(catastro, ndvi, sc, urban, file = 'c:/proyectos/_teletrabajo_coronavirus/_oviedo/datos_de_entrada.RData')

#cargar datos de entrada
load(file = 'c:/proyectos/_teletrabajo_coronavirus/_oviedo/datos_de_entrada.RData')

# funcion ((SE LE TENDRAN QUE PONER LOS ITEMS QUE VAN ANTES PARA LLEGAR A CALCULAR LOS DATOS DE ENTRADA))

# greenness <- function(sc = sc, ndvi = ndvi, catastro = catastro, radio = 300, calculo = mean, categorias = c(14100)){
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

  # extraer centroides catatro
  catastro <- sf::st_centroid(catastro)

  # buffer a lo urbano
  urban_bff <- sf::st_buffer(urban, radio)

  # determinar si los centroides están dentro o fuera de la influencia green urban
  catastro$dentro <- lengths(sf::st_intersects(catastro, urban_bff, sparse = TRUE)) > 0
  rm(urban_bff)

  # buffer con los centroides dentro de urban
  aux <- sf::st_buffer(catastro[catastro$dentro, ], radio)
  aux$area_bff <- sf::st_area(aux)

  # sacar urban
  aux <- sf::st_intersection(aux, urban)
  aux$area_i <- sf::st_area(aux)
  aux <- as.data.frame(aux)

  # catastro con datos del urban
  catastro$area_i <- aux$area_i[match(catastro$gml_id, aux$gml_id)]
  rm(aux, urban)

  # sacar ndvi
  catastro$ndvi <- raster::extract(
    ndvi,
    epsg_igual(ndvi,transformar(sf::st_buffer(catastro,radio), a_sp = TRUE)),
    fun=calculo,
    na.rm=TRUE)[,1]
  # save.image(file = 'c:/proyectos/_teletrabajo_coronavirus/_oviedo/calculos_ndvi_y_urban.RData' )

  # poner en catastro id seccion censal i transformar a data.frame
  aux <- as.data.frame(sf::st_intersection(epsg_igual(catastro,sc), catastro))

  # sacar correlacion entre ndvi y urban por sc
  corre <- sapply(
    unique(aux$seccion),
    function(x) {
      cor(aux$area_i[aux$seccion == x], aux$ndvi[aux$seccion == x])
    }
  )

  juntos <- sapply(
    unique(aux$seccion),
    function(x) {
      nd <- stats::weighted.mean(
        aux$ndvi[aux$seccion == x],
        aux$numberOfDwellings[aux$seccion == x]
      )
      ur <-   stats::weighted.mean(
          aux$area_i[aux$seccion == x]/(pi*radio^2)*100,
          aux$numberOfDwellings[aux$seccion == x]
      )
      c <- cor(aux$area_i[aux$seccion == x], aux$ndvi[aux$seccion == x])
    return(as.numeric(ur)+as.numeric(nd)+1)
    }
  )



  nd <- sapply(
    unique(aux$seccion),
    function(x) {
      stats::weighted.mean(
        aux$ndvi[aux$seccion == x],
        aux$numberOfDwellings[aux$seccion == x]
      )
    }
  )
  ur <- sapply(
    unique(aux$seccion),
    function(x) {
      stats::weighted.mean(
        aux$area_i[aux$seccion == x]/(pi*radio^2)*100,
        aux$numberOfDwellings[aux$seccion == x]
      )
    }
  )




    cor(catastro[!is.na(catastro$area_i),c(7)][[1]],catastro[!is.na(catastro$area_i),6][[1]])

# }


