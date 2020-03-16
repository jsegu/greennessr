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
  catastro <- as.data.frame(sf::st_intersection(epsg_igual(catastro,sc), catastro))

  # media ponderada de todos los items a la vez
  # con correlacion
  sc$greenness <- sapply(
    unique(catastro$seccion),
    function(x) {
      # sacar media ponderada ndvi
      nd <- stats::weighted.mean(
        catastro$ndvi[catastro$seccion == x],
        catastro$numberOfDwellings[catastro$seccion == x],
        na.rm = TRUE
      )

      # sacar media ponderada urban
      ur <- stats::weighted.mean(
          catastro$area_i[catastro$seccion == x]/(pi*radio^2)*100,
          catastro$numberOfDwellings[catastro$seccion == x],
          na.rm = TRUE
      )
      # pasar las secciones censales sin urban a 0
      ur[is.na(ur)] <- 0

      # sacar correlacion entre ndvi y urban por sc
      # c <- stats::cor(catastro$area_i[catastro$seccion == x], catastro$ndvi[catastro$seccion == x], use = "na.or.complete")

      # calculo de la ponderación con correlacion
      # res <- as.numeric(ur)*0.75+((as.numeric(nd)+1)*50*((c+1)/2))*0.25
      res <- as.numeric(ur)*0.8+(as.numeric(nd)+1)*50*0.2

      # cambiar los de la correlacion NA (debido a no tener las dos variables en la SC) por solo valor ndvi
      # if (is.na(res)){
      #   res <- (as.numeric(nd)+1)*50*0.25*0.5
      # }

      return(res)
    }
  )

library(leaflet)
st_crs(sc) <- 25830
st_crs(sc)
sc_plot <- st_transform(sc, 4326)

leaflet(sc_plot) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
            fillColor = ~colorQuantile("YlOrRd", greenness)(greenness),
            popup = sprintf("<b>Greenness: %s</b>", round(sc_plot$greenness, 2)),
            popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
            )
# }






## ## ## ## PRUEBAS ANTIGUAS
  # sin correlacion
  verde2 <- sapply(
    unique(catastro$seccion),
    function(x) {
      # sacar media ponderada ndvi
      nd <- stats::weighted.mean(
        catastro$ndvi[catastro$seccion == x],
        catastro$numberOfDwellings[catastro$seccion == x],
        na.rm = TRUE
      )

      # sacar media ponderada urban
      ur <- stats::weighted.mean(
        catastro$area_i[catastro$seccion == x]/(pi*radio^2)*100,
        catastro$numberOfDwellings[catastro$seccion == x],
        na.rm = TRUE
      )
      # pasar las secciones censales sin urban a 0
      ur[is.na(ur)] <- 0

      # calculo de la ponderación con correlacion
      res <- as.numeric(ur)*0.8+(as.numeric(nd)+1)*50*0.2

      # cambiar los de la correlacion NA (debido a no tener las dos variables en la SC) por solo valor ndvi
      # if (is.na(res)){
      #   res <- (as.numeric(nd)+1)*50*0.25
      # }
      return(res)
    }
  )



  ########## mirar si se puede calcular la coorelacion de las variables con NA
  nd <- sapply(
    unique(catastro$seccion),
    function(x) {
      # sacar media ponderada ndvi
      nd <- stats::weighted.mean(
        catastro$ndvi[catastro$seccion == x],
        catastro$numberOfDwellings[catastro$seccion == x],
        na.rm = TRUE
      )
    }
  )
  ur <- sapply(
    unique(catastro$seccion),
    function(x) {
      # sacar media ponderada ndvi
      ur <- stats::weighted.mean(
        catastro$area_i[catastro$seccion == x]/(pi*radio^2)*100,
        catastro$numberOfDwellings[catastro$seccion == x],
        na.rm = TRUE
      )
    }
  )

  ur[is.na(ur)] <- 0

  corre <- sapply(
    unique(catastro$seccion),
    function(x) {
      # sacar media ponderada ndvi
      stats::cor(catastro$area_i[catastro$seccion == x], catastro$ndvi[catastro$seccion == x], use = "na.or.complete")
    }
  )
