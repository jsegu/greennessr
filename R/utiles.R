
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
    stop("Objeto secciones censales no valido: Solo son validos para el calculo los objetos espaciales ej. SpatialPolygons o sf")
  }
  if (class(x)[1] != "sf"){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
