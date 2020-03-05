#' @title Ponderar
#'

load('//galera.isciii.es/UECA/area_compartida/cancer_infantil/jordi/_greenness_ciudades/03_resultado/FINAL_3000.RData')
datos <- res_Pamplona[[1]]
rm(list=setdiff(ls(), "datos"))


head(datos)

ponderar <- function(x, ndvi, urban){

}

x <- datos
ndvi <- 'ndvi'
urban <- 'area_verde'

###
##### aqui se inicia la funcion
#sacar valores
ndvi <- x[,c(ndvi)]
urban <- x[,c(urban)]
correlacion <- cor(ndvi, urban)

#estanderizar
ndvi <- (ndvi+1)*50
correlacion <- (correlacion+1)/2

#indice
ponderado <- (urban+(ndvi*correlacion))/2

## crear ponderacion
pond_cat <- lapply(ponderado,
                   FUN = )
