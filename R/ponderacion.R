#' @title Ponderar
#'

load('//galera.isciii.es/UECA/area_compartida/cancer_infantil/jordi/_greenness_ciudades/03_resultado/FINAL_3000.RData')
datos <- res_Pamplona[[1]]
rm(list=setdiff(ls(), "datos"))


head(datos)

x <- datos
ndvi <- 'ndvi'
urban <- 'area_verde'

ponderar <- function(x, urban, ndvi){
  #sacar valores
  ndvi <- x[,c(ndvi)]
  urban <- x[,c(urban)]
  correlacion <- cor(ndvi, urban)

  #estanderizar
  ndvi <- (ndvi+1)*50
  correlacion <- (correlacion+1)/2

  #indice ponderado
  x$ponderado <- (urban+(ndvi*correlacion))/2

  ## crear categorias
  x$green_cat <- transform(x$ponderado,
                           check = ave(x$ponderado, FUN=function(x){
                             ifelse(x<quantile(x,0.25), '1',
                                    ifelse(x<quantile(x,0.5),'2',
                                           ifelse(x<(quantile(x,0.75)),'3','4'
                                           )))
                           })
  )[,2]

  return(x)
}

kk <- ponderar(datos, 'area_verde', 'ndvi')
