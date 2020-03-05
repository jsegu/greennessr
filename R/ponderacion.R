#' @title Ponderar
#'
ponderar <- function(x, urban, ndvi){
  a <- function(x){
    ifelse(x<quantile(x,0.25), '1',
           ifelse(x<quantile(x,0.5),'2',
                  ifelse(x<(quantile(x,0.75)),'3','4'
                  )))}
  #indice ponderado
  x$ponderado <- (x[,c(urban)]+((x[,c(ndvi)]+1)*50*(cor(x[,c(ndvi)], x[,c(urban)])+1)/2))/2

  ## crear categorias
  x$green_cat <- a(x$ponderado)

  return(x)
}
