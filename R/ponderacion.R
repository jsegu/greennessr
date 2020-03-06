#' @title Ponderar
#'
ponderar <- function(x, urban, ndvi){
  res <- x
  x <- as.data.frame(x)
  a <- function(x){
    ifelse(x<quantile(x,0.25), '1',
           ifelse(x<quantile(x,0.5),'2',
                  ifelse(x<(quantile(x,0.75)),'3','4'
                  )))}
  #indice ponderado
  res$greenness <- (x[,c(urban)]+((x[,c(ndvi)]+1)*16.333*(cor(x[,c(ndvi)], x[,c(urban)])+1)/2))/2

  ## crear categorias
  res$green_cat <- a(res$greenness)

  return(res)
}
