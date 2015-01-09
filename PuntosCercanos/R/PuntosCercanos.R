#' Calcula los puntos cercanos de dos vectores
#' @description 
#'  Dados dos vectores de n puntos , calcula los puntos más cercanos según distancia Euclídea y 
#'  provee los índices y valores de ellos
#' 
#' @param x primer vector (M dimensiones es soportado)
#' @param y segundo vector (M dimensiones es soportado)
#' @return Índice devuelto 
#' @examples 
#' # Para calcular los puntos más cercanos de dos vectores de 5 puntos y 2 dimensiones sería: 
#' x1 <- matrix(runif(50,0,2),nrow=5,ncol=2) 
#' x2 <- matrix(runif(50,0,2),nrow=5,ncol=2)
#' PuntosCercanos(x1,x2)
#'
#' #La función soporta M-dimensiones, por ejemplo para calcular 
#' #la distancia en dos vectores de 5 puntos y 4 dimensiones sería:
#' 
#' x1 <- matrix(runif(20,0,2),nrow=5,ncol=4)
#' x2 <- matrix(runif(20,0,2),nrow=5,ncol=4)
#' PuntosCercanos(x1,x2)
#' @export

PuntosCercanos <- function(x,y) 
{
  indice <- which(rdist(x,y)==min(rdist(x,y)), arr.ind=TRUE)
  cat ("Los puntos mas cercanos se encuentran en: \n")
  cat ("Primer vector, posicion ",indice[1,1], "con valor(es):",x[indice[1,1],],"\n")
  cat ("Segundo vector, posicion ",indice[1,2], "con valor(es):",y[indice[1,2],],"\n")
  return (indice)
}