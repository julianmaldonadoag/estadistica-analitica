
#' Desconocemos el tamano de la poblacion.
#' @param e Error muestral permitido o desviacion respecto a la media.
#' @param Z Es una constante que depende del nivel de confianza que asignemos.
#' @param var Es la desviacion estandar de la poblacion al cuadrado == varianza.
sizeWithoutPop <- function(e, Z, var) {
  return ( (Z**2 * var) / e**2 )
}

#' Conocemos el tamano de la poblacion.
#' @param N Tamano de la poblacion.
#' @param e Error muestral permitido o desviacion respecto a la media.
#' @param Z Es una constante que depende del nivel de confianza que asignemos.
#' @param var Es la desviacion estandar de la poblacion al cuadrado == varianza.
sizeWithPop <- function(N, e, Z, var) {
  return ( (N * Z**2 * var) / (( (N-1) * e**2) + (Z**2 * var)) )
}

# var = p * q
# q = 1 - p
#' @param p Proporcion de elementos que poseen en la poblacion la caracteristica de estudio.
#' @param q Proporcion que no poseen esa caracteristica
#' Si los valores de p y/o q no son proporcionados, pasar NULL como valor.
computeVar <- function(p, q) {
  resultado <- NULL
  
  if (is.null(p) & is.null(q)) {
    resultado <- 0.25
  } else if (p >= 0 & is.null(q)) {
    q <- 1 - p
    resultado <- p * q
  } else if (is.null(p) & q >= 0) {
    p <- 1 - q
    resultado <- p * q
  } else if (!is.null(p) & !is.null(q)) {
    resultado <- p * q
  }
  
  return (resultado)
}

computeZ <- function(confidence) {
  a <- 1 - confidence
  
  return (round( qnorm(1 - (a/2) ), 2 ))
}


#### Ejemplo 1. ####

e <- 0.01
Z <- computeZ(0.95)
var <- computeVar(0.05, NULL)

sizeWithoutPop(e, Z, var)

#### Ejemplo 2. ####

N <- 10000
e <- 0.05
Z <- computeZ(0.90)
var <- computeVar(NULL, NULL)

sizeWithPop(N, e, Z, var)

#### Practica 1. Ratones ####

e <- 2.6
Z <- computeZ(0.91)
var <- 6**2

res <- sizeWithoutPop(e, Z, var)
ceiling(res)

#### Practica 2. Cenotes. ####

N <- 607
e <- 0.03
Z <- computeZ(0.94)
var <- 0.3**2

res <- sizeWithPop(N, e, Z, var)
ceiling(res)

#### Evaluacion 1. Ratones. ####

e <- 1.2
Z <- computeZ(0.96)
var <- 5.9**2

res <- sizeWithoutPop(e, Z, var)
ceiling(res)

#### Evaluacion 3. Cenotes. ####

N <- 593
e <- 0.1
Z <- computeZ(0.96)
var <- 0.31**2

res <- sizeWithPop(N, e, Z, var)
ceiling(res)
