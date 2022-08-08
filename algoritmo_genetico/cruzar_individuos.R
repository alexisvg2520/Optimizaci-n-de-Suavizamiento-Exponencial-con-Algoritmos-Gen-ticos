cruzar_individuos <- function(parental_1,
                              parental_2,
                              metodo_cruce = "uniforme",
                              verbose = TRUE) {
  # Esta función devuelve un individuo resultado de cruzar dos individuos
  # parentales con el método de cruzamiento uniforme o punto simple.
  #
  # ARGUMENTOS
  # ============================================================================
  # parental_1: vector que representa a un individuo.
  # parental_2: vector que representa a un individuo.
  # metodo_cruce: estrategia de cruzamiento (uniforme", "punto_simple")
  
  # RETORNO
  # ============================================================================
  # Un vector que representa a un nuevo individuo.
  
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (length(parental_1) != length(parental_2)) {
    stop(paste0(
      "La longitud de los dos vectores que representan a los ",
      "individuos debe ser la misma."
    ))
  }
  if (!(metodo_cruce %in% c("uniforme", "punto_simple"))) {
    stop("El método de cruzamiento debe ser: uniforme o punto_simple.")
  }
  
  # CRUCE
  # ----------------------------------------------------------------------------
  # Se crea el vector que representa el nuevo individuo
  descendencia <- rep(NA, times = length(parental_1))
  
  if (metodo_cruce == "uniforme") {
    # Se seleccionan aleatoriamente las posiciones que se heredan del parental_1.
    herencia_parent_1 <- sample(
      x       = c(TRUE, FALSE),
      size    = length(parental_1),
      replace = TRUE
    )
    # El resto de posiciones se heredan del parental_2.
    herencia_parent_2 <- !herencia_parent_1
    
    descendencia[herencia_parent_1] <- parental_1[herencia_parent_1]
    descendencia[herencia_parent_2] <- parental_2[herencia_parent_2]
  } else {
    punto_cruce <- sample(
      x    = 2:length(parental_1),
      size = 1
    )
    descendencia <- c(
      parental_1[1:(punto_cruce - 1)],
      parental_2[punto_cruce:length(parental_1)]
    )
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("---------------", "\n")
    cat("Cruce realizado", "\n")
    cat("---------------", "\n")
    cat("Método =", metodo_cruce, "\n")
    cat("Parental 1 = ", "\n")
    cat(parental_1, "\n")
    cat("Parental 2 = ", "\n")
    cat(parental_2, "\n")
    cat("Descendencia = ", "\n")
    cat(descendencia, "\n")
    cat("\n")
  }
  return(descendencia)
}
