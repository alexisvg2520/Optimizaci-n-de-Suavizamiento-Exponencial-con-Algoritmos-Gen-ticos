seleccionar_individuo <- function(vector_fitness, metodo_seleccion = "tournament",
                                  verbose = FALSE) {
  # Esta funci?n recibe como argumento un vector con el fitness de cada individuo
  # y selecciona una de las posiciones, donde la probabilidad de selecci?n es
  # proporcional al fitness.
  
  # ARGUMENTOS
  # ============================================================================
  # vector_fitness:   un vector con el fitness de cada individuo.
  # metodo_seleccion: m?todo para establecer la probabilidad de selecci?n. Puede
  #                   ser: "ruleta", "rank", o "tournament".
  # verbose:          mostrar informaci?n del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # El ?ndice que ocupa el individuo seleccionado.
  
  # COMPROBACIONES INICIALES
  # ---------------------------------------------------------------------------
  if (!metodo_seleccion %in% c("ruleta", "rank", "tournament")) {
    stop("El m?todo de selecci?n debe de ser ruleta, rank o tournament.")
  }
  
  # SELECCI?N DE INDIVIDUOS
  # ----------------------------------------------------------------------------
  # Se calcula la probabilidad de selecci?n de cada individuo en funci?n
  # de su fitness.
  
  if (metodo_seleccion == "ruleta") {
    probabilidad_seleccion <- (vector_fitness) / sum(vector_fitness)
    
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion
    )
  } else if (metodo_seleccion == "rank") {
    probabilidad_seleccion <- 1 / rank(-vector_fitness)
    
    ind_seleccionado <- sample(
      x    = 1:length(vector_fitness),
      size = 1,
      prob = probabilidad_seleccion
    )
  } else if (metodo_seleccion == "tournament") {
    
    # Se seleccionan aleatoriamente dos parejas de individuos.
    ind_candidatos_a <- sample(x = 1:length(vector_fitness), size = 2)
    ind_candidatos_b <- sample(x = 1:length(vector_fitness), size = 2)
    
    # De cada pareja se selecciona el de mayor fitness.
    ind_ganador_a <- ifelse(
      vector_fitness[ind_candidatos_a[1]] > vector_fitness[ind_candidatos_a[2]],
      ind_candidatos_a[1],
      ind_candidatos_a[2]
    )
    ind_ganador_b <- ifelse(
      vector_fitness[ind_candidatos_b[1]] > vector_fitness[ind_candidatos_b[2]],
      ind_candidatos_b[1],
      ind_candidatos_b[2]
    )
    
    # Se comparan los dos ganadores de cada pareja.
    ind_seleccionado <- ifelse(
      vector_fitness[ind_ganador_a] > vector_fitness[ind_ganador_b],
      ind_ganador_a,
      ind_ganador_b
    )
  }
  
  # INFORMACI?N DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("----------------------", "\n")
    cat("Individuo seleccionado", "\n")
    cat("----------------------", "\n")
    cat("M?todo selecci?n    =", metodo_seleccion, "\n")
    cat("?ndice seleccionado =", ind_seleccionado, "\n")
  }
  
  return(ind_seleccionado)
}
