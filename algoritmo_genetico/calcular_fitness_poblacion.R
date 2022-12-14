calcular_fitness_poblacion <- function(poblacion, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...) {
  # Esta funci?n devuelve el fitness de cada individuo de una poblaci?n.
  #
  # ARGUMENTOS
  # ============================================================================
  # poblacion:        matriz que representa la poblaci?n de individuos.
  # funcion_objetivo: nombre de la funci?n que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # optimizacion:     "maximizar" o "minimizar". Dependiendo de esto, la relaci?n
  #                   del fitness es directamente o indirectamente proporcional
  #                   al valor de la funci?n.
  # verbose:          mostrar informaci?n del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # Vector con el fitness de todos los individuos de la poblaci?n. El orden de
  # los valores se corresponde con el orden de las filas de la matriz poblaci?n.
  
  
  # C?LCULO DEL FITNESS DE CADA INDIVIDUO DE LA POBLACI?N
  # ----------------------------------------------------------------------------
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  for (i in 1:nrow(poblacion)) {
    individuo <- poblacion[i, ]
    
    fitness_individuo <- calcular_fitness_individuo(
      individuo        = individuo,
      funcion_objetivo = funcion_objetivo,
      optimizacion     = optimizacion,
      verbose          = verbose
    )
    fitness_poblacion[i] <- fitness_individuo
  }
  
  # MEJOR INDIVIDUO DE LA POBLACI?N
  # ----------------------------------------------------------------------------
  # Se identifica el mejor individuo de toda la poblaci?n, el de mayor
  # fitness.
  indice_mejor_individuo <- which.max(fitness_poblacion)
  
  # Se identifica el valor de la funci?n objetivo para el mejor individuo.
  if (optimizacion == "maximizar") {
    valor_funcion <- fitness_poblacion[indice_mejor_individuo]
  } else {
    valor_funcion <- -1*fitness_poblacion[indice_mejor_individuo]
  }
  
  # INFORMACI?N DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("------------------", "\n")
    cat("Poblaci?n evaluada", "\n")
    cat("------------------", "\n")
    cat("Optimizaci?n              =", optimizacion, "\n")
    cat("Mejor fitness encontrado  =", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor soluci?n encontrada =",
        paste(poblacion[indice_mejor_individuo,], collapse = " "), "\n")
    cat("Valor funci?n objetivo    =", valor_funcion, "\n")
    cat("\n")
  }
  
  return(fitness_poblacion)
}

