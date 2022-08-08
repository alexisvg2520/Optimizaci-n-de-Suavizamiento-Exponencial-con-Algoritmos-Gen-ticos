calcular_fitness_poblacion <- function(poblacion, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...) {
  # Esta función devuelve el fitness de cada individuo de una población.
  #
  # ARGUMENTOS
  # ============================================================================
  # poblacion:        matriz que representa la población de individuos.
  # funcion_objetivo: nombre de la función que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # optimizacion:     "maximizar" o "minimizar". Dependiendo de esto, la relación
  #                   del fitness es directamente o indirectamente proporcional
  #                   al valor de la función.
  # verbose:          mostrar información del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # Vector con el fitness de todos los individuos de la población. El orden de
  # los valores se corresponde con el orden de las filas de la matriz población.
  
  
  # CÁLCULO DEL FITNESS DE CADA INDIVIDUO DE LA POBLACIÓN
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
  
  # MEJOR INDIVIDUO DE LA POBLACIÓN
  # ----------------------------------------------------------------------------
  # Se identifica el mejor individuo de toda la población, el de mayor
  # fitness.
  indice_mejor_individuo <- which.max(fitness_poblacion)
  
  # Se identifica el valor de la función objetivo para el mejor individuo.
  if (optimizacion == "maximizar") {
    valor_funcion <- fitness_poblacion[indice_mejor_individuo]
  } else {
    valor_funcion <- -1*fitness_poblacion[indice_mejor_individuo]
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("------------------", "\n")
    cat("Población evaluada", "\n")
    cat("------------------", "\n")
    cat("Optimización              =", optimizacion, "\n")
    cat("Mejor fitness encontrado  =", fitness_poblacion[indice_mejor_individuo], "\n")
    cat("Mejor solución encontrada =",
        paste(poblacion[indice_mejor_individuo,], collapse = " "), "\n")
    cat("Valor función objetivo    =", valor_funcion, "\n")
    cat("\n")
  }
  
  return(fitness_poblacion)
}

