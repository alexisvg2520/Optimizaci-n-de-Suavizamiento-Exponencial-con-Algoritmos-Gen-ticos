calcular_fitness_individuo <- function(individuo, funcion_objetivo, optimizacion,
                                       verbose = TRUE, ...) {
  # Esta funci?n devuelve el fitness de cada individuo de una poblaci?n.
  #
  # ARGUMENTOS
  # ============================================================================
  # individuo:        vector con los valores de cada variable. El orden de los
  #                   valores debe coincidir con el de los argumentos de la
  #                   funci?n.
  # funcion_objetivo: nombre de la funci?n que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # optimizacion:    "maximizar" o "minimizar". Dependiendo de esto, la relaci?n
  #                   del fitness es directamente o indirectamente proporcional
  #                   al valor de la funci?n.
  # verbose:          mostrar informaci?n del proceso por pantalla.
  #
  # RETORNO
  # ============================================================================
  # fitness del individuo.
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (length(individuo) != length(names(formals(funcion_objetivo)))) {
    stop(paste("Los individuos deben tener tantos valores como argumentos tiene",
               "la funci?n objetivo."))
  }
  
  # C?LCULO FITNESS
  # ----------------------------------------------------------------------------
  if (optimizacion == "maximizar") {
    fitness <- do.call(funcion_objetivo, args = as.list(individuo))
  } else if (optimizacion == "minimizar") {
    fitness <- -(do.call(funcion_objetivo, args = as.list(individuo)))
  } else {
    stop("El argumento optimizaci?n debe ser maximizar o minimizar.")
  }
  
  # INFORMACI?N DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("El individuo ha sido evaluado", "\n")
    cat("-----------------------------", "\n")
    cat("Optimizaci?n =", optimizacion, "\n")
    cat("Individuo    =", paste(individuo, collapse = " "), "\n")
    cat("Fitness      =", fitness, "\n")
    cat("\n")
  }
  
  return(fitness)
}
