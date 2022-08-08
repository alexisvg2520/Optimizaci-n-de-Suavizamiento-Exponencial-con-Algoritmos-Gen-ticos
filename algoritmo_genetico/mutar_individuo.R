mutar_individuo <- function(individuo, limite_inf, limite_sup,
                            prob_mut = 0.01, distribucion = "uniforme",
                            media_distribucion = 1, sd_distribucion = 1,
                            min_distribucion = -1, max_distribucion = 1,
                            verbose = TRUE) {
  # ARGUMENTOS
  # =============================================================================
  # individuo: vector que representa a un individuo.
  # prob_mut:  probabilidad que tiene cada posición del individuo de mutar.
  # distribucion: distribución de la que obtener el factor de mutación. Puede
  #               ser: "normal", "uniforme" o "aleatoria".
  # media_distribucion: media de la distribución si se selecciona
  #                     distribucion = "normal".
  # sd_distribucion:    desviación estándar de la distribución si se selecciona
  #                     distribucion = "normal".
  # min_distribucion:   mínimo la distribución si se selecciona
  #                     distribucion = "uniforme".
  # max_distribucion:   máximo la distribución si se selecciona
  #                     distribucion = "uniforme".
  # 
  # RETORNO
  # ============================================================================
  # Un vector que representa al individuo tras someterse a las mutaciones.
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  if (!(distribucion %in% c("normal", "uniforme", "aleatoria"))) {
    stop("El argumento distribución debe ser: normal, uniforme o aleatoria.")
  }
  
  # CRUCE
  # ----------------------------------------------------------------------------
  # Selección de posiciones a mutar.
  posiciones_mutadas <- runif(n = length(individuo), min = 0, max = 1) < prob_mut
  
  # Se modifica el valor de aquellas posiciones que hayan sido seleccionadas para
  # mutar. Si el valor de prob_mut es muy bajo, las mutaciones serán muy poco
  # frecuentes y el individuo devuelto será casi siempre igual al original.
  
  # Si se emplea distribucion = "uniforme" o distribucion = "normal":
  if (distribucion == "normal" | distribucion == "uniforme") {
    # Se extrae un valor aleatorio de la distribución elegida que se suma
    # para modificar la/las posiciones mutadas.
    if (distribucion == "normal") {
      factor_mut <- rnorm(
        n = sum(posiciones_mutadas),
        mean = media_distribucion,
        sd = sd_distribucion
      )
    }
    if (distribucion == "uniforme") {
      factor_mut <- runif(
        n = sum(posiciones_mutadas),
        min = min_distribucion,
        max = max_distribucion
      )
    }
    
    individuo[posiciones_mutadas] <- individuo[posiciones_mutadas] + factor_mut
    
    # Se comprueba si algún valor mutado supera los límites impuestos. En tal caso
    #  se sobrescribe con el valor del límite correspondiente.
    for (i in which(posiciones_mutadas)) {
      if (individuo[i] < limite_inf[i]) {
        individuo[i] <- limite_inf[i]
      }
      if (individuo[i] > limite_sup[i]) {
        individuo[i] <- limite_sup[i]
      }
    }
  } else if (distribucion == "aleatoria") {
    for (i in which(posiciones_mutadas)) {
      individuo[i] <- runif(n = 1, min = limite_inf[i], max = limite_sup[i])
    }
  }
  
  # INFORMACIÓN DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose) {
    cat("-----------------", "\n")
    cat("Individuo mutado", "\n")
    cat("-----------------", "\n")
    cat("Probabilidad =", prob_mut, "\n")
    cat("Individuo    = ", individuo, "\n")
    cat("\n")
  }
  
  return(individuo)
}
