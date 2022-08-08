crear_poblacion <- function(n_poblacion, n_variables, limite_inf = NULL,
                            limite_sup = NULL, verbose = TRUE) {
  
  # Esta función crea una matriz en la que, cada fila, está formada por una
  # combinación de valores numéricos aleatorios. El rango de posibles valores
  # para cada variable puede estar acotado.
  #
  # ARGUMENTOS
  # ============================================================================
  # n_poblacion: número total de individuos de la población.
  # n_variables: longitud de los individuos.
  # limite_inf:  vector con el límite inferior de cada variable. Si solo se
  #              quiere imponer límites a algunas variables, emplear NA para
  #              las que no se quiere acotar.
  # limite_sup:  vector con el límite superior de cada variable. Si solo se
  #              quiere imponer límites a algunas variables, emplear NA para
  #              las que no se quieren acotar.
  # verbose:     mostrar información del proceso por pantalla.
  #   
  # RETORNO
  # ============================================================================
  # Una matriz de tamaño n_poblacion x n_variables que representa una población.
  
  # COMPROBACIONES
  # ----------------------------------------------------------------------------
  if (!is.null(limite_inf) & (length(limite_inf) != n_variables)) {
    stop(paste(
      "limite_inf debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (!is.null(limite_sup) & length(limite_sup) != n_variables) {
    stop(paste(
      "limite_sup debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere límite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (is.null(limite_sup) | is.null(limite_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los límites dentro de los",
      "cuales debe buscarse la solución de cada variable.",
      "Por defecto se emplea [-10^3, 10^3]."
    ))
  } else if (any(any(is.na(limite_sup)), any(is.na(limite_inf)))) {
    warning(paste(
      "Los límites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  # Si no se especifica limite_inf, el valor mínimo que pueden tomar las variables
  # es -10^3.
  if (is.null(limite_inf)) {
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  # Si no se especifica limite_sup, el valor máximo que pueden tomar las variables
  # es 10^3.
  if (is.null(limite_sup)) {
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  # Si los límites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3
  if (!is.null(limite_inf)) {
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if (!is.null(limite_sup)) {
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  # CREAR POBLACIÓN
  # ----------------------------------------------------------------------------
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  # Bucle para crear cada individuo.
  for (i in 1:n_poblacion) {
    # Se crea un vector de NA que representa el individuo.
    individuo <- rep(NA, times = n_variables)
    
    for (j in 1:n_variables) {
      # Para cada posición, se genera un valor aleatorio dentro del rango permitido
      # para cada variable.
      individuo[j] <- runif(n = 1, min = limite_inf[j], max = limite_sup[j])
    }
    # Se añade el nuevo individuo a la población.
    poblacion[i, ] <- individuo
  }
  
  # INFORMACIÓN ALMACENADA EN LOS ATRIBUTOS
  # ----------------------------------------------------------------------------
  attr(poblacion, 'fecha_creacion')    <- Sys.time()
  attr(poblacion, 'numero_individuos') <- n_poblacion
  attr(poblacion, "class") <- c("matrix", "poblacion")
  
  if (verbose) {
    cat("Población inicial creada", "\n")
    cat("------------------------", "\n")
    cat("Fecha creación:", as.character(Sys.time()), "\n")
    cat("Número de individuos =", n_poblacion, "\n")
    cat("Límites inferiores de cada variable =", paste(limite_inf, collapse = ", "), "\n")
    cat("Límites superiores de cada variable =", paste(limite_sup, collapse = ", "), "\n")
    cat("\n")
  }
  
  return(poblacion)
}
