crear_poblacion <- function(n_poblacion, n_variables, limite_inf = NULL,
                            limite_sup = NULL, verbose = TRUE) {
  
  # Esta funci?n crea una matriz en la que, cada fila, est? formada por una
  # combinaci?n de valores num?ricos aleatorios. El rango de posibles valores
  # para cada variable puede estar acotado.
  #
  # ARGUMENTOS
  # ============================================================================
  # n_poblacion: n?mero total de individuos de la poblaci?n.
  # n_variables: longitud de los individuos.
  # limite_inf:  vector con el l?mite inferior de cada variable. Si solo se
  #              quiere imponer l?mites a algunas variables, emplear NA para
  #              las que no se quiere acotar.
  # limite_sup:  vector con el l?mite superior de cada variable. Si solo se
  #              quiere imponer l?mites a algunas variables, emplear NA para
  #              las que no se quieren acotar.
  # verbose:     mostrar informaci?n del proceso por pantalla.
  #   
  # RETORNO
  # ============================================================================
  # Una matriz de tama?o n_poblacion x n_variables que representa una poblaci?n.
  
  # COMPROBACIONES
  # ----------------------------------------------------------------------------
  if (!is.null(limite_inf) & (length(limite_inf) != n_variables)) {
    stop(paste(
      "limite_inf debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere l?mite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (!is.null(limite_sup) & length(limite_sup) != n_variables) {
    stop(paste(
      "limite_sup debe tener un valor por cada variable.",
      "Si para alguna variable no se quiere l?mite, emplear NA.",
      "Ejemplo: lim_sup = c(10, NA, 10)"
    ))
  } else if (is.null(limite_sup) | is.null(limite_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los l?mites dentro de los",
      "cuales debe buscarse la soluci?n de cada variable.",
      "Por defecto se emplea [-10^3, 10^3]."
    ))
  } else if (any(any(is.na(limite_sup)), any(is.na(limite_inf)))) {
    warning(paste(
      "Los l?mites empleados por defecto cuando no se han definido son:",
      " [-10^3, 10^3]."
    ))
    cat("\n")
  }
  
  # Si no se especifica limite_inf, el valor m?nimo que pueden tomar las variables
  # es -10^3.
  if (is.null(limite_inf)) {
    limite_inf <- rep(x = -10^3, times = n_variables)
  }
  
  # Si no se especifica limite_sup, el valor m?ximo que pueden tomar las variables
  # es 10^3.
  if (is.null(limite_sup)) {
    limite_sup <- rep(x = 10^3, times = n_variables)
  }
  
  # Si los l?mites no son nulos, se reemplazan aquellas posiciones NA por el valor
  # por defecto -10^3 y 10^3
  if (!is.null(limite_inf)) {
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if (!is.null(limite_sup)) {
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  # CREAR POBLACI?N
  # ----------------------------------------------------------------------------
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  # Bucle para crear cada individuo.
  for (i in 1:n_poblacion) {
    # Se crea un vector de NA que representa el individuo.
    individuo <- rep(NA, times = n_variables)
    
    for (j in 1:n_variables) {
      # Para cada posici?n, se genera un valor aleatorio dentro del rango permitido
      # para cada variable.
      individuo[j] <- runif(n = 1, min = limite_inf[j], max = limite_sup[j])
    }
    # Se a?ade el nuevo individuo a la poblaci?n.
    poblacion[i, ] <- individuo
  }
  
  # INFORMACI?N ALMACENADA EN LOS ATRIBUTOS
  # ----------------------------------------------------------------------------
  attr(poblacion, 'fecha_creacion')    <- Sys.time()
  attr(poblacion, 'numero_individuos') <- n_poblacion
  attr(poblacion, "class") <- c("matrix", "poblacion")
  
  if (verbose) {
    cat("Poblaci?n inicial creada", "\n")
    cat("------------------------", "\n")
    cat("Fecha creaci?n:", as.character(Sys.time()), "\n")
    cat("N?mero de individuos =", n_poblacion, "\n")
    cat("L?mites inferiores de cada variable =", paste(limite_inf, collapse = ", "), "\n")
    cat("L?mites superiores de cada variable =", paste(limite_sup, collapse = ", "), "\n")
    cat("\n")
  }
  
  return(poblacion)
}
