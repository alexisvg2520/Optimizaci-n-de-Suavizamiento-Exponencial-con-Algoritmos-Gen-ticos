optimizar_ga <- function(
    funcion_objetivo,
    n_variables,
    optimizacion,
    limite_inf         = NULL,
    limite_sup         = NULL,
    n_poblacion        = 20,
    n_generaciones     = 50,
    elitismo           = 0.1,
    prob_mut           = 0.01,
    distribucion       = "uniforme",
    media_distribucion = 1,
    sd_distribucion    = 1,
    min_distribucion   = -1,
    max_distribucion   = 1,
    metodo_seleccion   = "tournament",
    metodo_cruce       = "uniforme",
    parada_temprana    = FALSE,
    rondas_parada      = NULL,
    tolerancia_parada  = NULL,
    verbose            = 1,
    y,
    ...) {
  
  # ARGUMENTOS
  # =============================================================================
  # funcion_objetivo: nombre de la funci?n que se desea optimizar. Debe de haber
  #                   sido definida previamente.
  # n_variables:      longitud de los individuos.
  # optimizacion:     "maximizar" o "minimizar". Dependiendo de esto, la relaci?n
  #                   del fitness es directamente o indirectamente proporcional al
  #                   valor de la funci?n.
  # limite_inf:       vector con el l?mite inferior de cada variable. Si solo se
  #                   quiere imponer l?mites a algunas variables, emplear NA para
  #                   las que no se quiere acotar.
  # limite_sup:       vector con el l?mite superior de cada variable. Si solo se
  #                   quiere imponer l?mites a algunas variables, emplear NA para
  #                   las que no se quieren acotar.
  # n_poblacion:      n?mero total de individuos de la poblaci?n.
  # n_generaciones:   n?mero total de generaciones creadas.
  # elitismo:         porcentaje de mejores individuos de la poblaci?n actual que
  #                   pasan directamente a la siguiente poblaci?n.
  # prob_mut:         probabilidad que tiene cada posici?n del individuo de mutar.
  # distribucion:     distribuci?n de la que obtener el factor de mutaci?n. Puede
  #                   ser: "normal", "uniforme" o "aleatoria".
  # media_distribucion: media de la distribuci?n si se selecciona distribucion="normal".
  # sd_distribucion:  desviaci?n est?ndar de la distribuci?n si se selecciona
  #                   distribucion="normal".
  # min_distribucion: m?nimo la distribuci?n si se selecciona distribucion="uniforme".
  # max_distribucion: m?ximo la distribuci?n si se selecciona distribucion="uniforme".
  # metodo_seleccion: m?todo para establecer la probabilidad de selecci?n. Puede
  #                   ser: "ruleta", "rank" o "tournament".
  # metodo_cruce: m?todo para cruzar los individuos. Puede ser: "uniforme",
  #                  "punto_simple".
  # parada_temprana:  si durante las ?ltimas "rondas_parada" generaciones la diferencia
  #                   absoluta entre mejores individuos no es superior al valor de
  #                  "tolerancia_parada", se detiene el algoritmo y no se crean
  #                   nuevas generaciones.
  # rondas_parada:    n?mero de generaciones consecutivas sin mejora m?nima para que
  #                   se active la parada temprana.
  # tolerancia_parada: valor m?nimo que debe tener la diferencia de generaciones
  #                    consecutivas para considerar que hay cambio.
  # verbose:          Nivel de detalle para que se imprima por pantalla el 
  #                   resultado de cada paso del algoritmo (0, 1, 2)
  
  # RETORNO
  # =============================================================================
  # La funci?n devuelve una lista con 5 elementos:
  # fitness:            una lista con el fitness del mejor individuo de cada
  #                     generaci?n.
  # mejores_individuos: una lista con la combinaci?n de predictores del mejor
  #                     individuo de cada generaci?n.
  # mejor_individuo:    combinaci?n de predictores del mejor individuo encontrado
  #                     en todo el proceso.
  # diferencia_abs:     una lista con la diferencia absoluta entre el fitness
  #                     del mejor individuo de generaciones consecutivas.
  # df_resultados:      un dataframe con todos los resultados anteriores.
  
  start_time <- Sys.time()
  
  # COMPROBACIONES INICIALES
  # ----------------------------------------------------------------------------
  # Si se activa la parada temprana, hay que especificar los argumentos
  # rondas_parada y tolerancia_parada.
  if (isTRUE(parada_temprana) &
      (is.null(rondas_parada) | is.null(tolerancia_parada)) ) {
    stop(paste(
      "Para activar la parada temprana es necesario indicar un valor",
      "de rondas_parada y de tolerancia_parada."
    ))
  }
  
  # ESTABLECER LOS L?MITES DE B?SQUEDA SI EL USUARIO NO LO HA HECHO
  # ----------------------------------------------------------------------------
  if (is.null(limite_sup) | is.null(limite_inf)) {
    warning(paste(
      "Es altamente recomendable indicar los l?mites dentro de los",
      "cuales debe buscarse la soluci?n de cada variable.",
      "Por defecto se emplea: [-10^3, 10^3]."
    ))
  }
  
  if (any(
    is.null(limite_sup), is.null(limite_inf), any(is.na(limite_sup)),
    any(is.na(limite_inf))
  )) {
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
  # por defecto -10^3 y 10^3.
  if (!is.null(limite_inf)) {
    limite_inf[is.na(limite_inf)] <- -10^3
  }
  
  if (!is.null(limite_sup)) {
    limite_sup[is.na(limite_sup)] <- 10^3
  }
  
  
  # ALMACENAMIENTO DE RESULTADOS
  # ----------------------------------------------------------------------------
  # Por cada generaci?n se almacena, la poblaci?n, el mejor individuo, su fitness,
  # y la diferencia absoluta respecto a la ?ltima generaci?n.
  poblaciones          <- vector(mode = "list", length = n_generaciones)
  resultados_fitness   <- vector(mode = "list", length = n_generaciones)
  resultados_individuo <- vector(mode = "list", length = n_generaciones)
  diferencia_abs       <- vector(mode = "list", length = n_generaciones)
  
  # ITERACI?N DE POBLACIONES
  # ----------------------------------------------------------------------------
  for (i in 1:n_generaciones) {
    if (verbose %in% c(1,2)) {
      cat("-------------------", "\n")
      cat("Generaci?n:", paste0(i, "\\", n_generaciones), "\n")
      cat("-------------------", "\n")
    }
    
    if (i == 1) {
      # CREACI?N DE LA POBLACI?N INICIAL
      # ------------------------------------------------------------------------
      poblacion <- crear_poblacion(
        n_poblacion = n_poblacion,
        n_variables = n_variables,
        limite_inf  = limite_inf,
        limite_sup  = limite_sup,
        verbose     = verbose %in% c(2)
      )
    }
    
    # CALCULAR FITNESS DE LOS INDIVIDUOS DE LA POBLACI?N
    # --------------------------------------------------------------------------
    fitness_ind_poblacion <- calcular_fitness_poblacion(
      poblacion        = poblacion,
      funcion_objetivo = funcion_objetivo,
      optimizacion     = optimizacion,
      verbose          = verbose %in% c(2)
    )
    
    # SE ALMACENA LA POBLACI?N Y SU MEJOR INDIVIDUO
    # --------------------------------------------------------------------------
    poblaciones[[i]]          <- poblacion
    fitness_mejor_individuo   <- max(fitness_ind_poblacion)
    mejor_individuo           <- poblacion[which.max(fitness_ind_poblacion), ]
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- mejor_individuo
    
    # SE CALCULA LA DIFERENCIA ABSOLUTA RESPECTO A LA GENERACI?N ANTERIOR
    # --------------------------------------------------------------------------
    # La diferencia solo puede calcularse a partir de la segunda generaci?n.
    if (i > 1) {
      diferencia_abs[[i]] <- abs(resultados_fitness[[i - 1]] - resultados_fitness[[i]])
    }
    
    # NUEVA POBLACI?N
    # --------------------------------------------------------------------------
    nueva_poblacion <- matrix(
      data = NA,
      nrow = nrow(poblacion),
      ncol = ncol(poblacion)
    )
    
    # ELITISMO
    # --------------------------------------------------------------------------
    # El elitismo indica el porcentaje de mejores individuos de la poblaci?n
    # actual que pasan directamente a la siguiente poblaci?n. De esta forma, se
    # asegura que, la siguiente generaci?n, no sea nunca inferior.
    
    if (elitismo > 0) {
      n_elitismo         <- ceiling(nrow(poblacion) * elitismo)
      posicion_n_mejores <- order(fitness_ind_poblacion, decreasing = TRUE)
      posicion_n_mejores <- posicion_n_mejores[1:n_elitismo]
      nueva_poblacion[1:n_elitismo, ] <- poblacion[posicion_n_mejores, ]
    } else {
      n_elitismo <- 0
    }
    
    # CREACI?N DE NUEVOS INDIVIDUOS POR CRUCES
    # --------------------------------------------------------------------------
    for (j in (n_elitismo + 1):nrow(nueva_poblacion)) {
      # Seleccionar parentales
      indice_parental_1 <- seleccionar_individuo(
        vector_fitness   = fitness_ind_poblacion,
        metodo_seleccion = metodo_seleccion,
        verbose          = verbose %in% c(2)
      )
      indice_parental_2 <- seleccionar_individuo(
        vector_fitness   = fitness_ind_poblacion,
        metodo_seleccion = metodo_seleccion,
        verbose          = verbose %in% c(2)
      )
      parental_1 <- poblacion[indice_parental_1, ]
      parental_2 <- poblacion[indice_parental_2, ]
      
      # Cruzar parentales para obtener la descendencia
      descendencia <- cruzar_individuos(
        parental_1   = parental_1,
        parental_2   = parental_2,
        metodo_cruce = metodo_cruce,
        verbose      = verbose %in% c(2)
      )
      # Mutar la descendencia
      descendencia <- mutar_individuo(
        individuo    = descendencia,
        prob_mut     = prob_mut,
        limite_inf   = limite_inf,
        limite_sup   = limite_sup,
        distribucion = distribucion,
        media_distribucion = media_distribucion,
        sd_distribucion    = sd_distribucion,
        min_distribucion   = min_distribucion,
        max_distribucion   = max_distribucion,
        verbose            = verbose %in% c(2)
      )
      
      nueva_poblacion[j, ] <- descendencia
    }
    poblacion <- nueva_poblacion
    
    # CRITERIO DE PARADA
    # --------------------------------------------------------------------------
    # Si durante las ?ltimas n generaciones, la diferencia absoluta entre mejores
    # individuos no es superior al valor de tolerancia_parada, se detiene el
    # algoritmo y no se crean nuevas generaciones.
    
    if (parada_temprana && (i > rondas_parada)) {
      ultimos_n <- tail(unlist(diferencia_abs), n = rondas_parada)
      if (all(ultimos_n < tolerancia_parada)) {
        cat(
          "Algoritmo detenido en la generacion", i,
          "por falta cambio m?nimo de", tolerancia_parada,
          "durante", rondas_parada,
          "generaciones consecutivas.",
          "\n"
        )
        break()
      }
    }
  }
  
  # IDENTIFICACI?N DEL MEJOR INDIVIDUO DE TODO EL PROCESO
  # ----------------------------------------------------------------------------
  indice_mejor_individuo_global <- which.max(unlist(resultados_fitness))
  mejor_fitness_global   <- resultados_fitness[[indice_mejor_individuo_global]]
  mejor_individuo_global <- resultados_individuo[[indice_mejor_individuo_global]]
  
  # Se identifica el valor de la funci?n objetivo para el mejor individuo.
  if (optimizacion == "maximizar") {
    mejor_valor_global <- mejor_fitness_global
  } else {
    mejor_valor_global <- -1*mejor_fitness_global
  }
  
  #Obtenci?n de m?tricas MAE MASE RMSE
  
  yopt <- function(y,x){
    sua = HoltWinters(y, alpha=x, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
    s1 = sua$fitted[,1] #valores del suavizamiento exponencial
  }
  
  y_p <- yopt(y,mejor_individuo_global)
  m1<-mae(y[2:(length(y))],y_p)
  m2<-rmse(y[2:(length(y))],y_p)
  m3<-mase(y[2:(length(y))],y_p,step_size = 1)
  
  # RESULTADOS
  # ----------------------------------------------------------------------------
  # Para crear el dataframe se convierten las listas a vectores del mismo tama?o.
  resultados_fitness <- unlist(resultados_fitness)
  diferencia_abs     <- c(NA, unlist(diferencia_abs))
  
  # Si hay parada temprana, algunas generaciones no se alcanzan: Se eliminan sus
  # posiciones de las listas de resultados
  resultados_individuo <- resultados_individuo[!sapply(resultados_individuo, is.null)]
  poblaciones          <- poblaciones[!sapply(poblaciones, is.null)]
  
  
  # Para poder a?adir al dataframe la secuencia variables, se concatenan.
  variables <- sapply(
    X = resultados_individuo,
    FUN = function(x) {
      paste(x, collapse = ", ")
    }
  )
  
  df_resultados <- data.frame(
    generacion        = seq_along(resultados_fitness),
    fitness           = resultados_fitness,
    predictores       = variables,
    diferencia_abs    = diferencia_abs
  )
  
  resultados <- list(
    mejor_individuo_global = mejor_individuo_global,
    mejor_valor_global     = mejor_valor_global,
    mejor_fitness_por_generacion   = resultados_fitness,
    mejor_individuo_por_generacion = resultados_individuo,
    diferencia_abs         = diferencia_abs,
    df_resultados          = df_resultados,
    poblaciones            = poblaciones,
    funcion_objetivo       = funcion_objetivo
  )
  
  end_time <- Sys.time()
  
  # INFORMACI?N ALMACENADA EN LOS ATRIBUTOS
  # ----------------------------------------------------------------------------
  attr(resultados, "class") <- "optimizacion_ga"
  attr(resultados, 'fecha_creacion')        <- end_time
  attr(resultados, 'duracion_optimizacion') <- paste(
    difftime(end_time, start_time, "secs"),
    "secs"
  )
  attr(resultados, 'optimizacion')          <- optimizacion
  attr(resultados, 'lim_inf')               <- limite_inf
  attr(resultados, 'lim_sup')               <- limite_sup
  attr(resultados, 'n_poblacion')           <- n_poblacion
  attr(resultados, 'generaciones')          <- i 
  attr(resultados, 'valor_variables')       <- mejor_individuo_global
  attr(resultados, 'mejor_fitness')         <- mejor_fitness_global 
  attr(resultados, 'optimo_encontrado')     <- mejor_valor_global 
  attr(resultados, 'n_poblacion')           <- n_poblacion 
  attr(resultados, 'elitismo')              <- elitismo
  attr(resultados, 'prob_mut')              <- prob_mut
  attr(resultados, 'metodo_seleccion')      <- metodo_seleccion
  attr(resultados, 'metodo_cruce')          <- metodo_cruce
  attr(resultados, 'parada_temprana')       <- parada_temprana
  attr(resultados, 'rondas_parada')         <- rondas_parada
  attr(resultados, 'tolerancia_parada')     <- tolerancia_parada
  
  
  # INFORMACI?N DEL PROCESO (VERBOSE)
  # ----------------------------------------------------------------------------
  if (verbose %in% c(1,2)) {
    cat("-----------------------", "\n")
    cat("Optimizaci?n finalizada", "\n")
    cat("-----------------------", "\n")
    cat("Duraci?n selecci?n  = ")
    print(difftime(end_time, start_time))
    cat("N?mero generaciones =", i, "\n")
    cat("L?mite inferior     =", paste(limite_inf, collapse = ", "), "\n")
    cat("L?mite superior     =", paste(limite_sup, collapse = ", "), "\n")
    cat("Optimizaci?n        =", optimizacion,"\n")
    cat("Valor alpha ?ptimo     =", mejor_individuo_global, "\n")
    cat("MSE ?ptimo encontrado   =", mejor_valor_global,"\n")
    cat("MAE     =", m1, "\n")
    cat("MASE     =", m3, "\n")
    cat("RMSE     =", m2, "\n")
    cat("\n")
  }
  return(resultados)
}
