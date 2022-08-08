pow<- function(y,salto) {
  alpha=salto
  Datos_CME=cbind() # Almacenamiento del CME (Error cuadratico medio (MSE))
  alpha1=alpha
  
  tic()
  while (alpha<1) # No puede ser mayor a 1
  {
    sua=HoltWinters(y,alpha,beta = FALSE,gamma=FALSE) # Metodo de Holtwinters 
    s1=sua$fitted[,1] # Valores de suavizamiento
    err_pr_sua=y[2:(length(y))]-s1 # Error del pronostico de suavizamiento
    err_pr_sua2=err_pr_sua^2       # Error del pronostico con suavizamiento al cuadrado
    CME_S=sum(err_pr_sua2)/length(s1)# CME superior
    Datos_CME=cbind(Datos_CME,c(CME_S,alpha))
    alpha=alpha+alpha1 
    
    m=(Datos_CME[2,]) # Numero de saltos
    al=(Datos_CME[1,]) # Alpha CME
    CME_min=min(Datos_CME[1,]) # Valor de CME
    valor_alpha=m[which.min(Datos_CME[1,])]
    min_CME=m[which.min(Datos_CME[1,])]
  }
  m1<-mae(y[2:(length(y))],s1)
  m2<-rmse(y[2:(length(y))],s1)
  m3<-mase(y[2:(length(y))],s1,step_size = 1)
  print(paste("Valores de Alpha:"))
  print(m) # Numero de saltos
  print(paste("Valores de MSE:"))
  print(al) # Valores de CME
  print(paste("Saltos de =",salto))
  print(paste("Alpha Optimo =",valor_alpha))
  print(paste("MSE Minimo =",CME_min))
  print(paste("MAE =",m1))
  print(paste("RMSE =",m2))
  print(paste("MASE =",m3))
  print(paste("Tiempo de Ejecucion:"))
  toc()    
}



