SA_QAP<-function(filename, alpha, T_max, T_min, iteraciones, repetitions, grafico = 1){
  
  
  instancia<-readQAP(filename)
  
  rep <- 0
  a<-NULL
  
  while (rep < repetitions) {
    
    
    sol <- sample(1:instancia$n,instancia$n,replace=F)
    fitness<-evaluarQAP(sol,instancia$f,instancia$d)
    
    T <- T_max
    delta<-0
    soluciones<-fitness
    malas <- 0
    
    while(T>T_min){
      iter <- 0
      
      while (iter < iteraciones) {
        
        #Generate neighbour solution
        x <- sample(1:instancia$n, 2, replace=F)
        vecino<-swap(sol,x[1],x[2])
        Cvecino<-evaluarQAP(vecino,instancia$f,instancia$d)
        delta <- Cvecino - fitness
        if(delta < 0){ #aceptar soluci??n vecina
          
          sol <- vecino
          fitness <- Cvecino
        }
        else {
          
          prob <- exp(-delta/T)
          
          if(runif(1,0,1) < prob){
            sol <- vecino
            fitness <- Cvecino
            malas <- malas + 1
          }
        }
        
        
        
        iter <- iter + 1
        
        soluciones<-c(soluciones,fitness)
      }  
      
      T=T*alpha
      
    }
    if(grafico == 1){
      plot(soluciones,type="l",main = "Simulated Annealing")
      lines(soluciones)
    }
    a<-rbind(a,c(filename, alpha, T_max, T_min, iteraciones, as.numeric(soluciones[length(soluciones)]), malas))
    rep<-rep + 1
  }
  
  colnames(a) <- rbind("instancia", "alpha", "T_max", "T_min", "iteraciones", "mejor", "malas")
  A<-transform(a, mejor = as.numeric(mejor))
  return(A)
}

SA_QAP("bur26a.dat", 0.8, 1000000, 10, 10, 100)
