
readQAP<-function(name){ 
  a <- read.delim(name,header=FALSE, sep ="")
  n<-as.integer(a[1,1])
  fl<-a[2:(n+1),1:n]
  dis<-a[(n+2):(n+n+1),1:n]
  d <- list(n=n, f= fl, d = dis)
  return(d)
}

evaluarQAP<-function(sol, f, d){
  
  acum<-0
  n<-length(sol)
  for(i in 1:n){
    for(j in 1:n){
      acum = acum + f[i,j]*d[sol[i],sol[j]]   
    }
  }
  return(acum)
}

swap<-function(sol,i,j){
  piv<-sol[i]
  sol[i]<-sol[j]
  sol[j]<-piv
  return(sol)
}

Inverse<-function(sol, largo, posInicial){
  aux<-array()
  if(largo+posInicial>length(sol)){
    return(sol)
  }
  arrParcial = array()
  for(i in 0:(largo-1)){
    arrParcial[largo-i] = sol[posInicial+i]
  }
  for (i in 1:posInicial-1) {
    aux[i] = sol[i]
  }
  for (i in 0:largo) {
    aux[i+posInicial] = arrParcial[i+1]
  }
  for (i in (posInicial+largo):length(sol)) {
    aux[i] = sol[i]
  }
  return(aux)
}

#leer instancia, crear y evaluar una solucion inicial
instancia<-readQAP("bur26a.dat")
sol<-c(1:instancia$n)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)
fitness

#generar soluci�n aleatoria y evaluar
sol <- sample(1:instancia$n,instancia$n,replace=F)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)
fitness

#generar 100 soluciones aleatorias y graficar
rm(fitness)
sol <- sample(1:instancia$n,instancia$n,replace=F)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)
for(i in 1:99){
  sol <- sample(1:instancia$n,instancia$n,replace=F)
  fitness<-c(fitness,evaluarQAP(sol,instancia$f,instancia$d))
}

plot(fitness,main="100 soluciones aleatorias", xlab = "Solutions")

#si ordenamos estas soluciones aleatorias...

plot(sort(fitness, decreasing=TRUE),main="100 soluciones aleatorias", xlab = "Solutions", ylab = "Fitness")


#intercambiar dos posiciones y evaluar la solucion
sol
x <- sample(1:instancia$n, 2, replace=F)
x
sol<-swap(sol,x[1],x[2])
evaluarQAP(sol,instancia$f,instancia$d)
sol

#generar 100 vecinos y graficar

sol <- sample(1:instancia$n,instancia$n,replace=F)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)
for(i in 1:100){
  x <- sample(1:instancia$n, 2, replace=F)
  sol<-swap(sol,x[1],x[2])
  fitness<-c(fitness,evaluarQAP(sol,instancia$f,instancia$d))
}

plot(fitness,main="100 soluciones vecinas", xlab = "Solutions")


#estrategia golosa (solo cambia si soluci�n vecina es mejor)
rm(fitness)
sol <- sample(1:instancia$n,instancia$n,replace=F)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)

for(i in 1:100){
  x <- sample(1:instancia$n, 2, replace=F)
  sol<-swap(sol,x[1],x[2])
  nuevo<-evaluarQAP(sol,instancia$f,instancia$d)
  if(nuevo<fitness[length(fitness)])
    fitness<-c(fitness,nuevo)
  else
    fitness<-c(fitness,fitness[length(fitness)])
}

plot(fitness,main = "Estrategia golosa", xlab = "Solutions")
print(paste("Mejor", fitness[length(fitness)]))
lines(fitness)



##
rm(fitness)
rm(fitness2)

sol <- sample(1:instancia$n,instancia$n,replace=F)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)
fitness2<-fitness
cont=0
for(i in 1:100){
  if(cont>=50){
    sol <- sample(1:instancia$n,instancia$n,replace=F)
    cont=0
  }else{
    x <- sample(1:instancia$n, 2, replace=F)
    sol<-swap(sol,x[1],x[2])
  }
  nuevo<-evaluarQAP(sol,instancia$f,instancia$d)
  fitness2<-c(fitness2,nuevo)
  if(nuevo<fitness[length(fitness)]){
    fitness<-c(fitness,nuevo)
    cont=0
  }
  else{
    fitness<-c(fitness,fitness[length(fitness)])
    cont=cont+1
  }
}

plot(fitness2,main = "Estrategia golosa", xlab = "Solutions")
lines(fitness)
print(paste("Mejor", fitness[length(fitness)]))



## MC
rm(fitness)
rm(fitness2)

sol <- sample(1:instancia$n,instancia$n,replace=F)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)
fitness2<-fitness
cont=0
x_best <- sol
for(i in 1:100){
  if(cont>=50){
    sol <- sample(1:instancia$n,instancia$n,replace=F)
    cont=0
  }else{
    
    x <- sample(1:instancia$n, 2, replace=F)
    while((x[1] == x_best[1]) || (x[1] == x_best[2]) || (x[2] == x_best[1]) || (x[2] == x_best[2]))
      x <- sample(1:instancia$n, 2, replace=F)
    
    sol<-swap(sol,x[1],x[2])
  }
  nuevo<-evaluarQAP(sol,instancia$f,instancia$d)
  fitness2<-c(fitness2,nuevo)
  if(nuevo<fitness[length(fitness)]){
    fitness<-c(fitness,nuevo)
    cont=0
    x_best <- x
  }
  else{
    fitness<-c(fitness,fitness[length(fitness)])
    cont=cont+1
  }
}

plot(fitness2, main = "Estrategia golosa", xlab = "Solutions")
lines(fitness)
print(paste("Mejor", fitness[length(fitness)]))



## DO
rm(fitness)
rm(fitness2)

sol <- sample(1:instancia$n,instancia$n,replace=F)
fitness<-evaluarQAP(sol,instancia$f,instancia$d)
fitness2<-fitness
cont=0
for(i in 1:30){
  if(cont>=50){
    sol <- sample(1:instancia$n,instancia$n,replace=F)
    nuevo<-evaluarQAP(sol,instancia$f,instancia$d)
    cont=0
  }else{
    
    x <- sample(1:instancia$n, 2, replace=F)
    sol1<-swap(sol,x[1],x[2])
    x <- sample(1:instancia$n, 2, replace=F)
    sol2<-swap(sol,x[1],x[2])
    x <- sample(1:instancia$n, 2, replace=F)
    sol3<-swap(sol,x[1],x[2])
    
    
    f1 <- evaluarQAP(sol1,instancia$f,instancia$d)
    f2 <- evaluarQAP(sol2,instancia$f,instancia$d)
    f3 <- evaluarQAP(sol3,instancia$f,instancia$d)
    
    if(f1<f2)
      if(f1<f3)
        sol <- sol1
    else
      sol <- sol3
    else
      if(f2<f3)
        sol <- sol2
    else
      sol <- sol3
    
    #nuevo<-min(evaluarQAP(sol1,instancia$f,instancia$d),evaluarQAP(sol2,instancia$f,instancia$d),evaluarQAP(sol3,instancia$f,instancia$d))
    nuevo<-evaluarQAP(sol,instancia$f,instancia$d)
  }
  
  fitness2<-c(fitness2,nuevo)
  if(nuevo<fitness[length(fitness)]){
    fitness<-c(fitness,nuevo)
    cont=0
  }
  else{
    fitness<-c(fitness,fitness[length(fitness)])
    cont=cont+1
  }
}

plot(fitness2, main = "Estrategia golosa", xlab = "Solutions")
lines(fitness)
print(paste("Mejor", fitness[length(fitness)]))


#########################################
#########################################












