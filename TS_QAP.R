# arr de la lista tabu y la lista de los vecinos encontrados
# esta función debe devolver la mejor solución sin que haya sido prohibido
# o a menos que este todo prohibido nos, quedaremos con la mejor solución
# si es que la encuentra
# i = tamaño de la intensidad, cuyo valor por defecto es 1
BuscarVecinoTabu <- function(tabuShortArr, tabuLongArr, sv, fit, intensidad, penalizacion, t_largo){
  #pos_min = which.min(sv)
  # si el movimiento no esta prohibido para es min
  #if(tabuArr[pos_min][i_] == 0){
  #  r = list(pos = pos_min, fit = sv[pos_min])
  #  return(r)
  #}
  i = intensidad
  new_sv = sort(sv, decreasing = F)
  temporal = 0
  #comportamiento de memoria de mediano plazo, guardamos los top 5 vecinos.
  for (k in 1:3) {
    min = new_sv[k]
    pos_real = as.numeric(match(c(min), sv))
    i_ = as.numeric(pos_real + i)
    if(i_ > 26){
      i_ = as.numeric(i_ -26)
    }
    tabuLongArr[pos_real,i_] = as.integer(tabuLongArr[pos_real,i_]) + 1
  }
  #comportamiento de memoria de corto plazo
  #añadir la penalización para el caso de la memoria de largo plazo
  for (j in 1:length(sv)) {
    min = new_sv[j]
    pos_real = match(c(min), sv)
    i_ = pos_real + i
    if(i_ > 26){
      i_ = i_ -26
    }
    #si no está prohibido, devuelve el mejor valor de la vecindad.
    if(tabuShortArr[pos_real, i_] == 0 & tabuLongArr[pos_real, i_] < t_largo){
      r = list(pos = pos_real, fit = sv[pos_real], tsm = as.matrix(tabuLongArr))
      return(r)
    }
    #si está prohibido, entonces devolverá el siguiente mejor valor de la vecindad
    #siempre cuando sea mejor que el fit actual
    else{
      #si la encuentra en la memoria, se revisa si es mejor solución de igual manera.
      if(tabuLongArr[pos_real, i_] >= t_largo ){
        min = min * penalizacion
      }
      if(min < fit & min < temporal){
        temporal = min
      }
    }
  }
  valor_min_tabu = match(c(temporal), sv)
  r = list(pos = valor_min_tabu, fit = temporal,  tsm = as.matrix(tabuLongArr))
  return(r)
}

restar = function(x){
  if(x > 0){
    x-1
  }
  else{
    0
  }
}

TS_QAP<-function(filename, tiempoLocal, tiempoEspacio, intensidad, mCorto, mLargo, penalizacion, nVecinos, grafico = 1){
  
  instancia<-readQAP(filename)
  
  #
  tLocal <- 0
  tEspacio <- 0
  sol <- sample(1:instancia$n,instancia$n,replace=F)
  fitness<-evaluarQAP(sol,instancia$f,instancia$d)
  largo = length(sol)
  tabu_long <- matrix(0, nrow = largo, ncol = largo)
  tabu_short <- matrix(0, nrow = largo, ncol = largo)
  print("solucion inicial")
  print(sol)
  #variables que deben ser dinámicas
  t_corto = mCorto
  t_largo = mLargo
  #
  soluciones <- fitness
  while (tEspacio < tiempoEspacio) {
    #Se guardaran las soluciones, donde la posicion indica la permutacions realizada
    #En este caso serán solo 25 permutaciones, ya que el último elemento no puede permutar, bajo la lógica
    #del swap. sv = soluciones vecinas
    sv = matrix(nrow = 1, ncol = largo-1)
    # intensificación
    for (j in 1:intensidad) {
      tLocal <- 0
      while(tLocal < tiempoLocal ){
        #Vamos a generar soluciones vecinas
        for (i in 1:nVecinos) {
          ij_ = i+j
          if( ij_>26){
            ij_ = ij_ - 26
          }
          vecino<-Inverse(sol,i, ij_)
          Cvecino<-evaluarQAP(vecino,instancia$f,instancia$d)
          if(!any(is.na(vecino))){
            sv[i] <- Cvecino  
          }
        }
        #encuentro la posición del swap realizado
        #guardo este movimiento en la memoria de corto plazo si es que no lo ha sido anteriormente
        p = BuscarVecinoTabu(tabu_short, tabu_long, sv, fitness, intensidad = j, penalizacion = penalizacion, t_largo)
        pos = as.integer(p$pos)
        j_ = pos + j
        if(j_ > 26){
          j_ = j_ - 26
        }
        #prohibición
        tabu_short[pos,j_] = t_corto
        tabu_short = apply(tabu_short,c(1, 2), restar)
        sol <-swap(sol,pos, j_)
        tabu_long = p$tsm
        soluciones<-c(soluciones,p$fit)
        tLocal <- tLocal + 1
      }
    }
    #tabu_long = apply(tabu_long,c(1, 2), restar)
    #tabu_short <- matrix(0, nrow = largo, ncol = largo)
    tEspacio <- tEspacio + 1
  }
  if(grafico == 1){
    plot(soluciones,type="l",main = "Tabu Search")
    lines(soluciones)
  }
  e = evaluarQAP(sol,instancia$f,instancia$d)
  return(list(ts = tabu_short, sol = sol, fit = e, tsm = tabu_long))
}

TS_QAP('bur26a.dat', tiempoLocal = 3, tiempoEspacio = 12, intensidad = 10, mCorto = 5, mLargo = 10, penalizacion = 0.5, nVecinos =  15)




