distribucion<-function(n,m){
  while(n<m){
    imprime<-pnorm(n:(n+0.0001),0,1)
    print(imprime)
    n<-n+0.0001
  }
  
}

distri=distribucion(-1,-0.999)


#Hecho por Juan Felipe Vales, Michael Rincon, Wilson Martinez, Oscar Torres