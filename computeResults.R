computeResults<-function(result, alpha4, S){
  
  
  Bias<-c()
  MSE<-c()
  Variance<-c()
  c<-c()
  for (model in 1:8) {
    
    gamma<-result[,(2*model) -1]
    
    
    Bias[model]<-(1/S)*sum(gamma-alpha4)
    
    MSE[model]<-(1/S)*sum((gamma-alpha4)^2) 
    
    Variance[model]<-MSE-Bias^2
    
    c[model]<-(1/S)sum(result[,2*model])
    
  }
  
  
  return(list(Bias=Bias, MSE=MSE, Variance=Variance, c=c))
}