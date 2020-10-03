
Method_exp1<- function(SimData, model){
  
  
  Y<-SimData$Y
  A<-SimData$A
  
  X1<-SimData$X1
  X2<-SimData$X2
  X3<-SimData$X3
  
  
  
  
  
  if(model==1){
    
    PS <- glm(A ~ X1, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==2){
    
    
    
    PS <- glm(A ~ X2, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
    
  }else if(model==3){
    
    
    PS <- glm(A ~ X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==4){
    
    PS <- glm(A ~ X1 + X2, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==5){
    
    PS <- glm(A ~ X1 + X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==6){
    
    
    PS <- glm(A ~ X2 + X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
    
  }else if(model==7){
  
    
    PS <- glm(A ~ X1 + X2 +X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
  }else if(model==8){
    
    PS <- glm(A ~ 1, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
  }  
  
  
  
  
  bPS<-splines2::bSpline(PS, knots = quantile(PS, names = F)[2:4] ) #bs(PS, knots=quantile(PS, names = F)[2,4])
  
  k<-NCOL(bPS)
  
  colnames(bPS)<- paste0("B",1:k)
  
  
  SimData<-cbind(SimData,bPS)
  
    
  outmodel<-glm(formula = Y ~ 1 + B1 + B2 + B3 + B4 +B5 +B6 + A, family=poisson(link=log), data = SimData)
  
  gamma<-outmodel$coefficients[length(outmodel$coefficients)]
    
  c<-0#MISSING  
  
  output<-list(gamma=gamma, c=c)
  
  return(output)
  
}

