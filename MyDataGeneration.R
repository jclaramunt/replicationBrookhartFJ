
MyDataGeneration <- function(samp, b0,b1,b2,b3,a0,a1,a2,a3,a4=0.5, std1=1, std2=1, std3=1){
  
  X1 <- rnorm(samp, 0, std1) 
  X2 <- rnorm(samp, 0, std2)
  X3 <- rnorm(samp, 0, std3)
  A<- mc2d::rbern(samp, prob=pnorm(q=b0+b1*X1+b2*X2+b3*X3))
  meanY<-exp(a0+a1*(((1+exp(-3*X1))^-1)-0.5)+a2*X2+a3*X3+a4*A) #option1
  #meanY<- exp(a0+a1*X1+a2*X2+a3*X3+a4*A) #option2  
  Y <- stats::rpois(samp,lambda = meanY) 
  dat <- data.frame(Y,A,X1,X2,X3)
  return(dat) 
}

