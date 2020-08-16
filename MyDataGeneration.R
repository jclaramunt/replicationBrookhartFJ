

#Comparison of power of Mann-Whitney U test and power of t-test
# In group 1 variable Y is from a normal distribution with mu = 0 and sigma = 1
# In group 2 variable Y is from a normal distribution with mu = 0 + es and sigma = 1
# es is "effect size", which is a factor in the simulation design (either 0.2, 0.5, or 0.8) 
# samp = sample size of group 1 = sample size of group 2
# samp = 10, 20, 40, 80
MyDataGeneration <- function(samp, b0,b1,b2,b3,a0,a1,a2,a3,a4){
  varX1 <- rnorm(samp, 0, 1) 
  varX2 <- rnorm(samp, 0, 1)
  varX3 <- rnorm(samp, 0, 1)
  A<- mc2d::rbern(samp, prob=pnorm(q=b0+b1*varX1+b2*varX2+b3*varX3))
  meanY<-exp(a0+a1*(((1+exp(-3*varX1))^-1)-0.5)+a2*varX2+a3*varX3+a4*A) #option1
  #meanY<- exp(a0+a1*varX1+a2*varX2+a3*varX3+a4*A) #option2  
  Y <- stats::rpois(samp,lambda = meanY) 
  group <- as.factor(c(rep(1, samp), rep(2,samp)))
  dat <- data.frame(Y,group)
  return(dat) 
}

