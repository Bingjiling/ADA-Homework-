pack = c("Sleuth3", "dplyr", "ggplot2", "car")
lapply(pack, library, character.only = TRUE)
Y = factor(c(0,0,1,1))
X = c(-2,-1,1,2)
logit = glm(Y ~ X - 1, family = "binomial")
## The maximuk likelihood of beta1 is 23.14.
## No, the estimate does not make sense.
predict.glm(logit,data.frame(X=0.5),type = "response")
## The estimate is 0.9999906. 
beta = 1:30
p = function(x){
  return(exp(x)/(1+exp(x)))
}
logL = log(1-p(-2*beta))+log(1-p(-1*beta))+log(p(1*beta))+log((2*beta))
plot(beta,logL)