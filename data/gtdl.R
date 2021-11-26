library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(survival)
library(survminer)
library(optimx)
library(ucminf)
library(msm)

datasetcovid_2 <- read_csv("datasetcovid.csv")

datasetcovid_2 <- datasetcovid_2 %>% mutate(INTER = 1)

dataset <- datasetcovid_2 %>% select(
  TEMP_SIN,
  EVOLUCAO,
  INTER,
  ASMA,
  CARDIOPATI,
  DIABETES,
  OBESIDADE,
  NU_IDADE_N,
  UTI,
  CS_ESCOL_N
)

### formatting the covariates

dados_beta = as.matrix(dataset)

dados_alpha = as.matrix(dataset$INTER)

################################################
#### Maximum Likelihood Estimation

### Function -log-likelihood GTDL gamma frailty model
## par = parameter vector
## data_ = matrix containing time, censorship indicator, intercept and covariates
## data_alpha = regression in the alpha parameter

log_like_beta_alpha_gama=function(par,dados_, dados_alpha){
  tempo=dados_[,1]
  cens=dados_[,2]
  x=dados_[,3:ncol(dados_)]
  x=as.matrix(x)
  dim=ncol(x)
  dados_alpha=as.matrix(dados_alpha)
  dim2=ncol(dados_alpha)
  tempo=as.matrix(tempo)
  alpha=matrix(par[1: dim2], ncol=dim2, nrow=1)
  beta=matrix(par[(dim2+1): (dim2+dim)], ncol=dim, nrow=1)
  theta=exp(par[(dim2+1+dim)])
  alpha_t=dados_alpha%*%t(alpha)
  funcao1=exp(x%*%t(beta))
  funcao2=exp(alpha_t*tempo+x%*%t(beta))
  funcao3=log((1+funcao2)/(1+funcao1))
  aux=+sum(cens*(x%*%t(beta)))+sum(alpha_t*tempo*cens) -sum(cens*log(1+funcao2))+sum((-cens-1/theta)*log(1+(theta/alpha_t)*funcao3))
  return(-aux)
}
### Function -log-likelihood GTDL model
## par = parameter vector
## data_ = matrix containing time, censorship indicator, intercept and covariates
## data_alpha = regression in the alpha parameter
log_like_beta_alpha <- function(par,dados_, dados_alpha){
  tempo=dados_[,1]
  cens=dados_[,2]
  x=dados_[,3:ncol(dados_)]
  x=as.matrix(x)
  dim=ncol(x)
  dados_alpha=as.matrix(dados_alpha)
  dim2=ncol(dados_alpha)
  tempo=as.matrix(tempo)
  alpha=matrix(par[1: dim2], ncol=dim2, nrow=1)
  beta=matrix(par[(dim2+1): (dim2+dim)], ncol=dim, nrow=1)
  alpha_t=dados_alpha%*%t(alpha)
  funcao1=exp(x%*%t(beta))
  funcao2=exp(alpha_t*tempo+x%*%t(beta))
  funcao3=log((1+funcao2)/(1+funcao1))
  aux=+sum(cens*(x%*%t(beta)))+sum(alpha_t*tempo*cens)-sum((cens+1/alpha_t)*log(1+funcao2))+sum((1/alpha_t)*log(1+funcao1))
  return(-aux)
}

###### Maximum likelihood estimate of the GTDL model
n = ncol(dataset) - 1
chute=rep(0.1, n)  #initial value
emv1=optimr(par=chute, log_like_beta_alpha, dados_= dados_beta, dados_alpha = dados_alpha, hessian=T,method='ucminf')  
emv1

### maximum likelihood estimate, standard error, and  asymptotic confidence interval 
result1=matrix(NA, ncol=10, nrow=9)
result1[,1]=emv1$par
result1[,2]=sqrt(diag(solve(emv1$hessian)))
result1[,3]=result1[,1]-1.645*result1[,2]
result1[,4]=result1[,1]+1.645*result1[,2]
##precisa acrescentar result1[,5]...result1[,10] ???
result1

###### Maximum likelihood estimate of the GTDL gamma frailty model 
chute2=rep(0.1, 10) #initial value
emv2=optimr(par=chute2, log_like_beta_alpha_gama, dados_=dados_beta, dados_alpha=dados_alpha, hessian=T,method='ucminf')
emv2

### maximum likelihood estimate, standard error, and  asymptotic confidence interval 
result2=matrix(NA, ncol=10, nrow=10)
result2[,1]=c(emv2$par[1:3],exp(emv2$par[4]))
result2[,2]=c(sqrt(diag(solve(emv2$hessian)))[1:3], deltamethod (~ exp(x4), emv2$par, solve(emv2$hessian)))
result2[,3]=result2[,1]-1.645*result2[,2]
result2[,4]=result2[,1]+1.645*result2[,2]
##precisa acrescentar result1[,5]...result1[,10] ???
result2

################################################################################
#### Residual analysis

### reliability calculation based on the GTDL model
#time= time 
#x= covariates
#par= maximum likelihood estimate
confiabilidade_gtdl=function(time, x, par){
  x=as.matrix(x)
  dim=ncol(x)
  alpha=par[1]
  beta=matrix(par[2: (1+dim)], ncol=dim, nrow=1)
  xbeta=x%*%t(beta)
  funcao1=exp(alpha*time+xbeta)
  funcao2=exp(xbeta)
  aux=((1+funcao1)/(1+funcao2))^(-1/alpha)
  return(aux)
}

### residual plot + envelope
## argument x=residuals
#possivel problema nos plots???
envelopeDS <- function(x){
  U	         <- x
  n	         <- length(x)
  d2s 	     <- sort(U)
  xq2 	     <- qnorm(ppoints(n))
  Xsim 	     <- matrix(0, 100, n)
  for(i in 1:100){
    u2       <- rnorm(n)
    Xsim[i,] <- u2
  }
  Xsim2      <- apply(Xsim, 1, sort)
  d21        <- matrix(0, n, 1)
  d22        <- matrix(0, n, 1)
  for(i in 1:n){
    d21[i]  <- quantile(Xsim2[i,], 0.025)
    d22[i]  <- quantile(Xsim2[i,], 0.975)
  }
  d2med      <- apply(Xsim2, 1, mean)
  fy         <- range(d2s, d21, d22)
  plot(xq2, d2s, xlab = quote("Theoretical Quantiles"),
       ylab = quote("Quantile Residuals"), 
       pch = 20, ylim = fy)
  par(new = T)
  plot(xq2, d21, type = "l", ylim = fy, xlab = "", ylab = "", lwd=1.2)
  par(new = T)
  plot(xq2, d2med, type = "l", ylim = fy, xlab = "", ylab = "", lwd=2)
  par(new = T)
  plot(xq2, d22, type = "l", ylim = fy, xlab = "", ylab = "", lwd=1.2)
}


## residuals plot 
dim(dataset)
x = dados_beta[,3:10]
conf = confiabilidade_gtdl(dataset$TEMP_SIN,x, emv1$par)
qr <- qnorm(dataset$EVOLUCAO* (1 - conf) + (1-dataset$EVOLUCAO)*runif(nrow(x),1-conf))
envelopeDS(qr) 


### Global influence analysis
gd_i = numeric()  #cook's distance
ld = numeric()  #likelihood distance
convergencia=numeric()
#problemas com o for????
for(j in 1:nrow(x)){
  dados_alpha_=dados_alpha[-j,]
  dados_beta_=dados_beta[-j,]
  chute=emv1$par
  emv_aux=optimr(par=chute, log_like_beta_alpha, dados_=dados_beta_, dados_alpha=dados_alpha_, hessian=T,method='ucminf')
  convergencia[j]=emv_aux$convergence
  gd_i[j]=t(emv1$par-emv_aux$par)%*%solve(emv1$hessian)%*%(emv1$par-emv_aux$par)
  ld[j]=2*(-emv1$value+emv_aux$value)  
}


summary(convergencia)
summary(gd_i)
summary(ld)

## cook's distance
plot(seq(1,nrow(x)), gd_i, type='h', lwd=2, ylab="Generalized Cook Distance" , xlab="Index", cex.lab=1.5, cex.axis=1.5)

#likelihood distance
plot(seq(1,nrow(x)), ld, type='h', lwd=2, ylab="Likelihood Distance ", xlab="Index", cex.lab=1.5, cex.axis=1.5)

################################################################################