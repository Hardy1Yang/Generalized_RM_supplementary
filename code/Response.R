
## theoretical value psychometric function
## theoretical value auxiliary  function



###change

# Psychometric function
PF = function(x, sd = PFpar[1], pl = PFpar[2], pg = PFpar[3],mu=PFpar[4])
{
  beta = (1/sd)*log(log(0.01/(1-pl-pg))/log((1-pl-pg -0.01)/(1-pl-pg)),10)
  alpha = 10^(mu)*(log((1-pl-pg)/(1-pl-(1-pl+pg)/2)))^(-1/beta)
  Prob <- (1-pl)-(1-pl-pg)*exp((-1)*(10^x/alpha)^beta)
  return(Prob)
}



## theoretical value auxiliary  function(confidence level & censored RT)

### confidence level (five level ,discretized normal response with fixed cutoff)
## mu is related to I(intensity)



# response confidence
Confidence_lv=function(I,ConPar=ConfidentPar){
  cutoff <- c(ConPar[2],ConPar[3],ConPar[4],ConPar[5])
  ConSize <- c(1,2,3,4,5)
  sd=ConPar[1]
  multicdf<-  pnorm(cutoff,mean=I,sd=sd)
  multiprob=c(multicdf[1],multicdf[2]-multicdf[1],
              multicdf[3]-multicdf[2],multicdf[4]-multicdf[3],
              1-multicdf[4])
  choice <- rmultinom(1,1,multiprob)
  reponse <- sum(ConSize*choice)
  return(reponse)
}


## assume the ratio of sd and mean of RT is non decreasing 
## function of deviation (X- theta)
### censored RT (Weibull distribution)



## Response  time distributions

iWeibullT = function(rt,I,r0 = RTpar[1], K = RTpar[2], w = RTpar[3], tho = RTpar[4])
{
  a = b = MRT = Want = d = numeric()
  t = rt-r0
  solvea = function(a){ (gamma(1+2*a^(-1))/gamma(1+a^(-1))^2)-1}	#解根

  MRT = r0+K*10^(I*(- tho))	# Piéron's law
  Want= (w*MRT)^2/(MRT-r0)^2
  a = uniroot(function(x) solvea(x)-Want,c(0.018,10))$root
  b = (MRT-r0)/gamma(1+a^(-1))
  d = dweibull(t, shape=a, scale=b)
  return(d)
}

## function of I ,which is the devation of  X and theta
rWbT=function(I, r0 = RTpar[1], K = RTpar[2], w = RTpar[3], tho = RTpar[4])
{
  MRT = r0+K*10^(I*(-tho))
  solvea = function(a) (gamma(1+2*a^(-1))/gamma(1+a^(-1))^2)-1
  Want = (w*MRT)^2/(MRT-r0)^2
  if (MRT>1e+150)
  {
    1e+120
    #print("too big")
  }
  else if (Want==Inf)
  {
    r0
    ##print("too small")
  }
  else
  {
    a = uniroot(function(x) solvea(x)-Want,c(0.018,10))$root
    b = (MRT-r0)/gamma(1+a^(-1))
    rweibull(1, shape=a, scale=b)+r0
  }
}
## joint response generate

response = function(distribution, intensity,theta=Theta,range=1,PFpar.=PFpar,RTpar.=RTpar)
{
  Upper=RTpar.[5]
  
  if (distribution== "response&confidence")
  {
    z = 1
    if (runif(1) > PF(intensity, PFpar.[1], PFpar.[2], PFpar.[3])){z = 0}

    deviant <- abs(intensity-theta)
    Confident <- Confidence_lv(deviant)
    Res <- c(z,Confident)
      }
  else if (distribution== "response&RT")
  {
    z = 1
    if (runif(1) > PF(intensity, PFpar.[1], PFpar.[2], PFpar.[3])){z = 0}
 
    deviant <- abs(intensity-theta)
    RT <- rWbT(deviant)
    RT=RT/1000
    if (RT>Upper){RT=Upper}
    Res <- c(z,RT)
  }
  else if (distribution== "response&TN") # truncated normal response 
  {
    z = 1
    if (runif(1) > PF(intensity, PFpar.[1], PFpar.[2], PFpar.[3])){z = 0}
    d <- abs(intensity-theta)
    M=2
    if(d>=2){
      
      R <- M
    }else{
      
      A=range*0.5
      B=range*12
      mu <- d/A
      sd <- d/B
      TN <- rtnorm(1, mean =mu , sd = sd, lower=0, upper=Inf)+0.1
      R <- min(TN,2)
    }
    
    
    Res <- c(z,R)
  }
  else{
    print("distribution incorrect, please type response&confidence,response&RT, or response&TN \n")}
  if ((distribution== "response&confidence")|(distribution== "response&RT")|(distribution== "response&TN")){
      return(Res)}
}









