
Estimation = function(X, Est_method = "last")
{

  reversion = 0
  N = length(X)
  R = xs = numeric()
  s = 1
  xs[1] = X[1]
  
  for (i in 2:N)
  {
    if (X[i]!=X[i-1])
    {
      s = s+1
      xs[s] = X[i]
    }
  }   
  N2 = length(xs)
  
  for (n in 1:N2)
  {
    if ((n>1) & (n<N2))
    {
      if (((xs[n]>xs[n-1]) & (xs[n]>xs[n+1])) | ((xs[n]<xs[n-1]) & (xs[n]<xs[n+1])))
      {
        reversion = reversion+1
        R[reversion] = xs[n]
      }
    }
  }  
  
  if (Est_method=="last")
    estimate = X[N]  
  else if (Est_method=="mode")
  {
    Xest <- table(xs)
    Xest = as.numeric(names(Xest[max(Xest)==Xest]))   
    if (length(Xest)==1)
      estimate = Xest
    else
      estimate = mean(Xest)
  }
  else if (Est_method > 0)   
  {
    last = Est_method
    if (last > reversion)
    {
      cat("It seems that there are more reversion arrangements than estimated...","\n")
      estimate = X[N]
    }
    else if (last <= 1)
    {
      cat("Might need more number of reversion","\n")
      estimate = X[N]
    }
    else
    {
      last = last%/%2*2    
      estimate = mean(R[(reversion-last+1):reversion])
    }
  }
  else if (Est_method <= 0)  
  {
    out = abs(Est_method)
    if ((reversion-out) < 0)
    {
      cat("","\n")
      estimate =X[N]
    } 
    else if ((reversion-out) <=1)
    {
      cat("","\n")
      estimate =X[N]
    }
    else
    {
      add = (reversion-out)%%2
      estimate = mean(R[(out+add+1):reversion])
    }
  }
  else {
    cat("","\n")}
  return(list(estimate = estimate, reversalN = reversion))
}
## RM method and its modification





RM_modified  = function(objective, trials, initial, Cvalue, distribution,k=0,Upper=RTpar[5],Lower=RTpar[1]*0.001,PFpar)
{
  x = d = z = numeric()
  x[1] = initial
  #開始進行模擬
  for (n in 1: (trials))
  {
    Res= response(distribution, x[n],PFpar.=PFpar)
    z[n]=Res[1] 
 
    if (k > 0){
      d[n] = (Cvalue/(round(n/k)+1))*(z[n]-objective)}

    else{
      if (distribution== "response&confidence"){
      
        d[n] = 2*(Cvalue/n*0.2*Res[2])*(z[n]-objective)   
        x[n+1] = x[n]-d[n]   
      }
      else if (distribution== "response&RT"){

        d[n] = 2*(Cvalue/n*(0.01+(Upper-Res[2])/(Upper-Lower)))*(z[n]-objective)  
        
        x[n+1] = x[n]-d[n]  
        
      }
      else if (distribution== "response&TN"){
        d[n] = (Cvalue/n*(Res[2]))*(z[n]-objective) 
        x[n+1] = x[n]-d[n]   
        
      }
    }
    
  }
  reversion = Estimation(x)$reversalN
  return(list(estimate = x[trials+1], trials = x, responses = z, percentile = objective, reversal.number =reversion))
}

RM= function(objective, trials, initial, Cvalue, distribution, k=0,PFpar)
{
  x = d = z = numeric()
  x[1] = initial

  for (n in 1: (trials)){
    Res= response(distribution, x[n],PFpar.=PFpar)
    z[n] = Res[1]

    if (k > 0){
      d[n] = (Cvalue/(round(n/k)+1))*(z[n]-objective)
    }
    
   
    else{
      d[n] = (Cvalue/n)*(z[n]-objective)  
     
    }
    x[n+1] = x[n]-d[n]  
  }
  reversion = Estimation(x)$reversalN
  return(list(estimate = x[trials+1], trials = x, responses = z, percentile = objective, reversal.number =reversion))
}

ASA = function(objective, trials, initial, Cvalue, distribution, k=0,PFpar)
{
  x = m = d = z = R = numeric()
  x[1] = initial
  Res1 =response(distribution, x[1],PFpar.=PFpar)   
  z[1] =Res1[1]
  m[1] = 1
  d[1] = (Cvalue/m[1])*(z[1]-objective)
  x[2] = x[1]-d[1]
  Res2 = response(distribution, x[2],PFpar.=PFpar)
  z[2] =Res2[1]
  m[2] = 2
  d[2] = (Cvalue/m[2])*(z[2]-objective)
  x[3] = x[2]-d[2]

  for (n in 3: (trials))
  {
    Res= response(distribution, x[n],PFpar.=PFpar)
    z[n]=Res[1]
    if (z[n]!=z[n-1])
      m[n] = m[n-1]+1
    else
      m[n] = m[n-1]   
  
    d[n] = (Cvalue/m[n])*(z[n]-objective)
    x[n+1] = x[n]-d[n]
  }
  reversion = Estimation(x)$reversalN
  list(estimate = x[trials+1], trials = x, responses = z, percentile = objective, reversal.number = reversion)
}

########
ASA_modified  = function(objective, trials, initial, Cvalue, distribution,k=0,Upper=RTpar[5],Lower=RTpar[1]*0.001,PFpar)
{
  x = m = d = z = R = numeric()
  x[1] = initial
  Res1 =response(distribution, x[1],PFpar.=PFpar)
  z[1] =Res1[1] 
  m[1] = 1
  
  if (k > 0){
    d[1] = (Cvalue/(round(m[1]/k)+1))*(z[1]-objective)}
 
  else{
    if (distribution== "response&confidence"){
      #print(Res[2])
      d[1] = 2*(Cvalue/m[1]*0.2*Res1[2])*(z[1]-objective)  
      x[2] = x[1]-d[1]   
    }
    else if (distribution== "response&RT"){
      d[1] = 2*(Cvalue/m[1]*(0+(Upper-Res1[2])/(Upper-Lower)))*(z[1]-objective)  
      
      x[2] = x[1]-d[1]   
      
    }
    else if (distribution== "response&TN"){
      d[1] = (Cvalue/m[1]*(Res1[2]))*(z[1]-objective) 
      x[2] = x[1]-d[1] 
      
    }
    
  }
  
  Res2 =response(distribution, x[2],PFpar.=PFpar)  
  z[2] = Res2[1] 
  m[2] = 2
  d[2] = (Cvalue/m[2])*(z[2]-objective)
  if (k > 0){
    d[2] = (Cvalue/(round(m[2]/k)+1))*(z[1]-objective)}
  
  else{
    if (distribution== "response&confidence"){
     
      d[2] = 2*(Cvalue/m[2]*0.2*Res2[2])*(z[2]-objective)   
      x[3] = x[2]-d[2]  
    }
    else if (distribution== "response&RT"){
      d[2] = 2*(Cvalue/m[2]*(0.01+(Upper-Res2[2])/(Upper-Lower)))*(z[2]-objective)
      
      x[3] = x[2]-d[2]  
      
    }
    else if (distribution== "response&TN"){
      d[2] = (Cvalue/m[2]*(Res2[2]))*(z[2]-objective)   
      x[3] = x[2]-d[2]  
      
    }
  }
  
  
  

  for (n in 3: (trials))
  {
    Res= response(distribution, x[n],PFpar.=PFpar)
    z[n]=Res[1] 
    if (z[n]!=z[n-1])
      m[n] = m[n-1]+1
    else
      m[n] = m[n-1]   
  
    if (k > 0){
      d[n] = (Cvalue/(round(m[n]/k)+1))*(z[n]-objective)}
    
 
    else{
      if (distribution== "response&confidence"){
     
        d[n] = 2*(Cvalue/m[n]*0.2*Res[2])*(z[n]-objective)   
        x[n+1] = x[n]-d[n]   
      }
      else if (distribution== "response&RT"){
        d[n] = 2*(Cvalue/m[n]*(0.01+(Upper-Res[2])/(Upper-Lower)))*(z[n]-objective)   # 這才是主要的部份： 隨trial增加，分母增加
        
        x[n+1] = x[n]-d[n]  
        
      }    else if (distribution== "response&TN"){
        d[n] = (Cvalue/m[n]*(Res[2]))*(z[n]-objective) 
        x[n+1] = x[n]-d[n] 
        
      }
      
    }
  }
  reversion = Estimation(x)$reversalN
  return(list(estimate = x[trials+1], trials = x, responses = z, percentile = objective, reversal.number =reversion))
}





# 12) Simulation with particular method
methods = function(Method, trials, initial, Cvalue, distribution,objective,PFpar)
{

   
  #print(objective)
  if (Method=="RM"|Method=="SA"){
    result= RM(objective, trials, initial, Cvalue, distribution, k=0,PFpar=PFpar)
  }
  
  
  else if (Method=="RM_modified"){
    result=RM_modified(objective, trials, initial, Cvalue, distribution,k=0,Upper=RTpar[5],Lower=RTpar[1]*0.001,PFpar=PFpar)}
  else if (Method=="ASA"){
    result=ASA(objective, trials, initial, Cvalue, distribution, k=0,PFpar=PFpar)}
  
  else if (Method=="ASA_modified"){
    result=ASA_modified(objective, trials, initial, Cvalue, distribution,k=0,Upper=RTpar[5],Lower=RTpar[1]*0.001,PFpar=PFpar)}
  
  
  else{
    cat(" incorrect adaptive method，please type RM (SA), RM_modified, ASA, or ASA_modified","\n")
  }
  return(result)
}



