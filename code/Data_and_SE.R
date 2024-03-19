Data_sim <- function(trial_opt=50,
                     C_opt=c(0.5,1,1.5),initial_opt=c(-1,1,3),
                     Distribution="response&confidence",sims=1000,Alpha=0.5,
                     theta=Theta,PFpar){
  print(PFpar)
  Data_all <- list()
  C_len=length(C_opt)
  initial_len=length(initial_opt)
  for (i in 1:C_len) {
    
    for (j in 1:initial_len) {
      k=(i-1)*3+j
      
      E1 = numeric()
      for (r in 1:sims){
        E1[r] = methods(Method="RM", distribution=Distribution, trials=trial_opt,
                        initial= initial_opt[j],Cvalue=C_opt[i],objective=Alpha,PFpar=PFpar)$estimate   # 模擬的估計值
      }
      E2 = numeric()
      for (r in 1:sims){
        E2[r] = methods(Method="RM_modified", distribution=Distribution, trials=trial_opt,
                        initial= initial_opt[j],Cvalue=C_opt[i],objective=Alpha,PFpar=PFpar)$estimate   # 模擬的估計值
      }
      E3 = numeric()
      for (r in 1:sims){
        E3[r] = methods(Method="ASA", distribution=Distribution, trials=trial_opt,
                        initial= initial_opt[j],Cvalue=C_opt[i],objective=Alpha,PFpar=PFpar)$estimate   # 模擬的估計值
      }
      E4 = numeric()
      for (r in 1:sims){
        E4[r] = methods(Method="ASA_modified", distribution=Distribution, trials=trial_opt,
                        initial= initial_opt[j],Cvalue=C_opt[i],objective=Alpha,PFpar=PFpar)$estimate   # 模擬的估計值
      }
      V <- c(var(E1),var(E2),var(E3),var(E4))
      bias <- c((mean(E1)-theta)^2,(mean(E2)-theta)^2,
                (mean(E3)-theta)^2,(mean(E4)-theta)^2)  
      title=paste("size:",as.character(C_opt[i]),"initial:",
                  as.character(initial_opt[j]))
      
      
      
      if (Distribution=="response&confidence"){
        label=c("RM","cRM","ASA","cASA")
        
      }
      else{
        label=c("RM","tRM","ASA","tASA")
      }
      simulate_result=list(method1=E1,method2=E2,method3=E3,method4=E4)
      
      
      
      data_set <- list(trial=trial_opt,C_val=C_opt[i],
                       initial= initial_opt[j],labels=label,
                       Variance=V,Bias=bias,
                       results=simulate_result)
      
      Data_all[[length(Data_all)+1]] <- data_set
      
      
    }
    
    
    
    
  }
  
  
  return(Data_all)
}



Monte_se <- function(Data,index='bias',theta=Theta){
  K <- lengths(Data[1])
  trial_par <- length(Data)
  std_error=list()
  if(index=='bias'){
    for (k in 1:K) {
      
      
      Se <- data.frame()
      for (c in 1:trial_par) {
        se <- Data[[c]][[k]]$Variance/1000 %>% sqrt()
        Se  <- rbind(Se,c(c*10,se))
        
      }
      colnames(Se) <- c("trial",Data[[1]][[1]]$labels)
      
      
      std_error[[length(std_error)+1]] <- Se
    }
    
    
  }
  
  
  if(index=='MSE'){
    for (k in 1:K) {
      
      
      Se <- data.frame()
      for (c in 1:trial_par) {
        se <- lapply(Data[[c]][[k]]$results, FUN  = function(x) sqrt(sum(  ( (x- theta)^2 -(sum((x-theta)^2)/1000) )^2 )/(1000*999)  ) )%>% unlist()
        Se  <- rbind(Se,c(c*10,se))
        
      }
      colnames(Se) <- c("trial",Data[[1]][[1]]$labels)
      
      
      std_error[[length(std_error)+1]] <- Se
    }
    
  }
  if(index=='Variance'){
    for (k in 1:K) {
      
      
      Se <- data.frame()
      for (c in 1:trial_par) {
        se <- Data[[c]][[k]]$Variance/(2*999) %>% sqrt()
        Se  <- rbind(Se,c(c*10,se))
        
      }
      colnames(Se) <- c("trial",Data[[1]][[1]]$labels)
      
      
      std_error[[length(std_error)+1]] <- Se
    }
    
  }
  
  return(std_error)
}
