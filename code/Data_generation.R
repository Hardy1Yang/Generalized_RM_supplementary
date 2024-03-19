rm(list = ls())
setwd("YOUR WORKING DIRECTORY")
source("code/import_all.R")


{
  sink(file = paste0(log.dir,"log_file_for_data_generation.log"), append = FALSE, type = c("output", "message"),
       split = TRUE)
  
  scale_setting <- c(0.5,1)
  #Other (default) parameter
  print("ConfidentPar")
  print(ConfidentPar)

  print("RTpar")
  print(RTpar)

  
  print("PFpar")
  print(PFpar)
  
  
  
  
  
Trials <- 15
  
  
  seed_code <- 5566
  print("seed_code")
  print(seed_code)
  Alpha_list <- c(0.25,0.5,0.75)
  print("Alpha_list")
  print(Alpha_list)
  set.seed(seed_code)
  Alpha_list <- c(0.25,0.5,0.75)
  Total_conf <- list()
  Total_RT <- list()
  

  PFpar[1]=scale_setting[1]


  for (m in 1:length(Alpha_list)) {
    scale_PF <- PFpar[1]
    alpha=Alpha_list[m]
    Theta=uniroot(function(x) PF(x,sd = PFpar[1], pl = PFpar[2], pg = PFpar[3],mu=PFpar[4])-alpha,c(-10,10))$root
    difference=c(-3,-2,-1,1,2,3)
    Ini_d <- difference*scale_PF+Theta
    Confidence <- list()
    print("Theta")
    print(Theta)
    print("alpha")
    print(alpha)
    print(paste("Confidence with",alpha,"alpha start"))
    Start <- Sys.time()
    for (i in 1:Trials) {
      k=i*10
      data_set <- Data_sim(trial_opt=k, initial_opt=Ini_d,
                           Distribution="response&confidence",Alpha=alpha,theta=Theta,PFpar=PFpar)
      Confidence[[length(Confidence)+1]] <- data_set
    }
    print(paste("Confidence with",alpha,"alpha done"))
    print(Sys.time()-Start)
    RT <- list()
    print(paste("RT with",alpha,"alpha start"))
    Start <- Sys.time()
    for (i in 1:Trials) {
      k=i*10
      data_set <- Data_sim(trial_opt=k, initial_opt=Ini_d,
                           Distribution="response&RT",Alpha=alpha,theta=Theta,PFpar=PFpar)
      RT[[length(RT)+1]] <- data_set
    }
    print(paste("RT with",alpha,"alpha done"))
    print(Sys.time()-Start)
    Total_conf[[m]] <- Confidence
    Total_RT[[m]] <- RT
    
  }
  
  
  saveRDS(Total_conf, file=paste0(data.dir,"Confidence_data_scale_",PFpar[1],".rds"))
  saveRDS(Total_RT, file=paste0(data.dir,"RT_data_scale_",PFpar[1],".rds"))
  sink()
}
