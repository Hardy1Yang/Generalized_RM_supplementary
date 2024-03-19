

{rm(list = ls())
#PFpar = c(0.5, 0, 0,10^-1.5) 
Alpha_list <- c(0.25,0.5,0.75)
Alpha_list2 <- c("quater","half","third_quater")
main <- "C:/Users/88697/Desktop/adaptive method/Adaptive_method_code_JMP"
setwd(main)
source("code/import_all.R")
T_set <- c(2:15)
  Total_conf=readRDS(paste0(data.dir,"Confidence_data_scale_",PFpar[1],".rds"))
  
  Total_RT=readRDS(paste0(data.dir,"RT_data_scale_",PFpar[1],".rds"))
  # Hierarchy: alpha, trial, combination of ini and step size
  #Total_RT[[3]][[7]][[1]]$initial
  
  total_RT_MSE_se <- list()
  total_conf_MSE_se <- list()
  
  total_RT_bias_se <- list()
  total_conf_bias_se <- list()
  
  total_RT_sd_se <- list()
  total_conf_sd_se <- list()
  
  for (i in 1:length(Alpha_list)) {
    alpha=Alpha_list[i]
    Theta=uniroot(function(x) PF(x,sd = PFpar[1], pl = PFpar[2], pg = PFpar[3],mu=PFpar[4])-alpha,c(-10,10))$root
    total_RT_MSE_se[[i]] <- Monte_se(Total_RT[[i]],index='MSE',theta = Theta)
    total_conf_MSE_se[[i]] <- Monte_se(Total_conf[[i]],index='MSE',theta = Theta)
    total_RT_sd_se[[i]] <- Monte_se(Total_RT[[i]],index='Variance',theta = Theta)
    total_conf_sd_se[[i]] <- Monte_se(Total_conf[[i]],index='Variance',theta = Theta)
    total_conf_bias_se[[i]] <- Monte_se(Total_conf[[i]],index='bias',theta = Theta)
    total_RT_bias_se[[i]] <- Monte_se(Total_RT[[i]],index='bias',theta = Theta)
    
  }


  for (i in 1:length(Alpha_list)) {
    alpha=Alpha_list[i]
    Theta=uniroot(function(x) PF(x,sd = PFpar[1], pl = PFpar[2], pg = PFpar[3],mu=PFpar[4])-alpha,c(-10,10))$root
    Bias_RT_name <- paste(fig.dir,"Extension(scale=05)_RT_bias_alpha_",Alpha_list2[i],".png",sep="")
    MSE_RT_name <- paste(fig.dir,"Extension(scale=05)_RT_MSE_alpha_",Alpha_list2[i],".png",sep="")
    sd_RT_name <- paste(fig.dir,"Extension(scale=05)_RT_sd_alpha_",Alpha_list2[i],".png",sep="")
    
    Bias_conf_name <-paste(fig.dir,"Extension(scale=05)_conf_bias_alpha_",Alpha_list2[i],".png",sep="")
    MSE_conf_name <- paste(fig.dir,"Extension(scale=05)_conf_MSE_alpha_",Alpha_list2[i],".png",sep="")
    sd_conf_name <- paste(fig.dir,"Extension(scale=05)_conf_sd_alpha_",Alpha_list2[i],".png",sep="")
    

    
    .A <-  Bias_plot(Total_RT[[i]],total_RT_bias_se[[i]],trial=T_set,Ini_all = 6,
                     lower=B_L,uper=B_U,theta = Theta)
    ggsave(Bias_RT_name,.A,height = 16, width = 32)
    .A <- sd_plot(Total_RT[[i]],total_RT_sd_se[[i]],trial=T_set,Ini_all = 6,
                  lower=S_L,uper=S_U,theta = Theta)
    ggsave(sd_RT_name,.A,height = 16, width = 32)
    .A <- MSE_plot(Total_RT[[i]],total_RT_MSE_se[[i]],trial=T_set,Ini_all = 6,
                   lower=M_L,uper=M_U,theta = Theta)
    ggsave(MSE_RT_name,.A,height = 16, width = 32)
    
    .A <- Bias_plot(Total_conf[[i]],total_conf_bias_se[[i]],trial=T_set,Ini_all = 6,lower=B_L,uper=B_U,theta = Theta)
    ggsave(Bias_conf_name,.A,height = 16, width = 32)
    .A <- sd_plot(Total_conf[[i]],total_conf_sd_se[[i]],trial=T_set,Ini_all = 6,lower=S_L,uper=S_U,theta = Theta)
    ggsave(sd_conf_name,.A,height = 16, width = 32)
    .A <- MSE_plot(Total_conf[[i]],total_conf_MSE_se[[i]],trial=T_set,Ini_all = 6,lower=M_L,uper=M_U,theta = Theta)
    ggsave(MSE_conf_name,.A,height = 16, width = 32)
  }
}  
  