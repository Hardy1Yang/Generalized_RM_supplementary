

MSE_plot=function(Data,SE,trial=c(1:15),cut=0.01,lower=0,uper=0.006,
                  theta=Theta,present="all",C_all=3,Ini_all=3,Tex_boook=Tbook){
  C_len=C_all
  sims=length(Data[[1]][[1]]$results$method1)
  initial_len=Ini_all
  p=list()

  
  

  for (i in 1:C_len) {
    for (j in 1:initial_len) {
      k=(i-1)*Ini_all+j
      
      se_max=switch(present,'all'=SE[[k]][,-1],'RM'=SE[[k]][,c(2,3)],'ASA'=SE[[k]][,c(4,5)])
      SE.max <- max(se_max) %>% round(.,digits = 5)

      title=TeX(Tex_boook[k])#paste("size: ",as.character(Data[[1]][[k]]$C_val),","," initial: ",,sep="")

      MSE <- data.frame()
      for (c in trial) {
        M <- Data[[c]][[k]]$Variance*(sims-1)/sims+Data[[c]][[k]]$Bias
        MSE <- rbind(MSE,c(c*10,M))
        
      }
      colnames(MSE) <- c("trial",Data[[1]][[1]]$labels)
      
      
      
      se=SE[[k]][trial,]
      

      se=switch(present,'all'=se,'RM'=se[,c(1,2,3)],'ASA'=se[,c(1,4,5)])
      se <- pivot_longer( se,!trial,names_to = 'method',values_to = "se")
      
      
      MSE=switch(present,'all'=MSE,'RM'=MSE[,c(1,2,3)],'ASA'=MSE[,c(1,4,5)])
      MSE <- pivot_longer(MSE,!trial,names_to = 'method',values_to = "MSE")
      
      MSE <- left_join(MSE,se,by = c("trial", "method"))
      
      MSE$method <- factor(MSE$method,level=Data[[1]][[1]]$labels)
      p[[k]] <- ggplot(MSE, aes(x=trial, y=MSE,group=method,color=method)) + 
        geom_line(size=4)+
        geom_point(size=8)+
        ggtitle(title) + 

        scale_size_manual(values=c(1, 1.5))+

        theme_bw()+
        theme(legend.position = "top",
              legend.title = element_text(size=0),
              legend.spacing.x = unit(2.0, 'cm'),
              #   axis.text.x=element_blank(),
              legend.text=element_text(size=40),
              legend.key.size = unit(3.0, 'cm'),
              plot.title = element_text(size=30))+
        theme(axis.text.x=element_text(size=30))+
        theme(axis.text.y=element_text(size=30))+
        theme(axis.title.x = element_text(size=0))+
        theme(axis.title.y = element_text(size=0))+
        coord_cartesian(ylim=c(lower, uper))+
        geom_hline(yintercept=0, size=2)

      
      if(j==1){p[[k]] <- p[[k]]+theme(axis.title.y = element_text(size=30))
      }else{p[[k]] <- p[[k]]+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
      }
      
      if(i==3){ 
        p[[k]] <- p[[k]]+theme(axis.title.x = element_text(size=30))
      }

      
    }
    
    
    
  }
  for (n in 2:length(p)) {
    p[[n]] <- p[[n]]+theme(legend.position="none")
  }
  legend <- cowplot::get_legend(p[[1]])
  p[[1]] <- p[[1]]+theme(legend.position="none")

  blankPlot1 <- ggplot()+geom_blank(aes(1,1)) + 
    cowplot::theme_nothing()
  blankPlot2 <- ggplot()+geom_blank(aes(1,1)) + 
    cowplot::theme_nothing()
  grid.arrange(blankPlot1,blankPlot2,legend,blankPlot2,blankPlot2,blankPlot2,
               p[[1]], p[[2]], p[[3]], p[[4]], 
               p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],
               p[[13]],p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],
               ncol=6, nrow=4,heights=c(1,3,3,3),widths=c(18/15,1,1,1,1,1))
  Out <- arrangeGrob(blankPlot1,blankPlot2,legend,blankPlot2,blankPlot2,blankPlot2,
                     p[[1]], p[[2]], p[[3]], p[[4]], 
                     p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],
                     p[[13]],p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],
                     ncol=6, nrow=4,heights=c(1,3,3,3),widths=c(18/15,1,1,1,1,1))
  return(Out)

}

sd_plot=function(Data,SE,trial=c(1:15),cut=0.01,lower=0,uper=0.01,
                 #order=c("RM","RM_modified","ASA","ASA_modified"),
                 theta=Theta,present="all",C_all=3,Ini_all=3,Tex_boook=Tbook
){
  C_len=C_all
  sims=length(Data[[1]][[1]]$results$method1)
  initial_len=Ini_all
  #trial=c(1:15)
  p=list()
  for (i in 1:C_len) {
    for (j in 1:initial_len) {
      k=(i-1)*Ini_all+j
      se_max=switch(present,'all'=SE[[k]][,-1],'RM'=SE[[k]][,c(2,3)],'ASA'=SE[[k]][,c(4,5)])
      SE.max <- max(se_max) %>% round(.,digits = 5)
      title=TeX(Tex_boook[k])
      variance <- data.frame()
      for (c in trial) {
        v2 <- Data[[c]][[k]]$Variance %>% sqrt()
        variance <- rbind(variance,c(c*10,v2))
        
      }
      colnames(variance) <- c("trial",Data[[1]][[1]]$labels)
      se=SE[[k]][trial,]
      se=switch(present,'all'=se,'RM'=se[,c(1,2,3)],'ASA'=se[,c(1,4,5)])
      se <- pivot_longer( se,!trial,names_to = 'method',values_to = "se")
      
      
      variance=switch(present,'all'=variance,'RM'=variance[,c(1,2,3)],'ASA'=variance[,c(1,4,5)])
      variance <- pivot_longer(variance,!trial,names_to = 'method',values_to ="sd")
      
      
      variance <- left_join(variance,se,by = c("trial", "method"))
      
      
      
      variance$method <- factor(variance$method,level=Data[[1]][[1]]$labels)
      
      
      
      
      p[[k]] <- ggplot(variance, aes(x=trial, y=sd,group=method,color=method))+
        geom_line(size=4) +
        ggtitle(title) + 
        geom_point(size=8)+

        theme_bw()+

        theme_bw()+
        theme(legend.position = "top",
              legend.title = element_text(size=0),
              legend.spacing.x = unit(2.0, 'cm'),
              #   axis.text.x=element_blank(),
              legend.text=element_text(size=40),
              legend.key.size = unit(3.0, 'cm'),
              plot.title = element_text(size=30))+
        theme(axis.text.x=element_text(size=30))+
        theme(axis.text.y=element_text(size=30))+
        theme(axis.title.x = element_text(size=0))+
        theme(axis.title.y = element_text(size=0))+
        coord_cartesian(ylim=c(lower, uper))+
        geom_hline(yintercept=0, size=2)

      
      if(j==1){p[[k]] <- p[[k]]+theme(axis.title.y = element_text(size=30))
      }else{p[[k]] <- p[[k]]+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
      }
      
      if(i==3){ 
        p[[k]] <- p[[k]]+theme(axis.title.x = element_text(size=30))
      }

    }
    
    
    
  }
  for (n in 2:length(p)) {
    p[[n]] <- p[[n]]+theme(legend.position="none")
  }
  legend <- cowplot::get_legend(p[[1]])
  p[[1]] <- p[[1]]+theme(legend.position="none")
  
  blankPlot1 <- ggplot()+geom_blank(aes(1,1)) + 
    cowplot::theme_nothing()
  blankPlot2 <- ggplot()+geom_blank(aes(1,1)) + 
    cowplot::theme_nothing()
  grid.arrange(blankPlot1,blankPlot2,legend,blankPlot2,blankPlot2,blankPlot2,
               p[[1]], p[[2]], p[[3]], p[[4]], 
               p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],
               p[[13]],p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],
               ncol=6, nrow=4,heights=c(1/3,1,1,1),widths=c(18/15,1,1,1,1,1))
  Out <- arrangeGrob(blankPlot1,blankPlot2,legend,blankPlot2,blankPlot2,blankPlot2,
                     p[[1]], p[[2]], p[[3]], p[[4]], 
                     p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],
                     p[[13]],p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],
                     ncol=6, nrow=4,heights=c(1/3,1,1,1),widths=c(18/15,1,1,1,1,1))
  return(Out)

}


Bias_plot=function(Data,SE,trial=c(1:15),cut=0.01,lower=0,uper=0.01,
                   theta=Theta,present="all",C_all=3,Ini_all=3,Tex_boook=Tbook
){
  
  C_len=C_all
  sims=length(Data[[1]][[1]]$results$method1)
  initial_len=Ini_all
  p=list()
  for (i in 1:C_len) {
    for (j in 1:initial_len) {
      k=(i-1)*Ini_all+j
      se_max=switch(present,'all'=SE[[k]][,-1],'RM'=SE[[k]][,c(2,3)],'ASA'=SE[[k]][,c(4,5)])
      SE.max <- max(se_max) %>% round(.,digits = 5)
      title=TeX(Tex_boook[k])

      Bias <- data.frame()
      for (c in trial) {
        b <- lapply(Data[[c]][[k]]$results,mean) %>% unlist() %>% -theta
        Bias  <- rbind(Bias,c(c*10,b))
        
      }
      colnames(Bias) <- c("trial",Data[[1]][[1]]$labels)
      se=SE[[k]][trial,]
      Bias=switch(present,'all'=Bias,'RM'=Bias[,c(1,2,3)],'ASA'=Bias[,c(1,4,5)])
      se=switch(present,'all'=se,'RM'=se[,c(1,2,3)],'ASA'=se[,c(1,4,5)])
      
      Bias <- pivot_longer(Bias,!trial,names_to = 'method',values_to = "Bias")
      se <- pivot_longer( se,!trial,names_to = 'method',values_to = "se")
      
      
      
      Bias <- left_join(Bias,se, by = c("trial", "method"))
      
      Bias$method <- factor(Bias$method,level=Data[[1]][[1]]$labels)
      p[[k]] <- ggplot(Bias, aes(x=trial, y=Bias,group=method,color=method)) + 
        geom_line(size=4) +
        geom_point(size=8)+
        ggtitle(title) + 
        theme_bw()+
        theme(legend.position = "top",
              legend.title = element_text(size=0),
              legend.spacing.x = unit(2.0, 'cm'),
              #   axis.text.x=element_blank(),
              legend.text=element_text(size=40),
              legend.key.size = unit(3.0, 'cm'),
              plot.title = element_text(size=30))+
        theme(axis.text.x=element_text(size=30))+
        theme(axis.text.y=element_text(size=30))+
        theme(axis.title.x = element_text(size=0))+
        theme(axis.title.y = element_text(size=0))+
        coord_cartesian(ylim=c(lower, uper))+
        geom_hline(yintercept=0, size=2)

      
      if(j==1){p[[k]] <- p[[k]]+theme(axis.title.y = element_text(size=30))
      }else{p[[k]] <- p[[k]]+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
      }
      
      if(i==3){ 
        p[[k]] <- p[[k]]+theme(axis.title.x = element_text(size=30))
      }

    }
    
    
    
  }
  
  for (n in 2:length(p)) {
    p[[n]] <- p[[n]]+theme(legend.position="none")
  }
  legend <- cowplot::get_legend(p[[1]])
  
  p[[1]] <- p[[1]]+theme(legend.position="none")
  
  blankPlot1 <- ggplot()+geom_blank(aes(1,1)) + 
    cowplot::theme_nothing()
  blankPlot2 <- ggplot()+geom_blank(aes(1,1)) + 
    cowplot::theme_nothing()
  grid.arrange(blankPlot1,blankPlot2,legend,blankPlot2,blankPlot2,blankPlot2,
               p[[1]], p[[2]], p[[3]], p[[4]], 
               p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],
               p[[13]],p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],
               ncol=6, nrow=4,heights=c(1/3,1,1,1),widths=c(18/15,1,1,1,1,1))
  Out <- arrangeGrob(blankPlot1,blankPlot2,legend,blankPlot2,blankPlot2,blankPlot2,
                     p[[1]], p[[2]], p[[3]], p[[4]], 
                     p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],
                     p[[13]],p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],
                     ncol=6, nrow=4,heights=c(1/3,1,1,1),widths=c(18/15,1,1,1,1,1))
  return(Out)

  
}




