make_CO2_difference_of_A_and_gs_plots_with_se <- function() {
    
    ### read input
    pilDF<-read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv",sep=",", header=TRUE)
    
    ### caclulate daily
    pilDF$Adaily <- (pilDF$Aearly + pilDF$Alate) / 2
    pilDF$gsdaily <- (pilDF$gsearly + pilDF$gslate) / 2
    
    popDF$Adaily <- (popDF$Aearly + popDF$Alate) / 2
    popDF$gsdaily <- (popDF$gsearly + popDF$gslate) / 2
    
    ### calculate iWUS
    pilDF$WUE_daily <- pilDF$Adaily / pilDF$gsdaily
    pilDF$WUE_early <- pilDF$Aearly / pilDF$gsearly
    pilDF$WUE_late <- pilDF$Alate / pilDF$gslate
    
    popDF$WUE_daily <- popDF$Adaily / popDF$gsdaily
    popDF$WUE_early <- popDF$Aearly / popDF$gsearly
    popDF$WUE_late <- popDF$Alate / popDF$gslate
    
    
    ### calculate log values
    pilDF$log_Adaily <- log(pilDF$Adaily)
    pilDF$log_Aearly <- log(pilDF$Aearly)
    pilDF$log_Alate <- log(pilDF$Alate)
    
    pilDF$log_gsdaily <- log(pilDF$gsdaily)
    pilDF$log_gsearly <- log(pilDF$gsearly)
    pilDF$log_gslate <- log(pilDF$gslate)
    
    pilDF$log_WUE_daily <- log(pilDF$WUE_daily)
    pilDF$log_WUE_early <- log(pilDF$WUE_early)
    pilDF$log_WUE_late <- log(pilDF$WUE_late)
    
    
    popDF$log_Adaily <- log(popDF$Adaily)
    popDF$log_Aearly <- log(popDF$Aearly)
    popDF$log_Alate <- log(popDF$Alate)
    
    popDF$log_gsdaily <- log(popDF$gsdaily)
    popDF$log_gsearly <- log(popDF$gsearly)
    popDF$log_gslate <- log(popDF$gslate)
    
    popDF$log_WUE_daily <- log(popDF$WUE_daily)
    popDF$log_WUE_early <- log(popDF$WUE_early)
    popDF$log_WUE_late <- log(popDF$WUE_late)
    
    ### summary
    pDF1 <- summaryBy(log_Adaily+log_Aearly+log_Alate+log_gsdaily+log_gsearly+log_gslate+
                          log_WUE_daily+log_WUE_early+log_WUE_late~Trt+CO2+H2O+Day,
                      FUN=c(mean,se), data=pilDF, keep.names=T)
    
    pDF2 <- summaryBy(log_Adaily+log_Aearly+log_Alate+log_gsdaily+log_gsearly+log_gslate+
                          log_WUE_daily+log_WUE_early+log_WUE_late~Trt+CO2+H2O+Day,
                      FUN=c(mean,se), data=popDF, keep.names=T)
    
    ### assign sample size
    #pDF1$n1 <- 12
    #pDF1$n2 <- 6
    
    #pDF2$n1 <- 12
    #pDF2$n2 <- 6
    
    ### day list
    d1 <- unique(pilDF$Day)
    d2 <- unique(popDF$Day)
    
    ### water treatment
    w <- c("D", "ND")
    
    ### plot DF
    plotDF1 <- data.frame(rep(w, length(d1)), 
                          rep(d1, each=2), NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA)
    
    plotDF2 <- data.frame(rep(w, length(d2)), 
                          rep(d2, each=2), NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA)
    
    colnames(plotDF1) <- colnames(plotDF2) <- c("Trt", "Day",
                                                "amb_log_Adaily", "ele_log_Adaily",
                                                "amb_log_Aearly", "ele_log_Aearly",
                                                "amb_log_Alate", "ele_log_Alate",
                                                "amb_log_GSdaily", "ele_log_GSdaily",
                                                "amb_log_GSearly", "ele_log_GSearly",
                                                "amb_log_GSlate", "ele_log_GSlate",
                                                "amb_log_WUEdaily", "ele_log_WUEdaily",
                                                "amb_log_WUEearly", "ele_log_WUEearly",
                                                "amb_log_WUElate", "ele_log_WUElate",
                                                "amb_log_AdailySE", "ele_log_AdailySE",
                                                "amb_log_AearlySE", "ele_log_AearlySE",
                                                "amb_log_AlateSE", "ele_log_AlateSE",
                                                "amb_log_GSdailySE", "ele_log_GSdailySE",
                                                "amb_log_GSearlySE", "ele_log_GSearlySE",
                                                "amb_log_GSlateSE", "ele_log_GSlateSE",
                                                "amb_log_WUEdailySE", "ele_log_WUEdailySE",
                                                "amb_log_WUEearlySE", "ele_log_WUEearlySE",
                                                "amb_log_WUElateSE", "ele_log_WUElateSE")
    
    ### fill values
    for (i in d1) {
        # A
        plotDF1$amb_log_Adaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Adaily.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_Aearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Aearly.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_Alate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Alate.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        
        plotDF1$ele_log_Adaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Adaily.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_Aearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Aearly.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_Alate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Alate.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        
        plotDF1$amb_log_Adaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Adaily.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_Aearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Aearly.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_Alate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Alate.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        
        plotDF1$ele_log_Adaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Adaily.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_Aearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Aearly.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_Alate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Alate.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        
        ### gs
        plotDF1$amb_log_GSdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsdaily.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_GSearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsearly.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_GSlate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gslate.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        
        plotDF1$ele_log_GSdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsdaily.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_GSearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsearly.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_GSlate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gslate.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        
        plotDF1$amb_log_GSdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsdaily.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_GSearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsearly.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_GSlate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gslate.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        
        plotDF1$ele_log_GSdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsdaily.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_GSearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsearly.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_GSlate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gslate.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        
        
        ### WUE
        plotDF1$amb_log_WUEdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_daily.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_WUEearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_early.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_WUElate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_late.mean[pDF1$Trt=="PILAD"&pDF1$Day==i]
        
        plotDF1$ele_log_WUEdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_daily.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_WUEearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_early.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_WUElate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_late.mean[pDF1$Trt=="PILED"&pDF1$Day==i]
        
        plotDF1$amb_log_WUEdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_daily.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_WUEearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_early.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_WUElate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_late.mean[pDF1$Trt=="PILAND"&pDF1$Day==i]
        
        plotDF1$ele_log_WUEdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_daily.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_WUEearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_early.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_WUElate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_late.mean[pDF1$Trt=="PILEND"&pDF1$Day==i]
        
        # A SE
        plotDF1$amb_log_AdailySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Adaily.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_AearlySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Aearly.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_AlateSE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Alate.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        
        plotDF1$ele_log_AdailySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Adaily.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_AearlySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Aearly.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_AlateSE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_Alate.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        
        plotDF1$amb_log_AdailySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Adaily.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_AearlySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Aearly.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_AlateSE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Alate.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        
        plotDF1$ele_log_AdailySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Adaily.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_AearlySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Aearly.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_AlateSE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_Alate.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        
        
        # gs SE
        plotDF1$amb_log_GSdailySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsdaily.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_GSearlySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsearly.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_GSlateSE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gslate.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        
        plotDF1$ele_log_GSdailySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsdaily.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_GSearlySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gsearly.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_GSlateSE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_gslate.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        
        plotDF1$amb_log_GSdailySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsdaily.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_GSearlySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsearly.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_GSlateSE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gslate.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        
        plotDF1$ele_log_GSdailySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsdaily.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_GSearlySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gsearly.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_GSlateSE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_gslate.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        
        
        # WUE SE
        plotDF1$amb_log_WUEdailySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_daily.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_WUEearlySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_early.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        plotDF1$amb_log_WUElateSE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_late.se[pDF1$Trt=="PILAD"&pDF1$Day==i]
        
        plotDF1$ele_log_WUEdailySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_daily.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_WUEearlySE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_early.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        plotDF1$ele_log_WUElateSE[plotDF1$Trt=="D"&plotDF1$Day==i] <- pDF1$log_WUE_late.se[pDF1$Trt=="PILED"&pDF1$Day==i]
        
        plotDF1$amb_log_WUEdailySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_daily.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_WUEearlySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_early.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        plotDF1$amb_log_WUElateSE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_late.se[pDF1$Trt=="PILAND"&pDF1$Day==i]
        
        plotDF1$ele_log_WUEdailySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_daily.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_WUEearlySE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_early.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        plotDF1$ele_log_WUElateSE[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pDF1$log_WUE_late.se[pDF1$Trt=="PILEND"&pDF1$Day==i]
        
    }
    
    
    
    for (i in d2) {
        # A
        plotDF2$amb_log_Adaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_Adaily.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                      NA)
        plotDF2$amb_log_Aearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_Aearly.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                      NA)
        plotDF2$amb_log_Alate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                     as.numeric(pDF2$log_Alate.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                     NA)
        
        plotDF2$ele_log_Adaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_Adaily.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                      NA)
        plotDF2$ele_log_Aearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_Aearly.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                      NA)
        plotDF2$ele_log_Alate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                     as.numeric(pDF2$log_Alate.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                     NA)
        
        plotDF2$amb_log_Adaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_Adaily.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$amb_log_Aearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_Aearly.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$amb_log_Alate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_Alate.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]), 
                                                                      NA)
        
        plotDF2$ele_log_Adaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_Adaily.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$ele_log_Aearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_Aearly.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$ele_log_Alate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_Alate.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]), 
                                                                      NA)
        ### gs
        plotDF2$amb_log_GSdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_gsdaily.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$amb_log_GSearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_gsearly.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$amb_log_GSlate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_gslate.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                      NA)
        
        plotDF2$ele_log_GSdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_gsdaily.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$ele_log_GSearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_gsearly.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                       NA)
        plotDF2$ele_log_GSlate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                      as.numeric(pDF2$log_gslate.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                      NA)
        
        plotDF2$amb_log_GSdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1, 
                                                                        as.numeric(pDF2$log_gsdaily.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]), 
                                                                        NA)
        plotDF2$amb_log_GSearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                        as.numeric(pDF2$log_gsearly.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                        NA)
        plotDF2$amb_log_GSlate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_gslate.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                       NA)
        
        plotDF2$ele_log_GSdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                        as.numeric(pDF2$log_gsdaily.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                        NA)
        plotDF2$ele_log_GSearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                        as.numeric(pDF2$log_gsearly.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                        NA)
        plotDF2$ele_log_GSlate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                       as.numeric(pDF2$log_gslate.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                       NA)
        
        
        ### WUE
        plotDF2$amb_log_WUEdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                           as.numeric(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                           NA)
        plotDF2$amb_log_WUEearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                           as.numeric(pDF2$log_WUE_early.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                           NA)
        plotDF2$amb_log_WUElate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.mean[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                          as.numeric(pDF2$log_WUE_late.mean[pDF2$Trt=="POPAD"&pDF2$Day==i]), 
                                                                          NA)
        
        plotDF2$ele_log_WUEdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                           as.numeric(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                           NA)
        plotDF2$ele_log_WUEearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                           as.numeric(pDF2$log_WUE_early.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                           NA)
        plotDF2$ele_log_WUElate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.mean[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                          as.numeric(pDF2$log_WUE_late.mean[pDF2$Trt=="POPED"&pDF2$Day==i]), 
                                                                          NA)
        
        plotDF2$amb_log_WUEdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1, 
                                                                            as.numeric(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]), 
                                                                            NA)
        plotDF2$amb_log_WUEearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                            as.numeric(pDF2$log_WUE_early.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                            NA)
        plotDF2$amb_log_WUElate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.mean[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                           as.numeric(pDF2$log_WUE_late.mean[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                           NA)
        
        plotDF2$ele_log_WUEdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                            as.numeric(pDF2$log_WUE_daily.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                            NA)
        plotDF2$ele_log_WUEearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                            as.numeric(pDF2$log_WUE_early.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                            NA)
        plotDF2$ele_log_WUElate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.mean[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                           as.numeric(pDF2$log_WUE_late.mean[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                           NA)
        
        # A SD
        plotDF2$amb_log_AdailySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_Adaily.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                               NA)
        plotDF2$amb_log_AearlySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_Aearly.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                               NA)
        plotDF2$amb_log_AlateSE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                              as.numeric(pDF2$log_Alate.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                              NA)
        
        plotDF2$ele_log_AdailySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_Adaily.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                               NA)
        plotDF2$ele_log_AearlySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_Aearly.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                               NA)
        plotDF2$ele_log_AlateSE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                              as.numeric(pDF2$log_Alate.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                              NA)
        
        plotDF2$amb_log_AdailySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_Adaily.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$amb_log_AearlySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_Aearly.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$amb_log_AlateSE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_Alate.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                               NA)
        
        plotDF2$ele_log_AdailySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Adaily.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_Adaily.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$ele_log_AearlySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Aearly.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_Aearly.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$ele_log_AlateSE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_Alate.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_Alate.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                               NA)
        
        # gs Se
        plotDF2$amb_log_GSdailySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_gsdaily.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$amb_log_GSearlySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_gsearly.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$amb_log_GSlateSE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_gslate.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                               NA)
        
        plotDF2$ele_log_GSdailySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_gsdaily.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$ele_log_GSearlySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_gsearly.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                                NA)
        plotDF2$ele_log_GSlateSE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                               as.numeric(pDF2$log_gslate.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                               NA)
        
        plotDF2$amb_log_GSdailySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                                 as.numeric(pDF2$log_gsdaily.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                                 NA)
        plotDF2$amb_log_GSearlySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                                 as.numeric(pDF2$log_gsearly.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                                 NA)
        plotDF2$amb_log_GSlateSE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_gslate.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                                NA)
        
        plotDF2$ele_log_GSdailySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsdaily.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                                 as.numeric(pDF2$log_gsdaily.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                                 NA)
        plotDF2$ele_log_GSearlySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gsearly.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                                 as.numeric(pDF2$log_gsearly.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                                 NA)
        plotDF2$ele_log_GSlateSE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_gslate.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                                as.numeric(pDF2$log_gslate.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                                NA)
        
        # WUE Se
        plotDF2$amb_log_WUEdailySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                             as.numeric(pDF2$log_WUE_daily.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                             NA)
        plotDF2$amb_log_WUEearlySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                             as.numeric(pDF2$log_WUE_early.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                             NA)
        plotDF2$amb_log_WUElateSE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.se[pDF2$Trt=="POPAD"&pDF2$Day==i])==1,
                                                                            as.numeric(pDF2$log_WUE_late.se[pDF2$Trt=="POPAD"&pDF2$Day==i]),
                                                                            NA)
        
        plotDF2$ele_log_WUEdailySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                             as.numeric(pDF2$log_WUE_daily.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                             NA)
        plotDF2$ele_log_WUEearlySE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                             as.numeric(pDF2$log_WUE_early.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                             NA)
        plotDF2$ele_log_WUElateSE[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.se[pDF2$Trt=="POPED"&pDF2$Day==i])==1,
                                                                            as.numeric(pDF2$log_WUE_late.se[pDF2$Trt=="POPED"&pDF2$Day==i]),
                                                                            NA)
        
        plotDF2$amb_log_WUEdailySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                              as.numeric(pDF2$log_WUE_daily.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                              NA)
        plotDF2$amb_log_WUEearlySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                              as.numeric(pDF2$log_WUE_early.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                              NA)
        plotDF2$amb_log_WUElateSE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.se[pDF2$Trt=="POPAND"&pDF2$Day==i])==1,
                                                                             as.numeric(pDF2$log_WUE_late.se[pDF2$Trt=="POPAND"&pDF2$Day==i]),
                                                                             NA)
        
        plotDF2$ele_log_WUEdailySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_daily.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                              as.numeric(pDF2$log_WUE_daily.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                              NA)
        plotDF2$ele_log_WUEearlySE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_early.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                              as.numeric(pDF2$log_WUE_early.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                              NA)
        plotDF2$ele_log_WUElateSE[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(pDF2$log_WUE_late.se[pDF2$Trt=="POPEND"&pDF2$Day==i])==1,
                                                                             as.numeric(pDF2$log_WUE_late.se[pDF2$Trt=="POPEND"&pDF2$Day==i]),
                                                                             NA)
    }
    
    ### ignore NAs
    #plotDF1 <- plotDF1[complete.cases(plotDF1),]
    #plotDF2 <- plotDF2[complete.cases(plotDF2),]
    #plotDF2 <- as.data.frame(sapply(plotDF2, as.numeric))
    #plotDF2$Trt <- gsub(1, "D", plotDF2$Trt)
    #plotDF2$Trt <- gsub(2, "ND", plotDF2$Trt)
    #plotDF2$Trt <- as.character(plotDF2$Trt)
    
    
    ### Calculate CO2 signal
    plotDF1$log_CO2_Adaily <- plotDF1$ele_log_Adaily - plotDF1$amb_log_Adaily
    plotDF1$log_CO2_Aearly <- plotDF1$ele_log_Aearly - plotDF1$amb_log_Aearly
    plotDF1$log_CO2_Alate <- plotDF1$ele_log_Alate - plotDF1$amb_log_Alate
    
    plotDF1$log_CO2_GSdaily <- plotDF1$ele_log_GSdaily / plotDF1$amb_log_GSdaily
    plotDF1$log_CO2_GSearly <- plotDF1$ele_log_GSearly / plotDF1$amb_log_GSearly
    plotDF1$log_CO2_GSlate <- plotDF1$ele_log_GSlate / plotDF1$amb_log_GSlate
    
    plotDF1$log_CO2_WUEdaily <- plotDF1$ele_log_WUEdaily / plotDF1$amb_log_WUEdaily
    plotDF1$log_CO2_WUEearly <- plotDF1$ele_log_WUEearly / plotDF1$amb_log_WUEearly
    plotDF1$log_CO2_WUElate <- plotDF1$ele_log_WUElate / plotDF1$amb_log_WUElate
    
    ## plotDF2
    plotDF2$log_CO2_Adaily <- plotDF2$ele_log_Adaily / plotDF2$amb_log_Adaily
    plotDF2$log_CO2_Aearly <- plotDF2$ele_log_Aearly / plotDF2$amb_log_Aearly
    plotDF2$log_CO2_Alate <- plotDF2$ele_log_Alate / plotDF2$amb_log_Alate
    
    
    plotDF2$log_CO2_GSdaily <- plotDF2$ele_log_GSdaily / plotDF2$amb_log_GSdaily
    plotDF2$log_CO2_GSearly <- plotDF2$ele_log_GSearly / plotDF2$amb_log_GSearly
    plotDF2$log_CO2_GSlate <- plotDF2$ele_log_GSlate / plotDF2$amb_log_GSlate
    
    plotDF2$log_CO2_WUEdaily <- plotDF2$ele_log_WUEdaily / plotDF2$amb_log_WUEdaily
    plotDF2$log_CO2_WUEearly <- plotDF2$ele_log_WUEearly / plotDF2$amb_log_WUEearly
    plotDF2$log_CO2_WUElate <- plotDF2$ele_log_WUElate / plotDF2$amb_log_WUElate
    
    ### Calculate CO2 ratio SE
    plotDF1$log_CO2_AdailySE <- sqrt(plotDF1$amb_log_AdailySE^2 + plotDF1$ele_log_AdailySE^2)
    plotDF1$log_CO2_AearlySE <- sqrt(plotDF1$amb_log_AearlySE^2 + plotDF1$ele_log_AearlySE^2)
    plotDF1$log_CO2_AlateSE <- sqrt(plotDF1$amb_log_AlateSE^2 + plotDF1$ele_log_AlateSE^2)
    
    plotDF1$log_CO2_GSdailySE <- sqrt(plotDF1$amb_log_GSdailySE^2 + plotDF1$ele_log_GSdailySE^2)
    plotDF1$log_CO2_GSearlySE <- sqrt(plotDF1$amb_log_GSearlySE^2 + plotDF1$ele_log_GSearlySE^2)
    plotDF1$log_CO2_GSlateSE <- sqrt(plotDF1$amb_log_GSlateSE^2 + plotDF1$ele_log_GSlateSE^2)
    
    plotDF1$log_CO2_WUEdailySE <- sqrt(plotDF1$amb_log_WUEdailySE^2 + plotDF1$ele_log_WUEdailySE^2)
    plotDF1$log_CO2_WUEearlySE <- sqrt(plotDF1$amb_log_WUEearlySE^2 + plotDF1$ele_log_WUEearlySE^2)
    plotDF1$log_CO2_WUElateSE <- sqrt(plotDF1$amb_log_WUElateSE^2 + plotDF1$ele_log_WUElateSE^2)
    
    # plotDF2
    plotDF2$log_CO2_AdailySE <- sqrt(plotDF2$amb_log_AdailySE^2 + plotDF2$ele_log_AdailySE^2)
    plotDF2$log_CO2_AearlySE <- sqrt(plotDF2$amb_log_AearlySE^2 + plotDF2$ele_log_AearlySE^2)
    plotDF2$log_CO2_AlateSE <- sqrt(plotDF2$amb_log_AlateSE^2 + plotDF2$ele_log_AlateSE^2)
    
    plotDF2$log_CO2_GSdailySE <- sqrt(plotDF2$amb_log_GSdailySE^2 + plotDF2$ele_log_GSdailySE^2)
    plotDF2$log_CO2_GSearlySE <- sqrt(plotDF2$amb_log_GSearlySE^2 + plotDF2$ele_log_GSearlySE^2)
    plotDF2$log_CO2_GSlateSE <- sqrt(plotDF2$amb_log_GSlateSE^2 + plotDF2$ele_log_GSlateSE^2)
    
    plotDF2$log_CO2_WUEdailySE <- sqrt(plotDF2$amb_log_WUEdailySE^2 + plotDF2$ele_log_WUEdailySE^2)
    plotDF2$log_CO2_WUEearlySE <- sqrt(plotDF2$amb_log_WUEearlySE^2 + plotDF2$ele_log_WUEearlySE^2)
    plotDF2$log_CO2_WUElateSE <- sqrt(plotDF2$amb_log_WUElateSE^2 + plotDF2$ele_log_WUElateSE^2)
    
    
    ### exponential mean
    plotDF1$CO2_Adaily <- exp(plotDF1$log_CO2_Adaily)
    plotDF1$CO2_Aearly <- exp(plotDF1$log_CO2_Aearly)
    plotDF1$CO2_Alate <- exp(plotDF1$log_CO2_Alate)
    
    plotDF1$CO2_GSdaily <- exp(plotDF1$log_CO2_GSdaily)
    plotDF1$CO2_GSearly <- exp(plotDF1$log_CO2_GSearly)
    plotDF1$CO2_GSlate <- exp(plotDF1$log_CO2_GSlate)
    
    plotDF1$CO2_WUEdaily <- exp(plotDF1$log_CO2_WUEdaily)
    plotDF1$CO2_WUEearly <- exp(plotDF1$log_CO2_WUEearly)
    plotDF1$CO2_WUElate <- exp(plotDF1$log_CO2_WUElate)
    
    ## plotDF2
    plotDF2$CO2_Adaily <- exp(plotDF2$log_CO2_Adaily)
    plotDF2$CO2_Aearly <- exp(plotDF2$log_CO2_Aearly)
    plotDF2$CO2_Alate <- exp(plotDF2$log_CO2_Alate)
    
    plotDF2$CO2_GSdaily <- exp(plotDF2$log_CO2_GSdaily)
    plotDF2$CO2_GSearly <- exp(plotDF2$log_CO2_GSearly)
    plotDF2$CO2_GSlate <- exp(plotDF2$log_CO2_GSlate)
    
    plotDF2$CO2_WUEdaily <- exp(plotDF2$log_CO2_WUEdaily)
    plotDF2$CO2_WUEearly <- exp(plotDF2$log_CO2_WUEearly)
    plotDF2$CO2_WUElate <- exp(plotDF2$log_CO2_WUElate)
    
    ### exponential se
    plotDF1$CO2_AdailyUP <- exp(plotDF1$log_CO2_Adaily+plotDF1$log_CO2_AdailySE)
    plotDF1$CO2_AdailyLO <- exp(plotDF1$log_CO2_Adaily-plotDF1$log_CO2_AdailySE)
    
    plotDF1$CO2_AearlyUP <- exp(plotDF1$log_CO2_Aearly+plotDF1$log_CO2_AearlySE)
    plotDF1$CO2_AearlyLO <- exp(plotDF1$log_CO2_Aearly-plotDF1$log_CO2_AearlySE)
    
    plotDF1$CO2_AlateUP <- exp(plotDF1$log_CO2_Alate+plotDF1$log_CO2_AlateSE)
    plotDF1$CO2_AlateLO <- exp(plotDF1$log_CO2_Alate-plotDF1$log_CO2_AlateSE)
    
    
    plotDF1$CO2_GSdailyUP <- exp(plotDF1$log_CO2_GSdaily+plotDF1$log_CO2_GSdailySE)
    plotDF1$CO2_GSdailyLO <- exp(plotDF1$log_CO2_GSdaily-plotDF1$log_CO2_GSdailySE)
    
    plotDF1$CO2_GSearlyUP <- exp(plotDF1$log_CO2_GSearly+plotDF1$log_CO2_GSearlySE)
    plotDF1$CO2_GSearlyLO <- exp(plotDF1$log_CO2_GSearly-plotDF1$log_CO2_GSearlySE)
    
    plotDF1$CO2_GSlateUP <- exp(plotDF1$log_CO2_GSlate+plotDF1$log_CO2_GSlateSE)
    plotDF1$CO2_GSlateLO <- exp(plotDF1$log_CO2_GSlate-plotDF1$log_CO2_GSlateSE)
    
    
    plotDF1$CO2_WUEdailyUP <- exp(plotDF1$log_CO2_WUEdaily+plotDF1$log_CO2_WUEdailySE)
    plotDF1$CO2_WUEdailyLO <- exp(plotDF1$log_CO2_WUEdaily-plotDF1$log_CO2_WUEdailySE)
    
    plotDF1$CO2_WUEearlyUP <- exp(plotDF1$log_CO2_WUEearly+plotDF1$log_CO2_WUEearlySE)
    plotDF1$CO2_WUEearlyLO <- exp(plotDF1$log_CO2_WUEearly-plotDF1$log_CO2_WUEearlySE)
    
    plotDF1$CO2_WUElateUP <- exp(plotDF1$log_CO2_WUElate+plotDF1$log_CO2_WUElateSE)
    plotDF1$CO2_WUElateLO <- exp(plotDF1$log_CO2_WUElate-plotDF1$log_CO2_WUElateSE)
    
    
    ## plotDF2
    plotDF2$CO2_AdailyUP <- exp(plotDF2$log_CO2_Adaily+plotDF2$log_CO2_AdailySE)
    plotDF2$CO2_AdailyLO <- exp(plotDF2$log_CO2_Adaily-plotDF2$log_CO2_AdailySE)
    
    plotDF2$CO2_AearlyUP <- exp(plotDF2$log_CO2_Aearly+plotDF2$log_CO2_AearlySE)
    plotDF2$CO2_AearlyLO <- exp(plotDF2$log_CO2_Aearly-plotDF2$log_CO2_AearlySE)
    
    plotDF2$CO2_AlateUP <- exp(plotDF2$log_CO2_Alate+plotDF2$log_CO2_AlateSE)
    plotDF2$CO2_AlateLO <- exp(plotDF2$log_CO2_Alate-plotDF2$log_CO2_AlateSE)
    
    
    plotDF2$CO2_GSdailyUP <- exp(plotDF2$log_CO2_GSdaily+plotDF2$log_CO2_GSdailySE)
    plotDF2$CO2_GSdailyLO <- exp(plotDF2$log_CO2_GSdaily-plotDF2$log_CO2_GSdailySE)
    
    plotDF2$CO2_GSearlyUP <- exp(plotDF2$log_CO2_GSearly+plotDF2$log_CO2_GSearlySE)
    plotDF2$CO2_GSearlyLO <- exp(plotDF2$log_CO2_GSearly-plotDF2$log_CO2_GSearlySE)
    
    plotDF2$CO2_GSlateUP <- exp(plotDF2$log_CO2_GSlate+plotDF2$log_CO2_GSlateSE)
    plotDF2$CO2_GSlateLO <- exp(plotDF2$log_CO2_GSlate-plotDF2$log_CO2_GSlateSE)
    
    
    plotDF2$CO2_WUEdailyUP <- exp(plotDF2$log_CO2_WUEdaily+plotDF2$log_CO2_WUEdailySE)
    plotDF2$CO2_WUEdailyLO <- exp(plotDF2$log_CO2_WUEdaily-plotDF2$log_CO2_WUEdailySE)
    
    plotDF2$CO2_WUEearlyUP <- exp(plotDF2$log_CO2_WUEearly+plotDF2$log_CO2_WUEearlySE)
    plotDF2$CO2_WUEearlyLO <- exp(plotDF2$log_CO2_WUEearly-plotDF2$log_CO2_WUEearlySE)
    
    plotDF2$CO2_WUElateUP <- exp(plotDF2$log_CO2_WUElate+plotDF2$log_CO2_WUElateSE)
    plotDF2$CO2_WUElateLO <- exp(plotDF2$log_CO2_WUElate-plotDF2$log_CO2_WUElateSE)
    
    
    ### plotting
    p1 <- ggplot(plotDF1, aes(x=Day, y=CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AdailyLO, 
                          ymax=CO2_AdailyUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p2 <- ggplot(plotDF1, aes(x=Day, y=CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AearlyLO, 
                          ymax=CO2_AearlyUP),
                      width=0.2)+
        geom_line(aes(col=Trt))+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p3 <- ggplot(plotDF1, aes(x=Day, y=CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AlateLO, 
                          ymax=CO2_AlateUP),
                      width=0.2)+
        geom_line(aes(col=Trt))+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p4 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSdailyLO, 
                          ymax=CO2_GSdailyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p5 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSearlyLO, 
                          ymax=CO2_GSearlyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p6 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSlateLO, 
                          ymax=CO2_GSlateUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    
    p7 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEdailyLO, 
                          ymax=CO2_WUEdailyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p8 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEearlyLO, 
                          ymax=CO2_WUEearlyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p9 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUElateLO, 
                          ymax=CO2_WUElateUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)",
                                         "(g)", "(h)", "(i)"), 
                                ncol=3, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf(paste0(outdir, "F10.1.CO2_ratio_pilularis.pdf"), width=14, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    
    
    
    ### plotting
    p1 <- ggplot(plotDF2, aes(x=Day, y=CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AdailyLO, 
                          ymax=CO2_AdailyUP),
                      width=1.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p2 <- ggplot(plotDF2, aes(x=Day, y=CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AearlyLO, 
                          ymax=CO2_AearlyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p3 <- ggplot(plotDF2, aes(x=Day, y=CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AlateLO, 
                          ymax=CO2_AlateUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    
    p4 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSdailyLO, 
                          ymax=CO2_GSdailyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p5 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSearlyLO, 
                          ymax=CO2_GSearlyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p6 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSlateLO, 
                          ymax=CO2_GSlateUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p7 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEdailyLO, 
                          ymax=CO2_WUEdailyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p8 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEearlyLO, 
                          ymax=CO2_WUEearlyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p9 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUElateLO, 
                          ymax=CO2_WUElateUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)",
                                         "(g)", "(h)", "(i)"), 
                                ncol=3, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf(paste0(outdir, "F10.2.CO2_ratio_populnea.pdf"), width=14, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
}