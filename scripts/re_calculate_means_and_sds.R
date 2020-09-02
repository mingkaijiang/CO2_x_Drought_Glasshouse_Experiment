re_calculate_means_and_sds <- function() {
    
    #################### E. pilularis
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    
    ### recalcualte E_psi
    myDF1$E_psi <- with(myDF1, (transp_plant/(psiPD-psiMD)))
    myDF1$Adaily <- with(myDF1, ((Aearly+Alate)/2))
    myDF1$gsdaily <- with(myDF1, ((gsearly+gslate)/2))
    
    ## create dailyDF
    #dDF1 <- myDF1[,c("Replicate", "Trt", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    #dDF2 <- myDF1[,c("Replicate", "Trt", "Day", "Alate", "gslate", "transp_leaf_late")]
    #
    #colnames(dDF1) <- colnames(dDF2) <- c("Replicate", "Trt", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    #
    #dDF <- rbind(dDF1, dDF2)
    #
    #### summary by
    #outDF1 <- summaryBy(psiPD+psiMD+transp_plant+E_psi+SWC+Aearly+Alate+gsearly+gslate+transp_leaf_early+transp_leaf_late~Trt+Day,
    #                    FUN=c(mean, sd, se), data=myDF1)
    #
    #outDF2 <- summaryBy(Adaily+gsdaily+transp_leaf_daily~Trt+Day,
    #                    FUN=c(mean, sd, se), data=dDF)
    #
    #outDF <- merge(outDF1, outDF2, by=c("Trt", "Day"))
    
    outDF <- summaryBy(psiPD+psiMD+transp_plant+E_psi+SWC+Aearly+Alate+gsearly+gslate+transp_leaf_early+transp_leaf_late+Adaily+gsdaily~Trt+Day,
                       FUN=c(mean, sd, se), data=myDF1)
    
    ### restructure
    outDF <- outDF[,c("Trt", "Day", 
                      "psiPD.mean", "psiPD.sd", "psiPD.se",
                      "psiMD.mean", "psiMD.sd", "psiMD.se",
                      "transp_plant.mean", "transp_plant.sd", "transp_plant.se",
                      "SWC.mean", "SWC.sd", "SWC.se",
                      "Adaily.mean", "Adaily.sd", "Adaily.se",
                      "Aearly.mean", "Aearly.sd", "Aearly.se",
                      "Alate.mean", "Alate.sd", "Alate.se",
                      "gsdaily.mean", "gsdaily.sd", "gsdaily.se",
                      "gsearly.mean", "gsearly.sd", "gsearly.se",
                      "gslate.mean", "gslate.sd", "gslate.se")]
    
    colnames(outDF) <- c("Trt", "Day", 
                         "psiPD", "psiPDSD", "psiPDSE",
                         "psiMD", "psiMDSD", "psiMDSE",
                         "transp_plant", "transp_plantSD", "transp_plantSE",
                         "SWC", "SWCSD", "SWCSE",
                         "Adaily", "AdailySD", "AdailySE",
                         "Aearly", "AearlySD", "AearlySE",
                         "Alate", "AlateSD", "AlateSE",
                         "gsdaily", "gsdailySD", "gsdailySE",
                         "gsearly", "gsearlySD", "gsearlySE",
                         "gslate", "gslateSD", "gslateSE")
    
    
    write.csv(outDF, "data/glasshouse2/Pilularis_Phys_Processed.csv",
              row.names=F)
    
    
    #################### E. populnea
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv")
    
    ### recalcualte E_psi
    myDF2$E_psi <- with(myDF2, (transp_plant/(psiPD-psiMD)))
    myDF2$Adaily <- with(myDF2, ((Aearly+Alate)/2))
    myDF2$gsdaily <- with(myDF2, ((gsearly+gslate)/2))
    
    ## create dailyDF
    #dDF1 <- myDF2[,c("Replicate", "Trt", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    #dDF2 <- myDF2[,c("Replicate", "Trt", "Day", "Alate", "gslate", "transp_leaf_late")]
    #
    #colnames(dDF1) <- colnames(dDF2) <- c("Replicate", "Trt", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    #
    #dDF <- rbind(dDF1, dDF2)
    #
    #### summary by
    #outDF1 <- summaryBy(psiPD+psiMD+transp_plant+E_psi+SWC+Aearly+Alate+gsearly+gslate+transp_leaf_early+transp_leaf_late~Trt+Day,
    #                    FUN=c(mean, sd, se), data=myDF2)
    #
    #outDF2 <- summaryBy(Adaily+gsdaily+transp_leaf_daily~Trt+Day,
    #                    FUN=c(mean, sd, se), data=dDF)
    #
    #outDF <- merge(outDF1, outDF2, by=c("Trt", "Day"))
    
    outDF <- summaryBy(psiPD+psiMD+transp_plant+E_psi+SWC+Aearly+Alate+gsearly+gslate+transp_leaf_early+transp_leaf_late+Adaily+gsdaily~Trt+Day,
                       FUN=c(mean, sd, se), data=myDF2)
    
    ### restructure
    outDF <- outDF[,c("Trt", "Day", 
                      "psiPD.mean", "psiPD.sd", "psiPD.se",
                      "psiMD.mean", "psiMD.sd", "psiMD.se",
                      "transp_plant.mean", "transp_plant.sd", "transp_plant.se",
                      "SWC.mean", "SWC.sd", "SWC.se",
                      "Adaily.mean", "Adaily.sd", "Adaily.se",
                      "Aearly.mean", "Aearly.sd", "Aearly.se",
                      "Alate.mean", "Alate.sd", "Alate.se",
                      "gsdaily.mean", "gsdaily.sd", "gsdaily.se",
                      "gsearly.mean", "gsearly.sd", "gsearly.se",
                      "gslate.mean", "gslate.sd", "gslate.se")]
    
    colnames(outDF) <- c("Trt", "Day", 
                         "psiPD", "psiPDSD", "psiPDSE",
                         "psiMD", "psiMDSD", "psiMDSE",
                         "transp_plant", "transp_plantSD", "transp_plantSE",
                         "SWC", "SWCSD", "SWCSE",
                         "Adaily", "AdailySD", "AdailySE",
                         "Aearly", "AearlySD", "AearlySE",
                         "Alate", "AlateSD", "AlateSE",
                         "gsdaily", "gsdailySD", "gsdailySE",
                         "gsearly", "gsearlySD", "gsearlySE",
                         "gslate", "gslateSD", "gslateSE")
    
    
    write.csv(outDF, "data/glasshouse2/Populnea_Phys_Processed.csv",
              row.names=F)
    
    
    
}