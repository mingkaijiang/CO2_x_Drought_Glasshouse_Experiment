make_CO2_ratios_of_A_and_gs_plots <- function() {
    
    ### read input
    pilDF<-read.csv("data/glasshouse2/Pilularis_Phys.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Populnea_Phys.csv",sep=",", header=TRUE)
    
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
                          NA, NA, NA, NA, NA, NA)
    
    plotDF2 <- data.frame(rep(w, length(d2)), 
                          rep(d2, each=2), NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA)
    colnames(plotDF1) <- colnames(plotDF2) <- c("Trt", "Day",
                                                "amb_Adaily", "ele_Adaily",
                                                "amb_Aearly", "ele_Aearly",
                                                "amb_Alate", "ele_Alate",
                                                "amb_GSdaily", "ele_GSdaily",
                                                "amb_GSearly", "ele_GSearly",
                                                "amb_GSlate", "ele_GSlate",
                                                "amb_AdailySD", "ele_AdailySD",
                                                "amb_AearlySD", "ele_AearlySD",
                                                "amb_AlateSD", "ele_AlateSD",
                                                "amb_GSdailySD", "ele_GSdailySD",
                                                "amb_GSearlySD", "ele_GSearlySD",
                                                "amb_GSlateSD", "ele_GSlateSD",
                                                "amb_AdailyN", "ele_AdailyN",
                                                "amb_AearlyN", "ele_AearlyN",
                                                "amb_AlateN", "ele_AlateN")
    
    for (i in d1) {
        # A
        plotDF1$amb_Adaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_Aearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_Alate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_Adaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_Aearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_Alate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_Adaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_Aearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_Alate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_Adaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_Aearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_Alate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILEND"&pilDF$Day==i]
        
        ### gs
        plotDF1$amb_GSdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSlate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_GSdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSlate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_GSdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSlate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_GSdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSlate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILEND"&pilDF$Day==i]
        
        # A SD
        plotDF1$amb_AdailySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$AdailySD[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_AearlySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$AearlySD[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_AlateSD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$AlateSD[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_AdailySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$AdailySD[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_AearlySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$AearlySD[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_AlateSD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$AlateSD[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_AdailySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$AdailySD[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_AearlySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$AearlySD[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_AlateSD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$AlateSD[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_AdailySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$AdailySD[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_AearlySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$AearlySD[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_AlateSD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$AlateSD[pilDF$Trt=="PILEND"&pilDF$Day==i]
        
        
        # A n
        plotDF1$amb_AdailyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.4[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_AearlyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.5[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_AlateN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.6[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_AdailyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.4[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_AearlyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.5[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_AlateN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.6[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_AdailyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.4[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_AearlyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.5[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_AlateN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.6[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_AdailyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.4[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_AearlyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.5[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_AlateN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.6[pilDF$Trt=="PILEND"&pilDF$Day==i]
        
        # gs SD
        plotDF1$amb_GSdailySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsDailySD[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSearlySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsearlySD[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSlateSD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gslateSD[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_GSdailySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsDailySD[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSearlySD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsearlySD[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSlateSD[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gslateSD[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_GSdailySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsDailySD[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSearlySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsearlySD[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSlateSD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gslateSD[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_GSdailySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsDailySD[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSearlySD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsearlySD[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSlateSD[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gslateSD[pilDF$Trt=="PILEND"&pilDF$Day==i]
        
        
        # gs n
        plotDF1$amb_GSdailyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.7[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSearlyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.8[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSlateN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.9[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_GSdailyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.7[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSearlyN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.8[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSlateN[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$n.9[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_GSdailyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.7[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSearlyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.8[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSlateN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.9[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_GSdailyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.7[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSearlyN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.8[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSlateN[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$n.9[pilDF$Trt=="PILEND"&pilDF$Day==i]
        
    }
    
    
    
    for (i in d2) {
        # A
        plotDF2$amb_Adaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Adaily[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                      NA)
        plotDF2$amb_Aearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Aearly[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                      NA)
        plotDF2$amb_Alate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                     as.numeric(popDF$Alate[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                     NA)
        
        plotDF2$ele_Adaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Adaily[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                      NA)
        plotDF2$ele_Aearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Aearly[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                      NA)
        plotDF2$ele_Alate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                     as.numeric(popDF$Alate[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                     NA)
        
        plotDF2$amb_Adaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Adaily[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_Aearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Aearly[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_Alate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Alate[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                      NA)
        
        plotDF2$ele_Adaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Adaily[popDF$Trt=="POPEND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_Aearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Aearly[popDF$Trt=="POPEND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_Alate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Alate[popDF$Trt=="POPEND"&popDF$Day==i]), 
                                                                      NA)
        ### gs
        plotDF2$amb_GSdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsDaily[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_GSearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsearly[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_GSlate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$gslate[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                      NA)
        
        plotDF2$ele_GSdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsDaily[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_GSearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsearly[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_GSlate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$gslate[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                      NA)
        
        plotDF2$amb_GSdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPAND"&popDF$Day==i])==1, 
                                                                        as.numeric(popDF$gsDaily[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                        NA)
        plotDF2$amb_GSearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                        as.numeric(popDF$gsearly[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                        NA)
        plotDF2$amb_GSlate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gslate[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                       NA)
        
        plotDF2$ele_GSdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                        as.numeric(popDF$gsDaily[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                        NA)
        plotDF2$ele_GSearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                        as.numeric(popDF$gsearly[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                        NA)
        plotDF2$ele_GSlate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gslate[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                       NA)
        
        # A SD
        plotDF2$amb_AdailySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$AdailySD[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$AdailySD[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                               NA)
        plotDF2$amb_AearlySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$AearlySD[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$AearlySD[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                               NA)
        plotDF2$amb_AlateSD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$AlateSD[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$AlateSD[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                              NA)
        
        plotDF2$ele_AdailySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$AdailySD[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$AdailySD[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                               NA)
        plotDF2$ele_AearlySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$AearlySD[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$AearlySD[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                               NA)
        plotDF2$ele_AlateSD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$AlateSD[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$AlateSD[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                              NA)
        
        plotDF2$amb_AdailySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$AdailySD[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$AdailySD[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$amb_AearlySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$AearlySD[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$AearlySD[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$amb_AlateSD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$AlateSD[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$AlateSD[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                               NA)
        
        plotDF2$ele_AdailySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$AdailySD[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$AdailySD[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$ele_AearlySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$AearlySD[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$AearlySD[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$ele_AlateSD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$AlateSD[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$AlateSD[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                               NA)
        
        
        # A n
        plotDF2$amb_AdailyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.4[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.4[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                              NA)
        plotDF2$amb_AearlyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.5[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.5[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                              NA)
        plotDF2$amb_AlateN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.6[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                             as.numeric(popDF$n.6[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                             NA)
        
        plotDF2$ele_AdailyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.4[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.4[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                              NA)
        plotDF2$ele_AearlyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.5[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.5[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                              NA)
        plotDF2$ele_AlateN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.6[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                             as.numeric(popDF$n.6[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                             NA)
        
        plotDF2$amb_AdailyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.4[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.4[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                               NA)
        plotDF2$amb_AearlyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.5[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.5[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                               NA)
        plotDF2$amb_AlateN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.6[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.6[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                              NA)
        
        plotDF2$ele_AdailyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.4[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.4[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                               NA)
        plotDF2$ele_AearlyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.5[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.5[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                               NA)
        plotDF2$ele_AlateN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.6[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.6[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                              NA)
        
        # gs SD
        plotDF2$amb_GSdailySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsDailySD[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$gsDailySD[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                                NA)
        plotDF2$amb_GSearlySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsearlySD[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$gsearlySD[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                                NA)
        plotDF2$amb_GSlateSD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gslateSD[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$gslateSD[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                               NA)
        
        plotDF2$ele_GSdailySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsDailySD[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$gsDailySD[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                                NA)
        plotDF2$ele_GSearlySD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsearlySD[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$gsearlySD[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                                NA)
        plotDF2$ele_GSlateSD[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gslateSD[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$gslateSD[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                               NA)
        
        plotDF2$amb_GSdailySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsDailySD[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                                 as.numeric(popDF$gsDailySD[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                                 NA)
        plotDF2$amb_GSearlySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsearlySD[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                                 as.numeric(popDF$gsearlySD[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                                 NA)
        plotDF2$amb_GSlateSD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gslateSD[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$gslateSD[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                                NA)
        
        plotDF2$ele_GSdailySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsDailySD[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                                 as.numeric(popDF$gsDailySD[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                                 NA)
        plotDF2$ele_GSearlySD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsearlySD[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                                 as.numeric(popDF$gsearlySD[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                                 NA)
        plotDF2$ele_GSlateSD[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gslateSD[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$gslateSD[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                                NA)
        
        
        # gs n
        plotDF2$amb_GSdailyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.7[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.7[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                               NA)
        plotDF2$amb_GSearlyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.8[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.8[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                               NA)
        plotDF2$amb_GSlateN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.9[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.9[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                              NA)
        
        plotDF2$ele_GSdailyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.7[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.7[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                               NA)
        plotDF2$ele_GSearlyN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.8[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.8[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                               NA)
        plotDF2$ele_GSlateN[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$n.9[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                              as.numeric(popDF$n.9[popDF$Trt=="POPED"&popDF$Day==i]),
                                                                              NA)
        
        plotDF2$amb_GSdailyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.7[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$n.7[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$amb_GSearlyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.8[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$n.8[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$amb_GSlateN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.9[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.9[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                               NA)
        
        plotDF2$ele_GSdailyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.7[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$n.7[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$ele_GSearlyN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.8[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                                as.numeric(popDF$n.8[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                                NA)
        plotDF2$ele_GSlateN[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$n.9[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                               as.numeric(popDF$n.9[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                               NA)
        
    }
    
    ### ignore NAs
    plotDF1 <- plotDF1[complete.cases(plotDF1),]
    plotDF2 <- plotDF2[complete.cases(plotDF2),]
    plotDF2 <- as.data.frame(sapply(plotDF2, as.numeric))
    plotDF2$Trt <- gsub(1, "D", plotDF2$Trt)
    plotDF2$Trt <- gsub(2, "ND", plotDF2$Trt)
    plotDF2$Trt <- as.character(plotDF2$Trt)
    
    ### Calcualte iWUE
    plotDF1$amb_WUEdaily <- plotDF1$amb_Adaily / plotDF1$amb_GSdaily
    plotDF1$amb_WUEearly <- plotDF1$amb_Aearly / plotDF1$amb_GSearly
    plotDF1$amb_WUElate <- plotDF1$amb_Alate / plotDF1$amb_GSlate
    
    plotDF1$ele_WUEdaily <- plotDF1$ele_Adaily / plotDF1$ele_GSdaily
    plotDF1$ele_WUEearly <- plotDF1$ele_Aearly / plotDF1$ele_GSearly
    plotDF1$ele_WUElate <- plotDF1$ele_Alate / plotDF1$ele_GSlate
    
    plotDF2$amb_WUEdaily <- plotDF2$amb_Adaily / plotDF2$amb_GSdaily
    plotDF2$amb_WUEearly <- plotDF2$amb_Aearly / plotDF2$amb_GSearly
    plotDF2$amb_WUElate <- plotDF2$amb_Alate / plotDF2$amb_GSlate
    
    plotDF2$ele_WUEdaily <- plotDF2$ele_Adaily / plotDF2$ele_GSdaily
    plotDF2$ele_WUEearly <- plotDF2$ele_Aearly / plotDF2$ele_GSearly
    plotDF2$ele_WUElate <- plotDF2$ele_Alate / plotDF2$ele_GSlate
    
    
    ### Calculate CO2 signal
    plotDF1$CO2_Adaily <- plotDF1$ele_Adaily / plotDF1$amb_Adaily
    plotDF1$CO2_Aearly <- plotDF1$ele_Aearly / plotDF1$amb_Aearly
    plotDF1$CO2_Alate <- plotDF1$ele_Alate / plotDF1$amb_Alate
    
    plotDF2$CO2_Adaily <- plotDF2$ele_Adaily / plotDF2$amb_Adaily
    plotDF2$CO2_Aearly <- plotDF2$ele_Aearly / plotDF2$amb_Aearly
    plotDF2$CO2_Alate <- plotDF2$ele_Alate / plotDF2$amb_Alate
    
    plotDF1$CO2_GSdaily <- plotDF1$ele_GSdaily / plotDF1$amb_GSdaily
    plotDF1$CO2_GSearly <- plotDF1$ele_GSearly / plotDF1$amb_GSearly
    plotDF1$CO2_GSlate <- plotDF1$ele_GSlate / plotDF1$amb_GSlate
    
    plotDF2$CO2_GSdaily <- plotDF2$ele_GSdaily / plotDF2$amb_GSdaily
    plotDF2$CO2_GSearly <- plotDF2$ele_GSearly / plotDF2$amb_GSearly
    plotDF2$CO2_GSlate <- plotDF2$ele_GSlate / plotDF2$amb_GSlate
    
    plotDF1$CO2_WUEdaily <- plotDF1$ele_WUEdaily / plotDF1$amb_WUEdaily
    plotDF1$CO2_WUEearly <- plotDF1$ele_WUEearly / plotDF1$amb_WUEearly
    plotDF1$CO2_WUElate <- plotDF1$ele_WUElate / plotDF1$amb_WUElate
    
    plotDF2$CO2_WUEdaily <- plotDF2$ele_WUEdaily / plotDF2$amb_WUEdaily
    plotDF2$CO2_WUEearly <- plotDF2$ele_WUEearly / plotDF2$amb_WUEearly
    plotDF2$CO2_WUElate <- plotDF2$ele_WUElate / plotDF2$amb_WUElate
    
    ### calculate log to obtain SE
    set.seed(123)
    for (i in w) {
        for (j in d1) {
            ## subset
            subDF <- subset(plotDF1, Trt==i & Day== j)
            
            ### create random data
            XA1 <- rnorm(n=subDF$amb_AdailyN, mean=subDF$amb_Adaily, 
                         sd=subDF$amb_AdailySD)
            XE1 <- rnorm(n=subDF$ele_AdailyN, mean=subDF$ele_Adaily, 
                         sd=subDF$ele_AdailySD)
            
            XA2 <- rnorm(n=subDF$amb_AearlyN, mean=subDF$amb_Aearly, 
                         sd=subDF$amb_AearlySD)
            XE2 <- rnorm(n=subDF$ele_AearlyN, mean=subDF$ele_Aearly, 
                         sd=subDF$ele_AearlySD)
            
            XA3 <- rnorm(n=subDF$amb_AlateN, mean=subDF$amb_Alate, 
                         sd=subDF$amb_AlateSD)
            XE3 <- rnorm(n=subDF$ele_AlateN, mean=subDF$ele_Alate, 
                         sd=subDF$ele_AlateSD)
            
            XA4 <- rnorm(n=subDF$amb_GSdailyN, mean=subDF$amb_GSdaily, 
                         sd=subDF$amb_GSdailySD)
            XE4 <- rnorm(n=subDF$ele_GSdailyN, mean=subDF$ele_GSdaily, 
                         sd=subDF$ele_GSdailySD)
            
            XA5 <- rnorm(n=subDF$amb_GSearlyN, mean=subDF$amb_GSearly, 
                         sd=subDF$amb_GSearlySD)
            XE5 <- rnorm(n=subDF$ele_GSearlyN, mean=subDF$ele_GSearly, 
                         sd=subDF$ele_GSearlySD)
            
            XA6 <- rnorm(n=subDF$amb_GSlateN, mean=subDF$amb_GSlate, 
                         sd=subDF$amb_GSlateSD)
            XE6 <- rnorm(n=subDF$ele_GSlateN, mean=subDF$ele_GSlate, 
                         sd=subDF$ele_GSlateSD)
            
            ## calculate log
            lnXA1 <- log(XA1) 
            lnXE1 <- log(XE1)  
            lnXA2 <- log(XA2) 
            lnXE2 <- log(XE2)  
            lnXA3 <- log(XA3) 
            lnXE3 <- log(XE3)  
            lnXA4 <- log(XA4) 
            lnXE4 <- log(XE4)  
            lnXA5 <- log(XA5) 
            lnXE5 <- log(XE5)  
            lnXA6 <- log(XA6) 
            lnXE6 <- log(XE6) 
            
            ## log difference
            ln.diff1 <- mean(lnXE1, na.rm=T) - mean(lnXA1, na.rm=T)
            ln.diff2 <- mean(lnXE2, na.rm=T) - mean(lnXA2, na.rm=T)
            ln.diff3 <- mean(lnXE3, na.rm=T) - mean(lnXA3, na.rm=T)
            ln.diff4 <- mean(lnXE4, na.rm=T) - mean(lnXA4, na.rm=T)
            ln.diff5 <- mean(lnXE5, na.rm=T) - mean(lnXA5, na.rm=T)
            ln.diff6 <- mean(lnXE6, na.rm=T) - mean(lnXA6, na.rm=T)
            
            ## SE
            ln.se1 <- sqrt(se(lnXA1)^2 + se(lnXE1)^2)
            ln.se2 <- sqrt(se(lnXA2)^2 + se(lnXE2)^2)
            ln.se3 <- sqrt(se(lnXA3)^2 + se(lnXE3)^2)
            ln.se4 <- sqrt(se(lnXA4)^2 + se(lnXE4)^2)
            ln.se5 <- sqrt(se(lnXA5)^2 + se(lnXE5)^2)
            ln.se6 <- sqrt(se(lnXA6)^2 + se(lnXE6)^2)
            
            ## assign to original dataframe
            plotDF1$CO2_AdailyUP[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff1+ln.se1)
            plotDF1$CO2_AearlyUP[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff2+ln.se2)
            plotDF1$CO2_AlateUP[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff3+ln.se3)
            plotDF1$CO2_GSdailyUP[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff4+ln.se4)
            plotDF1$CO2_GSearlyUP[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff5+ln.se5)
            plotDF1$CO2_GSlateUP[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff6+ln.se6)
            
            
            plotDF1$CO2_AdailyLO[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff1-ln.se1)
            plotDF1$CO2_AearlyLO[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff2-ln.se2)
            plotDF1$CO2_AlateLO[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff3-ln.se3)
            plotDF1$CO2_GSdailyLO[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff4-ln.se4)
            plotDF1$CO2_GSearlyLO[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff5-ln.se5)
            plotDF1$CO2_GSlateLO[plotDF1$Trt==i&plotDF1$Day==j] <- exp(ln.diff6-ln.se6)
            
        }
    }
    
    for (i in w) {
        for (j in d1) {
            ## subset
            subDF <- subset(plotDF2, Trt==i & Day== j)
            
            ### create random data
            XA1 <- rnorm(n=subDF$amb_AdailyN, mean=subDF$amb_Adaily, 
                         sd=subDF$amb_AdailySD)
            XE1 <- rnorm(n=subDF$ele_AdailyN, mean=subDF$ele_Adaily, 
                         sd=subDF$ele_AdailySD)
            
            XA2 <- rnorm(n=subDF$amb_AearlyN, mean=subDF$amb_Aearly, 
                         sd=subDF$amb_AearlySD)
            XE2 <- rnorm(n=subDF$ele_AearlyN, mean=subDF$ele_Aearly, 
                         sd=subDF$ele_AearlySD)
            
            XA3 <- rnorm(n=subDF$amb_AlateN, mean=subDF$amb_Alate, 
                         sd=subDF$amb_AlateSD)
            XE3 <- rnorm(n=subDF$ele_AlateN, mean=subDF$ele_Alate, 
                         sd=subDF$ele_AlateSD)
            
            XA4 <- rnorm(n=subDF$amb_GSdailyN, mean=subDF$amb_GSdaily, 
                         sd=subDF$amb_GSdailySD)
            XE4 <- rnorm(n=subDF$ele_GSdailyN, mean=subDF$ele_GSdaily, 
                         sd=subDF$ele_GSdailySD)
            
            XA5 <- rnorm(n=subDF$amb_GSearlyN, mean=subDF$amb_GSearly, 
                         sd=subDF$amb_GSearlySD)
            XE5 <- rnorm(n=subDF$ele_GSearlyN, mean=subDF$ele_GSearly, 
                         sd=subDF$ele_GSearlySD)
            
            XA6 <- rnorm(n=subDF$amb_GSlateN, mean=subDF$amb_GSlate, 
                         sd=subDF$amb_GSlateSD)
            XE6 <- rnorm(n=subDF$ele_GSlateN, mean=subDF$ele_GSlate, 
                         sd=subDF$ele_GSlateSD)
            
            ## calculate log
            lnXA1 <- log(XA1) 
            lnXE1 <- log(XE1)  
            lnXA2 <- log(XA2) 
            lnXE2 <- log(XE2)  
            lnXA3 <- log(XA3) 
            lnXE3 <- log(XE3)  
            lnXA4 <- log(XA4) 
            lnXE4 <- log(XE4)  
            lnXA5 <- log(XA5) 
            lnXE5 <- log(XE5)  
            lnXA6 <- log(XA6) 
            lnXE6 <- log(XE6) 
            
            ## log difference
            ln.diff1 <- mean(lnXE1, na.rm=T) - mean(lnXA1, na.rm=T)
            ln.diff2 <- mean(lnXE2, na.rm=T) - mean(lnXA2, na.rm=T)
            ln.diff3 <- mean(lnXE3, na.rm=T) - mean(lnXA3, na.rm=T)
            ln.diff4 <- mean(lnXE4, na.rm=T) - mean(lnXA4, na.rm=T)
            ln.diff5 <- mean(lnXE5, na.rm=T) - mean(lnXA5, na.rm=T)
            ln.diff6 <- mean(lnXE6, na.rm=T) - mean(lnXA6, na.rm=T)
            
            ## SE
            ln.se1 <- sqrt(se(lnXA1)^2 + se(lnXE1)^2)
            ln.se2 <- sqrt(se(lnXA2)^2 + se(lnXE2)^2)
            ln.se3 <- sqrt(se(lnXA3)^2 + se(lnXE3)^2)
            ln.se4 <- sqrt(se(lnXA4)^2 + se(lnXE4)^2)
            ln.se5 <- sqrt(se(lnXA5)^2 + se(lnXE5)^2)
            ln.se6 <- sqrt(se(lnXA6)^2 + se(lnXE6)^2)
            
            ## assign to original dataframe
            plotDF2$CO2_AdailyUP[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff1+ln.se1)
            plotDF2$CO2_AearlyUP[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff2+ln.se2)
            plotDF2$CO2_AlateUP[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff3+ln.se3)
            plotDF2$CO2_GSdailyUP[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff4+ln.se4)
            plotDF2$CO2_GSearlyUP[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff5+ln.se5)
            plotDF2$CO2_GSlateUP[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff6+ln.se6)
            
            
            plotDF2$CO2_AdailyLO[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff1-ln.se1)
            plotDF2$CO2_AearlyLO[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff2-ln.se2)
            plotDF2$CO2_AlateLO[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff3-ln.se3)
            plotDF2$CO2_GSdailyLO[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff4-ln.se4)
            plotDF2$CO2_GSearlyLO[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff5-ln.se5)
            plotDF2$CO2_GSlateLO[plotDF2$Trt==i&plotDF2$Day==j] <- exp(ln.diff6-ln.se6)
        }
    }
    
    
    ### plotting
    p1 <- ggplot(plotDF1, aes(x=Day, y=CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AdailyLO, ymax=CO2_AdailyUP),
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
        ylim(0, 12)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p2 <- ggplot(plotDF1, aes(x=Day, y=CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 8)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p3 <- ggplot(plotDF1, aes(x=Day, y=CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 8)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p4 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p5 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p6 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    
    p7 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p8 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p9 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 5)+
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
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p2 <- ggplot(plotDF2, aes(x=Day, y=CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p3 <- ggplot(plotDF2, aes(x=Day, y=CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    
    p4 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p5 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p6 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p7 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p8 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p9 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylim(0, 3)+
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