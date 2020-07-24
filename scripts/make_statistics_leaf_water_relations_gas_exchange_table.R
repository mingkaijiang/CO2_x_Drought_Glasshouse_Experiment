make_statistics_leaf_water_relations_gas_exchange_table <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv")
    
    myDF2 <- subset(myDF2, Day <= 16)
    
    myDF1$Day <- as.character(myDF1$Day)
    myDF2$Day <- as.character(myDF2$Day)
    
    
    #### prepare output
    outDF <- data.frame(rep(c("Epilularis", "Epopulnea"), each = 8),
                        rep(c("swc", "transpiration", "psiPD", "psiMD",
                              "Aearly", "Alate", "gsearly", "gslate"), 2),
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA)
    
    colnames(outDF) <- c("Species", "Variable", 
                          "Nominator_CO2", "Nominator_H2O", "Nominator_T",
                          "Nominator_CO2_H2O", "Nominator_CO2_T", "Nominator_H2O_T", 
                          "Nominator_CO2_H2O_T",
                          "Denominator_CO2", "Denominator_H2O", "Denominator_T", 
                          "Denominator_CO2_H2O", "Denominator_CO2_T", "Denominator_H2O_T", 
                          "Denominator_CO2_H2O_T",
                          "F_CO2", "F_H2O", "F_T", 
                          "F_CO2_H2O", "F_CO2_T", "F_H2O_T", 
                          "F_CO2_H2O_T",
                          "p_CO2", "p_H2O", "p_T", 
                          "p_CO2_H2O", "p_CO2_T", "p_H2O_T", 
                          "p_CO2_H2O_T")
    
    
    ### SWC
    mod1 <- lmer(SWC ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### transpiration
    mod1 <- lmer(transp_plant ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### psiPD
    mod1 <- lmer(psiPD ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### psiMD
    mod1 <- lmer(psiMD ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[7], 4)
    
    ### Aearly
    mod1 <- lmer(Aearly ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### Alate
    mod1 <- lmer(Alate ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### gsearly
    mod1 <- lmer(gsearly ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### gslate
    mod1 <- lmer(gslate ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epilularis"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    
    ######################## populnea
    ### SWC
    mod1 <- lmer(SWC ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### transpiration
    mod1 <- lmer(transp_plant ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### psiPD
    mod1 <- lmer(psiPD ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### psiMD
    mod1 <- lmer(psiMD ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[7], 4)
    
    ### Aearly
    mod1 <- lmer(Aearly ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Aearly"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### Alate
    mod1 <- lmer(Alate ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="Alate"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### gsearly
    mod1 <- lmer(gsearly ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gsearly"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### gslate
    mod1 <- lmer(gslate ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$NumDF[2]
    outDF$Nominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$NumDF[3]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$NumDF[4]
    outDF$Nominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$NumDF[5]
    outDF$Nominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$NumDF[6]
    outDF$Nominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$NumDF[7]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$DenDF[2]
    outDF$Denominator_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$DenDF[3]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$DenDF[4]
    outDF$Denominator_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$DenDF[5]
    outDF$Denominator_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$DenDF[6]
    outDF$Denominator_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$DenDF[7]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$`F value`[2]
    outDF$F_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$`F value`[3]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$`F value`[4]
    outDF$F_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$`F value`[5]
    outDF$F_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$`F value`[6]
    outDF$F_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- anov$`F value`[7]    
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[3], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[4], 4)
    outDF$p_CO2_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[5], 4)
    outDF$p_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[6], 4)
    outDF$p_CO2_H2O_T[outDF$Species=="Epopulnea"&outDF$Variable=="gslate"] <- round(anov$`Pr(>F)`[7], 4)
    
    
    ### save output
    write.csv(outDF, paste0(outdir, "statistics_leaf_water_relations_gas_exchange_by_species.csv"), row.names=F)
    
    
    
    
}