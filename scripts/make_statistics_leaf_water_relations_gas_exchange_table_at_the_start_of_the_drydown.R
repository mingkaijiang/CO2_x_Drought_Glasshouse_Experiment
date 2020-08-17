make_statistics_leaf_water_relations_gas_exchange_table_at_the_start_of_the_drydown <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv")
    
    myDF1 <- subset(myDF1, Day == 1)
    myDF2 <- subset(myDF2, Day == 1)
    
    ### set class
    myDF1$Glasshouse <- as.character(myDF1$Glasshouse)
    myDF2$Glasshouse <- as.character(myDF2$Glasshouse)
    
    myDF1$CO2 <- as.character(myDF1$CO2)
    myDF2$CO2 <- as.character(myDF2$CO2)
    
    myDF1$H2O <- as.character(myDF1$H2O)
    myDF2$H2O <- as.character(myDF2$H2O)
    
    ### calculations
    myDF1$Adaily <- with(myDF1, (Aearly+Alate)/2)
    myDF2$Adaily <- with(myDF2, (Aearly+Alate)/2)
    
    myDF1$gsdaily <- with(myDF1, (gsearly+gslate)/2)
    myDF2$gsdaily <- with(myDF2, (gsearly+gslate)/2)
    

    #### prepare output
    outDF <- data.frame(rep(c("Epilularis", "Epopulnea"), each = 7),
                        rep(c("swc", "transpiration", 
                              "psiPD", "psiMD",
                              "Adaily", "gsdaily", 
                              "E_psi"), 2),
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA)
    
    colnames(outDF) <- c("Species", "Variable", 
                          "Nominator_CO2", "Nominator_H2O", 
                          "Nominator_CO2_H2O", 
                          "Denominator_CO2", "Denominator_H2O", 
                          "Denominator_CO2_H2O", 
                          "F_CO2", "F_H2O", 
                          "F_CO2_H2O", 
                          "p_CO2", "p_H2O", 
                          "p_CO2_H2O")
    
    
    ### SWC
    mod1 <- lmer(SWC ~ CO2 * H2O + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### transpiration
    mod1 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### psiPD
    mod1 <- lmer(psiPD ~ CO2 * H2O + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- anov$`F value`[3]

    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### psiMD
    mod1 <- lmer(psiMD ~ CO2 * H2O + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[3], 4)

    ### A daily
    mod1 <- lmer(Adaily ~ CO2 * H2O + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="Adaily"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### gs daily
    mod1 <- lmer(gsdaily ~ CO2 * H2O + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="gsdaily"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### E_psi
    mod1 <- lmer(E_psi ~ CO2 * H2O + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="E_psi"] <- round(anov$`Pr(>F)`[3], 4)

    
    ######################## populnea
    ### SWC
    mod1 <- lmer(SWC ~ CO2 * H2O + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- anov$`F value`[3]
    
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="swc"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### transpiration
    mod1 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- anov$`F value`[3]

    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="transpiration"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### psiPD
    mod1 <- lmer(psiPD ~ CO2 * H2O + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- anov$`F value`[3]

    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiPD"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### psiMD
    mod1 <- lmer(psiMD ~ CO2 * H2O  + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="psiMD"] <- round(anov$`Pr(>F)`[3], 4)

    ### Adaily
    mod1 <- lmer(Adaily ~ CO2 * H2O + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="Adaily"] <- round(anov$`Pr(>F)`[3], 4)

    
    
    
    ### gsdaily
    mod1 <- lmer(gsdaily ~ CO2 * H2O + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="gsdaily"] <- round(anov$`Pr(>F)`[3], 4)

    
    ### E_psi
    mod1 <- lmer(E_psi ~ CO2 * H2O + (1|Glasshouse), data=myDF2)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$NumDF[3]

    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$DenDF[3]

    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- anov$`F value`[3]

    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="E_psi"] <- round(anov$`Pr(>F)`[3], 4)

    ### save output
    write.csv(outDF, paste0(outdir, "statistics_leaf_water_relations_gas_exchange_by_species_at_the_start_of_drawdown.csv"), row.names=F)
    
    
    
    
}