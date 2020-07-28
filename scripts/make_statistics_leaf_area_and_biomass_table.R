make_statistics_leaf_area_and_biomass_table <- function() {
    
    ### processing
    myDF <- read.csv("data/glasshouse2/EUC_Leaf_Area_data.csv")
    
    colnames(myDF) <- c("ID", "Species", "Species_Treatment", "GH",
                        "Treatment", "CO2", "H2O", "LA_early", "LA_plant",
                        "LA_final", "LA", "Stem", "SM", "Leaf", "LM", 
                        "Root", "RM", "CoarseRoot", "CRM", "FineRoot", 
                        "FRM")
    
    ### total biomass
    myDF$TOT <- with(myDF, SM+LM+FRM+CRM)
    
    ### Root to shoot ratio
    myDF$RS_ratio <- with(myDF, (Root/(Leaf+Stem)))
    
    ### Root mass to leaf area ratio
    myDF$RL_ratio <- with(myDF, (Root/LA_plant))
    
    
    ### prepare outDF
    outDF <- data.frame(rep(c("Epilularis", "Epopulnea"), each = 7),
                        rep(c("LA", "total", "leaf", "stem",
                              "root", "RS_ratio", "RL_ratio"), 2),
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Species", "Variable", 
                         "Nominator_CO2", "Nominator_H2O", "Nominator_CO2_H2O", 
                         "Denominator_CO2", "Denominator_H2O", "Denominator_CO2_H2O", 
                         "F_CO2", "F_H2O", "F_CO2_H2O", 
                         "p_CO2", "p_H2O", "p_CO2_H2O")
    
    outDF2 <- data.frame(c("LA", "total", "leaf", "stem",
                               "root", "RS_ratio", "RL_ratio"),
                         NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA)
    
    colnames(outDF2) <- c("Variable", 
                         "Nominator_CO2", "Nominator_H2O", "Nominator_S",
                         "Nominator_CO2_H2O", "Nominator_CO2_S", "Nominator_H2O_S", 
                         "Nominator_CO2_H2O_S",
                         "Denominator_CO2", "Denominator_H2O", "Denominator_S", 
                         "Denominator_CO2_H2O", "Denominator_CO2_S", "Denominator_H2O_S", 
                         "Denominator_CO2_H2O_S",
                         "F_CO2", "F_H2O", "F_S", 
                         "F_CO2_H2O", "F_CO2_S", "F_H2O_S", 
                         "F_CO2_H2O_S",
                         "p_CO2", "p_H2O", "p_S", 
                         "p_CO2_H2O", "p_CO2_S", "p_H2O_S", 
                         "p_CO2_H2O_S")
    
    
    ############################# E.pilularis ##############################
    subDF <- subset(myDF, Species=="PIL")
    
    ### LA
    mod1 <- lmer(LA ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="LA"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### total biomass
    mod1 <- lmer(TOT ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="total"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### leaf biomass
    mod1 <- lmer(Leaf ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="leaf"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### Stem biomass
    mod1 <- lmer(Stem ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="stem"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### root biomass
    mod1 <- lmer(Root ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="root"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### RS ratio
    mod1 <- lmer(RS_ratio ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### RL_ratio 
    mod1 <- lmer(RL_ratio ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    
    ############################# E.populnea ##############################
    subDF <- subset(myDF, Species=="POP")
    
    ### LA
    mod1 <- lmer(LA ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="LA"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### total biomass
    mod1 <- lmer(TOT ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="total"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### leaf biomass
    mod1 <- lmer(Leaf ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="leaf"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### Stem biomass
    mod1 <- lmer(Stem ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="stem"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### root biomass
    mod1 <- lmer(Root ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="root"] <- round(anov$`Pr(>F)`[3], 4)
    
    ### RS ratio
    mod1 <- lmer(RS_ratio ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### RL_ratio 
    mod1 <- lmer(RL_ratio ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### save output
    write.csv(outDF, paste0(outdir, "statistics_leaf_area_biomass_by_species.csv"), row.names=F)
    
    
    ############################# Group species ###############################
    
    mod1 <- lmer(LA ~ CO2 * H2O * Species + (1|GH), data=myDF)
    anov <- anova(mod1)
    
    # nominator
    outDF2$Nominator_CO2[outDF2$Variable=="LA"] <- anov$NumDF[1]
    outDF2$Nominator_H2O[outDF2$Variable=="LA"] <- anov$NumDF[2]
    outDF2$Nominator_S[outDF2$Variable=="LA"] <- anov$NumDF[3]
    outDF2$Nominator_CO2_H2O[outDF2$Variable=="LA"] <- anov$NumDF[4]
    outDF2$Nominator_CO2_S[outDF2$Variable=="LA"] <- anov$NumDF[5]
    outDF2$Nominator_H2O_S[outDF2$Variable=="LA"] <- anov$NumDF[6]
    outDF2$Nominator_CO2_H2O_S[outDF2$Variable=="LA"] <- anov$NumDF[7]
    
    #denominator
    outDF2$Denominator_CO2[outDF2$Variable=="LA"] <- anov$DenDF[1]
    outDF2$Denominator_H2O[outDF2$Variable=="LA"] <- anov$DenDF[2]
    outDF2$Denominator_S[outDF2$Variable=="LA"] <- anov$DenDF[3]
    outDF2$Denominator_CO2_H2O[outDF2$Variable=="LA"] <- anov$DenDF[4]
    outDF2$Denominator_CO2_S[outDF2$Variable=="LA"] <- anov$DenDF[5]
    outDF2$Denominator_H2O_S[outDF2$Variable=="LA"] <- anov$DenDF[6]
    outDF2$Denominator_CO2_H2O_S[outDF2$Variable=="LA"] <- anov$DenDF[7]
    
    # F-value
    outDF2$F_CO2[outDF2$Variable=="LA"] <- anov$`F value`[1]
    outDF2$F_H2O[outDF2$Variable=="LA"] <- anov$`F value`[2]
    outDF2$F_S[outDF2$Variable=="LA"] <- anov$`F value`[3]
    outDF2$F_CO2_H2O[outDF2$Variable=="LA"] <- anov$`F value`[4]
    outDF2$F_CO2_S[outDF2$Variable=="LA"] <- anov$`F value`[5]
    outDF2$F_H2O_S[outDF2$Variable=="LA"] <- anov$`F value`[6]
    outDF2$F_CO2_H2O_S[outDF2$Variable=="LA"] <- anov$`F value`[7]
    
    # p-value
    outDF2$p_CO2[outDF2$Variable=="LA"] <- round(anov$`Pr(>F)`[1], 4)
    outDF2$p_H2O[outDF2$Variable=="LA"] <- round(anov$`Pr(>F)`[2], 4)
    outDF2$p_S[outDF2$Variable=="LA"] <- anov$`Pr(>F)`[3]
    outDF2$p_CO2_H2O[outDF2$Variable=="LA"] <- anov$`Pr(>F)`[4]
    outDF2$p_CO2_S[outDF2$Variable=="LA"] <- anov$`Pr(>F)`[5]
    outDF2$p_H2O_S[outDF2$Variable=="LA"] <- anov$`Pr(>F)`[6]
    outDF2$p_CO2_H2O_S[outDF2$Variable=="LA"] <- anov$`Pr(>F)`[7]
    
    ### total biomass
    mod1 <- lmer(TOT ~ CO2 * H2O * Species + (1|GH), data=myDF)
    anov <- anova(mod1)
    
    # nominator
    outDF2$Nominator_CO2[outDF2$Variable=="total"] <- anov$NumDF[1]
    outDF2$Nominator_H2O[outDF2$Variable=="total"] <- anov$NumDF[2]
    outDF2$Nominator_S[outDF2$Variable=="total"] <- anov$NumDF[3]
    outDF2$Nominator_CO2_H2O[outDF2$Variable=="total"] <- anov$NumDF[4]
    outDF2$Nominator_CO2_S[outDF2$Variable=="total"] <- anov$NumDF[5]
    outDF2$Nominator_H2O_S[outDF2$Variable=="total"] <- anov$NumDF[6]
    outDF2$Nominator_CO2_H2O_S[outDF2$Variable=="total"] <- anov$NumDF[7]
    
    #denominator
    outDF2$Denominator_CO2[outDF2$Variable=="total"] <- anov$DenDF[1]
    outDF2$Denominator_H2O[outDF2$Variable=="total"] <- anov$DenDF[2]
    outDF2$Denominator_S[outDF2$Variable=="total"] <- anov$DenDF[3]
    outDF2$Denominator_CO2_H2O[outDF2$Variable=="total"] <- anov$DenDF[4]
    outDF2$Denominator_CO2_S[outDF2$Variable=="total"] <- anov$DenDF[5]
    outDF2$Denominator_H2O_S[outDF2$Variable=="total"] <- anov$DenDF[6]
    outDF2$Denominator_CO2_H2O_S[outDF2$Variable=="total"] <- anov$DenDF[7]
    
    # F-value
    outDF2$F_CO2[outDF2$Variable=="total"] <- anov$`F value`[1]
    outDF2$F_H2O[outDF2$Variable=="total"] <- anov$`F value`[2]
    outDF2$F_S[outDF2$Variable=="total"] <- anov$`F value`[3]
    outDF2$F_CO2_H2O[outDF2$Variable=="total"] <- anov$`F value`[4]
    outDF2$F_CO2_S[outDF2$Variable=="total"] <- anov$`F value`[5]
    outDF2$F_H2O_S[outDF2$Variable=="total"] <- anov$`F value`[6]
    outDF2$F_CO2_H2O_S[outDF2$Variable=="total"] <- anov$`F value`[7]
    
    # p-value
    outDF2$p_CO2[outDF2$Variable=="total"] <- round(anov$`Pr(>F)`[1], 4)
    outDF2$p_H2O[outDF2$Variable=="total"] <- round(anov$`Pr(>F)`[2], 4)
    outDF2$p_S[outDF2$Variable=="total"] <- anov$`Pr(>F)`[3]
    outDF2$p_CO2_H2O[outDF2$Variable=="total"] <- anov$`Pr(>F)`[4]
    outDF2$p_CO2_S[outDF2$Variable=="total"] <- anov$`Pr(>F)`[5]
    outDF2$p_H2O_S[outDF2$Variable=="total"] <- anov$`Pr(>F)`[6]
    outDF2$p_CO2_H2O_S[outDF2$Variable=="total"] <- anov$`Pr(>F)`[7]
    
    
    ### leaf biomass
    mod1 <- lmer(Leaf ~ CO2 * H2O * Species + (1|GH), data=myDF)
    anov <- anova(mod1)
    
    # nominator
    outDF2$Nominator_CO2[outDF2$Variable=="leaf"] <- anov$NumDF[1]
    outDF2$Nominator_H2O[outDF2$Variable=="leaf"] <- anov$NumDF[2]
    outDF2$Nominator_S[outDF2$Variable=="leaf"] <- anov$NumDF[3]
    outDF2$Nominator_CO2_H2O[outDF2$Variable=="leaf"] <- anov$NumDF[4]
    outDF2$Nominator_CO2_S[outDF2$Variable=="leaf"] <- anov$NumDF[5]
    outDF2$Nominator_H2O_S[outDF2$Variable=="leaf"] <- anov$NumDF[6]
    outDF2$Nominator_CO2_H2O_S[outDF2$Variable=="leaf"] <- anov$NumDF[7]
    
    #denominator
    outDF2$Denominator_CO2[outDF2$Variable=="leaf"] <- anov$DenDF[1]
    outDF2$Denominator_H2O[outDF2$Variable=="leaf"] <- anov$DenDF[2]
    outDF2$Denominator_S[outDF2$Variable=="leaf"] <- anov$DenDF[3]
    outDF2$Denominator_CO2_H2O[outDF2$Variable=="leaf"] <- anov$DenDF[4]
    outDF2$Denominator_CO2_S[outDF2$Variable=="leaf"] <- anov$DenDF[5]
    outDF2$Denominator_H2O_S[outDF2$Variable=="leaf"] <- anov$DenDF[6]
    outDF2$Denominator_CO2_H2O_S[outDF2$Variable=="leaf"] <- anov$DenDF[7]
    
    # F-value
    outDF2$F_CO2[outDF2$Variable=="leaf"] <- anov$`F value`[1]
    outDF2$F_H2O[outDF2$Variable=="leaf"] <- anov$`F value`[2]
    outDF2$F_S[outDF2$Variable=="leaf"] <- anov$`F value`[3]
    outDF2$F_CO2_H2O[outDF2$Variable=="leaf"] <- anov$`F value`[4]
    outDF2$F_CO2_S[outDF2$Variable=="leaf"] <- anov$`F value`[5]
    outDF2$F_H2O_S[outDF2$Variable=="leaf"] <- anov$`F value`[6]
    outDF2$F_CO2_H2O_S[outDF2$Variable=="leaf"] <- anov$`F value`[7]
    
    # p-value
    outDF2$p_CO2[outDF2$Variable=="leaf"] <- round(anov$`Pr(>F)`[1], 4)
    outDF2$p_H2O[outDF2$Variable=="leaf"] <- round(anov$`Pr(>F)`[2], 4)
    outDF2$p_S[outDF2$Variable=="leaf"] <- anov$`Pr(>F)`[3]
    outDF2$p_CO2_H2O[outDF2$Variable=="leaf"] <- anov$`Pr(>F)`[4]
    outDF2$p_CO2_S[outDF2$Variable=="leaf"] <- anov$`Pr(>F)`[5]
    outDF2$p_H2O_S[outDF2$Variable=="leaf"] <- anov$`Pr(>F)`[6]
    outDF2$p_CO2_H2O_S[outDF2$Variable=="leaf"] <- anov$`Pr(>F)`[7]
    
    
    ### Stem biomass
    mod1 <- lmer(Stem ~ CO2 * H2O * Species + (1|GH), data=myDF)
    anov <- anova(mod1)
    
    # nominator
    outDF2$Nominator_CO2[outDF2$Variable=="stem"] <- anov$NumDF[1]
    outDF2$Nominator_H2O[outDF2$Variable=="stem"] <- anov$NumDF[2]
    outDF2$Nominator_S[outDF2$Variable=="stem"] <- anov$NumDF[3]
    outDF2$Nominator_CO2_H2O[outDF2$Variable=="stem"] <- anov$NumDF[4]
    outDF2$Nominator_CO2_S[outDF2$Variable=="stem"] <- anov$NumDF[5]
    outDF2$Nominator_H2O_S[outDF2$Variable=="stem"] <- anov$NumDF[6]
    outDF2$Nominator_CO2_H2O_S[outDF2$Variable=="stem"] <- anov$NumDF[7]
    
    #denominator
    outDF2$Denominator_CO2[outDF2$Variable=="stem"] <- anov$DenDF[1]
    outDF2$Denominator_H2O[outDF2$Variable=="stem"] <- anov$DenDF[2]
    outDF2$Denominator_S[outDF2$Variable=="stem"] <- anov$DenDF[3]
    outDF2$Denominator_CO2_H2O[outDF2$Variable=="stem"] <- anov$DenDF[4]
    outDF2$Denominator_CO2_S[outDF2$Variable=="stem"] <- anov$DenDF[5]
    outDF2$Denominator_H2O_S[outDF2$Variable=="stem"] <- anov$DenDF[6]
    outDF2$Denominator_CO2_H2O_S[outDF2$Variable=="stem"] <- anov$DenDF[7]
    
    # F-value
    outDF2$F_CO2[outDF2$Variable=="stem"] <- anov$`F value`[1]
    outDF2$F_H2O[outDF2$Variable=="stem"] <- anov$`F value`[2]
    outDF2$F_S[outDF2$Variable=="stem"] <- anov$`F value`[3]
    outDF2$F_CO2_H2O[outDF2$Variable=="stem"] <- anov$`F value`[4]
    outDF2$F_CO2_S[outDF2$Variable=="stem"] <- anov$`F value`[5]
    outDF2$F_H2O_S[outDF2$Variable=="stem"] <- anov$`F value`[6]
    outDF2$F_CO2_H2O_S[outDF2$Variable=="stem"] <- anov$`F value`[7]
    
    # p-value
    outDF2$p_CO2[outDF2$Variable=="stem"] <- round(anov$`Pr(>F)`[1], 4)
    outDF2$p_H2O[outDF2$Variable=="stem"] <- round(anov$`Pr(>F)`[2], 4)
    outDF2$p_S[outDF2$Variable=="stem"] <- anov$`Pr(>F)`[3]
    outDF2$p_CO2_H2O[outDF2$Variable=="stem"] <- anov$`Pr(>F)`[4]
    outDF2$p_CO2_S[outDF2$Variable=="stem"] <- anov$`Pr(>F)`[5]
    outDF2$p_H2O_S[outDF2$Variable=="stem"] <- anov$`Pr(>F)`[6]
    outDF2$p_CO2_H2O_S[outDF2$Variable=="stem"] <- anov$`Pr(>F)`[7]
    
    
    ### root biomass
    mod1 <- lmer(Root ~ CO2 * H2O * Species + (1|GH), data=myDF)
    anov <- anova(mod1)
    
    # nominator
    outDF2$Nominator_CO2[outDF2$Variable=="root"] <- anov$NumDF[1]
    outDF2$Nominator_H2O[outDF2$Variable=="root"] <- anov$NumDF[2]
    outDF2$Nominator_S[outDF2$Variable=="root"] <- anov$NumDF[3]
    outDF2$Nominator_CO2_H2O[outDF2$Variable=="root"] <- anov$NumDF[4]
    outDF2$Nominator_CO2_S[outDF2$Variable=="root"] <- anov$NumDF[5]
    outDF2$Nominator_H2O_S[outDF2$Variable=="root"] <- anov$NumDF[6]
    outDF2$Nominator_CO2_H2O_S[outDF2$Variable=="root"] <- anov$NumDF[7]
    
    #denominator
    outDF2$Denominator_CO2[outDF2$Variable=="root"] <- anov$DenDF[1]
    outDF2$Denominator_H2O[outDF2$Variable=="root"] <- anov$DenDF[2]
    outDF2$Denominator_S[outDF2$Variable=="root"] <- anov$DenDF[3]
    outDF2$Denominator_CO2_H2O[outDF2$Variable=="root"] <- anov$DenDF[4]
    outDF2$Denominator_CO2_S[outDF2$Variable=="root"] <- anov$DenDF[5]
    outDF2$Denominator_H2O_S[outDF2$Variable=="root"] <- anov$DenDF[6]
    outDF2$Denominator_CO2_H2O_S[outDF2$Variable=="root"] <- anov$DenDF[7]
    
    # F-value
    outDF2$F_CO2[outDF2$Variable=="root"] <- anov$`F value`[1]
    outDF2$F_H2O[outDF2$Variable=="root"] <- anov$`F value`[2]
    outDF2$F_S[outDF2$Variable=="root"] <- anov$`F value`[3]
    outDF2$F_CO2_H2O[outDF2$Variable=="root"] <- anov$`F value`[4]
    outDF2$F_CO2_S[outDF2$Variable=="root"] <- anov$`F value`[5]
    outDF2$F_H2O_S[outDF2$Variable=="root"] <- anov$`F value`[6]
    outDF2$F_CO2_H2O_S[outDF2$Variable=="root"] <- anov$`F value`[7]
    
    # p-value
    outDF2$p_CO2[outDF2$Variable=="root"] <- round(anov$`Pr(>F)`[1], 4)
    outDF2$p_H2O[outDF2$Variable=="root"] <- round(anov$`Pr(>F)`[2], 4)
    outDF2$p_S[outDF2$Variable=="root"] <- anov$`Pr(>F)`[3]
    outDF2$p_CO2_H2O[outDF2$Variable=="root"] <- anov$`Pr(>F)`[4]
    outDF2$p_CO2_S[outDF2$Variable=="root"] <- anov$`Pr(>F)`[5]
    outDF2$p_H2O_S[outDF2$Variable=="root"] <- anov$`Pr(>F)`[6]
    outDF2$p_CO2_H2O_S[outDF2$Variable=="root"] <- anov$`Pr(>F)`[7]
    
    ### RS ratio
    mod1 <- lmer(RS_ratio ~ CO2 * H2O * Species + (1|GH), data=myDF)
    anov <- anova(mod1)
    
    # nominator
    outDF2$Nominator_CO2[outDF2$Variable=="RS_ratio"] <- anov$NumDF[1]
    outDF2$Nominator_H2O[outDF2$Variable=="RS_ratio"] <- anov$NumDF[2]
    outDF2$Nominator_S[outDF2$Variable=="RS_ratio"] <- anov$NumDF[3]
    outDF2$Nominator_CO2_H2O[outDF2$Variable=="RS_ratio"] <- anov$NumDF[4]
    outDF2$Nominator_CO2_S[outDF2$Variable=="RS_ratio"] <- anov$NumDF[5]
    outDF2$Nominator_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$NumDF[6]
    outDF2$Nominator_CO2_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$NumDF[7]
    
    #denominator
    outDF2$Denominator_CO2[outDF2$Variable=="RS_ratio"] <- anov$DenDF[1]
    outDF2$Denominator_H2O[outDF2$Variable=="RS_ratio"] <- anov$DenDF[2]
    outDF2$Denominator_S[outDF2$Variable=="RS_ratio"] <- anov$DenDF[3]
    outDF2$Denominator_CO2_H2O[outDF2$Variable=="RS_ratio"] <- anov$DenDF[4]
    outDF2$Denominator_CO2_S[outDF2$Variable=="RS_ratio"] <- anov$DenDF[5]
    outDF2$Denominator_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$DenDF[6]
    outDF2$Denominator_CO2_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$DenDF[7]
    
    # F-value
    outDF2$F_CO2[outDF2$Variable=="RS_ratio"] <- anov$`F value`[1]
    outDF2$F_H2O[outDF2$Variable=="RS_ratio"] <- anov$`F value`[2]
    outDF2$F_S[outDF2$Variable=="RS_ratio"] <- anov$`F value`[3]
    outDF2$F_CO2_H2O[outDF2$Variable=="RS_ratio"] <- anov$`F value`[4]
    outDF2$F_CO2_S[outDF2$Variable=="RS_ratio"] <- anov$`F value`[5]
    outDF2$F_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$`F value`[6]
    outDF2$F_CO2_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$`F value`[7]
    
    # p-value
    outDF2$p_CO2[outDF2$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[1], 4)
    outDF2$p_H2O[outDF2$Variable=="RS_ratio"] <- round(anov$`Pr(>F)`[2], 4)
    outDF2$p_S[outDF2$Variable=="RS_ratio"] <- anov$`Pr(>F)`[3]
    outDF2$p_CO2_H2O[outDF2$Variable=="RS_ratio"] <- anov$`Pr(>F)`[4]
    outDF2$p_CO2_S[outDF2$Variable=="RS_ratio"] <- anov$`Pr(>F)`[5]
    outDF2$p_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$`Pr(>F)`[6]
    outDF2$p_CO2_H2O_S[outDF2$Variable=="RS_ratio"] <- anov$`Pr(>F)`[7]
    
    
    
    ### RL_ratio 
    mod1 <- lmer(RL_ratio ~ CO2 * H2O * Species + (1|GH), data=myDF)
    anov <- anova(mod1)
    
    # nominator
    outDF2$Nominator_CO2[outDF2$Variable=="RL_ratio"] <- anov$NumDF[1]
    outDF2$Nominator_H2O[outDF2$Variable=="RL_ratio"] <- anov$NumDF[2]
    outDF2$Nominator_S[outDF2$Variable=="RL_ratio"] <- anov$NumDF[3]
    outDF2$Nominator_CO2_H2O[outDF2$Variable=="RL_ratio"] <- anov$NumDF[4]
    outDF2$Nominator_CO2_S[outDF2$Variable=="RL_ratio"] <- anov$NumDF[5]
    outDF2$Nominator_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$NumDF[6]
    outDF2$Nominator_CO2_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$NumDF[7]
    
    #denominator
    outDF2$Denominator_CO2[outDF2$Variable=="RL_ratio"] <- anov$DenDF[1]
    outDF2$Denominator_H2O[outDF2$Variable=="RL_ratio"] <- anov$DenDF[2]
    outDF2$Denominator_S[outDF2$Variable=="RL_ratio"] <- anov$DenDF[3]
    outDF2$Denominator_CO2_H2O[outDF2$Variable=="RL_ratio"] <- anov$DenDF[4]
    outDF2$Denominator_CO2_S[outDF2$Variable=="RL_ratio"] <- anov$DenDF[5]
    outDF2$Denominator_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$DenDF[6]
    outDF2$Denominator_CO2_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$DenDF[7]
    
    # F-value
    outDF2$F_CO2[outDF2$Variable=="RL_ratio"] <- anov$`F value`[1]
    outDF2$F_H2O[outDF2$Variable=="RL_ratio"] <- anov$`F value`[2]
    outDF2$F_S[outDF2$Variable=="RL_ratio"] <- anov$`F value`[3]
    outDF2$F_CO2_H2O[outDF2$Variable=="RL_ratio"] <- anov$`F value`[4]
    outDF2$F_CO2_S[outDF2$Variable=="RL_ratio"] <- anov$`F value`[5]
    outDF2$F_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$`F value`[6]
    outDF2$F_CO2_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$`F value`[7]
    
    # p-value
    outDF2$p_CO2[outDF2$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[1], 4)
    outDF2$p_H2O[outDF2$Variable=="RL_ratio"] <- round(anov$`Pr(>F)`[2], 4)
    outDF2$p_S[outDF2$Variable=="RL_ratio"] <- anov$`Pr(>F)`[3]
    outDF2$p_CO2_H2O[outDF2$Variable=="RL_ratio"] <- anov$`Pr(>F)`[4]
    outDF2$p_CO2_S[outDF2$Variable=="RL_ratio"] <- anov$`Pr(>F)`[5]
    outDF2$p_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$`Pr(>F)`[6]
    outDF2$p_CO2_H2O_S[outDF2$Variable=="RL_ratio"] <- anov$`Pr(>F)`[7]
    
    
    
    ### save output
    write.csv(outDF2, paste0(outdir, "statistics_leaf_area_biomass.csv"), row.names=F)
    
    
    
    
    
    
}