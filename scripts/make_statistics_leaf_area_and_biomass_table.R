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
    
    
    ### prepare outDF
    outDF <- data.frame(rep(c("Epilularis", "Epopulnea"), each = 7),
                        rep(c("LA", "total", "leaf", "stem",
                              "root", "froot", "croot"), 2),
                        NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Species", "Variable", 
                         "Nominator_CO2", "Nominator_H2O", "Nominator_CO2_H2O", 
                         "Denominator_CO2", "Denominator_H2O", "Denominator_CO2_H2O", 
                         "F_CO2", "F_H2O", "F_CO2_H2O", 
                         "p_CO2", "p_H2O", "p_CO2_H2O")
    
    outDF2 <- data.frame(c("LA", "total", "leaf", "stem",
                               "root", "froot", "croot"),
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
    
    ### fineroot biomass
    mod1 <- lmer(FineRoot ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="froot"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### Croot biomass
    mod1 <- lmer(CoarseRoot ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epilularis"&outDF$Variable=="croot"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    
    ############################# E.pilularis ##############################
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
    
    ### fineroot biomass
    mod1 <- lmer(FineRoot ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="froot"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### Croot biomass
    mod1 <- lmer(CoarseRoot ~ CO2 * H2O + (1|GH), data=subDF)
    anov <- anova(mod1)
    
    # nominator
    outDF$Nominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$NumDF[1]
    outDF$Nominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$NumDF[2]
    outDF$Nominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$NumDF[3]
    
    #denominator
    outDF$Denominator_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$DenDF[1]
    outDF$Denominator_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$DenDF[2]
    outDF$Denominator_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$DenDF[3]
    
    # F-value
    outDF$F_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$`F value`[1]
    outDF$F_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$`F value`[2]
    outDF$F_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- anov$`F value`[3]
    
    # p-value
    outDF$p_CO2[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- round(anov$`Pr(>F)`[1], 4)
    outDF$p_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- round(anov$`Pr(>F)`[2], 4)
    outDF$p_CO2_H2O[outDF$Species=="Epopulnea"&outDF$Variable=="croot"] <- round(anov$`Pr(>F)`[3], 4)
    
    
    ### save output
    write.csv(outDF, paste0(outdir, "statistics_leaf_area_biomass_by_species.csv"), row.names=F)
    
    
    
    
    
    
    
    ############################# Finish statistical tests ###############################
    
}