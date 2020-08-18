make_statistics_leaf_area_and_biomass_table_revised <- function() {
    
    ### processing
    myDF <- read.csv("data/glasshouse2/EUC_Leaf_Area_data.csv")
    
    colnames(myDF) <- c("Replicate", "Species", "Species_Treatment", "GH",
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
    
    ### Root mass to leaf area ratio
    myDF$FRLA_ratio <- with(myDF, (FineRoot/LA_plant))
    
    ## set class
    myDF$Species <- as.factor(myDF$Species)
    myDF$Species_Treatment <- as.factor(myDF$Species_Treatment)
    myDF$GH <- as.factor(myDF$GH)
    myDF$Treatment <- as.factor(myDF$Treatment)
    myDF$CO2 <- as.factor(myDF$CO2)
    myDF$H2O <- as.factor(myDF$H2O)
    
    
    
    
    ############################# E.pilularis ##############################
    subDF <- subset(myDF, Species=="PIL")
    
    ######################################################################
    ### LAI
    mod1<-lme(LA~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/LA-initial.csv")
    
    
    ######################################################################
    ### TOT
    mod1<-lme(TOT~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/TOT-initial.csv")
    
    
    ######################################################################
    ### Leaf
    mod1<-lme(LM~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/LM-initial.csv")
    
    
    ######################################################################
    ### stem
    mod1<-lme(Stem~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Stem-initial.csv")
    
    
    ######################################################################
    ### Root
    mod1<-lme(Root~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Root-initial.csv")
    
    
    ######################################################################
    ### Fineroot
    mod1<-lme(FineRoot~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/FR-initial.csv")
    
    
    ######################################################################
    ### RS ratio
    mod1<-lme(RS_ratio~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/RS_ratio-initial.csv")
    
    
    ######################################################################
    ### RL ratio
    mod1<-lme(RL_ratio~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/RL_ratio-initial.csv")
    
    
    ############################# E.populnea ##############################
    subDF <- subset(myDF, Species=="POP")
    
    ### LAI
    mod1<-lme(LA~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/LA-initial.csv")
    
    
    ######################################################################
    ### TOT
    mod1<-lme(TOT~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/TOT-initial.csv")
    
    
    ######################################################################
    ### Leaf
    mod1<-lme(LM~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/LM-initial.csv")
    
    
    ######################################################################
    ### stem
    mod1<-lme(Stem~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Stem-initial.csv")
    
    
    ######################################################################
    ### Root
    mod1<-lme(Root~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Root-initial.csv")
    
    
    ######################################################################
    ### Fineroot
    mod1<-lme(FineRoot~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/FR-initial.csv")
    
    
    ######################################################################
    ### RS ratio
    mod1<-lme(RS_ratio~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/RS_ratio-initial.csv")
    
    
    ######################################################################
    ### RL ratio
    mod1<-lme(RL_ratio~CO2*H2O,random=~1|GH/Replicate,data=subDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/RL_ratio-initial.csv")
    
    
    
    ############################# Group species ###############################
    ######################################################################
    ### LAI
    mod1<-lme(log(LA)~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/LA-initial.csv")
    
    ######################################################################
    ### TOT
    mod1<-lme(TOT~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/TOT-initial.csv")
    
    
    ######################################################################
    ### Leaf
    mod1<-lme(log(LM)~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/LM-initial.csv")
    
    
    ######################################################################
    ### stem
    mod1<-lme(Stem~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/Stem-initial.csv")
    
    
    ######################################################################
    ### Root
    mod1<-lme(Root~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/Root-initial.csv")
    
    
    ######################################################################
    ### Fineroot
    mod1<-lme(FineRoot~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/FR-initial.csv")
    
    
    ######################################################################
    ### RS ratio
    mod1<-lme(log(RS_ratio)~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/RS_ratio-initial.csv")
    
    
    ######################################################################
    ### RL ratio
    mod1<-lme(RL_ratio~CO2*H2O*Species,random=~1|GH/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/RL_ratio-initial.csv")
    
}