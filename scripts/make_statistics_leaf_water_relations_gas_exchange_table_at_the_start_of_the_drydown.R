make_statistics_leaf_water_relations_gas_exchange_table_at_the_start_of_the_drydown <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv")
    
    myDF1 <- subset(myDF1, Day == 1)
    myDF2 <- subset(myDF2, Day == 1)
    
    ### set class
    myDF1$Glasshouse <- as.factor(myDF1$Glasshouse)
    myDF2$Glasshouse <- as.factor(myDF2$Glasshouse)
    
    myDF1$CO2 <- as.factor(myDF1$CO2)
    myDF2$CO2 <- as.factor(myDF2$CO2)
    
    myDF1$H2O <- as.factor(myDF1$H2O)
    myDF2$H2O <- as.factor(myDF2$H2O)
    
    myDF1$Adaily <- with(myDF1, ((Aearly+Alate)/2))
    myDF1$gsdaily <- with(myDF1, ((gsearly+gslate)/2))
    
    myDF2$Adaily <- with(myDF2, ((Aearly+Alate)/2))
    myDF2$gsdaily <- with(myDF2, ((gsearly+gslate)/2))
    
    myDF1$Species <- "PIL"
    myDF2$Species <- "POP"
    
    myDF1$psiPD_psiMD <- NULL
    
    ## merge the two species
    myDF <- rbind(myDF1, myDF2)
    
    myDF$Species <- as.factor(myDF$Species)
    
    ### daily DF
    ## E. pilularis
    #tmpDF1 <- myDF1[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    #tmpDF2 <- myDF1[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Alate", "gslate", "transp_leaf_late")]
    #
    #colnames(tmpDF1) <- colnames(tmpDF2) <- c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    #
    #dDF1 <- rbind(tmpDF1, tmpDF2)
    #
    ## E. populnea
    #tmpDF1 <- myDF2[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    #tmpDF2 <- myDF2[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Alate", "gslate", "transp_leaf_late")]
    #
    #colnames(tmpDF1) <- colnames(tmpDF2) <- c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    #
    #dDF2 <- rbind(tmpDF1, tmpDF2)
    
    
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/swc-initial.csv")
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/transpiration-initial.csv")
    
    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/psiPD-initial.csv")
    
    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/psiMD-initial.csv")
    
    
    
    ######################################################################
    ### Adaily
    mod1<-lme(log(Adaily)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/Adaily-initial.csv")
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(log(Aearly)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/Aearly-initial.csv")
    
    
    ######################################################################
    ### Alate
    mod1<-lme(log(Alate)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/Alate-initial.csv")
    
    
    ######################################################################
    ### gsdaily
    mod1<-lme(log(gsdaily)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/gsdaily-initial.csv")
    
    ######################################################################
    ### gs early
    mod1<-lme(log(gsearly)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/gsearly-initial.csv")
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/gslate-initial.csv")
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O*Species,random=~1|Glasshouse/Replicate,data=myDF)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Species/E_psi-initial.csv")
    
    
    
    
    
    
    
    
    
    
    
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/swc-initial.csv")
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/transpiration-initial.csv")
    
    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/psiPD-initial.csv")
    
    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/psiMD-initial.csv")
    
    
    
    ######################################################################
    ### Adaily
    mod1<-lme(Adaily~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Adaily-initial.csv")
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(Aearly~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Aearly-initial.csv")
    
    
    ######################################################################
    ### Alate
    mod1<-lme(Alate~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Alate-initial.csv")
    
    
    ######################################################################
    ### gsdaily
    mod1<-lme(gsdaily~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gsdaily-initial.csv")
    
    ######################################################################
    ### gs early
    mod1<-lme(gsearly~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gsearly-initial.csv")
    
    ######################################################################
    ### gslate
    mod1<-lme(gslate~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gslate-initial.csv")
    
    ######################################################################
    ### Epsi
    mod1<-lme(E_psi~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/E_psi-initial.csv")
    
    
    ######################## populnea
    ######################################################################
    ### SWC
    mod1<-lme(SWC~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/swc-initial.csv")
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(transp_plant~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/transpiration-initial.csv")
    
    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/psiPD-initial.csv")
    
    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/psiMD-initial.csv")
    
    
    
    ######################################################################
    ### Adaily
    mod1<-lme(Adaily~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Adaily-initial.csv")
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(Aearly~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Aearly-initial.csv")
    
    
    ######################################################################
    ### Alate
    mod1<-lme(Alate~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Alate-initial.csv")
    
    
    ######################################################################
    ### gsdaily
    mod1<-lme(gsdaily~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gsdaily-initial.csv")
    
    ######################################################################
    ### gsearly
    mod1<-lme(gsearly~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gsearly-initial.csv")
    
    ######################################################################
    ### gslate
    mod1<-lme(gslate~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gslate-initial.csv")
    
    ######################################################################
    ### Epsi
    mod1<-lme(E_psi~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/E_psi-initial.csv")
    
    
}

