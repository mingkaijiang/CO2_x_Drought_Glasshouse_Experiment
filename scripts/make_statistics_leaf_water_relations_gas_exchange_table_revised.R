make_statistics_leaf_water_relations_gas_exchange_table_revised <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv")
    
    
    ### set class
    myDF1$Glasshouse <- as.factor(myDF1$Glasshouse)
    myDF2$Glasshouse <- as.factor(myDF2$Glasshouse)
    
    myDF1$CO2 <- as.factor(myDF1$CO2)
    myDF2$CO2 <- as.factor(myDF2$CO2)
    
    myDF1$H2O <- as.factor(myDF1$H2O)
    myDF2$H2O <- as.factor(myDF2$H2O)
    
    ### there are two ways of obtaining Adaily and gsdaily
    ### 1. average Aearly and Alate for each day, hence n = 6
    ### 2. group all Aearly and Alate for each day, hence = 12
    ### For 2, we would have different demoninators
    
    ### Method 1 average
    myDF1$Adaily <- with(myDF1, ((Aearly+Alate)/2))
    myDF1$gsdaily <- with(myDF1, ((gsearly+gslate)/2))
    
    myDF2$Adaily <- with(myDF2, ((Aearly+Alate)/2))
    myDF2$gsdaily <- with(myDF2, ((gsearly+gslate)/2))
    
    ### Method 2 daily DF
    ## E. pilularis
    tmpDF1 <- myDF1[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    tmpDF2 <- myDF1[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Alate", "gslate", "transp_leaf_late")]
    
    colnames(tmpDF1) <- colnames(tmpDF2) <- c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    
    dDF1 <- rbind(tmpDF1, tmpDF2)
    
    # E. populnea
    tmpDF1 <- myDF2[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    tmpDF2 <- myDF2[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Alate", "gslate", "transp_leaf_late")]
    
    colnames(tmpDF1) <- colnames(tmpDF2) <- c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    
    dDF2 <- rbind(tmpDF1, tmpDF2)

    
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1, na.action = na.omit)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/swc.csv")
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/transpiration.csv")
    
    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/psiPD.csv")
    
    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/psiMD.csv")
    
    
    
    ######################################################################
    ### Adaily 1
    mod1<-lme(Adaily~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Adaily.csv")
    
    
    ### Adaily 2
    mod1<-lme(Adaily~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=dDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    #write.csv(anov,file="output/BM/statistics/Epilularis/Adaily.csv")
    
    ######################################################################
    ### Aearly
    mod1<-lme(Aearly~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Aearly.csv")
    
    ######################################################################
    ### Alate
    mod1<-lme(Alate~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Alate.csv")
    
    
    ######################################################################
    ### gsdaily 1
    mod1<-lme(log(gsdaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gsdaily.csv")
    
    
    ### gsdaily 2
    mod1<-lme(log(gsdaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=dDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    #write.csv(anov,file="output/BM/statistics/Epilularis/gsdaily.csv")
   
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gsearly.csv")
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gslate.csv")
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/E_psi.csv")
    
    
    ######################## populnea
    ######################################################################
    ### SWC
    mod1<-lme(SWC~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/swc.csv")
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/transpiration.csv")
    
    
    ######################################################################
    ### psiPD
    mod1<-lme(log(abs(psiPD))~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/psiPD.csv")
    
    
    ######################################################################
    ### psiMD
    mod1<-lme(log(abs(psiMD))~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/psiMD.csv")
    
    
    
    ######################################################################
    ### Adaily 1 
    mod1<-lme(Adaily~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Adaily.csv")
    
    ### Adaily 2 
    mod1<-lme(Adaily~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=dDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    #write.csv(anov,file="output/BM/statistics/Epopulnea/Adaily.csv")
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(Aearly~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Aearly.csv")
    
    
    ######################################################################
    ### Alate
    mod1<-lme(Alate~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Alate.csv")
    
    
    ######################################################################
    ### gsdaily
    mod1<-lme(log(gsdaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gsdaily.csv")
    
    ### gsdaily 2
    mod1<-lme(log(gsdaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=dDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    #write.csv(anov,file="output/BM/statistics/Epopulnea/gsdaily.csv")
    
    
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gsearly.csv")
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gslate.csv")
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/E_psi.csv")
    

    
}