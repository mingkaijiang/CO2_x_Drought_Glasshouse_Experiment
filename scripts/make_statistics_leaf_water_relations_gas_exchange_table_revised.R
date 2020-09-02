make_statistics_leaf_water_relations_gas_exchange_table_revised <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv")
    
    #myDF1 <- subset(myDF1, Day <= 6)
    #myDF2 <- subset(myDF2, Day <= 16)
    
    ### set class
    myDF1$Glasshouse <- as.factor(myDF1$Glasshouse)
    myDF2$Glasshouse <- as.factor(myDF2$Glasshouse)
    
    myDF1$CO2 <- as.factor(myDF1$CO2)
    myDF2$CO2 <- as.factor(myDF2$CO2)
    
    myDF1$H2O <- as.factor(myDF1$H2O)
    myDF2$H2O <- as.factor(myDF2$H2O)
    
    ### daily DF
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
    mod1<-lme(SWC~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
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
    ### Adaily
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
    write.csv(anov,file="output/BM/statistics/Epilularis/Adaily.csv")
    
    
    ######################################################################
    ### gsdaily
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
    write.csv(anov,file="output/BM/statistics/Epilularis/gsdaily.csv")
    
   
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
    mod1<-lme(transp_plant~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
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
    ### Adaily
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
    write.csv(anov,file="output/BM/statistics/Epopulnea/Adaily.csv")
    
    
    ######################################################################
    ### gsdaily
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
    write.csv(anov,file="output/BM/statistics/Epopulnea/gsdaily.csv")
    
    
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