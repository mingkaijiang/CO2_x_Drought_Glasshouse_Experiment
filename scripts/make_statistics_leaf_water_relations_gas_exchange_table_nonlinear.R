make_statistics_leaf_water_relations_gas_exchange_table_nonlinear <- function() {
    
    
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

    myDF1$psiPD_psiMD <- NULL
    myDF1$Species <- "PIL"
    myDF2$Species <- "POP"
    
    myDF <- rbind(myDF1, myDF2)
    
    
    ######################################################################
    
    library(nlme)
    data <- groupedData(SWC ~ Day | Glasshouse, data=myDF1) ## not strictly necessary
    initVals <- getInitial(SWC ~ SSasymp(Day, Asym, CO2, H2O), data = data)
    baseModel<- nlme(SWC ~ SSasymp(Day, Asym, CO2, H2O),
                     data = data,
                     fixed = list(Asym ~ 1, CO2 ~ 1, H2O ~ 1),
                     random = Asym + CO2 + H2O ~ 1|Glasshouse,
                     start = initVals
    )
    
    
    summary(baseModel)
    
    
    
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
    
    startvec <- c(Asym = 200, xmid = 725, scal = 350)
    nform <- ~Asym/(1+exp((xmid-input)/scal))
    ## b. Use deriv() to construct function:
    nfun <- deriv(nform,namevec=c("Asym","xmid","scal"),
                  function.arg=c("input","Asym","xmid","scal"))
    
    (nm1 <- nlmer(circumference ~ nfun(age, Asym, xmid, scal) ~ Asym|Tree,
                  Orange, start = startvec))
    
    
    with(Orange, plot(circumference~age, col=Tree))
    
    
    ### SWC
    test <- groupedData(transp_plant ~ Day | Glasshouse/Replicate, data=myDF1) ## not strictly necessary
    initVals <- getInitial(transp_plant ~ SSlogis(Day, CO2, H2O), data = test)
    baseModel<- nlme(transp_plant ~ SSlogis(Day, CO2, H2O),
                     data = test,
                     fixed = list(CO2 ~ 1, H2O ~ 1),
                     random = Day + CO2 + H2O ~ 1|Glasshouse/Replicate,
                     start = initVals
    )
    
    
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
    mod1<-lme(log(Adaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
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
    mod1<-lme(log(Aearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
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
    mod1<-lme(log(Alate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
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
    mod1<-lme(log(SWC)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
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
    mod1<-lme(psiPD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
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
    mod1<-lme(psiMD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
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
    mod1<-lme(log(Adaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
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
    mod1<-lme(log(Adaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=dDF2)
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
    mod1<-lme(log(Aearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
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
    mod1<-lme(log(Alate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
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
    

    
    
    
    
    
    
    #### Day as factor
    
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1, na.action = na.omit)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/swc_dayfactor.csv")
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/transpiration_dayfactor.csv")
    
    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/psiPD_dayfactor.csv")
    
    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/psiMD_dayfactor.csv")
    
    
    
    ######################################################################
    ### Adaily 1
    mod1<-lme(log(Adaily)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Adaily_dayfactor.csv")
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(log(Aearly)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Aearly_dayfactor.csv")
    
    ######################################################################
    ### Alate
    mod1<-lme(log(Alate)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/Alate_dayfactor.csv")
    
    
    ######################################################################
    ### gsdaily 1
    mod1<-lme(log(gsdaily)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gsdaily_dayfactor.csv")
    
    
    
    
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gsearly_dayfactor.csv")
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/gslate_dayfactor.csv")
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epilularis/E_psi_dayfactor.csv")
    
    
    ######################## populnea
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/swc_dayfactor.csv")
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/transpiration_dayfactor.csv")
    
    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/psiPD_dayfactor.csv")
    
    
    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/psiMD_dayfactor.csv")
    
    
    
    ######################################################################
    ### Adaily 1 
    mod1<-lme(log(Adaily)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Adaily_dayfactor.csv")
    
    
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(log(Aearly)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Aearly_dayfactor.csv")
    
    
    ######################################################################
    ### Alate
    mod1<-lme(log(Alate)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/Alate_dayfactor.csv")
    
    
    ######################################################################
    ### gsdaily
    mod1<-lme(log(gsdaily)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gsdaily_dayfactor.csv")
    
    
    
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gsearly_dayfactor.csv")
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/gslate_dayfactor.csv")
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O*DayFactor,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    # save
    write.csv(anov,file="output/BM/statistics/Epopulnea/E_psi_dayfactor.csv")
    
    
}


