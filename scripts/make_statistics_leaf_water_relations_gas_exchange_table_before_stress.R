make_statistics_leaf_water_relations_gas_exchange_table_before_stress <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv")
    
    myDF1 <- subset(myDF1, Day <= 6)
    myDF2 <- subset(myDF2, Day <= 16)
    
    ### set class
    myDF1$Glasshouse <- as.factor(myDF1$Glasshouse)
    myDF2$Glasshouse <- as.factor(myDF2$Glasshouse)
    
    myDF1$CO2 <- as.factor(myDF1$CO2)
    myDF2$CO2 <- as.factor(myDF2$CO2)
    
    myDF1$H2O <- as.factor(myDF1$H2O)
    myDF2$H2O <- as.factor(myDF2$H2O)
    
    
    myDF1$DayFactor <- as.character(myDF1$Day)
    myDF2$DayFactor <- as.factor(as.character(myDF2$Day))
    
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
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1, na.action = na.omit)
    summary(mod1)
    
    # anova
    anov<-anova(mod1)
    anov
    
   
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    # anova
    anov<-anova(mod1)
    anov
   
    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    

    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
   
    # anova
    anov<-anova(mod1)
    anov
    
   
    
    ######################################################################
    ### Adaily 1
    mod1<-lme(log(Adaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    ######################################################################
    ### Aearly
    mod1<-lme(log(Aearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### Alate
    mod1<-lme(log(Alate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    ######################################################################
    ### gsdaily 1
    mod1<-lme(log(gsdaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
   
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
 
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    

    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    
    ######################## populnea
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    

    
    ######################################################################
    ### psiPD
    mod1<-lme(psiPD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### psiMD
    mod1<-lme(psiMD~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### Adaily 1 
    mod1<-lme(log(Adaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(log(Aearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    
   
    
    ######################################################################
    ### Alate
    mod1<-lme(log(Alate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### gsdaily
    mod1<-lme(log(gsdaily)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    
    
    
    
}


