make_average_and_fixed_date_rate_comparison_before_threshold <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv")
    
    myDF1 <- subset(myDF1, Day == 6)
    myDF2 <- subset(myDF2, Day == 16)
    
    ### set class
    myDF1$Glasshouse <- as.character(myDF1$Glasshouse)
    myDF2$Glasshouse <- as.character(myDF2$Glasshouse)
    
    myDF1$CO2 <- as.character(myDF1$CO2)
    myDF2$CO2 <- as.character(myDF2$CO2)
    
    myDF1$H2O <- as.character(myDF1$H2O)
    myDF2$H2O <- as.character(myDF2$H2O)
    
    myDF1$DayFactor <- as.character(myDF1$Day)
    myDF2$DayFactor <- as.character(myDF2$Day)
    
    
    ### calculations
    myDF1$Adaily <- with(myDF1, (Aearly+Alate)/2)
    myDF2$Adaily <- with(myDF2, (Aearly+Alate)/2)
    
    myDF1$gsdaily <- with(myDF1, (gsearly+gslate)/2)
    myDF2$gsdaily <- with(myDF2, (gsearly+gslate)/2)
    
    
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1, na.action = na.omit)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
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
    
    
    ######################################################################
    ### Adaily 1
    mod1<-lme(log(Adaily)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    ######################################
    ### Aearly
    mod1<-lme(log(Aearly)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### Alate
    mod1<-lme(log(Alate)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### gsdaily 1
    mod1<-lme(log(gsdaily)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF1)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################## populnea
    ######################################################################
    ### SWC
    mod1<-lme(log(SWC)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### transpiration
    mod1<-lme(log(transp_plant)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
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
    
        
    
    
    ######################################################################
    ### Adaily 1 
    mod1<-lme(log(Adaily)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### Aearly
    mod1<-lme(log(Aearly)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### Alate
    mod1<-lme(log(Alate)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### gsdaily
    mod1<-lme(log(gsdaily)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### gsearly
    mod1<-lme(log(gsearly)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    ######################################################################
    ### gslate
    mod1<-lme(log(gslate)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    ######################################################################
    ### Epsi
    mod1<-lme(log(E_psi)~CO2*H2O,random=~1|Glasshouse/Replicate,data=myDF2)
    summary(mod1)
    
    #Testing normality of residuals
    qqnorm(resid(mod1))
    qqline(resid(mod1))
    shapiro.test(resid(mod1))
    
    # anova
    anov<-anova(mod1)
    anov
    
    
    
    
}