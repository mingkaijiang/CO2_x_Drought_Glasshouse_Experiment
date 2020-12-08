check_statistics_on_number_of_unstress_days <- function() {
    
    ### read in un-stressed days
    myDF <- read.csv("data/glasshouse2/unstress_days.csv")
    
    pilDF <- myDF[myDF$Species == "PIL",]
    popDF <- myDF[myDF$Species == "POP",]
    
    
    ### check statistics on no. of un-stressed days
    mod1<-lme(Day~CO2*H2O,random=~1|Glasshouse/Replicate,data=pilDF)
    summary(mod1)
    anov<-anova(mod1)
    anov
    
    mod1<-lme(Day~CO2*H2O,random=~1|Glasshouse/Replicate,data=popDF)
    summary(mod1)
    anov<-anova(mod1)
    anov
    
    ### rename columns
    colnames(pilDF) <- colnames(popDF) <- c("Replicate", "Trt", "CO2", "H2O",
                                            "Glasshouse", "Nday", "Species")
    
    
    hist(pilDF$Nday, group=pilDF$H2O)
    
    ### Make a plot
    p1 <- ggplot(pilDF, aes(x=Nday, fill=H2O))+
        geom_histogram(position="identity")+
        scale_fill_manual(values=c("#69b3a2", "#404080")) +
        labs(fill="")
    
    plot(p1)
    
    
    
    ### merge of the full dataset to see if number of unstressed days explain anything
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv")
    
    myDF1 <- merge(myDF1, pilDF, by=c("Replicate", "Trt", "CO2", "H2O", "Glasshouse"))
    myDF2 <- merge(myDF2, popDF, by=c("Replicate", "Trt", "CO2", "H2O", "Glasshouse"))
    
    
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
    mod1<-lme(log(SWC)~CO2*H2O*Day,random=~1|Glasshouse/Replicate,data=myDF1, na.action = na.omit)
    summary(mod1)
    
    # anova
    anov<-anova(mod1)
    anov
    
}