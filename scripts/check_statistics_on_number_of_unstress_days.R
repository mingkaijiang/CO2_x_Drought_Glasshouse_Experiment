check_statistics_on_number_of_unstress_days <- function() {
    
    myDF <- read.csv("data/glasshouse2/unstress_days.csv")
    
    pilDF <- myDF[myDF$Species == "PIL",]
    popDF <- myDF[myDF$Species == "POP",]
    
    mod1<-lme(Day~CO2*H2O,random=~1|Glasshouse/Replicate,data=pilDF)
    summary(mod1)
    anov<-anova(mod1)
    anov
    
    
    mod1<-lme(Day~CO2*H2O,random=~1|Glasshouse/Replicate,data=popDF)
    summary(mod1)
    anov<-anova(mod1)
    anov
    
}