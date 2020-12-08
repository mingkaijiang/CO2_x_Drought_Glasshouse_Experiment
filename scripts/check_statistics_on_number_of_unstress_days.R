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
    
    
    
}