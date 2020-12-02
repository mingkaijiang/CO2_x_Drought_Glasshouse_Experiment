check_effect_of_VPD <- function() {
    
    ### read in met data
    myDF1 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/AMBIENT_GH_RH_AND_TEMP_DAY.csv")
    myDF2 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/AMBIENT_GH_RH_AND_TEMP_NIGHT.csv")
    myDF3 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/ELEVATED_GH_RH_AND_TEMP_DAY.csv")
    myDF4 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/ELEVATED_GH_RH_AND_TEMP_NIGHT.csv")
    
    ### merge by day and night separately
    metDF <- rbind(myDF1, myDF2)

    colnames(metDF) <- c("DateTime", "Tair", "RH", "SVP",
                         "RHdeficit", "VPD", "DAY",
                         "GH", "CO2")
    
    ### setting date and time
    metDF$DateTime <- paste0(metDF$DateTime, ":00")
    metDF$DateTime <- as.character(metDF$DateTime)
    metDF$Date <- sub(" .*", "", metDF$DateTime)
    metDF$Date <- as.Date(as.character(metDF$Date), format="%m/%d/%Y")
    metDF$Time <- sub(".+? ", "", metDF$DateTime)
    metDF$Time <- chron(times=metDF$Time)    
    metDF$DateTime <- as.POSIXct(paste0(as.character(metDF$Date), " ", 
                                         as.character(metDF$Time)),
                                  format="%Y-%m-%d %H:%M:%S")
    
    ### check if there is a DAY, GH and CO2 effect on VPD
    mod1<-lme(VPD~CO2+DAY,random=~1|GH,data=metDF)
    summary(mod1)
    
    
    ### make daily summary
    sumDF <- summaryBy(VPD~DAY+GH+CO2, FUN=c(max, mean, se),
                       keep.names=T, data=metDF)
    
    
    ### check if there is a day, gh and CO2 effect on VPD
    mod1<-lme(VPD.max~CO2+DAY,random=~1|GH,data=sumDF)
    summary(mod1)
    
    ################################################################
    ### result indicate VPD are similar in different CO2 and chamber
    ################################################################
    
    colnames(sumDF) <- c("Day", "Glasshouse", "CO2", "VPD.max",
                         "VPD.mean", "VPD.se")
    
    
    ### read in physiologycal measurements
    pilDF <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    popDF <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv")
    
   
    ### merge
    pilDF2 <- merge(pilDF, sumDF, by=c("Day", "Glasshouse", "CO2"))
    popDF2 <- merge(popDF, sumDF, by=c("Day", "Glasshouse", "CO2"))
    
    
    ### check stats on effect of VPD
    
    ### transpiration - pilularis
    mod1<-lme(log(transp_plant)~CO2*Day*VPD.max,random=~1|Glasshouse/Replicate,
              data=pilDF2, na.action = na.omit)
    summary(mod1)
    
    ### transpiration - populnea
    mod1<-lme(log(transp_plant)~CO2*Day*VPD.max,random=~1|Glasshouse/Replicate,
              data=popDF2, na.action = na.omit)
    summary(mod1)
    
    
    ### no effect of VPD
    
    print("no VPD effect found")
}
