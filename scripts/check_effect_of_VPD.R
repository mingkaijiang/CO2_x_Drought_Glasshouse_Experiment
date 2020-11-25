check_effect_of_VPD <- function() {
    
    
    ### read in met data
    myDF1 <- read.csv("data/glasshouse2/met/AMBIENT_GH_TEMP_RH_PILULARIS_DRYDOWN_DAY.csv")
    myDF2 <- read.csv("data/glasshouse2/met/ELEVATED_GH_TEMP_RH_PILULARIS_DRYDOWN_DAY.csv")

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
    
    
   
    
}