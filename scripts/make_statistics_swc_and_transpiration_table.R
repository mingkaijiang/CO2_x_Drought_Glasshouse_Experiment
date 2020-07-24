make_statistics_swc_and_transpiration_table <- function() {
    
    
    ############################# perform statistical tests ##############################
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv")
    
    myDF1$Day <- as.character(myDF1$Day)
    myDF2$Day <- as.character(myDF2$Day)
    
    ### perform linear mixed effect model statistics on SWC PIL
    mod1 <- lmer(SWC ~ CO2 * H2O * Day + (1|Glasshouse), data=myDF1)
    anov <- anova(mod1)
    write.csv(outDF, paste0(outdir, "statistics_leaf_area.csv"))
    
    
    
    
    
    
}