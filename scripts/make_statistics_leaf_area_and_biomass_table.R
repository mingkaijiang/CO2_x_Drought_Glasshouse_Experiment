make_statistics_leaf_area_and_biomass_table <- function() {
    
    ### processing
    myDF <- read.csv("data/glasshouse2/EUC_Leaf_Area_data.csv")
    
    colnames(myDF) <- c("ID", "Species", "Species_Treatment", "GH",
                        "Treatment", "CO2", "H2O", "LA_early", "LA_plant",
                        "LA_final", "LA", "Stem", "SM", "Leaf", "LM", 
                        "Root", "RM", "CoarseRoot", "CRM", "FineRoot", 
                        "FRM")
    
    ### total biomass
    myDF$TOT <- with(myDF, SM+LM+FRM+CRM)
    
    
    ############################# perform statistical tests ##############################
    ### perform linear mixed effect model statistics on LA
    mod1 <- lmer(LA ~ CO2 * H2O * Species + (1|GH), data=myDF)
    outDF.la <- anova(mod1)
    write.csv(outDF.la, paste0(outdir, "statistics_leaf_area.csv"))
    
    mod2 <- lmer(TOT ~ CO2 * H2O * Species + (1|GH), data=myDF)
    outDF.tot <- anova(mod2)
    write.csv(outDF.tot, paste0(outdir, "statistics_total_biomass.csv"))
    
    
    ############################# Finish statistical tests ###############################
    
}