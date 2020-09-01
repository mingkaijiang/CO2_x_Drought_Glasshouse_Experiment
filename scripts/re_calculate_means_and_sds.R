re_calculate_means_and_sds <- function() {
    
    #################### E. pilularis
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    
    myDF1$Adaily <- with(myDF1, (Aearly+Alate)/2)
    
    
    
    
    
    
    
    #################### E. populnea
}