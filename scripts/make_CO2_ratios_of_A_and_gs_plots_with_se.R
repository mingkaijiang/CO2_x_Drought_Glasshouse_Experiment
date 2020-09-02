make_CO2_ratios_of_A_and_gs_plots_with_se <- function() {
    
    ### read input
    pilDF<-read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv",sep=",", header=TRUE)
    
    
    ### Method 1 average
    pilDF$Adaily <- with(pilDF, ((Aearly+Alate)/2))
    pilDF$gsdaily <- with(pilDF, ((gsearly+gslate)/2))
    
    popDF$Adaily <- with(popDF, ((Aearly+Alate)/2))
    popDF$gsdaily <- with(popDF, ((gsearly+gslate)/2))
    
    pilDF$WUE_daily <- pilDF$Adaily / pilDF$gsdaily
    popDF$WUE_daily <- popDF$Adaily / popDF$gsdaily
    

    
    ### daily DF
    ### E. pilularis
    #tmpDF1 <- myDF1[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    #tmpDF2 <- myDF1[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Alate", "gslate", "transp_leaf_late")]
    #
    #colnames(tmpDF1) <- colnames(tmpDF2) <- c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    #
    #dDF1 <- rbind(tmpDF1, tmpDF2)
    #
    ## E. populnea
    #tmpDF1 <- myDF2[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Aearly", "gsearly", "transp_leaf_early")]
    #tmpDF2 <- myDF2[,c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Alate", "gslate", "transp_leaf_late")]
    #
    #colnames(tmpDF1) <- colnames(tmpDF2) <- c("Replicate", "Trt", "CO2", "H2O", "Glasshouse", "Day", "Adaily", "gsdaily", "transp_leaf_daily")
    #
    #dDF2 <- rbind(tmpDF1, tmpDF2)
    
    ### calculate iWUE
    #dDF1$WUE_daily <- dDF1$Adaily / dDF1$gsdaily
    pilDF$WUE_early <- pilDF$Aearly / pilDF$gsearly
    pilDF$WUE_late <- pilDF$Alate / pilDF$gslate
    
    #dDF2$WUE_daily <- dDF2$Adaily / dDF2$gsdaily
    popDF$WUE_early <- popDF$Aearly / popDF$gsearly
    popDF$WUE_late <- popDF$Alate / popDF$gslate
    
    
    ### calculate log values
    pilDF$log_Adaily <- log(pilDF$Adaily)
    pilDF$log_Aearly <- log(pilDF$Aearly)
    pilDF$log_Alate <- log(pilDF$Alate)
    
    pilDF$log_gsdaily <- log(pilDF$gsdaily)
    pilDF$log_gsearly <- log(pilDF$gsearly)
    pilDF$log_gslate <- log(pilDF$gslate)
    
    pilDF$log_WUE_daily <- log(pilDF$WUE_daily)
    pilDF$log_WUE_early <- log(pilDF$WUE_early)
    pilDF$log_WUE_late <- log(pilDF$WUE_late)
    
    
    popDF$log_Adaily <- log(popDF$Adaily)
    popDF$log_Aearly <- log(popDF$Aearly)
    popDF$log_Alate <- log(popDF$Alate)
    
    popDF$log_gsdaily <- log(popDF$gsdaily)
    popDF$log_gsearly <- log(popDF$gsearly)
    popDF$log_gslate <- log(popDF$gslate)
    
    popDF$log_WUE_daily <- log(popDF$WUE_daily)
    popDF$log_WUE_early <- log(popDF$WUE_early)
    popDF$log_WUE_late <- log(popDF$WUE_late)
    

    ### summary
    pilDF1 <- summaryBy(log_Aearly+log_Alate+log_gsearly+log_gslate+
                            log_WUE_early+log_WUE_late~Trt+CO2+H2O+Day,
                        FUN=c(mean,se), data=pilDF, keep.names=T)
    
    pilDF2 <- summaryBy(log_Adaily+log_gsdaily+
                            log_WUE_daily~Trt+CO2+H2O+Day,
                        FUN=c(mean,se), data=pilDF, keep.names=T)
    
    
    popDF1 <- summaryBy(log_Aearly+log_Alate+log_gsearly+log_gslate+
                          log_WUE_early+log_WUE_late~Trt+CO2+H2O+Day,
                      FUN=c(mean,se), data=popDF, keep.names=T)
    
    popDF2 <- summaryBy(log_Adaily+log_gsdaily+
                            log_WUE_daily~Trt+CO2+H2O+Day,
                        FUN=c(mean,se), data=popDF, keep.names=T)
    
    ### merge
    pDF1 <- merge(pilDF1, pilDF2, by=c("Trt", "CO2", "H2O", "Day"))
    pDF2 <- merge(popDF1, popDF2, by=c("Trt", "CO2", "H2O", "Day"))
    

    ### day list
    d1 <- unique(pilDF$Day)
    d2 <- unique(popDF$Day)
    
    ### water treatment
    w <- c("D", "ND")
    
    ### subset
    pDF1.amb <- subset(pDF1, CO2 == "A")
    pDF1.ele <- subset(pDF1, CO2 == "E")
    
    pDF2.amb <- subset(pDF2, CO2 == "A")
    pDF2.ele <- subset(pDF2, CO2 == "E")
    
    ### merge the two datasets
    plotDF1 <- merge(pDF1.amb, pDF1.ele, by=c("Day", "H2O"), keep.all=T)
    plotDF2 <- merge(pDF2.amb, pDF2.ele, by=c("Day", "H2O"), keep.all=T)
    
    colnames(plotDF1) <- colnames(plotDF2) <- c("Day", "H2O", "Trt.amb", "CO2.amb",
                                                "amb_log_Aearly_mean", "amb_log_Alate_mean",
                                                "amb_log_GSearly_mean", "amb_log_GSlate_mean",
                                                "amb_log_WUEearly_mean", "amb_log_WUElate_mean",
                                                "amb_log_Aearly_se", "amb_log_Alate_se",
                                                "amb_log_GSearly_se", "amb_log_GSlate_se",
                                                "amb_log_WUEearly_se", "amb_log_WUElate_se",
                                                "amb_log_Adaily_mean", "amb_log_GSdaily_mean", 
                                                "amb_log_WUEdaily_mean", 
                                                "amb_log_Adaily_se", "amb_log_GSdaily_se", 
                                                "amb_log_WUEdaily_se", 
                                                "Trt.ele", "CO2.ele",
                                                "ele_log_Aearly_mean", "ele_log_Alate_mean",
                                                "ele_log_GSearly_mean", "ele_log_GSlate_mean",
                                                "ele_log_WUEearly_mean", "ele_log_WUElate_mean",
                                                "ele_log_Aearly_se", "ele_log_Alate_se",
                                                "ele_log_GSearly_se", "ele_log_GSlate_se",
                                                "ele_log_WUEearly_se", "ele_log_WUElate_se",
                                                "ele_log_Adaily_mean", "ele_log_GSdaily_mean", 
                                                "ele_log_WUEdaily_mean", 
                                                "ele_log_Adaily_se", "ele_log_GSdaily_se", 
                                                "ele_log_WUEdaily_se")
    
    ### Calculate CO2 signal
    plotDF1$log_CO2_Adaily <- plotDF1$ele_log_Adaily_mean - plotDF1$amb_log_Adaily_mean
    plotDF1$log_CO2_Aearly <- plotDF1$ele_log_Aearly_mean - plotDF1$amb_log_Aearly_mean
    plotDF1$log_CO2_Alate <- plotDF1$ele_log_Alate_mean - plotDF1$amb_log_Alate_mean
    
    plotDF1$log_CO2_GSdaily <- plotDF1$ele_log_GSdaily_mean - plotDF1$amb_log_GSdaily_mean
    plotDF1$log_CO2_GSearly <- plotDF1$ele_log_GSearly_mean - plotDF1$amb_log_GSearly_mean
    plotDF1$log_CO2_GSlate <- plotDF1$ele_log_GSlate_mean - plotDF1$amb_log_GSlate_mean
    
    plotDF1$log_CO2_WUEdaily <- plotDF1$ele_log_WUEdaily_mean - plotDF1$amb_log_WUEdaily_mean
    plotDF1$log_CO2_WUEearly <- plotDF1$ele_log_WUEearly_mean - plotDF1$amb_log_WUEearly_mean
    plotDF1$log_CO2_WUElate <- plotDF1$ele_log_WUElate_mean - plotDF1$amb_log_WUElate_mean
    
    ## plotDF2
    plotDF2$log_CO2_Adaily <- plotDF2$ele_log_Adaily_mean - plotDF2$amb_log_Adaily_mean
    plotDF2$log_CO2_Aearly <- plotDF2$ele_log_Aearly_mean - plotDF2$amb_log_Aearly_mean
    plotDF2$log_CO2_Alate <- plotDF2$ele_log_Alate_mean - plotDF2$amb_log_Alate_mean
    
    
    plotDF2$log_CO2_GSdaily <- plotDF2$ele_log_GSdaily_mean - plotDF2$amb_log_GSdaily_mean
    plotDF2$log_CO2_GSearly <- plotDF2$ele_log_GSearly_mean - plotDF2$amb_log_GSearly_mean
    plotDF2$log_CO2_GSlate <- plotDF2$ele_log_GSlate_mean - plotDF2$amb_log_GSlate_mean
    
    plotDF2$log_CO2_WUEdaily <- plotDF2$ele_log_WUEdaily_mean - plotDF2$amb_log_WUEdaily_mean
    plotDF2$log_CO2_WUEearly <- plotDF2$ele_log_WUEearly_mean - plotDF2$amb_log_WUEearly_mean
    plotDF2$log_CO2_WUElate <- plotDF2$ele_log_WUElate_mean - plotDF2$amb_log_WUElate_mean
    
    ### Calculate CO2 ratio SE
    plotDF1$log_CO2_Adaily_se <- sqrt(plotDF1$amb_log_Adaily_se^2 + plotDF1$ele_log_Adaily_se^2)
    plotDF1$log_CO2_Aearly_se <- sqrt(plotDF1$amb_log_Aearly_se^2 + plotDF1$ele_log_Aearly_se^2)
    plotDF1$log_CO2_Alate_se <- sqrt(plotDF1$amb_log_Alate_se^2 + plotDF1$ele_log_Alate_se^2)
    
    plotDF1$log_CO2_GSdaily_se <- sqrt(plotDF1$amb_log_GSdaily_se^2 + plotDF1$ele_log_GSdaily_se^2)
    plotDF1$log_CO2_GSearly_se <- sqrt(plotDF1$amb_log_GSearly_se^2 + plotDF1$ele_log_GSearly_se^2)
    plotDF1$log_CO2_GSlate_se <- sqrt(plotDF1$amb_log_GSlate_se^2 + plotDF1$ele_log_GSlate_se^2)
    
    plotDF1$log_CO2_WUEdaily_se <- sqrt(plotDF1$amb_log_WUEdaily_se^2 + plotDF1$ele_log_WUEdaily_se^2)
    plotDF1$log_CO2_WUEearly_se <- sqrt(plotDF1$amb_log_WUEearly_se^2 + plotDF1$ele_log_WUEearly_se^2)
    plotDF1$log_CO2_WUElate_se <- sqrt(plotDF1$amb_log_WUElate_se^2 + plotDF1$ele_log_WUElate_se^2)
    
    # plotDF2
    plotDF2$log_CO2_Adaily_se <- sqrt(plotDF2$amb_log_Adaily_se^2 + plotDF2$ele_log_Adaily_se^2)
    plotDF2$log_CO2_Aearly_se <- sqrt(plotDF2$amb_log_Aearly_se^2 + plotDF2$ele_log_Aearly_se^2)
    plotDF2$log_CO2_Alate_se <- sqrt(plotDF2$amb_log_Alate_se^2 + plotDF2$ele_log_Alate_se^2)
    
    plotDF2$log_CO2_GSdaily_se <- sqrt(plotDF2$amb_log_GSdaily_se^2 + plotDF2$ele_log_GSdaily_se^2)
    plotDF2$log_CO2_GSearly_se <- sqrt(plotDF2$amb_log_GSearly_se^2 + plotDF2$ele_log_GSearly_se^2)
    plotDF2$log_CO2_GSlate_se <- sqrt(plotDF2$amb_log_GSlate_se^2 + plotDF2$ele_log_GSlate_se^2)
    
    plotDF2$log_CO2_WUEdaily_se <- sqrt(plotDF2$amb_log_WUEdaily_se^2 + plotDF2$ele_log_WUEdaily_se^2)
    plotDF2$log_CO2_WUEearly_se <- sqrt(plotDF2$amb_log_WUEearly_se^2 + plotDF2$ele_log_WUEearly_se^2)
    plotDF2$log_CO2_WUElate_se <- sqrt(plotDF2$amb_log_WUElate_se^2 + plotDF2$ele_log_WUElate_se^2)
    
    
    ### exponential mean
    plotDF1$CO2_Adaily <- exp(plotDF1$log_CO2_Adaily)
    plotDF1$CO2_Aearly <- exp(plotDF1$log_CO2_Aearly)
    plotDF1$CO2_Alate <- exp(plotDF1$log_CO2_Alate)
    
    plotDF1$CO2_GSdaily <- exp(plotDF1$log_CO2_GSdaily)
    plotDF1$CO2_GSearly <- exp(plotDF1$log_CO2_GSearly)
    plotDF1$CO2_GSlate <- exp(plotDF1$log_CO2_GSlate)
    
    plotDF1$CO2_WUEdaily <- exp(plotDF1$log_CO2_WUEdaily)
    plotDF1$CO2_WUEearly <- exp(plotDF1$log_CO2_WUEearly)
    plotDF1$CO2_WUElate <- exp(plotDF1$log_CO2_WUElate)
    
    ## plotDF2
    plotDF2$CO2_Adaily <- exp(plotDF2$log_CO2_Adaily)
    plotDF2$CO2_Aearly <- exp(plotDF2$log_CO2_Aearly)
    plotDF2$CO2_Alate <- exp(plotDF2$log_CO2_Alate)
    
    plotDF2$CO2_GSdaily <- exp(plotDF2$log_CO2_GSdaily)
    plotDF2$CO2_GSearly <- exp(plotDF2$log_CO2_GSearly)
    plotDF2$CO2_GSlate <- exp(plotDF2$log_CO2_GSlate)
    
    plotDF2$CO2_WUEdaily <- exp(plotDF2$log_CO2_WUEdaily)
    plotDF2$CO2_WUEearly <- exp(plotDF2$log_CO2_WUEearly)
    plotDF2$CO2_WUElate <- exp(plotDF2$log_CO2_WUElate)
    
    ### exponential se
    plotDF1$CO2_AdailyUP <- exp(plotDF1$log_CO2_Adaily+plotDF1$log_CO2_Adaily_se)
    plotDF1$CO2_AdailyLO <- exp(plotDF1$log_CO2_Adaily-plotDF1$log_CO2_Adaily_se)
    
    plotDF1$CO2_AearlyUP <- exp(plotDF1$log_CO2_Aearly+plotDF1$log_CO2_Aearly_se)
    plotDF1$CO2_AearlyLO <- exp(plotDF1$log_CO2_Aearly-plotDF1$log_CO2_Aearly_se)
    
    plotDF1$CO2_AlateUP <- exp(plotDF1$log_CO2_Alate+plotDF1$log_CO2_Alate_se)
    plotDF1$CO2_AlateLO <- exp(plotDF1$log_CO2_Alate-plotDF1$log_CO2_Alate_se)
    
    
    plotDF1$CO2_GSdailyUP <- exp(plotDF1$log_CO2_GSdaily+plotDF1$log_CO2_GSdaily_se)
    plotDF1$CO2_GSdailyLO <- exp(plotDF1$log_CO2_GSdaily-plotDF1$log_CO2_GSdaily_se)
    
    plotDF1$CO2_GSearlyUP <- exp(plotDF1$log_CO2_GSearly+plotDF1$log_CO2_GSearly_se)
    plotDF1$CO2_GSearlyLO <- exp(plotDF1$log_CO2_GSearly-plotDF1$log_CO2_GSearly_se)
    
    plotDF1$CO2_GSlateUP <- exp(plotDF1$log_CO2_GSlate+plotDF1$log_CO2_GSlate_se)
    plotDF1$CO2_GSlateLO <- exp(plotDF1$log_CO2_GSlate-plotDF1$log_CO2_GSlate_se)
    
    
    plotDF1$CO2_WUEdailyUP <- exp(plotDF1$log_CO2_WUEdaily+plotDF1$log_CO2_WUEdaily_se)
    plotDF1$CO2_WUEdailyLO <- exp(plotDF1$log_CO2_WUEdaily-plotDF1$log_CO2_WUEdaily_se)
    
    plotDF1$CO2_WUEearlyUP <- exp(plotDF1$log_CO2_WUEearly+plotDF1$log_CO2_WUEearly_se)
    plotDF1$CO2_WUEearlyLO <- exp(plotDF1$log_CO2_WUEearly-plotDF1$log_CO2_WUEearly_se)
    
    plotDF1$CO2_WUElateUP <- exp(plotDF1$log_CO2_WUElate+plotDF1$log_CO2_WUElate_se)
    plotDF1$CO2_WUElateLO <- exp(plotDF1$log_CO2_WUElate-plotDF1$log_CO2_WUElate_se)
    
    
    ## plotDF2
    plotDF2$CO2_AdailyUP <- exp(plotDF2$log_CO2_Adaily+plotDF2$log_CO2_Adaily_se)
    plotDF2$CO2_AdailyLO <- exp(plotDF2$log_CO2_Adaily-plotDF2$log_CO2_Adaily_se)
    
    plotDF2$CO2_AearlyUP <- exp(plotDF2$log_CO2_Aearly+plotDF2$log_CO2_Aearly_se)
    plotDF2$CO2_AearlyLO <- exp(plotDF2$log_CO2_Aearly-plotDF2$log_CO2_Aearly_se)
    
    plotDF2$CO2_AlateUP <- exp(plotDF2$log_CO2_Alate+plotDF2$log_CO2_Alate_se)
    plotDF2$CO2_AlateLO <- exp(plotDF2$log_CO2_Alate-plotDF2$log_CO2_Alate_se)
    
    
    plotDF2$CO2_GSdailyUP <- exp(plotDF2$log_CO2_GSdaily+plotDF2$log_CO2_GSdaily_se)
    plotDF2$CO2_GSdailyLO <- exp(plotDF2$log_CO2_GSdaily-plotDF2$log_CO2_GSdaily_se)
    
    plotDF2$CO2_GSearlyUP <- exp(plotDF2$log_CO2_GSearly+plotDF2$log_CO2_GSearly_se)
    plotDF2$CO2_GSearlyLO <- exp(plotDF2$log_CO2_GSearly-plotDF2$log_CO2_GSearly_se)
    
    plotDF2$CO2_GSlateUP <- exp(plotDF2$log_CO2_GSlate+plotDF2$log_CO2_GSlate_se)
    plotDF2$CO2_GSlateLO <- exp(plotDF2$log_CO2_GSlate-plotDF2$log_CO2_GSlate_se)
    
    
    plotDF2$CO2_WUEdailyUP <- exp(plotDF2$log_CO2_WUEdaily+plotDF2$log_CO2_WUEdaily_se)
    plotDF2$CO2_WUEdailyLO <- exp(plotDF2$log_CO2_WUEdaily-plotDF2$log_CO2_WUEdaily_se)
    
    plotDF2$CO2_WUEearlyUP <- exp(plotDF2$log_CO2_WUEearly+plotDF2$log_CO2_WUEearly_se)
    plotDF2$CO2_WUEearlyLO <- exp(plotDF2$log_CO2_WUEearly-plotDF2$log_CO2_WUEearly_se)
    
    plotDF2$CO2_WUElateUP <- exp(plotDF2$log_CO2_WUElate+plotDF2$log_CO2_WUElate_se)
    plotDF2$CO2_WUElateLO <- exp(plotDF2$log_CO2_WUElate-plotDF2$log_CO2_WUElate_se)
    
    
    ### treatment factor
    names(plotDF1)[names(plotDF1)=="H2O"] <- "Trt"
    names(plotDF2)[names(plotDF2)=="H2O"] <- "Trt"
    
    
    ### plotting
    p1 <- ggplot(plotDF1, aes(x=Day, y=CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AdailyLO, 
                          ymax=CO2_AdailyUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(0, 8)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p2 <- ggplot(plotDF1, aes(x=Day, y=CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AearlyLO, 
                          ymax=CO2_AearlyUP),
                      width=0.2)+
        geom_line(aes(col=Trt))+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(0, 8)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p3 <- ggplot(plotDF1, aes(x=Day, y=CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AlateLO, 
                          ymax=CO2_AlateUP),
                      width=0.2)+
        geom_line(aes(col=Trt))+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(0, 8)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p4 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSdailyLO, 
                          ymax=CO2_GSdailyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p5 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSearlyLO, 
                          ymax=CO2_GSearlyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p6 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSlateLO, 
                          ymax=CO2_GSlateUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    
    p7 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEdailyLO, 
                          ymax=CO2_WUEdailyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p8 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEearlyLO, 
                          ymax=CO2_WUEearlyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p9 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUElateLO, 
                          ymax=CO2_WUElateUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)",
                                         "(g)", "(h)", "(i)"), 
                                ncol=3, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf(paste0(outdir, "F10.1.CO2_ratio_pilularis.pdf"), width=14, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    
    
    
    ### plotting
    p1 <- ggplot(plotDF2, aes(x=Day, y=CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AdailyLO, 
                          ymax=CO2_AdailyUP),
                      width=1.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    p2 <- ggplot(plotDF2, aes(x=Day, y=CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AearlyLO, 
                          ymax=CO2_AearlyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    p3 <- ggplot(plotDF2, aes(x=Day, y=CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AlateLO, 
                          ymax=CO2_AlateUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    p4 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSdailyLO, 
                          ymax=CO2_GSdailyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    p5 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSearlyLO, 
                          ymax=CO2_GSearlyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    p6 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSlateLO, 
                          ymax=CO2_GSlateUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    p7 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEdailyLO, 
                          ymax=CO2_WUEdailyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    p8 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEearlyLO, 
                          ymax=CO2_WUEearlyUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    p9 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUElateLO, 
                          ymax=CO2_WUElateUP),
                      width=1.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("grey", "black"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("grey", "black"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)",
                                         "(g)", "(h)", "(i)"), 
                                ncol=3, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf(paste0(outdir, "F10.2.CO2_ratio_populnea.pdf"), width=14, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    
    
    
    
    
    
    ### combined species plot
    p1 <- ggplot(plotDF1, aes(x=Day, y=CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=5)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AdailyLO, 
                          ymax=CO2_AdailyUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("brown", "orange"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("brown", "orange"),
                          guide=guide_legend(nrow=1))+
        ggtitle("E. pilularis")+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("brown", "orange"),
                                                       col = c("brown", "orange"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p3 <- ggplot(plotDF1, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=5)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSdailyLO, 
                          ymax=CO2_GSdailyUP),
                      width=0.2)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("brown", "orange"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("brown", "orange"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("brown", "orange"),
                                                       col = c("brown", "orange"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p5 <- ggplot(plotDF1, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=5)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEdailyLO, 
                          ymax=CO2_WUEdailyUP),
                      width=0.2)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("brown", "orange"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("brown", "orange"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("brown", "orange"),
                                                       col = c("brown", "orange"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p2 <- ggplot(plotDF2, aes(x=Day, y=CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=5)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_AdailyLO, 
                          ymax=CO2_AdailyUP),
                      width=1.)+
        geom_hline(yintercept=1, col="black", lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("brown", "orange"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("brown", "orange"),
                          guide=guide_legend(nrow=1))+
        ggtitle("E. populnea")+
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("brown", "orange"),
                                                       col = c("brown", "orange"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    p4 <- ggplot(plotDF2, aes(x=Day, y=CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=5)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_GSdailyLO, 
                          ymax=CO2_GSdailyUP),
                      width=1.)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("brown", "orange"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("brown", "orange"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("brown", "orange"),
                                                       col = c("brown", "orange"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    p6 <- ggplot(plotDF2, aes(x=Day, y=CO2_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=5)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=CO2_WUEdailyLO, 
                          ymax=CO2_WUEdailyUP),
                      width=1.)+
        theme_linedraw() +
        geom_hline(yintercept=1, col="black", lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(CO[2]* " ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("brown", "orange"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("brown", "orange"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("brown", "orange"),
                                                       col = c("brown", "orange"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6,
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
                                ncol=2, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf(paste0(outdir, "F10.CO2_ratio_daily_species_combined.pdf"), width=12, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
}