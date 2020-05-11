make_CO2_difference_of_A_and_gs_plots_with_se <- function() {
    
    ### read input
    pilDF<-read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv",sep=",", header=TRUE)
    
    ### caclulate daily
    pilDF$Adaily <- (pilDF$Aearly + pilDF$Alate) / 2
    pilDF$gsdaily <- (pilDF$gsearly + pilDF$gslate) / 2
    
    popDF$Adaily <- (popDF$Aearly + popDF$Alate) / 2
    popDF$gsdaily <- (popDF$gsearly + popDF$gslate) / 2
    
    ### calculate iWUE
    pilDF$WUE_daily <- pilDF$Adaily / pilDF$gsdaily
    pilDF$WUE_early <- pilDF$Aearly / pilDF$gsearly
    pilDF$WUE_late <- pilDF$Alate / pilDF$gslate
    
    popDF$WUE_daily <- popDF$Adaily / popDF$gsdaily
    popDF$WUE_early <- popDF$Aearly / popDF$gsearly
    popDF$WUE_late <- popDF$Alate / popDF$gslate
    
    
    ### summary
    pDF1 <- summaryBy(Adaily+Aearly+Alate+gsdaily+gsearly+gslate+
                          WUE_daily+WUE_early+WUE_late~Trt+CO2+H2O+Day,
                      FUN=c(mean,se), data=pilDF, keep.names=T)
    
    pDF2 <- summaryBy(Adaily+Aearly+Alate+gsdaily+gsearly+gslate+
                          WUE_daily+WUE_early+WUE_late~Trt+CO2+H2O+Day,
                      FUN=c(mean,se), data=popDF, keep.names=T)
    
    
    ### subset
    pDF1.amb <- subset(pDF1, CO2 == "A")
    pDF1.ele <- subset(pDF1, CO2 == "E")
    
    pDF2.amb <- subset(pDF2, CO2 == "A")
    pDF2.ele <- subset(pDF2, CO2 == "E")
    
    ### merge the two datasets
    plotDF1 <- merge(pDF1.amb, pDF1.ele, by=c("Day", "H2O"), keep.all=T)
    plotDF2 <- merge(pDF2.amb, pDF2.ele, by=c("Day", "H2O"), keep.all=T)
    
    colnames(plotDF1) <- colnames(plotDF2) <- c("Day", "H2O", "Trt.amb", "CO2.amb",
                                                "amb_Adaily_mean", "amb_Aearly_mean", "amb_Alate_mean",
                                                "amb_GSdaily_mean", "amb_GSearly_mean", "amb_GSlate_mean",
                                                "amb_WUEdaily_mean", "amb_WUEearly_mean", "amb_WUElate_mean",
                                                "amb_Adaily_se", "amb_Aearly_se", "amb_Alate_se",
                                                "amb_GSdaily_se", "amb_GSearly_se", "amb_GSlate_se",
                                                "amb_WUEdaily_se", "amb_WUEearly_se", "amb_WUElate_se",
                                                "Trt.ele", "CO2.ele",
                                                "ele_Adaily_mean", "ele_Aearly_mean", "ele_Alate_mean",
                                                "ele_GSdaily_mean", "ele_GSearly_mean", "ele_GSlate_mean",
                                                "ele_WUEdaily_mean", "ele_WUEearly_mean", "ele_WUElate_mean",
                                                "ele_Adaily_se", "ele_Aearly_se", "ele_Alate_se",
                                                "ele_GSdaily_se", "ele_GSearly_se", "ele_GSlate_se",
                                                "ele_WUEdaily_se", "ele_WUEearly_se", "ele_WUElate_se")
    
    
    ### Calculate CO2 signal
    plotDF1$CO2_Adaily <- plotDF1$ele_Adaily_mean - plotDF1$amb_Adaily_mean
    plotDF1$CO2_Aearly <- plotDF1$ele_Aearly_mean - plotDF1$amb_Aearly_mean
    plotDF1$CO2_Alate <- plotDF1$ele_Alate_mean - plotDF1$amb_Alate_mean
    
    plotDF1$CO2_GSdaily <- plotDF1$ele_GSdaily_mean - plotDF1$amb_GSdaily_mean
    plotDF1$CO2_GSearly <- plotDF1$ele_GSearly_mean - plotDF1$amb_GSearly_mean
    plotDF1$CO2_GSlate <- plotDF1$ele_GSlate_mean - plotDF1$amb_GSlate_mean
    
    plotDF1$CO2_WUEdaily <- plotDF1$ele_WUEdaily_mean - plotDF1$amb_WUEdaily_mean
    plotDF1$CO2_WUEearly <- plotDF1$ele_WUEearly_mean - plotDF1$amb_WUEearly_mean
    plotDF1$CO2_WUElate <- plotDF1$ele_WUElate_mean - plotDF1$amb_WUElate_mean
    
    ## plotDF2
    plotDF2$CO2_Adaily <- plotDF2$ele_Adaily_mean - plotDF2$amb_Adaily_mean
    plotDF2$CO2_Aearly <- plotDF2$ele_Aearly_mean - plotDF2$amb_Aearly_mean
    plotDF2$CO2_Alate <- plotDF2$ele_Alate_mean - plotDF2$amb_Alate_mean
    
    
    plotDF2$CO2_GSdaily <- plotDF2$ele_GSdaily_mean - plotDF2$amb_GSdaily_mean
    plotDF2$CO2_GSearly <- plotDF2$ele_GSearly_mean - plotDF2$amb_GSearly_mean
    plotDF2$CO2_GSlate <- plotDF2$ele_GSlate_mean - plotDF2$amb_GSlate_mean
    
    plotDF2$CO2_WUEdaily <- plotDF2$ele_WUEdaily_mean - plotDF2$amb_WUEdaily_mean
    plotDF2$CO2_WUEearly <- plotDF2$ele_WUEearly_mean - plotDF2$amb_WUEearly_mean
    plotDF2$CO2_WUElate <- plotDF2$ele_WUElate_mean - plotDF2$amb_WUElate_mean
    
    ### Calculate CO2 ratio SE
    plotDF1$CO2_Adaily_se <- sqrt(plotDF1$amb_Adaily_se^2 + plotDF1$ele_Adaily_se^2)
    plotDF1$CO2_Aearly_se <- sqrt(plotDF1$amb_Aearly_se^2 + plotDF1$ele_Aearly_se^2)
    plotDF1$CO2_Alate_se <- sqrt(plotDF1$amb_Alate_se^2 + plotDF1$ele_Alate_se^2)
    
    plotDF1$CO2_GSdaily_se <- sqrt(plotDF1$amb_GSdaily_se^2 + plotDF1$ele_GSdaily_se^2)
    plotDF1$CO2_GSearly_se <- sqrt(plotDF1$amb_GSearly_se^2 + plotDF1$ele_GSearly_se^2)
    plotDF1$CO2_GSlate_se <- sqrt(plotDF1$amb_GSlate_se^2 + plotDF1$ele_GSlate_se^2)
    
    plotDF1$CO2_WUEdaily_se <- sqrt(plotDF1$amb_WUEdaily_se^2 + plotDF1$ele_WUEdaily_se^2)
    plotDF1$CO2_WUEearly_se <- sqrt(plotDF1$amb_WUEearly_se^2 + plotDF1$ele_WUEearly_se^2)
    plotDF1$CO2_WUElate_se <- sqrt(plotDF1$amb_WUElate_se^2 + plotDF1$ele_WUElate_se^2)
    
    # plotDF2
    plotDF2$CO2_Adaily_se <- sqrt(plotDF2$amb_Adaily_se^2 + plotDF2$ele_Adaily_se^2)
    plotDF2$CO2_Aearly_se <- sqrt(plotDF2$amb_Aearly_se^2 + plotDF2$ele_Aearly_se^2)
    plotDF2$CO2_Alate_se <- sqrt(plotDF2$amb_Alate_se^2 + plotDF2$ele_Alate_se^2)
    
    plotDF2$CO2_GSdaily_se <- sqrt(plotDF2$amb_GSdaily_se^2 + plotDF2$ele_GSdaily_se^2)
    plotDF2$CO2_GSearly_se <- sqrt(plotDF2$amb_GSearly_se^2 + plotDF2$ele_GSearly_se^2)
    plotDF2$CO2_GSlate_se <- sqrt(plotDF2$amb_GSlate_se^2 + plotDF2$ele_GSlate_se^2)
    
    plotDF2$CO2_WUEdaily_se <- sqrt(plotDF2$amb_WUEdaily_se^2 + plotDF2$ele_WUEdaily_se^2)
    plotDF2$CO2_WUEearly_se <- sqrt(plotDF2$amb_WUEearly_se^2 + plotDF2$ele_WUEearly_se^2)
    plotDF2$CO2_WUElate_se <- sqrt(plotDF2$amb_WUElate_se^2 + plotDF2$ele_WUElate_se^2)
    
    
    ### range
    plotDF1$CO2_AdailyUP <- plotDF1$CO2_Adaily+plotDF1$CO2_Adaily_se
    plotDF1$CO2_AdailyLO <- plotDF1$CO2_Adaily-plotDF1$CO2_Adaily_se
    
    plotDF1$CO2_AearlyUP <- plotDF1$CO2_Aearly+plotDF1$CO2_Aearly_se
    plotDF1$CO2_AearlyLO <- plotDF1$CO2_Aearly-plotDF1$CO2_Aearly_se
    
    plotDF1$CO2_AlateUP <- plotDF1$CO2_Alate+plotDF1$CO2_Alate_se
    plotDF1$CO2_AlateLO <- plotDF1$CO2_Alate-plotDF1$CO2_Alate_se
    
    
    plotDF1$CO2_GSdailyUP <- plotDF1$CO2_GSdaily+plotDF1$CO2_GSdaily_se
    plotDF1$CO2_GSdailyLO <- plotDF1$CO2_GSdaily-plotDF1$CO2_GSdaily_se
    
    plotDF1$CO2_GSearlyUP <- plotDF1$CO2_GSearly+plotDF1$CO2_GSearly_se
    plotDF1$CO2_GSearlyLO <- plotDF1$CO2_GSearly-plotDF1$CO2_GSearly_se
    
    plotDF1$CO2_GSlateUP <- plotDF1$CO2_GSlate+plotDF1$CO2_GSlate_se
    plotDF1$CO2_GSlateLO <- plotDF1$CO2_GSlate-plotDF1$CO2_GSlate_se
    
    
    plotDF1$CO2_WUEdailyUP <- plotDF1$CO2_WUEdaily+plotDF1$CO2_WUEdaily_se
    plotDF1$CO2_WUEdailyLO <- plotDF1$CO2_WUEdaily-plotDF1$CO2_WUEdaily_se
    
    plotDF1$CO2_WUEearlyUP <- plotDF1$CO2_WUEearly+plotDF1$CO2_WUEearly_se
    plotDF1$CO2_WUEearlyLO <- plotDF1$CO2_WUEearly-plotDF1$CO2_WUEearly_se
    
    plotDF1$CO2_WUElateUP <- plotDF1$CO2_WUElate+plotDF1$CO2_WUElate_se
    plotDF1$CO2_WUElateLO <- plotDF1$CO2_WUElate-plotDF1$CO2_WUElate_se
    
    
    ## plotDF2
    plotDF2$CO2_AdailyUP <- plotDF2$CO2_Adaily+plotDF2$CO2_Adaily_se
    plotDF2$CO2_AdailyLO <- plotDF2$CO2_Adaily-plotDF2$CO2_Adaily_se
    
    plotDF2$CO2_AearlyUP <- plotDF2$CO2_Aearly+plotDF2$CO2_Aearly_se
    plotDF2$CO2_AearlyLO <- plotDF2$CO2_Aearly-plotDF2$CO2_Aearly_se
    
    plotDF2$CO2_AlateUP <- plotDF2$CO2_Alate+plotDF2$CO2_Alate_se
    plotDF2$CO2_AlateLO <- plotDF2$CO2_Alate-plotDF2$CO2_Alate_se
    
    
    plotDF2$CO2_GSdailyUP <- plotDF2$CO2_GSdaily+plotDF2$CO2_GSdaily_se
    plotDF2$CO2_GSdailyLO <- plotDF2$CO2_GSdaily-plotDF2$CO2_GSdaily_se
    
    plotDF2$CO2_GSearlyUP <- plotDF2$CO2_GSearly+plotDF2$CO2_GSearly_se
    plotDF2$CO2_GSearlyLO <- plotDF2$CO2_GSearly-plotDF2$CO2_GSearly_se
    
    plotDF2$CO2_GSlateUP <- plotDF2$CO2_GSlate+plotDF2$CO2_GSlate_se
    plotDF2$CO2_GSlateLO <- plotDF2$CO2_GSlate-plotDF2$CO2_GSlate_se
    
    
    plotDF2$CO2_WUEdailyUP <- plotDF2$CO2_WUEdaily+plotDF2$CO2_WUEdaily_se
    plotDF2$CO2_WUEdailyLO <- plotDF2$CO2_WUEdaily-plotDF2$CO2_WUEdaily_se
    
    plotDF2$CO2_WUEearlyUP <- plotDF2$CO2_WUEearly+plotDF2$CO2_WUEearly_se
    plotDF2$CO2_WUEearlyLO <- plotDF2$CO2_WUEearly-plotDF2$CO2_WUEearly_se
    
    plotDF2$CO2_WUElateUP <- plotDF2$CO2_WUElate+plotDF2$CO2_WUElate_se
    plotDF2$CO2_WUElateLO <- plotDF2$CO2_WUElate-plotDF2$CO2_WUElate_se
    
    
    
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * A[sat] * 
                                  " (" * mu * "mol " * m^-2 * " " * 
                                  s^-1 *")")))+
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
        ylim(-5, 15)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * A[sat])))+
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
        ylim(-5, 15)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * A[sat])))+
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
        ylim(-5, 15)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * g[s])))+
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
        ylim(-0.3, 0.1)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * g[s])))+
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
        ylim(-0.3, 0.1)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * g[s])))+
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
        ylim(-0.3, 0.1)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * "iWUE")))+
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
        ylim(-50, 250)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * "iWUE")))+
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
        ylim(-50, 250)+
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
        geom_hline(yintercept=0, col="black", lty=2)+
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * "iWUE")))+
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
        ylim(-50, 250)+
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
    
    
    pdf(paste0(outdir, "F10.1.CO2_diff_pilularis.pdf"), width=14, height=12)
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * A[sat])))+
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
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * A[sat])))+
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
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * A[sat])))+
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
        ylim(0, 20)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * g[s])))+
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
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * g[s])))+
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
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * g[s])))+
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
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * "iWUE")))+
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
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * "iWUE")))+
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
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
        ylab(expression(paste(eC[a] * " - " * aC[a] * " for " * "iWUE")))+
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
        ylim(0, 10)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("grey", "black"),
                                                       col = c("grey", "black"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
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
    
    
    pdf(paste0(outdir, "F10.2.CO2_diff_populnea.pdf"), width=14, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
}