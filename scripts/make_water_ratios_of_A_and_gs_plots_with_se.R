make_water_ratios_of_A_and_gs_plots_with_se <- function() {
    
    ### read input
    pilDF<-read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv",sep=",", header=TRUE)
    
    ## remove data points with unequal sample size
    ## E.pilularis: Day > 6, well-watered
    ## E. populnea: POPAD: Day > 30
    ## E. populnea: POPAND: day >=19
    ## E. populnea: POPED: day >= 22
    ## E. populnea: POPEND: day >= 19
    
    ### pilDF
    subDF1 <- pilDF[pilDF$Trt%in%c("PILAD", "PILED"),]
    subDF2 <- pilDF[pilDF$Trt%in%c("PILAND", "PILEND"),]
    subDF2 <- subDF2[subDF2$Day <= 6, ]
    subDF1 <- subDF1[subDF1$Day <= 7, ]
    pilDF <- rbind(subDF1, subDF2)
    
    ### popDF
    subDF1 <- popDF[popDF$Trt=="POPAD"&popDF$Day<= 30,]
    subDF2 <- popDF[popDF$Trt=="POPAND"&popDF$Day< 19,]
    subDF3 <- popDF[popDF$Trt=="POPED"&popDF$Day< 22,]
    subDF4 <- popDF[popDF$Trt=="POPEND"&popDF$Day< 19,]
    
    popDF <- rbind(subDF1, subDF2, subDF3, subDF4)
    
    ### caclulate daily
    pilDF$Adaily <- (pilDF$Aearly + pilDF$Alate) / 2
    pilDF$gsdaily <- (pilDF$gsearly + pilDF$gslate) / 2
    
    popDF$Adaily <- (popDF$Aearly + popDF$Alate) / 2
    popDF$gsdaily <- (popDF$gsearly + popDF$gslate) / 2
    
    ### calculate iWUS
    pilDF$WUE_daily <- pilDF$Adaily / pilDF$gsdaily
    pilDF$WUE_early <- pilDF$Aearly / pilDF$gsearly
    pilDF$WUE_late <- pilDF$Alate / pilDF$gslate
    
    popDF$WUE_daily <- popDF$Adaily / popDF$gsdaily
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
    pDF1 <- summaryBy(log_Adaily+log_Aearly+log_Alate+log_gsdaily+log_gsearly+log_gslate+
                          log_WUE_daily+log_WUE_early+log_WUE_late~Trt+CO2+H2O+Day,
                      FUN=c(mean,se), data=pilDF, keep.names=T)
    
    pDF2 <- summaryBy(log_Adaily+log_Aearly+log_Alate+log_gsdaily+log_gsearly+log_gslate+
                          log_WUE_daily+log_WUE_early+log_WUE_late~Trt+CO2+H2O+Day,
                      FUN=c(mean,se), data=popDF, keep.names=T)

    ### day list
    d1 <- unique(pilDF$Day)
    d2 <- unique(popDF$Day)
    
    ### CO2 treatment
    w <- c("A", "E")
    
    ### subset
    pDF1.d <- subset(pDF1, H2O == "D")
    pDF1.nd <- subset(pDF1, H2O == "ND")
    
    pDF2.d <- subset(pDF2, H2O == "D")
    pDF2.nd <- subset(pDF2, H2O == "ND")
    
    ### merge the two datasets
    plotDF1 <- merge(pDF1.d, pDF1.nd, by=c("Day", "CO2"), keep.all=T)
    plotDF2 <- merge(pDF2.d, pDF2.nd, by=c("Day", "CO2"), keep.all=T)
    
    colnames(plotDF1) <- colnames(plotDF2) <- c("Day", "CO2", "Trt.d", "H2O.d",
                                                "d_log_Adaily_mean", "d_log_Aearly_mean", "d_log_Alate_mean",
                                                "d_log_GSdaily_mean", "d_log_GSearly_mean", "d_log_GSlate_mean",
                                                "d_log_WUEdaily_mean", "d_log_WUEearly_mean", "d_log_WUElate_mean",
                                                "d_log_Adaily_se", "d_log_Aearly_se", "d_log_Alate_se",
                                                "d_log_GSdaily_se", "d_log_GSearly_se", "d_log_GSlate_se",
                                                "d_log_WUEdaily_se", "d_log_WUEearly_se", "d_log_WUElate_se",
                                                "Trt.nd", "CO2.nd",
                                                "nd_log_Adaily_mean", "nd_log_Aearly_mean", "nd_log_Alate_mean",
                                                "nd_log_GSdaily_mean", "nd_log_GSearly_mean", "nd_log_GSlate_mean",
                                                "nd_log_WUEdaily_mean", "nd_log_WUEearly_mean", "nd_log_WUElate_mean",
                                                "nd_log_Adaily_se", "nd_log_Aearly_se", "nd_log_Alate_se",
                                                "nd_log_GSdaily_se", "nd_log_GSearly_se", "nd_log_GSlate_se",
                                                "nd_log_WUEdaily_se", "nd_log_WUEearly_se", "nd_log_WUElate_se")
    
    ### Calculate CO2 signal
    plotDF1$log_Water_Adaily <- plotDF1$nd_log_Adaily_mean - plotDF1$d_log_Adaily_mean
    plotDF1$log_Water_Aearly <- plotDF1$nd_log_Aearly_mean - plotDF1$d_log_Aearly_mean
    plotDF1$log_Water_Alate <- plotDF1$nd_log_Alate_mean - plotDF1$d_log_Alate_mean
    
    plotDF1$log_Water_GSdaily <- plotDF1$nd_log_GSdaily_mean - plotDF1$d_log_GSdaily_mean
    plotDF1$log_Water_GSearly <- plotDF1$nd_log_GSearly_mean - plotDF1$d_log_GSearly_mean
    plotDF1$log_Water_GSlate <- plotDF1$nd_log_GSlate_mean - plotDF1$d_log_GSlate_mean
    
    plotDF1$log_Water_WUEdaily <- plotDF1$nd_log_WUEdaily_mean - plotDF1$d_log_WUEdaily_mean
    plotDF1$log_Water_WUEearly <- plotDF1$nd_log_WUEearly_mean - plotDF1$d_log_WUEearly_mean
    plotDF1$log_Water_WUElate <- plotDF1$nd_log_WUElate_mean - plotDF1$d_log_WUElate_mean
    
    ## plotDF2
    plotDF2$log_Water_Adaily <- plotDF2$nd_log_Adaily_mean - plotDF2$d_log_Adaily_mean
    plotDF2$log_Water_Aearly <- plotDF2$nd_log_Aearly_mean - plotDF2$d_log_Aearly_mean
    plotDF2$log_Water_Alate <- plotDF2$nd_log_Alate_mean - plotDF2$d_log_Alate_mean
    
    
    plotDF2$log_Water_GSdaily <- plotDF2$nd_log_GSdaily_mean - plotDF2$d_log_GSdaily_mean
    plotDF2$log_Water_GSearly <- plotDF2$nd_log_GSearly_mean - plotDF2$d_log_GSearly_mean
    plotDF2$log_Water_GSlate <- plotDF2$nd_log_GSlate_mean - plotDF2$d_log_GSlate_mean
    
    plotDF2$log_Water_WUEdaily <- plotDF2$nd_log_WUEdaily_mean - plotDF2$d_log_WUEdaily_mean
    plotDF2$log_Water_WUEearly <- plotDF2$nd_log_WUEearly_mean - plotDF2$d_log_WUEearly_mean
    plotDF2$log_Water_WUElate <- plotDF2$nd_log_WUElate_mean - plotDF2$d_log_WUElate_mean
    
    ### Calculate Water ratio SE
    plotDF1$log_Water_Adaily_se <- sqrt(plotDF1$d_log_Adaily_se^2 + plotDF1$nd_log_Adaily_se^2)
    plotDF1$log_Water_Aearly_se <- sqrt(plotDF1$d_log_Aearly_se^2 + plotDF1$nd_log_Aearly_se^2)
    plotDF1$log_Water_Alate_se <- sqrt(plotDF1$d_log_Alate_se^2 + plotDF1$nd_log_Alate_se^2)
    
    plotDF1$log_Water_GSdaily_se <- sqrt(plotDF1$d_log_GSdaily_se^2 + plotDF1$nd_log_GSdaily_se^2)
    plotDF1$log_Water_GSearly_se <- sqrt(plotDF1$d_log_GSearly_se^2 + plotDF1$nd_log_GSearly_se^2)
    plotDF1$log_Water_GSlate_se <- sqrt(plotDF1$d_log_GSlate_se^2 + plotDF1$nd_log_GSlate_se^2)
    
    plotDF1$log_Water_WUEdaily_se <- sqrt(plotDF1$d_log_WUEdaily_se^2 + plotDF1$nd_log_WUEdaily_se^2)
    plotDF1$log_Water_WUEearly_se <- sqrt(plotDF1$d_log_WUEearly_se^2 + plotDF1$nd_log_WUEearly_se^2)
    plotDF1$log_Water_WUElate_se <- sqrt(plotDF1$d_log_WUElate_se^2 + plotDF1$nd_log_WUElate_se^2)
    
    # plotDF2
    plotDF2$log_Water_Adaily_se <- sqrt(plotDF2$d_log_Adaily_se^2 + plotDF2$nd_log_Adaily_se^2)
    plotDF2$log_Water_Aearly_se <- sqrt(plotDF2$d_log_Aearly_se^2 + plotDF2$nd_log_Aearly_se^2)
    plotDF2$log_Water_Alate_se <- sqrt(plotDF2$d_log_Alate_se^2 + plotDF2$nd_log_Alate_se^2)
    
    plotDF2$log_Water_GSdaily_se <- sqrt(plotDF2$d_log_GSdaily_se^2 + plotDF2$nd_log_GSdaily_se^2)
    plotDF2$log_Water_GSearly_se <- sqrt(plotDF2$d_log_GSearly_se^2 + plotDF2$nd_log_GSearly_se^2)
    plotDF2$log_Water_GSlate_se <- sqrt(plotDF2$d_log_GSlate_se^2 + plotDF2$nd_log_GSlate_se^2)
    
    plotDF2$log_Water_WUEdaily_se <- sqrt(plotDF2$d_log_WUEdaily_se^2 + plotDF2$nd_log_WUEdaily_se^2)
    plotDF2$log_Water_WUEearly_se <- sqrt(plotDF2$d_log_WUEearly_se^2 + plotDF2$nd_log_WUEearly_se^2)
    plotDF2$log_Water_WUElate_se <- sqrt(plotDF2$d_log_WUElate_se^2 + plotDF2$nd_log_WUElate_se^2)
    
    
    ### exponential mean
    plotDF1$Water_Adaily <- exp(plotDF1$log_Water_Adaily)
    plotDF1$Water_Aearly <- exp(plotDF1$log_Water_Aearly)
    plotDF1$Water_Alate <- exp(plotDF1$log_Water_Alate)
    
    plotDF1$Water_GSdaily <- exp(plotDF1$log_Water_GSdaily)
    plotDF1$Water_GSearly <- exp(plotDF1$log_Water_GSearly)
    plotDF1$Water_GSlate <- exp(plotDF1$log_Water_GSlate)
    
    plotDF1$Water_WUEdaily <- exp(plotDF1$log_Water_WUEdaily)
    plotDF1$Water_WUEearly <- exp(plotDF1$log_Water_WUEearly)
    plotDF1$Water_WUElate <- exp(plotDF1$log_Water_WUElate)
    
    ## plotDF2
    plotDF2$Water_Adaily <- exp(plotDF2$log_Water_Adaily)
    plotDF2$Water_Aearly <- exp(plotDF2$log_Water_Aearly)
    plotDF2$Water_Alate <- exp(plotDF2$log_Water_Alate)
    
    plotDF2$Water_GSdaily <- exp(plotDF2$log_Water_GSdaily)
    plotDF2$Water_GSearly <- exp(plotDF2$log_Water_GSearly)
    plotDF2$Water_GSlate <- exp(plotDF2$log_Water_GSlate)
    
    plotDF2$Water_WUEdaily <- exp(plotDF2$log_Water_WUEdaily)
    plotDF2$Water_WUEearly <- exp(plotDF2$log_Water_WUEearly)
    plotDF2$Water_WUElate <- exp(plotDF2$log_Water_WUElate)
    
    ### exponential se
    plotDF1$Water_AdailyUP <- exp(plotDF1$log_Water_Adaily+plotDF1$log_Water_Adaily_se)
    plotDF1$Water_AdailyLO <- exp(plotDF1$log_Water_Adaily-plotDF1$log_Water_Adaily_se)
    
    plotDF1$Water_AearlyUP <- exp(plotDF1$log_Water_Aearly+plotDF1$log_Water_Aearly_se)
    plotDF1$Water_AearlyLO <- exp(plotDF1$log_Water_Aearly-plotDF1$log_Water_Aearly_se)
    
    plotDF1$Water_AlateUP <- exp(plotDF1$log_Water_Alate+plotDF1$log_Water_Alate_se)
    plotDF1$Water_AlateLO <- exp(plotDF1$log_Water_Alate-plotDF1$log_Water_Alate_se)
    
    
    plotDF1$Water_GSdailyUP <- exp(plotDF1$log_Water_GSdaily+plotDF1$log_Water_GSdaily_se)
    plotDF1$Water_GSdailyLO <- exp(plotDF1$log_Water_GSdaily-plotDF1$log_Water_GSdaily_se)
    
    plotDF1$Water_GSearlyUP <- exp(plotDF1$log_Water_GSearly+plotDF1$log_Water_GSearly_se)
    plotDF1$Water_GSearlyLO <- exp(plotDF1$log_Water_GSearly-plotDF1$log_Water_GSearly_se)
    
    plotDF1$Water_GSlateUP <- exp(plotDF1$log_Water_GSlate+plotDF1$log_Water_GSlate_se)
    plotDF1$Water_GSlateLO <- exp(plotDF1$log_Water_GSlate-plotDF1$log_Water_GSlate_se)
    
    
    plotDF1$Water_WUEdailyUP <- exp(plotDF1$log_Water_WUEdaily+plotDF1$log_Water_WUEdaily_se)
    plotDF1$Water_WUEdailyLO <- exp(plotDF1$log_Water_WUEdaily-plotDF1$log_Water_WUEdaily_se)
    
    plotDF1$Water_WUEearlyUP <- exp(plotDF1$log_Water_WUEearly+plotDF1$log_Water_WUEearly_se)
    plotDF1$Water_WUEearlyLO <- exp(plotDF1$log_Water_WUEearly-plotDF1$log_Water_WUEearly_se)
    
    plotDF1$Water_WUElateUP <- exp(plotDF1$log_Water_WUElate+plotDF1$log_Water_WUElate_se)
    plotDF1$Water_WUElateLO <- exp(plotDF1$log_Water_WUElate-plotDF1$log_Water_WUElate_se)
    
    
    ## plotDF2
    plotDF2$Water_AdailyUP <- exp(plotDF2$log_Water_Adaily+plotDF2$log_Water_Adaily_se)
    plotDF2$Water_AdailyLO <- exp(plotDF2$log_Water_Adaily-plotDF2$log_Water_Adaily_se)
    
    plotDF2$Water_AearlyUP <- exp(plotDF2$log_Water_Aearly+plotDF2$log_Water_Aearly_se)
    plotDF2$Water_AearlyLO <- exp(plotDF2$log_Water_Aearly-plotDF2$log_Water_Aearly_se)
    
    plotDF2$Water_AlateUP <- exp(plotDF2$log_Water_Alate+plotDF2$log_Water_Alate_se)
    plotDF2$Water_AlateLO <- exp(plotDF2$log_Water_Alate-plotDF2$log_Water_Alate_se)
    
    
    plotDF2$Water_GSdailyUP <- exp(plotDF2$log_Water_GSdaily+plotDF2$log_Water_GSdaily_se)
    plotDF2$Water_GSdailyLO <- exp(plotDF2$log_Water_GSdaily-plotDF2$log_Water_GSdaily_se)
    
    plotDF2$Water_GSearlyUP <- exp(plotDF2$log_Water_GSearly+plotDF2$log_Water_GSearly_se)
    plotDF2$Water_GSearlyLO <- exp(plotDF2$log_Water_GSearly-plotDF2$log_Water_GSearly_se)
    
    plotDF2$Water_GSlateUP <- exp(plotDF2$log_Water_GSlate+plotDF2$log_Water_GSlate_se)
    plotDF2$Water_GSlateLO <- exp(plotDF2$log_Water_GSlate-plotDF2$log_Water_GSlate_se)
    
    
    plotDF2$Water_WUEdailyUP <- exp(plotDF2$log_Water_WUEdaily+plotDF2$log_Water_WUEdaily_se)
    plotDF2$Water_WUEdailyLO <- exp(plotDF2$log_Water_WUEdaily-plotDF2$log_Water_WUEdaily_se)
    
    plotDF2$Water_WUEearlyUP <- exp(plotDF2$log_Water_WUEearly+plotDF2$log_Water_WUEearly_se)
    plotDF2$Water_WUEearlyLO <- exp(plotDF2$log_Water_WUEearly-plotDF2$log_Water_WUEearly_se)
    
    plotDF2$Water_WUElateUP <- exp(plotDF2$log_Water_WUElate+plotDF2$log_Water_WUElate_se)
    plotDF2$Water_WUElateLO <- exp(plotDF2$log_Water_WUElate-plotDF2$log_Water_WUElate_se)
    
    
    ### treatment factor
    names(plotDF1)[names(plotDF1)=="CO2"] <- "Trt"
    names(plotDF2)[names(plotDF2)=="CO2"] <- "Trt"
    
    
    ### plotting
    p1 <- ggplot(plotDF1, aes(x=Day, y=Water_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_AdailyLO, 
                          ymax=Water_AdailyUP),
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
        ylab(expression(paste(H[2] * "O ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p2 <- ggplot(plotDF1, aes(x=Day, y=Water_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_AearlyLO, 
                          ymax=Water_AearlyUP),
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
        ylab(expression(paste(H[2] * "O ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p3 <- ggplot(plotDF1, aes(x=Day, y=Water_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_AlateLO, 
                          ymax=Water_AlateUP),
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
        ylab(expression(paste(H[2] * "O ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p4 <- ggplot(plotDF1, aes(x=Day, y=Water_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_GSdailyLO, 
                          ymax=Water_GSdailyUP),
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
        ylab(expression(paste(H[2] * "O ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p5 <- ggplot(plotDF1, aes(x=Day, y=Water_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_GSearlyLO, 
                          ymax=Water_GSearlyUP),
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
        ylab(expression(paste(H[2] * "O ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p6 <- ggplot(plotDF1, aes(x=Day, y=Water_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_GSlateLO, 
                          ymax=Water_GSlateUP),
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
        ylab(expression(paste(H[2] * "O ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    
    p7 <- ggplot(plotDF1, aes(x=Day, y=Water_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_WUEdailyLO, 
                          ymax=Water_WUEdailyUP),
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
        ylab(expression(paste(H[2] * "O ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2.5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p8 <- ggplot(plotDF1, aes(x=Day, y=Water_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_WUEearlyLO, 
                          ymax=Water_WUEearlyUP),
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
        ylab(expression(paste(H[2] * "O ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2.5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p9 <- ggplot(plotDF1, aes(x=Day, y=Water_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_WUElateLO, 
                          ymax=Water_WUElateUP),
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
        ylab(expression(paste(H[2] * "O ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2.5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
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
    
    
    pdf(paste0(outdir, "F11.1.Water_ratio_pilularis.pdf"), width=14, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    
    
    
    ### plotting
    p1 <- ggplot(plotDF2, aes(x=Day, y=Water_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_AdailyLO, 
                          ymax=Water_AdailyUP),
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
        ylab(expression(paste(H[2]* "O ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(0, 1.5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p2 <- ggplot(plotDF2, aes(x=Day, y=Water_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_AearlyLO, 
                          ymax=Water_AearlyUP),
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
        ylab(expression(paste(H[2]* "O ratio "  * A[sat])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(0, 1.5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p3 <- ggplot(plotDF2, aes(x=Day, y=Water_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_AlateLO, 
                          ymax=Water_AlateUP),
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
        ylab(expression(paste(H[2]* "O ratio "  * A[sat])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(0, 1.5)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    
    p4 <- ggplot(plotDF2, aes(x=Day, y=Water_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_GSdailyLO, 
                          ymax=Water_GSdailyUP),
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
        ylab(expression(paste(H[2]* "O ratio "  * g[s])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p5 <- ggplot(plotDF2, aes(x=Day, y=Water_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_GSearlyLO, 
                          ymax=Water_GSearlyUP),
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
        ylab(expression(paste(H[2]* "O ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p6 <- ggplot(plotDF2, aes(x=Day, y=Water_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_GSlateLO, 
                          ymax=Water_GSlateUP),
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
        ylab(expression(paste(H[2]* "O ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p7 <- ggplot(plotDF2, aes(x=Day, y=Water_WUEdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_WUEdailyLO, 
                          ymax=Water_WUEdailyUP),
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
        ylab(expression(paste(H[2]* "O ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p8 <- ggplot(plotDF2, aes(x=Day, y=Water_WUEearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_WUEearlyLO, 
                          ymax=Water_WUEearlyUP),
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
        ylab(expression(paste(H[2]* "O ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p9 <- ggplot(plotDF2, aes(x=Day, y=Water_WUElate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Water_WUElateLO, 
                          ymax=Water_WUElateUP),
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
        ylab(expression(paste(H[2]* "O ratio iWUE")))+
        scale_color_manual(name="",
                           limits=c("A", "E"),
                           labels=c(expression(aC[a]), 
                                    expression(eC[a])),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("A", "E"),
                          labels=c(expression(aC[a]), 
                                   expression(eC[a])),
                          values=c("blue3", "red2"),
                          guide=guide_legend(nrow=1))+
        ylim(0, 2)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("blue3", "red2"),
                                                       col = c("blue3", "red2"))))+
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
    
    
    pdf(paste0(outdir, "F11.2.Water_ratio_populnea.pdf"), width=14, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
}
