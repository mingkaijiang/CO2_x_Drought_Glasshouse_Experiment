make_log_CO2_ratios_of_A_and_gs_plots <- function() {
    
    ### read input
    pilDF<-read.csv("data/glasshouse2/Pilularis_Phys.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Populnea_Phys.csv",sep=",", header=TRUE)
    
    ### day list
    d1 <- unique(pilDF$Day)
    d2 <- unique(popDF$Day)
    
    ### water treatment
    w <- c("D", "ND")
    
    ### plot DF
    plotDF1 <- data.frame(rep(w, length(d1)), 
                          rep(d1, each=2), NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA)
    
    plotDF2 <- data.frame(rep(w, length(d2)), 
                          rep(d2, each=2), NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA)
    colnames(plotDF1) <- colnames(plotDF2) <- c("Trt", "Day",
                                                "amb_Adaily", "ele_Adaily",
                                                "amb_Aearly", "ele_Aearly",
                                                "amb_Alate", "ele_Alate",
                                                "amb_GSdaily", "ele_GSdaily",
                                                "amb_GSearly", "ele_GSearly",
                                                "amb_GSlate", "ele_GSlate")
    
    for (i in d1) {
        # A
        plotDF1$amb_Adaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_Aearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_Alate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_Adaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_Aearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_Alate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_Adaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_Aearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_Alate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_Adaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Adaily[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_Aearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Aearly[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_Alate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$Alate[pilDF$Trt=="PILEND"&pilDF$Day==i]
        
        ### gs
        plotDF1$amb_GSdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILAD"&pilDF$Day==i]
        plotDF1$amb_GSlate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILAD"&pilDF$Day==i]
        
        plotDF1$ele_GSdaily[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSearly[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILED"&pilDF$Day==i]
        plotDF1$ele_GSlate[plotDF1$Trt=="D"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILED"&pilDF$Day==i]
        
        plotDF1$amb_GSdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILAND"&pilDF$Day==i]
        plotDF1$amb_GSlate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILAND"&pilDF$Day==i]
        
        plotDF1$ele_GSdaily[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsDaily[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSearly[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gsearly[pilDF$Trt=="PILEND"&pilDF$Day==i]
        plotDF1$ele_GSlate[plotDF1$Trt=="ND"&plotDF1$Day==i] <- pilDF$gslate[pilDF$Trt=="PILEND"&pilDF$Day==i]
    }
    
    
    
    for (i in d2) {
        # A
        plotDF2$amb_Adaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Adaily[popDF$Trt=="POPAD"&popDF$Day==i]),
                                                                      NA)
        plotDF2$amb_Aearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Aearly[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                      NA)
        plotDF2$amb_Alate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                     as.numeric(popDF$Alate[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                     NA)
        
        plotDF2$ele_Adaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Adaily[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                      NA)
        plotDF2$ele_Aearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Aearly[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                      NA)
        plotDF2$ele_Alate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                     as.numeric(popDF$Alate[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                     NA)
        
        plotDF2$amb_Adaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Adaily[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_Aearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Aearly[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_Alate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Alate[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                      NA)
        
        plotDF2$ele_Adaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Adaily[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Adaily[popDF$Trt=="POPEND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_Aearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Aearly[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$Aearly[popDF$Trt=="POPEND"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_Alate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$Alate[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$Alate[popDF$Trt=="POPEND"&popDF$Day==i]), 
                                                                      NA)
        ### gs
        plotDF2$amb_GSdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsDaily[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_GSearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsearly[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$amb_GSlate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPAD"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$gslate[popDF$Trt=="POPAD"&popDF$Day==i]), 
                                                                      NA)
        
        plotDF2$ele_GSdaily[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsDaily[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_GSearly[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gsearly[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                       NA)
        plotDF2$ele_GSlate[plotDF2$Trt=="D"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPED"&popDF$Day==i])==1,
                                                                      as.numeric(popDF$gslate[popDF$Trt=="POPED"&popDF$Day==i]), 
                                                                      NA)
        
        plotDF2$amb_GSdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPAND"&popDF$Day==i])==1, 
                                                                        as.numeric(popDF$gsDaily[popDF$Trt=="POPAND"&popDF$Day==i]), 
                                                                        NA)
        plotDF2$amb_GSearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                        as.numeric(popDF$gsearly[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                        NA)
        plotDF2$amb_GSlate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPAND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gslate[popDF$Trt=="POPAND"&popDF$Day==i]),
                                                                       NA)
        
        plotDF2$ele_GSdaily[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsDaily[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                        as.numeric(popDF$gsDaily[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                        NA)
        plotDF2$ele_GSearly[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gsearly[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                        as.numeric(popDF$gsearly[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                        NA)
        plotDF2$ele_GSlate[plotDF2$Trt=="ND"&plotDF2$Day==i] <- ifelse(length(popDF$gslate[popDF$Trt=="POPEND"&popDF$Day==i])==1,
                                                                       as.numeric(popDF$gslate[popDF$Trt=="POPEND"&popDF$Day==i]),
                                                                       NA)
    }
    
    ### ignore NAs
    plotDF1 <- plotDF1[complete.cases(plotDF1),]
    plotDF2 <- plotDF2[complete.cases(plotDF2),]
    plotDF2 <- as.data.frame(sapply(plotDF2, as.numeric))
    plotDF2$Trt <- gsub(1, "D", plotDF2$Trt)
    plotDF2$Trt <- gsub(2, "ND", plotDF2$Trt)
    plotDF2$Trt <- as.character(plotDF2$Trt)
    
    ### Calculate CO2 signal
    plotDF1$CO2_Adaily <- plotDF1$ele_Adaily / plotDF1$amb_Adaily
    plotDF1$CO2_Aearly <- plotDF1$ele_Aearly / plotDF1$amb_Aearly
    plotDF1$CO2_Alate <- plotDF1$ele_Alate / plotDF1$amb_Alate
    
    plotDF2$CO2_Adaily <- plotDF2$ele_Adaily / plotDF2$amb_Adaily
    plotDF2$CO2_Aearly <- plotDF2$ele_Aearly / plotDF2$amb_Aearly
    plotDF2$CO2_Alate <- plotDF2$ele_Alate / plotDF2$amb_Alate
    
    plotDF1$CO2_GSdaily <- plotDF1$ele_GSdaily / plotDF1$amb_GSdaily
    plotDF1$CO2_GSearly <- plotDF1$ele_GSearly / plotDF1$amb_GSearly
    plotDF1$CO2_GSlate <- plotDF1$ele_GSlate / plotDF1$amb_GSlate
    
    plotDF2$CO2_GSdaily <- plotDF2$ele_GSdaily / plotDF2$amb_GSdaily
    plotDF2$CO2_GSearly <- plotDF2$ele_GSearly / plotDF2$amb_GSearly
    plotDF2$CO2_GSlate <- plotDF2$ele_GSlate / plotDF2$amb_GSlate
    
    ### calculate the log
    plotDF1$log_CO2_Adaily <- log(plotDF1$CO2_Adaily)
    plotDF1$log_CO2_Aearly <- log(plotDF1$CO2_Aearly)
    plotDF1$log_CO2_Alate <- log(plotDF1$CO2_Alate)
    
    plotDF1$log_CO2_GSdaily <- log(plotDF1$CO2_GSdaily)
    plotDF1$log_CO2_GSearly <- log(plotDF1$CO2_GSearly)
    plotDF1$log_CO2_GSlate <- log(plotDF1$CO2_GSlate)
    
    plotDF2$log_CO2_Adaily <- log(plotDF2$CO2_Adaily)
    plotDF2$log_CO2_Aearly <- log(plotDF2$CO2_Aearly)
    plotDF2$log_CO2_Alate <- log(plotDF2$CO2_Alate)
    
    plotDF2$log_CO2_GSdaily <- log(plotDF2$CO2_GSdaily)
    plotDF2$log_CO2_GSearly <- log(plotDF2$CO2_GSearly)
    plotDF2$log_CO2_GSlate <- log(plotDF2$CO2_GSlate)
    
    ### plotting
    p1 <- ggplot(plotDF1, aes(x=Day, y=log_CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p2 <- ggplot(plotDF1, aes(x=Day, y=log_CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p3 <- ggplot(plotDF1, aes(x=Day, y=log_CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p4 <- ggplot(plotDF1, aes(x=Day, y=log_CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p5 <- ggplot(plotDF1, aes(x=Day, y=log_CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    p6 <- ggplot(plotDF1, aes(x=Day, y=log_CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, 
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
                                ncol=3, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf("output/F10.1.log_CO2_ratio_pilularis.pdf", width=14, height=8)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    
    
    
    ### plotting
    p1 <- ggplot(plotDF2, aes(x=Day, y=log_CO2_Adaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Daily")+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p2 <- ggplot(plotDF2, aes(x=Day, y=log_CO2_Aearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Morning")+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p3 <- ggplot(plotDF2, aes(x=Day, y=log_CO2_Alate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylab(expression(paste(CO[2]* " ratio " * A[sat])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ggtitle("Midday")+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    
    p4 <- ggplot(plotDF2, aes(x=Day, y=log_CO2_GSdaily, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
        geom_line(aes(col=Trt))+
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
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p5 <- ggplot(plotDF2, aes(x=Day, y=log_CO2_GSearly, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p6 <- ggplot(plotDF2, aes(x=Day, y=log_CO2_GSlate, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt), pch=21, size=2)+
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
        ylab(expression(paste(CO[2]* " ratio " * g[s])))+
        scale_color_manual(name="",
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c("red3", "blue2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("D", "ND"),
                          labels=c("Droughted", "Well-watered"),
                          values=c("red3", "blue2"),
                          guide=guide_legend(nrow=1))+
        ylim(-2, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21),
                                                       fill = c("red3", "blue2"),
                                                       col = c("red3", "blue2"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, 
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
                                ncol=3, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf("output/F10.2.log_CO2_ratio_populnea.pdf", width=14, height=8)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
}