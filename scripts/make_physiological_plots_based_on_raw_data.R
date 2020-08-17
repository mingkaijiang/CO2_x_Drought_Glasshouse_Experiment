make_physiological_plots_based_on_raw_data <- function() {
        
    ### read in raw data
    myDF1 <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv")
    myDF2 <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv")
    
    myDF1 <- subset(myDF1, Day <= 6)
    myDF2 <- subset(myDF2, Day <= 16)
    
    ### set class
    myDF1$Glasshouse <- as.character(myDF1$Glasshouse)
    myDF2$Glasshouse <- as.character(myDF2$Glasshouse)
    
    myDF1$CO2 <- as.character(myDF1$CO2)
    myDF2$CO2 <- as.character(myDF2$CO2)
    
    myDF1$H2O <- as.character(myDF1$H2O)
    myDF2$H2O <- as.character(myDF2$H2O)
    
    ### calculations
    myDF1$Adaily <- with(myDF1, (Aearly+Alate)/2)
    myDF2$Adaily <- with(myDF2, (Aearly+Alate)/2)
    
    myDF1$gsdaily <- with(myDF1, (gsearly+gslate)/2)
    myDF2$gsdaily <- with(myDF2, (gsearly+gslate)/2)
    
    
    pilDF <- summaryBy(psiPD+psiMD+transp_plant+E_psi+SWC+Aearly+Alate+gsearly+gslate+Ciearly+Cilate+Adaily+gsdaily~Trt+Day, 
                       FUN=c(mean, se), data=myDF1, keep.names=T)
    
    
    popDF <- summaryBy(psiPD+psiMD+transp_plant+E_psi+SWC+Aearly+Alate+gsearly+gslate+Ciearly+Cilate+Adaily+gsdaily~Trt+Day, 
                       FUN=c(mean, se), data=myDF2, keep.names=T)
    
    
    
    ################################### Plotting ######################################
    p1 <- ggplot(pilDF, aes(x=Day, y=Adaily.mean, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Adaily.mean-Adaily.se, ymax=Adaily.mean+Adaily.se),
                      width=0.2)+
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_line(aes(col=Trt, lty=Trt))+
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
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[sat]*" (" * mu *"mol " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("blue3", "blue3", "red2", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "blue3", "white", "red2"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "solid", "dotted", "solid"))+
        scale_shape_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,21,21))+
        ggtitle("E. pilularis")+
        ylim(0, 30)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 8),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p2 <- ggplot(popDF, aes(x=Day, y=Adaily.mean, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Adaily.mean-Adaily.se, ymax=Adaily.mean+Adaily.se),
                      width=0.6)+
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_line(aes(col=Trt, lty=Trt))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[sat]*" (" * mu *"mol " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("blue3", "blue3", "red2", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "blue3", "white", "red2"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "solid", "dotted", "solid"))+
        scale_shape_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,21,21))+
        ggtitle("E. populnea")+
        ylim(0, 30)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 20),
                           breaks=c(0, 4, 8, 12, 16, 20))
    
    
    p3 <- ggplot(pilDF, aes(x=Day, y=gsDaily.mean, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=gsDaily.mean-gsDaily.se, ymax=gsDaily.mean+gsDaily.se),
                      width=0.2)+
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_line(aes(col=Trt, lty=Trt))+
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
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(g[s]*" (" * mu *"mol " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("blue3", "blue3", "red2", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "blue3", "white", "red2"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "solid", "dotted", "solid"))+
        scale_shape_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,21,21))+
        ylim(0, 0.4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 8),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    
    p4 <- ggplot(popDF, aes(x=Day, y=gsDaily.mean, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=gsDaily.mean-gsDaily.se, ymax=gsDaily.mean+gsDaily.se),
                      width=0.6)+
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_line(aes(col=Trt, lty=Trt))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(g[s]*" (" * mu *"mol " * m^-2 * " " * s^-1 * ")")))+
        scale_color_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("blue3", "blue3", "red2", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "blue3", "white", "red2"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "solid", "dotted", "solid"))+
        scale_shape_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,21,21))+
        ylim(0, 0.4)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 20),
                           breaks=c(0, 4, 8, 12, 16, 20))
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4,
                                labels=c("(a)", "(b)", "(c)", "(d)"), 
                                ncol=2, align="vh", axis = "l",
                                label_x=0.84, label_y=0.86)
    
    
    pdf(paste0(outdir, "F8.Gas_exchange_daily_based_on_raw_data.pdf"), width=8, height=8)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    
}

