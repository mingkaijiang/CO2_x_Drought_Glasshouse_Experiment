make_whole_plant_hydraulic_conductance_plot <- function() {
 
    #Eucalyptus pilularis
    pilDF<-read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv",sep=",", header=TRUE)
    #Eucalyptus populnea
    popDF<-read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv",sep=",", header=TRUE)
    
    ## remove data points with unequal sample size
    #pilDF <- pilDF[pilDF$Day<7, ]
    #popDF <- popDF[popDF$Day<19, ]
    
    
    ### plotting DFs
    plotDF1 <- summaryBy(E_psi~Trt+Day, FUN=c(mean, se), 
                         data=pilDF, keep.names=T)
    
    plotDF2 <- summaryBy(E_psi~Trt+Day, FUN=c(mean, se), 
                         data=popDF, keep.names=T)
    
    ### calculating logs
    plotDF1$log_E_psi <- log(plotDF1$E_psi.mean)
    plotDF1$log_E_psi_pos <- log(plotDF1$E_psi.mean + plotDF1$E_psi.se)
    plotDF1$log_E_psi_neg <- log(plotDF1$E_psi.mean - plotDF1$E_psi.se)
    
    plotDF2$log_E_psi <- log(plotDF2$E_psi.mean)
    plotDF2$log_E_psi_pos <- log(plotDF2$E_psi.mean + plotDF2$E_psi.se)
    plotDF2$log_E_psi_neg <- log(plotDF2$E_psi.mean - plotDF2$E_psi.se)
    
    
    #### plotting
    p1 <- ggplot(plotDF1, aes(x=Day, y=E_psi.mean, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=E_psi.mean-E_psi.se, ymax=E_psi.mean+E_psi.se),
                      width=0.2)+
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
        ylab(expression(paste("E / "*psi[pd] * "-" * psi[md] * 
                                  " (l " * d^-1 * " " * MPa^-1 * ")")))+
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
        ylim(0, 15)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    

    p2 <- ggplot(plotDF2, aes(x=Day, y=E_psi.mean, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=E_psi.mean-E_psi.se, ymax=E_psi.mean+E_psi.se),
                      width=0.6)+
        geom_line(aes(col=Trt, lty=Trt))+
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
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("E / "*psi[pd] * "-" * psi[md] * 
                                  " (l " * d^-1 * " " * MPa^-1 * ")")))+
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
        ylim(0, 8)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"), 
                                ncol=2, align="vh", axis = "l",
                                label_x=0.14, label_y=0.9)
    
    
    pdf(paste0(outdir, "F7.whole_plant_hydraulics_plot.pdf"), width=8, height=4)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    #### plotting
    p1 <- ggplot(plotDF1, aes(x=Day, y=log_E_psi, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=log_E_psi_neg, ymax=log_E_psi_pos),
                      width=0.2)+
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
        ylab(expression(paste("log [E / ("*psi[pd] * "-" * psi[md] * 
                                  ")] (l " * d^-1 * " " * MPa^-1 * ")")))+
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
        ylim(-3, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10)) +
        geom_segment(aes(x=7, y=-1.7, xend=9, yend=-1.7), col="blue3") +  # A-ND
        geom_segment(aes(x=7, y=-1.7, xend=7, yend=-1.5), col="blue3")+  # A-ND
        geom_segment(aes(x=9, y=-1.7, xend=9, yend=-1.5), col="blue3")+  # A-ND
        geom_segment(aes(x=7, y=-2.0, xend=9, yend=-2.0), col="red2") + # E-ND
        geom_segment(aes(x=7, y=-1.8, xend=7, yend=-2.0), col="red2")+  # E-ND
        geom_segment(aes(x=9, y=-1.8, xend=9, yend=-2.0), col="red2")   # E-ND
    
    
    p2 <- ggplot(plotDF2, aes(x=Day, y=log_E_psi, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=log_E_psi_neg, ymax=log_E_psi_pos),
                      width=0.6)+
        geom_line(aes(col=Trt, lty=Trt))+
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
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("log E / "*psi[pd] * "-" * psi[md] * 
                                  " (l " * d^-1 * " " * MPa^-1 * ")")))+
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
        ylim(-3, 3)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                       fill = c("white", "blue3", "white", "red2"),
                                                       col = c("blue3", "blue3", "red2", "red2"),
                                                       linetype = c("dotted", "solid", "dotted", "solid"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))+
        geom_segment(aes(x=19, y=1.0, xend=37, yend=1.0), col="blue3")+   #A-ND
        geom_segment(aes(x=19, y=1.0, xend=19, yend=0.8), col="blue3")+   #A-ND
        geom_segment(aes(x=37, y=1.0, xend=37, yend=0.8), col="blue3")+   #A-ND
        geom_segment(aes(x=19, y=1.3, xend=37, yend=1.3), col="red2")+   #E-ND
        geom_segment(aes(x=19, y=1.3, xend=19, yend=1.1), col="red2")+   #E-ND
        geom_segment(aes(x=37, y=1.3, xend=37, yend=1.1), col="red2")+   #E-ND
        geom_segment(aes(x=31, y=0.4, xend=37, yend=0.4), col="blue3", lty="dotted") +   #A-D
        geom_segment(aes(x=31, y=0.4, xend=31, yend=0.2), col="blue3", lty="dotted")+   #A-D
        geom_segment(aes(x=37, y=0.4, xend=37, yend=0.2), col="blue3", lty="dotted")+   #A-D
        geom_segment(aes(x=22, y=0.7, xend=37, yend=0.7), col="red2", lty="dotted")+   #E-D
        geom_segment(aes(x=22, y=0.7, xend=22, yend=0.5), col="red2", lty="dotted")+   #E-D
        geom_segment(aes(x=37, y=0.7, xend=37, yend=0.5), col="red2", lty="dotted") 
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"), 
                                ncol=2, align="h", axis = "l",
                                label_x=c(0.16, 0.1), label_y=0.9)
    
    
    pdf(paste0(outdir, "F7.whole_plant_hydraulics_log_plot.pdf"), width=8, height=4)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    

}

