make_leaf_water_potential_plots_DT <- function() {
    
    ### E. pilularis
    pilDF<-read.csv("data/glasshouse2/Pilularis_Phys.csv",sep=",", header=TRUE)
    
    ### E. populnea
    popDF<-read.csv("data/glasshouse2/Populnea_Phys.csv",sep=",", header=TRUE)
    
    
    
    #### plotting
    p1 <- ggplot(pilDF, aes(x=Day, y=psiPD, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=psiPD-psiPDSE, ymax=psiPD+psiPDSE),
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
        ylab(expression(paste(psi[pd]*" (MPa)")))+
        scale_color_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ggtitle("E. pilularis")+
        ylim(-4, 0)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p2 <- ggplot(popDF, aes(x=Day, y=psiPD, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=psiPD-psiPDSE, ymax=psiPD+psiPDSE),
                      width=1.2)+
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
        ylab(expression(paste(psi[pd]*" (MPa)")))+
        scale_color_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ggtitle("E. populnea")+
        ylim(-4, 0)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    
    
    p3 <- ggplot(pilDF, aes(x=Day, y=psiMD, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=psiMD-psiMDSE, ymax=psiMD+psiMDSE),
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
        ylab(expression(paste(psi[md]*" (MPa)")))+
        scale_color_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name="",
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ylim(-4, 0)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    
    p4 <- ggplot(popDF, aes(x=Day, y=psiMD, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=psiMD-psiMDSE, ymax=psiMD+psiMDSE),
                      width=1.2)+
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
        ylab(expression(paste(psi[md]*" (MPa)")))+
        scale_color_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name="",
                          limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name="",
                              limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name="",
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ylim(-4, 0)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4,
                                labels=c("(a)", "(b)", "(c)", "(d)"), 
                                ncol=2, align="vh", axis = "l",
                                label_x=0.16, label_y=0.9)
    
    
    pdf(paste0(outdir, "F5.leaf_water_potentials.pdf"), width=8, height=8)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 

    
    
}