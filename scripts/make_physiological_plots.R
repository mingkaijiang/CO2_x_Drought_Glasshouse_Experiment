make_physiological_plots <- function() {
        
    #FIGURES (Eucalyptus pilularis)
    pilDF<-read.csv("data/glasshouse2/Pilularis_Phys_Processed.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Populnea_Phys_Processed.csv",sep=",", header=TRUE)
    
    
    
    
    ################################### Plotting ######################################
    p1 <- ggplot(pilDF, aes(x=Day, y=Adaily, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Adaily-AdailySE, ymax=Adaily+AdailySE),
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
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))+
        geom_segment(aes(x=7, y=25, xend=9, yend=25), col="blue3") +  # A-ND
        geom_segment(aes(x=7, y=23, xend=7, yend=25), col="blue3")+  # A-ND
        geom_segment(aes(x=9, y=23, xend=9, yend=25), col="blue3")+  # A-ND
        geom_segment(aes(x=7, y=22, xend=9, yend=22), col="red2") + # E-ND
        geom_segment(aes(x=7, y=20, xend=7, yend=22), col="red2")+  # E-ND
        geom_segment(aes(x=9, y=20, xend=9, yend=22), col="red2")   # E-ND
    
    
    p2 <- ggplot(popDF, aes(x=Day, y=Adaily, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=Adaily-AdailySE, ymax=Adaily+AdailySE),
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
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))+
        geom_segment(aes(x=19, y=26, xend=37, yend=26), col="blue3")+   #A-ND
        geom_segment(aes(x=19, y=26, xend=19, yend=24), col="blue3")+   #A-ND
        geom_segment(aes(x=37, y=26, xend=37, yend=24), col="blue3")+   #A-ND
        geom_segment(aes(x=19, y=29, xend=37, yend=29), col="red2")+   #E-ND
        geom_segment(aes(x=19, y=27, xend=19, yend=29), col="red2")+   #E-ND
        geom_segment(aes(x=37, y=27, xend=37, yend=29), col="red2")+   #E-ND
        geom_segment(aes(x=31, y=20, xend=37, yend=20), col="blue3", lty="dotted") +   #A-D
        geom_segment(aes(x=31, y=20, xend=31, yend=18), col="blue3", lty="dotted")+   #A-D
        geom_segment(aes(x=37, y=20, xend=37, yend=18), col="blue3", lty="dotted")+   #A-D
        geom_segment(aes(x=22, y=23, xend=37, yend=23), col="red2", lty="dotted")+   #E-D
        geom_segment(aes(x=22, y=23, xend=22, yend=21), col="red2", lty="dotted")+   #E-D
        geom_segment(aes(x=37, y=23, xend=37, yend=21), col="red2", lty="dotted")    #E-D
    
    
    p3 <- ggplot(pilDF, aes(x=Day, y=gsdaily, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=gsdaily-gsdailySE, ymax=gsdaily+gsdailySE),
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
        ylab(expression(paste(g[s]*" (mol " * m^-2 * " " * s^-1 * ")")))+
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
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    
    p4 <- ggplot(popDF, aes(x=Day, y=gsdaily, group=Trt)) +
        geom_errorbar(aes(col=Trt, x=Day, 
                          ymin=gsdaily-gsdailySE, ymax=gsdaily+gsdailySE),
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
        ylab(expression(paste(g[s]*" (mol " * m^-2 * " " * s^-1 * ")")))+
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
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4,
                                labels=c("(a)", "(b)", "(c)", "(d)"), 
                                ncol=2, align="vh", axis = "l",
                                label_x=0.2, label_y=0.86)
    
    
    pdf(paste0(outdir, "F8.Gas_exchange_daily.pdf"), width=8, height=8)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    
}

