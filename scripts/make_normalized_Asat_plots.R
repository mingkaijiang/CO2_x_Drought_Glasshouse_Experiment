make_normalized_Asat_plots <- function() {
    
    ### read input
    pilDF<-read.csv("data/glasshouse2/Pilularis_Phys.csv",sep=",", header=TRUE)
    popDF<-read.csv("data/glasshouse2/Populnea_Phys.csv",sep=",", header=TRUE)
    
    ### baseline 1 day Asat
    bDF1 <- subset(pilDF, Day == 1)
    bDF2 <- subset(popDF, Day == 1)
    
    ### calculate normalized responses
    # bDF1
    for (i in unique(bDF1$Trt)) {
        pilDF$AdailyNORM[pilDF$Trt==i] <- pilDF$Adaily[pilDF$Trt==i] / bDF1$Adaily[bDF1$Trt==i]
    }
    
    for (i in unique(bDF1$Trt)) {
        pilDF$AearlyNORM[pilDF$Trt==i] <- pilDF$Aearly[pilDF$Trt==i] / bDF1$Aearly[bDF1$Trt==i]
    }
    
    for (i in unique(bDF1$Trt)) {
        pilDF$AlateNORM[pilDF$Trt==i] <- pilDF$Alate[pilDF$Trt==i] / bDF1$Alate[bDF1$Trt==i]
    }
    
    # bDF2
    for (i in unique(bDF2$Trt)) {
        popDF$AdailyNORM[popDF$Trt==i] <- popDF$Adaily[popDF$Trt==i] / bDF2$Adaily[bDF2$Trt==i]
    }
    
    for (i in unique(bDF2$Trt)) {
        popDF$AearlyNORM[popDF$Trt==i] <- popDF$Aearly[popDF$Trt==i] / bDF2$Aearly[bDF2$Trt==i]
    }
    
    for (i in unique(bDF2$Trt)) {
        popDF$AlateNORM[popDF$Trt==i] <- popDF$Alate[popDF$Trt==i] / bDF2$Alate[bDF2$Trt==i]
    }
    
    
    #### plotting
    p1 <- ggplot(pilDF, aes(x=Day, y=AdailyNORM, group=Trt)) +
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
        ylab("Daily")+
        scale_color_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(paste("Normalized " * A[sat])),
                          limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name=expression(paste("Normalized " * A[sat])),
                              limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ggtitle("E. pilularis")+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p3 <- ggplot(pilDF, aes(x=Day, y=AearlyNORM, group=Trt)) +
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
        ylab("Morning")+
        scale_color_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(paste("Normalized " * A[sat])),
                          limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name=expression(paste("Normalized " * A[sat])),
                              limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p5 <- ggplot(pilDF, aes(x=Day, y=AlateNORM, group=Trt)) +
        geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
        geom_line(aes(col=Trt, lty=Trt))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
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
        ylab("Midday")+
        scale_color_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(paste("Normalized " * A[sat])),
                          limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name=expression(paste("Normalized " * A[sat])),
                              limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 10),
                           breaks=c(0, 2, 4, 6, 8, 10))
    
    
    p2 <- ggplot(popDF, aes(x=Day, y=AdailyNORM, group=Trt)) +
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
        ylab("Daily")+
        scale_color_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(paste("Normalized " * A[sat])),
                          limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name=expression(paste("Normalized " * A[sat])),
                              limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ggtitle("E. populnea")+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p4 <- ggplot(popDF, aes(x=Day, y=AearlyNORM, group=Trt)) +
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
        ylab("Morning")+
        scale_color_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(paste("Normalized " * A[sat])),
                          limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name=expression(paste("Normalized " * A[sat])),
                              limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ylim(0, 1)+
        xlab("Day")+
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 22,22),
                                                       fill = c("white", "white", "red2", "blue3"),
                                                       col = c("red2", "blue3", "red2", "blue3"),
                                                       linetype = c("dotted", "dotted", "solid", "solid"))))+
        scale_x_continuous(limits=c(0, 40),
                           breaks=c(0, 5, 10, 20, 30, 40))
    
    p6 <- ggplot(popDF, aes(x=Day, y=AlateNORM, group=Trt)) +
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
        ylab("Midday")+
        scale_color_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c("red2", "blue3", "red2", "blue3"),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(paste("Normalized " * A[sat])),
                          limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                          labels=c(expression(paste(aC[a]*" - D")), 
                                   expression(paste(aC[a]*" - W")),
                                   expression(paste(eC[a]*" - D")),
                                   expression(paste(eC[a]*" - W"))),
                          values=c("white", "white", "red2", "blue3"),
                          guide=guide_legend(nrow=1))+
        scale_linetype_manual(name=expression(paste("Normalized " * A[sat])),
                              limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                              labels=c(expression(paste(aC[a]*" - D")), 
                                       expression(paste(aC[a]*" - W")),
                                       expression(paste(eC[a]*" - D")),
                                       expression(paste(eC[a]*" - W"))),
                              values=c("dotted", "dotted", "solid", "solid"))+
        scale_shape_manual(name=expression(paste("Normalized " * A[sat])),
                           limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                           labels=c(expression(paste(aC[a]*" - D")), 
                                    expression(paste(aC[a]*" - W")),
                                    expression(paste(eC[a]*" - D")),
                                    expression(paste(eC[a]*" - W"))),
                           values=c(21,21,22,22))+
        ylim(0, 1)+
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
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, 
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
                                ncol=2, align="h", axis = "l",
                                rel_widths=c(1,0.9),
                                label_x=0.85, label_y=0.85)
    
    
    pdf("output/F9.normalized_Asat.pdf", width=8, height=8)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
}