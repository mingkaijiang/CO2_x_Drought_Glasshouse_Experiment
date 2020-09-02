make_predawn_water_potential_with_swc_plots <- function() {
   
   #Reconfigured to show CI - DISPLAYING MEANS AND SE FOR PRE-DAWN LWP, RATHER THAN THE FULL DATA SET
   PILPhysGraph<-read.csv("data/glasshouse2/Pilularis_Phys_Processed.csv",sep=",", header=TRUE)
   POPPhysGraph<-read.csv("data/glasshouse2/Populnea_Phys_Processed.csv",sep=",", header=TRUE)
   
   ### revise errobar values
   PILPhysGraph$UpsiPD <- PILPhysGraph$psiPD + PILPhysGraph$psiPDSE
   PILPhysGraph$LpsiPD <- PILPhysGraph$psiPD - PILPhysGraph$psiPDSE
   
   PILPhysGraph$UpsiMD <- PILPhysGraph$psiMD + PILPhysGraph$psiMDSE
   PILPhysGraph$LpsiMD <- PILPhysGraph$psiMD - PILPhysGraph$psiMDSE
   
   POPPhysGraph$UpsiPD <- POPPhysGraph$psiPD + POPPhysGraph$psiPDSE
   POPPhysGraph$LpsiPD <- POPPhysGraph$psiPD - POPPhysGraph$psiPDSE
   
   POPPhysGraph$UpsiMD <- POPPhysGraph$psiMD + POPPhysGraph$psiMDSE
   POPPhysGraph$LpsiMD <- POPPhysGraph$psiMD - POPPhysGraph$psiMDSE
   
   
   ## remove data points with unequal sample size
   #n1 <- min(unique(PILPhysGraph[PILPhysGraph$n.1<6,]$Day))
   #n2 <- min(unique(POPPhysGraph[POPPhysGraph$n.1<6,]$Day))
   
   pilDF <- PILPhysGraph#[PILPhysGraph$Day<n1, ]
   popDF <- POPPhysGraph#[POPPhysGraph$Day<n2, ]
   
   
   
   
   
   
   
   ################################### Plotting ######################################
   #### plotting
   p1 <- ggplot(pilDF, aes(x=SWC, y=psiPD, group=Trt)) +
      geom_errorbar(aes(col=Trt, x=SWC, 
                        ymin=psiPD-psiPDSE, ymax=psiPD+psiPDSE), width=0.01)+
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
      xlab(expression(paste("s.w.c. ("*m^3*" "*m^-3*")")))+
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
      ylim(-4, 0)+
      ylab(expression(bold(psi[pd]~~(MPa))))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                     fill = c("white", "blue3", "white", "red2"),
                                                     col = c("blue3", "blue3", "red2", "red2"),
                                                     linetype = c("dotted", "solid", "dotted", "solid"))))+
      scale_x_continuous(limits=c(0, 0.35),
                         breaks=c(0, 0.1, 0.2, 0.3))
   
   
   p2 <- ggplot(popDF, aes(x=SWC, y=psiPD, group=Trt)) +
      geom_errorbar(aes(col=Trt, x=SWC, 
                        ymin=psiPD-psiPDSE, ymax=psiPD+psiPDSE), width=0.01)+
      geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
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
      xlab(expression(paste("s.w.c. ("*m^3*" "*m^-3*")")))+
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
      ylim(-4, 0)+
      ylab(expression(bold(psi[pd]~~(MPa))))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                     fill = c("white", "blue3", "white", "red2"),
                                                     col = c("blue3", "blue3", "red2", "red2"),
                                                     linetype = c("dotted", "solid", "dotted", "solid"))))+
      scale_x_continuous(limits=c(0, 0.35),
                         breaks=c(0, 0.1, 0.2, 0.3))
   
   
   
   p3 <- ggplot(pilDF, aes(x=SWC, y=psiMD, group=Trt)) +
      geom_errorbar(aes(col=Trt, x=SWC, 
                        ymin=psiMD-psiPDSE, ymax=psiMD+psiMDSE), width=0.01)+
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
      xlab(expression(paste("s.w.c. ("*m^3*" "*m^-3*")")))+
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
      #ggtitle("E. pilularis")+
      ylim(-4, 0)+
      ylab(expression(bold(psi[md]~~(MPa))))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                     fill = c("white", "blue3", "white", "red2"),
                                                     col = c("blue3", "blue3", "red2", "red2"),
                                                     linetype = c("dotted", "solid", "dotted", "solid"))))+
      scale_x_continuous(limits=c(0, 0.35),
                         breaks=c(0, 0.1, 0.2, 0.3))
   
   
   p4 <- ggplot(popDF, aes(x=SWC, y=psiMD, group=Trt)) +
      geom_errorbar(aes(col=Trt, x=SWC, 
                        ymin=psiMD-psiMDSE, ymax=psiMD+psiMDSE), width=0.01)+
      geom_point(aes(col=Trt, fill=Trt, pch=Trt), size=2)+
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
      xlab(expression(paste("s.w.c. ("*m^3*" "*m^-3*")")))+
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
      #ggtitle("E. populnea")+
      ylim(-4, 0)+
      ylab(expression(bold(psi[md]~~(MPa))))+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                     fill = c("white", "blue3", "white", "red2"),
                                                     col = c("blue3", "blue3", "red2", "red2"),
                                                     linetype = c("dotted", "solid", "dotted", "solid"))))+
      scale_x_continuous(limits=c(0, 0.35),
                         breaks=c(0, 0.1, 0.2, 0.3))
   
   
   ### output
   combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'vertical',
                                            legend.box.just = 'left'))
   
   
   combined_plots <- plot_grid(p1, p2, p3, p4,
                               labels=c("(a)", "(b)", "(c)", "(d)"), 
                               ncol=2, align="vh", axis = "l",
                               label_x=0.16, label_y=0.9)
   
   
   pdf(paste0(outdir, "F6.predawn_leaf_water_potential_vs_swc_with_se.pdf"), width=8, height=8)
   plot_grid(combined_plots, combined_legend, 
             ncol=1, rel_heights=c(1, 0.1))
   dev.off() 
   
   
   
   
   
}


