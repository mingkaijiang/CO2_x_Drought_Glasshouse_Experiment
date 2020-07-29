make_predawn_water_potential_with_swc_plots <- function() {
   
   #Reconfigured to show CI - DISPLAYING MEANS AND SE FOR PRE-DAWN LWP, RATHER THAN THE FULL DATA SET
   PILPhysGraph<-read.csv("data/glasshouse2/Pilularis_Phys.csv",sep=",", header=TRUE)
   POPPhysGraph<-read.csv("data/glasshouse2/Populnea_Phys.csv",sep=",", header=TRUE)
   
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
   n1 <- min(unique(PILPhysGraph[PILPhysGraph$n.1<6,]$Day))
   n2 <- min(unique(POPPhysGraph[POPPhysGraph$n.1<6,]$Day))
   
   pilDF <- PILPhysGraph[PILPhysGraph$Day<n1, ]
   popDF <- POPPhysGraph[POPPhysGraph$Day<n2, ]
   
   
   
   
   
   
   
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
      ylim(-3, 0)+
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
      ylim(-3, 0)+
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
   
   
   
   ################################### Plotting ######################################
   #
   ## Eucalyptus pilularis & populnea (Pre-dawn leaf water potential (mean & SE) vs. Soil water content)
   #pdf(paste0(outdir, "F6.predawn_leaf_water_potential_vs_swc_with_se.pdf"), 
   #    width=8, height=6)
   ##A
   ##bottom,left,top,right
   #par(mfrow=c(1,2), omi=c(.65, 0.75, 0.75, .65)) 
   #par(mar=c(2, 1, 4, 0.5))
   #par(xaxs="i",yaxs="i")
   ##PIL pre-dawn leaf water potential vs. soil water content
   #par(las=1)
   #with(PILPhysGraph,plot(psiPD[Trt=="PILAD"]~SWC[Trt=="PILAD"],col="blue",pch=1,cex=1.25,cex.axis=1.25,xaxt="n",ylab="",
   #                       type="o", lty=2,ylim=range(-3,0.05),xlim=range(-0.025,1.025*max(SWC))))
   #with(PILPhysGraph,arrows(SWC[Trt=="PILAD"],
   #                         UpsiPD[Trt=="PILAD"], SWC[Trt=="PILAD"], LpsiPD[Trt=="PILAD"]
   #                         , length = .035, angle = 90, code = 3,col="blue"))
   #with(PILPhysGraph,points(psiPD[Trt=="PILAND"]~SWC[Trt=="PILAND"],col="blue",pch=19,cex=1.25,type="o", lty=1))
   #with(PILPhysGraph,arrows(SWC[Trt=="PILAND"],
   #                         UpsiPD[Trt=="PILAND"], SWC[Trt=="PILAND"], LpsiPD[Trt=="PILAND"]
   #                         , length = .035, angle = 90, code = 3,col="blue")) 
   #with(PILPhysGraph,points(psiPD[Trt=="PILED"]~SWC[Trt=="PILED"],col="red",pch=1,lty=2,cex=1.25,type="o"))
   #with(PILPhysGraph,arrows(SWC[Trt=="PILED"],
   #                         UpsiPD[Trt=="PILED"], SWC[Trt=="PILED"], LpsiPD[Trt=="PILED"]
   #                         , length = .035, angle = 90, code = 3,col="red")) 
   #with(PILPhysGraph,points(psiPD[Trt=="PILEND"]~SWC[Trt=="PILEND"],col="red",pch=19,cex=1.25,type="o"))
   #with(PILPhysGraph,arrows(SWC[Trt=="PILEND"],
   #                         UpsiPD[Trt=="PILEND"], SWC[Trt=="PILEND"], LpsiPD[Trt=="PILEND"]
   #                         , length = .035, angle = 90, code = 3,col="red")) 
   #par(las=3)
   #mtext(side = 2, text =expression(bold(psi[pd]~~(MPa))),
   #      font=2,cex=1.0, line = 3)	
   #par(las=1)
   #mtext(side=1,text=expression(bold(s.w.c.~(m^3~m^-3))),line=3,cex=1.25)
   #axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
   #title(main="E. pilularis",  font.main=4,cex.main=1.1,line=0.5)
   #legend("bottomleft",  expression(aC[a]~-~W,aC[a]~-~D,eC[a]~-~W,eC[a]~-~D),
   #       cex=1.025,bty="n",
   #       pch=c(19,1,19,1), col=c("blue","blue","red","red"))
   #text(x=0, y=-0.18, expression(bold("(a)")), cex=1.2)
   #
   ##POP pre-dawn leaf water potential vs. soil water content
   #par(mar=c(2, 0.5, 4, 1))
   #par(las=1)
   #with(POPPhysGraph,plot(psiPD[Trt=="POPAD"]~SWC[Trt=="POPAD"],col="blue",pch=1,cex=1.25,cex.axis=1.25,xaxt="n",ylab="",yaxt="n",
   #                       type="o",lty=2,ylim=range(-4.25,0.05),xlim=range(-0.025,1.025*max(SWC))))
   #with(POPPhysGraph,arrows(SWC[Trt=="POPAD"],
   #                         UpsiPD[Trt=="POPAD"], SWC[Trt=="POPAD"], LpsiPD[Trt=="POPAD"]
   #                         , length = .035, angle = 90, code = 3,col="blue"))
   #with(POPPhysGraph,points(psiPD[Trt=="POPAND"]~SWC[Trt=="POPAND"],col="blue",pch=19,cex=1.25,type="o", lty=1))
   #with(POPPhysGraph,arrows(SWC[Trt=="POPAND"],
   #                         UpsiPD[Trt=="POPAND"], SWC[Trt=="POPAND"], LpsiPD[Trt=="POPAND"]
   #                         , length = .035, angle = 90, code = 3,col="blue")) 
   #with(POPPhysGraph,points(psiPD[Trt=="POPED"]~SWC[Trt=="POPED"],col="red",pch=1,lty=2,cex=1.25,type="o"))
   #with(POPPhysGraph,arrows(SWC[Trt=="POPED"],
   #                         UpsiPD[Trt=="POPED"], SWC[Trt=="POPED"], LpsiPD[Trt=="POPED"]
   #                         , length = .035, angle = 90, code = 3,col="red")) 
   #with(POPPhysGraph,points(psiPD[Trt=="POPEND"]~SWC[Trt=="POPEND"],col="red",pch=19,cex=1.25,type="o"))
   #with(POPPhysGraph,arrows(SWC[Trt=="POPEND"],
   #                         UpsiPD[Trt=="POPEND"], SWC[Trt=="POPEND"], LpsiPD[Trt=="POPEND"]
   #                         , length = .035, angle = 90, code = 3,col="red"))
   #par(las=3)
   #mtext(side = 4, text =expression(bold(psi[pd]~~(MPa))),
   #      font=2,cex=1.0, line = 3)	
   #par(las=1)
   #axis(4,labels=TRUE,tck=-0.03,cex.axis=1.25)
   #par(las=1)
   #mtext(side=1,text=expression(bold(s.w.c.~(m^3~m^-3))),line=3,cex=1.25)
   #axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
   #title(main="E. populnea",  font.main=4,cex.main=1.1,line=0.5)
   #text(x=0, y=-0.25, expression(bold("(b)")), cex=1.2)
   #
   #dev.off()
   
}


