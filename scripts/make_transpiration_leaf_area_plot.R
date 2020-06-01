make_transpiration_leaf_area_plot <- function() {
   
   ### read in data
   pilDF<-read.csv("data/glasshouse2/PILtransp_leafarea.csv",sep=",", header=TRUE)
   popDF<-read.csv("data/glasshouse2/POPtransp_leafarea.csv",sep=",", header=TRUE)
   
   
   #### obtain summary DF
   sumDF1 <- summaryBy(leaf_area+transp_plant~Trt, FUN=c(mean,se),
                       data=pilDF, keep.names=T)
   
   sumDF2 <- summaryBy(leaf_area+transp_plant~Trt, FUN=c(mean,se),
                       data=popDF, keep.names=T)
   
   ############################# perform statistical tests ##############################
   ### perform linear mixed effect model statistics 
   mod1 <- lmer(leaf_area ~ CO2 * H2O + (1|Glasshouse), data=pilDF)
   outDF1 <- anova(mod1)
   write.csv(outDF1, paste0(outdir, "statistics_leaf_area_pilularis.csv"))
   
   mod2 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=pilDF)
   outDF2 <- anova(mod2)
   write.csv(outDF2, paste0(outdir, "statistics_transpiration_pilularis.csv"))
   
   
   ### perform linear mixed effect model statistics on LA
   mod1 <- lmer(leaf_area ~ CO2 * H2O + (1|Glasshouse), data=popDF)
   outDF1 <- anova(mod1)
   write.csv(outDF1, paste0(outdir, "statistics_leaf_area_populnea.csv"))
   
   mod2 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=popDF)
   outDF2 <- anova(mod2)
   write.csv(outDF2, paste0(outdir, "statistics_transpiration_populnea.csv"))
   
   
   ############################# Finish statistical tests ###############################
   
   
   
   ################################### Plotting ######################################
   p1 <- ggplot() +
      geom_point(pilDF, mapping=aes(x=leaf_area, y=transp_plant, group=Trt,
                                    col=Trt, fill=Trt, pch=Trt), size=2)+
      geom_errorbar(sumDF1, mapping=aes(col=Trt, x=leaf_area.mean, 
                        ymin=transp_plant.mean-transp_plant.se, 
                        ymax=transp_plant.mean+transp_plant.se),
                    width=0.2)+
      geom_errorbarh(sumDF1, mapping=aes(col=Trt, xmin=leaf_area.mean-leaf_area.se, 
                                         xmax=leaf_area.mean+leaf_area.se, 
                                        y=transp_plant.mean),
                    height=0.2)+
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
      ylab(expression(paste("Transpiration (l " * d^-1 * ")")))+
      xlab(expression(paste("Leaf area (" * m^2 * ")")))+
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
      ylim(0, 5)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                     fill = c("white", "blue3", "white", "red2"),
                                                     col = c("blue3", "blue3", "red2", "red2"),
                                                     linetype = c("dotted", "dotted", "solid", "solid"))))+
      scale_x_continuous(limits=c(0, 10),
                         breaks=c(0, 2, 4, 6, 8, 10))
   
   
   p2 <- ggplot() +
      geom_point(popDF, mapping=aes(x=leaf_area, y=transp_plant, group=Trt,
                                    col=Trt, fill=Trt, pch=Trt), size=2)+
      geom_errorbar(sumDF2, mapping=aes(col=Trt, x=leaf_area.mean, 
                                        ymin=transp_plant.mean-transp_plant.se, 
                                        ymax=transp_plant.mean+transp_plant.se),
                    width=0.04)+
      geom_errorbarh(sumDF2, mapping=aes(col=Trt, xmin=leaf_area.mean-leaf_area.se, 
                                         xmax=leaf_area.mean+leaf_area.se, 
                                         y=transp_plant.mean),
                     height=0.2)+
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
      ylab(expression(paste("Transpiration (l " * d^-1 * ")")))+
      xlab(expression(paste("Leaf area (" * m^2 * ")")))+
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
      ylim(0, 5)+
      guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21),
                                                     fill = c("white", "blue3", "white", "red2"),
                                                     col = c("blue3", "blue3", "red2", "red2"),
                                                     linetype = c("dotted", "dotted", "solid", "solid"))))+
      scale_x_continuous(limits=c(0, 2),
                         breaks=c(0, 0.5, 1, 1.5, 2.0))
   
   combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'vertical',
                                            legend.box.just = 'left'))
   
   
   combined_plots <- plot_grid(p1, p2, 
                               labels=c("(a)", "(b)"), 
                               ncol=2, align="vh", axis = "l",
                               label_x=0.16, label_y=0.9)
   
   
   pdf(paste0(outdir, "F4.transpiration_leaf_area.pdf"), width=8, height=4)
   plot_grid(combined_plots, combined_legend, 
             ncol=1, rel_heights=c(1, 0.1))
   dev.off() 
   
   
   
  
}

