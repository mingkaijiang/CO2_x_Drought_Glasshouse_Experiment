make_transpiration_leaf_area_plot <- function() {
   
   ### read in data
   pilDF<-read.csv("data/glasshouse2/PILtransp_leafarea.csv",sep=",", header=TRUE)
   popDF<-read.csv("data/glasshouse2/POPtransp_leafarea.csv",sep=",", header=TRUE)
   
   ### test statistics for day 1
   subDF1 <- subset(pilDF, Day == 1)
   subDF2 <- subset(popDF, Day == 1)
   
   mod1 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=subDF1)
   anov1 <- anova(mod1)
   anov1
   
   mod2 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=subDF2)
   anov2 <- anova(mod2)
   anov2
   
   #### obtain summary DF
   sumDF1 <- summaryBy(leaf_area+transp_plant~Trt, FUN=c(mean,se),
                       data=pilDF, keep.names=T)
   
   sumDF2 <- summaryBy(leaf_area+transp_plant~Trt, FUN=c(mean,se),
                       data=popDF, keep.names=T)
   
   
   ### calculate transpiration per leaf area
   pilDF$transp_leaf <- with(pilDF, transp_plant/leaf_area)
   popDF$transp_leaf <- with(popDF, transp_plant/leaf_area)
   
   barDF1 <- summaryBy(transp_leaf~Trt, FUN=c(mean,se), data=pilDF, keep.names=T)
   barDF2 <- summaryBy(transp_leaf~Trt, FUN=c(mean,se), data=popDF, keep.names=T)
   
   barDF1$brk[barDF1$Trt=="PILAD"] <- 3.2
   barDF1$brk[barDF1$Trt=="PILED"] <- 3.8
   
   barDF1$brk[barDF1$Trt=="PILAND"] <- 1.2
   barDF1$brk[barDF1$Trt=="PILEND"] <- 1.8
   
   barDF2$brk[barDF2$Trt=="POPAD"] <- 3.2
   barDF2$brk[barDF2$Trt=="POPED"] <- 3.8
   
   barDF2$brk[barDF2$Trt=="POPAND"] <- 1.2
   barDF2$brk[barDF2$Trt=="POPEND"] <- 1.8
   
   ### merge
   plotDF1 <- merge(barDF1, sumDF1, by="Trt")
   plotDF2 <- merge(barDF2, sumDF2, by="Trt")
   
   
   ############################# perform statistical tests ##############################
   ### perform linear mixed effect model statistics 
   mod1 <- lmer(leaf_area ~ CO2 * H2O + (1|Glasshouse), data=pilDF)
   outDF1 <- anova(mod1)
   #write.csv(outDF1, paste0(outdir, "statistics_leaf_area_pilularis.csv"))
   
   mod2 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=pilDF)
   outDF2 <- anova(mod2)
   #write.csv(outDF2, paste0(outdir, "statistics_transpiration_pilularis.csv"))
   
   mod3 <- lmer(transp_leaf ~ CO2 * H2O + (1|Glasshouse), data=pilDF)
   outDF3 <- anova(mod3)
   write.csv(outDF3, paste0(outdir, "statistics_transpiration_per_leaf_area_pilularis.csv"))
   
   
   ### perform linear mixed effect model statistics on LA
   mod1 <- lmer(leaf_area ~ CO2 * H2O + (1|Glasshouse), data=popDF)
   outDF1 <- anova(mod1)
   #write.csv(outDF1, paste0(outdir, "statistics_leaf_area_populnea.csv"))
   
   mod2 <- lmer(transp_plant ~ CO2 * H2O + (1|Glasshouse), data=popDF)
   outDF2 <- anova(mod2)
   #write.csv(outDF2, paste0(outdir, "statistics_transpiration_populnea.csv"))
   
   mod4 <- lmer(transp_leaf ~ CO2 * H2O + (1|Glasshouse), data=popDF)
   outDF4 <- anova(mod4)
   write.csv(outDF4, paste0(outdir, "statistics_transpiration_per_leaf_area_populnea.csv"))
   
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
                                                     linetype = c("dotted", "solid", "dotted", "solid"))))+
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
                                                     linetype = c("dotted", "solid", "dotted", "solid"))))+
      scale_x_continuous(limits=c(0, 2),
                         breaks=c(0, 0.5, 1, 1.5, 2.0))
   
   
   ### plotting seting
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
   
   
   ### plant total transpiration 
   p1 <- ggplot(data=plotDF1, 
                aes(brk, transp_plant.mean)) +
      geom_bar(stat = "identity", aes(fill=Trt), 
               position="dodge", col="black") +
      geom_errorbar(aes(x=brk, ymin=transp_plant.mean-transp_plant.se, 
                        ymax=transp_plant.mean+transp_plant.se), 
                    position=position_dodge(0.9), width=0.2) +
      ggtitle("E. pilularis")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("Plant transpiration (l " * " " * d^-1 * ")")))+
      scale_fill_manual(name="",
                        limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                        labels=c(expression(paste(aC[a]*" - D")), 
                                 expression(paste(aC[a]*" - W")),
                                 expression(paste(eC[a]*" - D")),
                                 expression(paste(eC[a]*" - W"))),
                        values=c(alpha("blue3", 0.2), "blue3", 
                                 alpha("red2", 0.2), "red2"),
                        guide=guide_legend(nrow=1))+
      xlab("")+
      scale_x_continuous(limits=c(0.5, 4.5),
                         breaks=c(1.5, 3.5),
                         labels=c("Well-watered","Droughted"))+
      ylim(0, 5)
   
   p2 <- ggplot(data=plotDF2, 
                aes(brk, transp_plant.mean)) +
      geom_bar(stat = "identity", aes(fill=Trt), 
               position="dodge", col="black") +
      geom_errorbar(aes(x=brk, ymin=transp_plant.mean-transp_plant.se, 
                        ymax=transp_plant.mean+transp_plant.se), 
                    position=position_dodge(0.9), width=0.2) +
      ggtitle("E. populnea")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("Plant transpiration (l " * " " * d^-1 * ")")))+
      scale_fill_manual(name="",
                        limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                        labels=c(expression(paste(aC[a]*" - D")), 
                                 expression(paste(aC[a]*" - W")),
                                 expression(paste(eC[a]*" - D")),
                                 expression(paste(eC[a]*" - W"))),
                        values=c(alpha("blue3", 0.2), "blue3", 
                                 alpha("red2", 0.2), "red2"),
                        guide=guide_legend(nrow=1))+
      xlab("")+
      scale_x_continuous(limits=c(0.5, 4.5),
                         breaks=c(1.5, 3.5),
                         labels=c("Well-watered","Droughted"))+
      ylim(0, 4)
   
   
   #### plant transpiration per leaf area bar plant
   p3 <- ggplot(data=barDF1, 
                aes(brk, transp_leaf.mean)) +
      geom_bar(stat = "identity", aes(fill=Trt), 
               position="dodge", col="black") +
      geom_errorbar(aes(x=brk, ymin=transp_leaf.mean-transp_leaf.se, 
                        ymax=transp_leaf.mean+transp_leaf.se), 
                    position=position_dodge(0.9), width=0.2) +
      ggtitle("E. pilularis")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("Leaf transpiration (l " * m^-2 * " " * d^-1 * ")")))+
      scale_fill_manual(name="",
                        limits=c("PILAD", "PILAND", "PILED", "PILEND"),
                        labels=c(expression(paste(aC[a]*" - D")), 
                                 expression(paste(aC[a]*" - W")),
                                 expression(paste(eC[a]*" - D")),
                                 expression(paste(eC[a]*" - W"))),
                        values=c(alpha("blue3", 0.2), "blue3", 
                                 alpha("red2", 0.2), "red2"),
                        guide=guide_legend(nrow=1))+
      xlab("")+
      scale_x_continuous(limits=c(0.5, 4.5),
                         breaks=c(1.5, 3.5),
                         labels=c("Well-watered","Droughted"))+
      ylim(0, 1)#+
      #annotate('text', x=3.7, y=1, label = expression(CO[2] * " n.s.    "))+
      #annotate('text', x=3.7, y=0.92, label = expression(H[2] * "O *      "))+
      #annotate('text', x=3.9, y=0.84, label = expression(CO[2] * " x " * H[2] * "O n.s."))
   
   
   
   p4 <- ggplot(data=barDF2, 
                aes(brk, transp_leaf.mean)) +
      geom_bar(stat = "identity", aes(fill=Trt), 
               position="dodge", col="black") +
      geom_errorbar(aes(x=brk, ymin=transp_leaf.mean-transp_leaf.se, 
                        ymax=transp_leaf.mean+transp_leaf.se), 
                    position=position_dodge(0.9), width=0.2) +
      ggtitle("E. populnea")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("Leaf transpiration (l " * m^-2 * " " * d^-1 * ")")))+
      scale_fill_manual(name="",
                        limits=c("POPAD", "POPAND", "POPED", "POPEND"),
                        labels=c(expression(paste(aC[a]*" - D")), 
                                 expression(paste(aC[a]*" - W")),
                                 expression(paste(eC[a]*" - D")),
                                 expression(paste(eC[a]*" - W"))),
                        values=c(alpha("blue3", 0.2), "blue3", 
                                 alpha("red2", 0.2), "red2"),
                        guide=guide_legend(nrow=1))+
      xlab("")+
      scale_x_continuous(limits=c(0.5, 4.5),
                         breaks=c(1.5, 3.5),
                         labels=c("Well-watered","Droughted"))+
      ylim(0, 4)#+
      #annotate('text', x=3.65, y=4, label = expression(CO[2] * " **     "))+
      #annotate('text', x=3.7, y=3.7, label = expression(H[2] * "O n.s.   "))+
      #annotate('text', x=3.89, y=3.4, label = expression(CO[2] * " x " * H[2] * "O n.s."))
   
   
   combined_legend <- get_legend(p3 + theme(legend.position="bottom",
                                            legend.box = 'vertical',
                                            legend.box.just = 'left'))
   
   
   combined_plots <- plot_grid(p3, p4, 
                               labels=c("(a)", "(b)"), 
                               ncol=2, align="vh", axis = "l",
                               label_x=0.2, label_y=0.9)
   
   
   pdf(paste0(outdir, "F4.transpiration_per_leaf_area.pdf"), width=8, height=4)
   plot_grid(combined_plots, combined_legend, 
             ncol=1, rel_heights=c(1, 0.1))
   dev.off() 
  
   
   legend1 <- get_legend(p1 + theme(legend.position="bottom",
                                    legend.box = 'horizontal',
                                    legend.box.just = 'left'))
   
   legend2 <- get_legend(p3 + theme(legend.position="bottom",
                                    legend.box = 'horizontal',
                                    legend.box.just = 'left'))
   
   combined_plots1 <- plot_grid(p1, p2, 
                                labels=c("(a)", "(b)"), 
                                ncol=2, align="vh", axis = "l",
                                label_x=0.12, label_y=0.9)
   
   combined_plots2 <- plot_grid(p3, p4, 
                                labels=c("(c)", "(d)"), 
                                ncol=2, align="vh", axis = "l",
                                label_x=0.18, label_y=0.9)
   
   pdf(paste0(outdir, "F4.transpiration_per_leaf_area_combined.pdf"), width=10, height=10)
   plot_grid(combined_plots1, legend1, 
             combined_plots2, legend2,
             ncol=1, rel_heights=c(1, 0.3, 1, 0.3))
   dev.off() 
   
   
}

