make_leaf_area_and_biomass_plots_DT <- function() {
    
    ### processing
    myDF <- read.csv("data/glasshouse2/EUC_Leaf_Area_data.csv")
    
    colnames(myDF) <- c("ID", "Species", "Species_Treatment", "GH",
                        "Treatment", "CO2", "H2O", "LA_early", "LA_plant",
                        "LA_final", "LA", "Stem", "SM", "Leaf", "LM", 
                        "Root", "RM", "CoarseRoot", "CRM", "FineRoot", 
                        "FRM")
    
    ### total biomass
    myDF$TOT <- with(myDF, SM+LM+FRM+CRM)
    
    
    ### summarize data - Leaf area
    sumDF <- summaryBy(LA_early+LA_plant+LA_final+SM+LM+RM+CRM+FRM+TOT~Species+Species_Treatment+CO2+H2O, 
                       FUN=c(mean, se),
                       data=myDF, keep.names=T)
    
    subDF1 <- subset(sumDF, Species == "PIL")
    subDF2 <- subset(sumDF, Species == "POP")
    
    ### summarize data - biomass
    subDF3 <- rbind(subDF1[,1:4], subDF1[,1:4], 
                    subDF1[,1:4], subDF1[,1:4])
    subDF3$Component <- rep(c("SM", "LM", "CRM", "FRM"), each=4)
    
    subDF4 <- rbind(subDF2[,1:4], subDF2[,1:4], 
                    subDF2[,1:4], subDF2[,1:4])
    subDF4$Component <- rep(c("SM", "LM", "CRM", "FRM"), each=4)
    
    for (i in unique(subDF3$Species_Treatment)) {
        subDF3$BM[subDF3$Species_Treatment==i&subDF3$Component=="SM"] <- subDF1$SM.mean[subDF1$Species_Treatment==i]
        subDF3$BM[subDF3$Species_Treatment==i&subDF3$Component=="LM"] <- subDF1$LM.mean[subDF1$Species_Treatment==i]
        subDF3$BM[subDF3$Species_Treatment==i&subDF3$Component=="CRM"] <- subDF1$CRM.mean[subDF1$Species_Treatment==i]
        subDF3$BM[subDF3$Species_Treatment==i&subDF3$Component=="FRM"] <- subDF1$FRM.mean[subDF1$Species_Treatment==i]
        
    }
    
    for (i in unique(subDF4$Species_Treatment)) {
        subDF4$BM[subDF4$Species_Treatment==i&subDF4$Component=="SM"] <- subDF2$SM.mean[subDF2$Species_Treatment==i]
        subDF4$BM[subDF4$Species_Treatment==i&subDF4$Component=="LM"] <- subDF2$LM.mean[subDF2$Species_Treatment==i]
        subDF4$BM[subDF4$Species_Treatment==i&subDF4$Component=="CRM"] <- subDF2$CRM.mean[subDF2$Species_Treatment==i]
        subDF4$BM[subDF4$Species_Treatment==i&subDF4$Component=="FRM"] <- subDF2$FRM.mean[subDF2$Species_Treatment==i]
        
    }
    
    #### Plotting
    color.blind.pal <- rev(viridis(4))
    
    subDF1$brk[subDF1$Species_Treatment=="PILAD"] <- 1.8
    subDF1$brk[subDF1$Species_Treatment=="PILAND"] <- 1.2
    subDF1$brk[subDF1$Species_Treatment=="PILED"] <- 3.8
    subDF1$brk[subDF1$Species_Treatment=="PILEND"] <- 3.2
    
    subDF3$brk[subDF3$Species_Treatment=="PILAD"] <- 1.8
    subDF3$brk[subDF3$Species_Treatment=="PILAND"] <- 1.2
    subDF3$brk[subDF3$Species_Treatment=="PILED"] <- 3.8
    subDF3$brk[subDF3$Species_Treatment=="PILEND"] <- 3.2
    
    subDF2$brk[subDF2$Species_Treatment=="POPAD"] <- 1.8
    subDF2$brk[subDF2$Species_Treatment=="POPAND"] <- 1.2
    subDF2$brk[subDF2$Species_Treatment=="POPED"] <- 3.8
    subDF2$brk[subDF2$Species_Treatment=="POPEND"] <- 3.2
    
    subDF4$brk[subDF4$Species_Treatment=="POPAD"] <- 1.8
    subDF4$brk[subDF4$Species_Treatment=="POPAND"] <- 1.2
    subDF4$brk[subDF4$Species_Treatment=="POPED"] <- 3.8
    subDF4$brk[subDF4$Species_Treatment=="POPEND"] <- 3.2
    
    ### plotting
    p1 <- ggplot(data=subDF1, 
                 aes(brk, LA_final.mean)) +
        geom_bar(stat = "identity", aes(alpha=CO2, fill=H2O), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=brk, ymin=LA_final.mean-LA_final.se, 
                          ymax=LA_final.mean+LA_final.se), 
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
        ylab(expression(paste("Leaf area ("*m^2*")")))+
        scale_alpha_manual(name=expression(CO[2]),
                          limits=c("A", "E"),
                          labels=c("amb", "ele"),
                          values=c(0.2, 1.0),
                          guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(H[2]*O),
                           limits=c("D", "ND"),
                          values=c("red2", "blue2"),
                           labels=c("Droughted", "Well-watered"),
                           guide=guide_legend(nrow=1))+
        xlab("")+
        scale_x_continuous(limits=c(0.5, 4.5),
                         breaks=c(1.5, 3.5),
                         labels=c("ambient","elevated"))+
        ylim(0, 10)
    
    
    p2 <- ggplot(data=subDF2, 
                 aes(brk, LA_final.mean)) +
        geom_bar(stat = "identity", aes(alpha=CO2, fill=H2O), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=brk, ymin=LA_final.mean-LA_final.se, 
                          ymax=LA_final.mean+LA_final.se), 
                      position=position_dodge(0.9), width=0.2) +
        ggtitle("E. populnea")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Leaf area ("*m^2*")")))+
        scale_alpha_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c(0.2, 1.0),
                           guide=guide_legend(nrow=1))+
        scale_fill_manual(name=expression(H[2]*O),
                          limits=c("D", "ND"),
                          values=c("red2", "blue2"),
                          labels=c("Droughted", "Well-watered"),
                          guide=guide_legend(nrow=1))+
        xlab("")+
        scale_x_continuous(limits=c(0.5, 4.5),
                           breaks=c(1.5, 3.5),
                           labels=c("ambient","elevated"))+
        ylim(0, 2)
    
    
    p3 <- ggplot() +
        geom_bar(data=subDF3, stat = "identity", aes(brk, BM,
                                                     fill=Component, alpha=H2O), 
                 position="stack", col="black") +
        geom_errorbar(data=subDF1, 
                      aes(x=brk, ymin=TOT.mean-TOT.se, 
                          ymax=TOT.mean+TOT.se), 
                      position="dodge", width=0.2) +
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
        ylab("Biomass (g)")+
        scale_fill_manual(name="Component",
                          values=color.blind.pal)+
        scale_alpha_manual(name=expression(H[2]*O),
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c(0.2, 1.0),
                           guide=guide_legend(nrow=1))+
        xlab("")+
        ylim(0, 3000)+
        scale_x_continuous(limits=c(0.5, 4.5),
                           breaks=c(1.5, 3.5),
                           labels=c("ambient","elevated"))
    
    p4 <- ggplot() +
        geom_bar(data=subDF4, stat = "identity", aes(brk, BM,
                                                     fill=Component, alpha=H2O), 
                 position="stack", col="black") +
        geom_errorbar(data=subDF2, 
                      aes(x=brk, ymin=TOT.mean-TOT.se, 
                          ymax=TOT.mean+TOT.se), 
                      position="dodge", width=0.2) +
        ggtitle("E. populnea")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Biomass (g)")+
        scale_fill_manual(name="Component",
                          values=color.blind.pal)+
        scale_alpha_manual(name=expression(H[2]*O),
                           limits=c("D", "ND"),
                           labels=c("Droughted", "Well-watered"),
                           values=c(0.2, 1.0),
                           guide=guide_legend(nrow=1))+
        xlab("")+
        ylim(0, 1000)+
        scale_x_continuous(limits=c(0.5, 4.5),
                           breaks=c(1.5, 3.5),
                           labels=c("ambient","elevated"))
    
    
    ### output
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
                                 label_x=0.12, label_y=0.9)
    
    pdf(paste0(outdir, "F2.leaf_area_biomass.pdf"), width=12, height=10)
    plot_grid(combined_plots1, legend1, 
              combined_plots2, legend2,
              ncol=1, rel_heights=c(1, 0.3, 1, 0.3))
    dev.off() 
    
}
