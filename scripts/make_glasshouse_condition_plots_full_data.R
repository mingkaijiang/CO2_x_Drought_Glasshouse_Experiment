make_glasshouse_condition_plots_full_data <- function() {
    
    ### This is the meteorological condition throuhgout the whole experiment
    ### we are only interested in day-time conditions over the dry-down period
    ### hence no need to analyze the full set.
    
    ### four documents
    myDF1 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/AMBIENT_GH_RH_AND_TEMP_DAY.csv")
    myDF2 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/AMBIENT_GH_RH_AND_TEMP_NIGHT.csv")
    myDF3 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/ELEVATED_GH_RH_AND_TEMP_DAY.csv")
    myDF4 <- read.csv("data/glasshouse2/Full_Experiment_HOBO_Data_Loggers/ELEVATED_GH_RH_AND_TEMP_NIGHT.csv")
    
    ### merge by day and night separately
    plotDF1 <- rbind(myDF1, myDF3)
    plotDF2 <- rbind(myDF2, myDF4)
    
    plotDF <- rbind(plotDF1, plotDF2)
    
    colnames(plotDF) <- colnames(plotDF1) <- colnames(plotDF2) <- c("DateTime", "Tair", "RH", "SVP",
                                                                    "RHdeficit", "VPD",
                                                                    "GH", "CO2")
    
    ### setting date and time
    plotDF$DateTime <- paste0(plotDF$DateTime, ":00")
    plotDF$DateTime <- as.character(plotDF$DateTime)
    plotDF$Date <- sub(" .*", "", plotDF$DateTime)
    plotDF$Date <- as.Date(as.character(plotDF$Date), format="%m/%d/%Y")
    plotDF$Time <- sub(".+? ", "", plotDF$DateTime)
    plotDF$Time <- chron(times=plotDF$Time)    
    plotDF$DateTime <- as.POSIXct(paste0(as.character(plotDF$Date), " ", 
                                          as.character(plotDF$Time)),
                                   format="%Y-%m-%d %H:%M:%S")
    
    
    ### setting date and time
    plotDF1$DateTime <- paste0(plotDF1$DateTime, ":00")
    plotDF1$DateTime <- as.character(plotDF1$DateTime)
    plotDF1$Date <- sub(" .*", "", plotDF1$DateTime)
    plotDF1$Date <- as.Date(as.character(plotDF1$Date), format="%m/%d/%Y")
    plotDF1$Time <- sub(".+? ", "", plotDF1$DateTime)
    plotDF1$Time <- chron(times=plotDF1$Time)    
    plotDF1$DateTime <- as.POSIXct(paste0(as.character(plotDF1$Date), " ", 
                                          as.character(plotDF1$Time)),
                                   format="%Y-%m-%d %H:%M:%S")
    
    plotDF2$DateTime <- paste0(plotDF2$DateTime, ":00")
    plotDF2$DateTime <- as.character(plotDF2$DateTime)
    plotDF2$Date <- sub(" .*", "", plotDF2$DateTime)
    plotDF2$Date <- as.Date(as.character(plotDF2$Date), format="%m/%d/%Y")
    plotDF2$Time <- sub(".+? ", "", plotDF2$DateTime)
    plotDF2$Time <- chron(times=plotDF2$Time)    
    plotDF2$DateTime <- as.POSIXct(paste0(as.character(plotDF2$Date), " ", 
                                          as.character(plotDF2$Time)),
                                   format="%Y-%m-%d %H:%M:%S")
    
    
    test <- summaryBy(VPD~CO2+GH+Date, FUN=mean,
                      data=plotDF, keep.names=T) 
    
    mean(test$VPD)
    max(test$VPD)
    
    ### prepare day list for each species
    subDF1 <- subset(plotDF, Date > "2010-11-07")
    subDF1$Day <- as.numeric(subDF1$Date - as.Date("2010-11-07"))
    subDF1 <- subset(subDF1, Day <= 9)
    
    subDF2 <- subset(plotDF, Date > "2011-01-03")
    subDF2$Day <- as.numeric(subDF2$Date - as.Date("2011-01-03"))
    
    
    ### add species information
    subDF1$Species <- "PIL"
    subDF2$Species <- "POP"
    
    ### normalize the date
    subDF1$tmp1 <- subDF1$Date + 57
    subDF1$NormDateTime <- as.POSIXct(paste0(as.character(subDF1$tmp1), " ", 
                                             as.character(subDF1$Time)),
                                      format="%Y-%m-%d %H:%M:%S")
    
    subDF1$tmp1 <- NULL
    
    subDF2$NormDateTime <- subDF2$DateTime

    ### merge
    plotDF <- rbind(subDF1, subDF2)    
    
    
    
    ### plotting
    #p1 <- ggplot(plotDF1, aes(x=DateTime, y=Tair, group=CO2)) +
    #    geom_point(aes(color=CO2), pch=19)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_blank(),
    #          axis.title.x=element_blank(),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=24),
    #          legend.title=element_text(size=20),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=30, face="bold", 
    #                                    hjust = 0.5))+
    #    ylab(expression(paste("Temperature ("*degree*"C)")))+
    #    scale_shape_manual(name=expression(CO[2]),
    #                      limits=c("A", "E"),
    #                      labels=c("amb", "ele"),
    #                      values=c(19,19),
    #                      guide=guide_legend(nrow=1))+
    #    scale_color_manual(name=expression(CO[2]),
    #                       limits=c("A", "E"),
    #                       labels=c("amb", "ele"),
    #                       values=c("blue3", "red2"),
    #                       guide=guide_legend(nrow=1))+
    #    scale_x_datetime(name="Date", 
    #                     breaks=date_breaks("24 hour"),
    #                     labels=date_format("%m-%d"))+
    #    ggtitle("Daytime")+
    #    ylim(10, 35)+
    #    guides(fill = guide_legend(override.aes = list(shape = c(19, 19),
    #                                                   fill = c("blue3", "red2"),
    #                                                   size= c(10, 10))))
    #
    #### night time
    #p2 <- ggplot(plotDF2, aes(x=DateTime, y=Tair, group=CO2)) +
    #    geom_point(aes(color=CO2), pch=19)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_blank(),
    #          axis.title.x=element_blank(),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_blank(),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=30, face="bold", 
    #                                    hjust = 0.5))+
    #    ylab(expression(paste("Temperature ("*degree*"C)")))+
    #    scale_shape_manual(name=expression(CO[2]),
    #                       limits=c("A", "E"),
    #                       labels=c("amb", "ele"),
    #                       values=c(19,19),
    #                       guide=guide_legend(nrow=1))+
    #    scale_color_manual(name=expression(CO[2]),
    #                      limits=c("A", "E"),
    #                      labels=c("amb", "ele"),
    #                      values=c("blue3", "red2"),
    #                      guide=guide_legend(nrow=1))+
    #    scale_x_datetime(name="Date", 
    #                     breaks=date_breaks("24 hour"),
    #                     labels=date_format("%m-%d"))+
    #    ggtitle("Nighttime")+
    #    ylim(10, 35)
    #
    #
    #p3 <- ggplot(plotDF1, aes(x=DateTime, y=RH, group=CO2)) +
    #    geom_point(aes(color=CO2), pch=19)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_blank(),
    #          axis.title.x=element_blank(),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=14, face="bold", 
    #                                    hjust = 0.5))+
    #    ylab("Relative Humidity (%)")+
    #    scale_shape_manual(name=expression(CO[2]),
    #                       limits=c("A", "E"),
    #                       labels=c("amb", "ele"),
    #                       values=c(19,19),
    #                       guide=guide_legend(nrow=1))+
    #    scale_color_manual(name=expression(CO[2]),
    #                      limits=c("A", "E"),
    #                      labels=c("amb", "ele"),
    #                      values=c("blue3", "red2"),
    #                      guide=guide_legend(nrow=1))+
    #    scale_x_datetime(name="Date", 
    #                     breaks=date_breaks("24 hour"),
    #                     labels=date_format("%m-%d"))+
    #    ylim(35, 100)
    #
    #### night time
    #p4 <- ggplot(plotDF2, aes(x=DateTime, y=RH, group=CO2)) +
    #    geom_point(aes(color=CO2), pch=19)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_blank(),
    #          axis.title.x=element_blank(),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_blank(),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=14, face="bold", 
    #                                    hjust = 0.5))+
    #    ylab("Relative Humidity (%)")+
    #    scale_shape_manual(name=expression(CO[2]),
    #                       limits=c("A", "E"),
    #                       labels=c("amb", "ele"),
    #                       values=c(19,19),
    #                       guide=guide_legend(nrow=1))+
    #    scale_color_manual(name=expression(CO[2]),
    #                      limits=c("A", "E"),
    #                      labels=c("amb", "ele"),
    #                      values=c("blue3", "red2"),
    #                      guide=guide_legend(nrow=1))+
    #    scale_x_datetime(name="Date", 
    #                     breaks=date_breaks("24 hour"),
    #                     labels=date_format("%m-%d"))+
    #    ylim(35, 100)
    #
    #
    #p5 <- ggplot(plotDF1, aes(x=DateTime, y=VPD, group=CO2)) +
    #    geom_point(aes(color=CO2), pch=19)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=14, face="bold", 
    #                                    hjust = 0.5))+
    #    ylab("Vapor Pressure Deficit (kPa)")+
    #    scale_shape_manual(name=expression(CO[2]),
    #                       limits=c("A", "E"),
    #                       labels=c("amb", "ele"),
    #                       values=c(19,19),
    #                       guide=guide_legend(nrow=1))+
    #    scale_color_manual(name=expression(CO[2]),
    #                      limits=c("A", "E"),
    #                      labels=c("amb", "ele"),
    #                      values=c("blue3", "red2"),
    #                      guide=guide_legend(nrow=1))+
    #    scale_x_datetime(name="Date", 
    #                     breaks=date_breaks("24 hour"),
    #                     labels=date_format("%m-%d"))+
    #    ylim(0,3)
    #
    #### night time
    #p6 <- ggplot(plotDF2, aes(x=DateTime, y=VPD, group=CO2)) +
    #    geom_point(aes(color=CO2), pch=19)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_blank(),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=14, face="bold", 
    #                                    hjust = 0.5))+
    #    ylab("Vapor Pressure Deficit (kPa)")+
    #    scale_shape_manual(name=expression(CO[2]),
    #                       limits=c("A", "E"),
    #                       labels=c("amb", "ele"),
    #                       values=c(19,19),
    #                       guide=guide_legend(nrow=1))+
    #    scale_color_manual(name=expression(CO[2]),
    #                      limits=c("A", "E"),
    #                      labels=c("amb", "ele"),
    #                      values=c("blue3", "red2"),
    #                      guide=guide_legend(nrow=1))+
    #    scale_x_datetime(name="Date", 
    #                     breaks=date_breaks("24 hour"),
    #                     labels=date_format("%m-%d"))+
    #    ylim(0,3)
    #
    #### output
    #combined_legend <- get_legend(p1 + theme(legend.position="bottom",
    #                                 legend.box = 'vertical',
    #                                 legend.box.just = 'left'))
    #
    #
    #combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6,
    #                             labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
    #                             ncol=2, align="vh", axis = "l",
    #                             label_x=0.08, label_y=0.85)
    #
    
    
    ### plotting combined data frame
    p1 <- ggplot(subDF1, aes(x=DateTime, y=Tair, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=24),
              legend.title=element_text(size=20),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=30, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste("Temperature ("*degree*"C)")))+
        scale_shape_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c(19,19),
                           guide=guide_legend(nrow=1))+
        scale_color_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_x_datetime(name="Date", 
                         breaks=date_breaks("24 hour"),
                         labels=date_format("%m-%d"))+
        ylim(10, 35)+
        guides(fill = guide_legend(override.aes = list(shape = c(19, 19),
                                                       fill = c("blue3", "red2"),
                                                       size= c(10, 10))))
    

    
    p3 <- ggplot(subDF1, aes(x=DateTime, y=RH, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold", 
                                        hjust = 0.5))+
        ylab("Relative Humidity (%)")+
        scale_shape_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c(19,19),
                           guide=guide_legend(nrow=1))+
        scale_color_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_x_datetime(name="Date", 
                         breaks=date_breaks("24 hour"),
                         labels=date_format("%m-%d"))+
        ylim(35, 100)
    
    p5 <- ggplot(subDF1, aes(x=DateTime, y=VPD, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=8),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold", 
                                        hjust = 0.5))+
        ylab("Vapor Pressure Deficit (kPa)")+
        scale_shape_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c(19,19),
                           guide=guide_legend(nrow=1))+
        scale_color_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_x_datetime(name="Date", 
                         breaks=date_breaks("24 hour"),
                         labels=date_format("%m-%d"))+
        ylim(0,3)
    
    
    p2 <- ggplot(subDF2, aes(x=DateTime, y=Tair, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=24),
              legend.title=element_text(size=20),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=30, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste("Temperature ("*degree*"C)")))+
        scale_shape_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c(19,19),
                           guide=guide_legend(nrow=1))+
        scale_color_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_x_datetime(name="Date", 
                         breaks=date_breaks("24 hour"),
                         labels=date_format("%m-%d"))+
        ylim(10, 35)+
        guides(fill = guide_legend(override.aes = list(shape = c(19, 19),
                                                       fill = c("blue3", "red2"),
                                                       size= c(10, 10))))
    
    
    
    p4 <- ggplot(subDF2, aes(x=DateTime, y=RH, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold", 
                                        hjust = 0.5))+
        ylab("Relative Humidity (%)")+
        scale_shape_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c(19,19),
                           guide=guide_legend(nrow=1))+
        scale_color_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_x_datetime(name="Date", 
                         breaks=date_breaks("24 hour"),
                         labels=date_format("%m-%d"))+
        ylim(35, 100)
    
    p6 <- ggplot(subDF2, aes(x=DateTime, y=VPD, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=8),
              axis.title.x=element_text(size=14),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold", 
                                        hjust = 0.5))+
        ylab("Vapor Pressure Deficit (kPa)")+
        scale_shape_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c(19,19),
                           guide=guide_legend(nrow=1))+
        scale_color_manual(name=expression(CO[2]),
                           limits=c("A", "E"),
                           labels=c("amb", "ele"),
                           values=c("blue3", "red2"),
                           guide=guide_legend(nrow=1))+
        scale_x_datetime(name="Date", 
                         breaks=date_breaks("24 hour"),
                         labels=date_format("%m-%d"))+
        ylim(0,3)
    
    #plot(p5)
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6, 
                                labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
                                ncol=2, align="v", axis = "l",
                                label_x=c(0.12, 0.08), label_y=0.9,
                                rel_widths=c(0.8, 1.8))
    
    pdf(paste0(outdir, "F1.glasshouse_condition_full.pdf"), width=18, height=8)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
    
    ################################################################
    ###         Check glasshouse effect on VPD                   ###
    ################################################################
    
    ### compute daily mean and max of VPD
    statDF <- summaryBy(VPD~CO2+GH+Day+Species, FUN=c(mean, max),
                        data=plotDF, keep.names=T)

    
    ### split by species
    testDF1 <- subset(statDF, Species == "PIL")
    testDF2 <- subset(statDF, Species == "POP")
    
    ### check if VPD differ between glasshouses
    mod1 <- lm(VPD.mean~GH*Species, data=statDF)
    summary(mod1)
    
    mod1 <- lm(VPD.max~GH*Species, data=statDF)
    summary(mod1)

    
    
    ### check if GH is a random factor
    mod1<-lme(VPD.mean~Species*CO2,random=~1|GH,data=statDF)
    summary(mod1) 
    mod1<-lme(VPD.max~Species*CO2,random=~1|GH,data=statDF)
    summary(mod1) 
    
    
    ### check if VPD differ by glasshouse and day
    mod1<-lm(VPD.mean~GH*Day,data=testDF1)
    summary(mod1)
    mod2<-lm(VPD.mean~GH*Day,data=testDF2)
    summary(mod2)
    
    mod1<-lm(VPD.max~GH*Day,data=testDF1)
    summary(mod1)
    mod2<-lm(VPD.max~GH*Day,data=testDF2)
    summary(mod2)
    
    ### check if VPD differ by species, with GH as random factor
    mod1<-lme(VPD.mean~Species,random=~1|GH,data=statDF)
    summary(mod1) # species effect
    mod2<-lme(VPD.max~Species,random=~1|GH,data=statDF)
    summary(mod2) 
    
    ### check if VPD differ by species, with day + RH as random factor
    mod1<-lme(VPD.mean~Species,random=~1|GH/Day,data=statDF)
    summary(mod1)
    mod2<-lme(VPD.max~Species,random=~1|GH/Day,data=statDF)
    summary(mod2)
    
    ### check if VPD differ by species and CO2, with glasshouse as random factor
    mod1<-lme(VPD.mean~CO2*Species,random=~1|GH,data=statDF)
    summary(mod1) # species effect but very very weak
    mod1<-lme(VPD.max~CO2*Species,random=~1|GH,data=statDF)
    summary(mod1) 
    
    ### check if VPD differ by species and CO2, with glasshouse and day as random factors
    #mod1<-lme(VPD.mean~CO2*Species,random=~1|GH/Day,data=statDF)
    #summary(mod1) 
    #mod1<-lme(VPD.max~CO2*Species,random=~1|GH/Day,data=statDF)
    #summary(mod1) 
    
    ## we can confirm that, VPD do not differ among CO2 treatment and between species
    
    
    
    ################################################################
    ###    Check if VPD predicts transpiration                   ###
    ################################################################
    ### assign column names
    colnames(statDF) <- c("CO2", "Glasshouse", "Day", "Species", 
                       "VPD.mean", "VPD.max")
    
    
    ### read in physiologycal measurements
    pilDF <- read.csv("data/glasshouse2/Drydown_gasexchange_pilularis_processed.csv")
    popDF <- read.csv("data/glasshouse2/Drydown_gasexchange_populnea_processed.csv")
    
    pilDF$Species <- "PIL"
    popDF$Species <- "POP"
    
    pilDF$psiPD_psiMD <- NULL
    
    ### merge
    physDF <- rbind(pilDF, popDF)
    
    ### merge
    mgDF <- merge(physDF, statDF, by=c("Day", "Glasshouse", "CO2", "Species"))

    ### split by species
    statDF1 <- mgDF[mgDF$Species=="PIL",]
    statDF2 <- mgDF[mgDF$Species=="POP",]
    
    
    ### check if VPD explains transpiration     
    mod1<-lme(log(transp_plant)~CO2*Day*VPD.mean,random=~1|Glasshouse/Replicate,
              data=statDF1)
    summary(mod1)
    
    mod1<-lme(log(transp_plant)~CO2*Day*VPD.max,random=~1|Glasshouse/Replicate,
              data=statDF1)
    summary(mod1)
    
    mod1<-lme(log(transp_plant)~CO2*Day*VPD.mean,random=~1|Glasshouse/Replicate,
              data=statDF2)
    summary(mod1)
    
    mod1<-lme(log(transp_plant)~CO2*Day*VPD.max,random=~1|Glasshouse/Replicate,
              data=statDF2)
    summary(mod1)
    
    
    
    mod1<-lme(log(transp_plant)~CO2*VPD.mean,random=~1|Glasshouse/Replicate,
              data=statDF1)
    summary(mod1)
    
    mod1<-lme(log(transp_plant)~CO2*VPD.max,random=~1|Glasshouse/Replicate,
              data=statDF1)
    summary(mod1)
    
    mod1<-lme(log(transp_plant)~CO2*VPD.mean,random=~1|Glasshouse/Replicate,
              data=statDF2)
    summary(mod1)
    
    mod1<-lme(log(transp_plant)~CO2*VPD.max,random=~1|Glasshouse/Replicate,
              data=statDF2)
    summary(mod1)
    

}