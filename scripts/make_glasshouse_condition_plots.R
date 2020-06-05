make_glasshouse_condition_plots <- function() {
    ### four documents
    
    myDF1 <- read.csv("data/glasshouse2/met/AMBIENT_GH_TEMP_RH_PILULARIS_DRYDOWN_DAY.csv")
    myDF2 <- read.csv("data/glasshouse2/met/AMBIENT_GH_TEMP_RH_PILULARIS_DRYDOWN_NIGHT.csv")
    myDF3 <- read.csv("data/glasshouse2/met/ELEVATED_GH_TEMP_RH_PILULARIS_DRYDOWN_DAY.csv")
    myDF4 <- read.csv("data/glasshouse2/met/ELEVATED_GH_TEMP_RH_PILULARIS_DRYDOWN_NIGHT.csv")
    
    ### merge by day and night separately
    plotDF1 <- rbind(myDF1, myDF3)
    plotDF2 <- rbind(myDF2, myDF4)
    
    plotDF <- rbind(plotDF1, plotDF2)
    
    colnames(plotDF) <- colnames(plotDF1) <- colnames(plotDF2) <- c("DateTime", "Tair", "RH", "SVP",
                                                                    "RHdeficit", "VPD", "DAY",
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

    
    ### plotting
    p1 <- ggplot(plotDF1, aes(x=DateTime, y=Tair, group=CO2)) +
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
        ggtitle("Daytime")+
        ylim(10, 35)+
        guides(fill = guide_legend(override.aes = list(shape = c(19, 19),
                                                       fill = c("blue3", "red2"),
                                                       size= c(10, 10))))
    
    ### night time
    p2 <- ggplot(plotDF2, aes(x=DateTime, y=Tair, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
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
        ggtitle("Nighttime")+
        ylim(10, 35)
    
    
    p3 <- ggplot(plotDF1, aes(x=DateTime, y=RH, group=CO2)) +
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
    
    ### night time
    p4 <- ggplot(plotDF2, aes(x=DateTime, y=RH, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
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
    
    
    p5 <- ggplot(plotDF1, aes(x=DateTime, y=VPD, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
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
    
    ### night time
    p6 <- ggplot(plotDF2, aes(x=DateTime, y=VPD, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
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
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                     legend.box = 'vertical',
                                     legend.box.just = 'left'))

    
    combined_plots <- plot_grid(p1, p2, p3, p4, p5, p6,
                                 labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
                                 ncol=2, align="vh", axis = "l",
                                 label_x=0.08, label_y=0.85)
    
    
    
    ### plotting combined data frame
    p1 <- ggplot(plotDF, aes(x=DateTime, y=Tair, group=CO2)) +
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
    
    
    p3 <- ggplot(plotDF1, aes(x=DateTime, y=RH, group=CO2)) +
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
    
    p5 <- ggplot(plotDF1, aes(x=DateTime, y=VPD, group=CO2)) +
        geom_point(aes(color=CO2), pch=19)+
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
    
    
    ### output
    combined_legend <- get_legend(p1 + theme(legend.position="bottom",
                                             legend.box = 'vertical',
                                             legend.box.just = 'left'))
    
    
    combined_plots <- plot_grid(p1, p3, p5,
                                labels=c("(a)", "(b)", "(c)"), 
                                ncol=1, align="vh", axis = "l",
                                label_x=0.08, label_y=0.85)
    
    pdf(paste0(outdir, "F1.glasshouse_condition.pdf"), width=8, height=12)
    plot_grid(combined_plots, combined_legend, 
              ncol=1, rel_heights=c(1, 0.1))
    dev.off() 
    
}