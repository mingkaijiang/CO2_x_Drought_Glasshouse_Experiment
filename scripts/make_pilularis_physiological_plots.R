make_pilularis_physiological_plots <- function() {
        
    #FIGURES (Eucalyptus pilularis)
    PILPhysGraph<-read.csv("data/glasshouse2/Pilularis_Phys.csv",sep=",", header=TRUE)
    
    ## remove data points with unequal sample size
    PILPhysGraph <- PILPhysGraph[PILPhysGraph$n.1==6, ]
    
    
    ################################### Plotting ######################################
    # Eucalyptus pilularis (Daily, morning and midday Asat & gs)
    pdf(paste0(outdir, "F8.1.Pil_gas_exchange.pdf"), width=8, height=6)
    #par(mfrow=c(2,3))
    #bottom,left,top,right
    par(mfrow=c(2,3), omi=c(.5, 0.75, 0.75, .5)) 
    par(mar=c(0, 0, 0, 0))
    
    #PIL A Daily
    par(las=1)
    with(PILPhysGraph,plot(Adaily[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex.axis=1.25,
                           ylim=range(0,29),xlim=range(0,1.05*max(Day)),
                           ylab="",xaxt="n",cex.lab=1,cex=1.25,type="o",lty=2))
    axis(1,labels=FALSE,tck=0.03)
    with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
                             UAdaily[Trt=="PILAD"], Day[Trt=="PILAD"], LAdaily[Trt=="PILAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(PILPhysGraph,points(Adaily[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=19,cex=1.25,type="o", lty=1))
    with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
                             UAdaily[Trt=="PILAND"], Day[Trt=="PILAND"], LAdaily[Trt=="PILAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(PILPhysGraph,points(Adaily[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o",lty=2))
    with(PILPhysGraph,arrows(Day[Trt=="PILED"],
                             UAdaily[Trt=="PILED"], Day[Trt=="PILED"], LAdaily[Trt=="PILED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(PILPhysGraph,points(Adaily[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=19,cex=1.25,type="o"))
    with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
                             UAdaily[Trt=="PILEND"], Day[Trt=="PILEND"], LAdaily[Trt=="PILEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    par(las=3)
    mtext(side = 2, text =expression(bold(A[sat])), line = 3.75,font=2.5, cex=1.0)
    mtext(side = 2, text =expression(bold((mu~mol~m^-2~s^-1))),
          font=2,cex=0.75, line = 2.25)	
    par(las=1)
    mtext(side=3,text="Daily",line=0.5,font=1,cex=1.0)
    text(8.25,27.75, "(a)", pos=4,cex=1,font=2)
    
    #PIL A Early
    par(las=1)
    with(PILPhysGraph,plot(Aearly[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,
                           ylim=range(0,29),xlim=range(0,1.05*max(Day)),
                           xaxt="n",yaxt="n",cex=1.25,type="o",lty=2))
    axis(2,labels=FALSE,tck=0.03)
    axis(1,labels=FALSE,tck=0.03)
    with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
                             UAearly[Trt=="PILAD"], Day[Trt=="PILAD"], LAearly[Trt=="PILAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(PILPhysGraph,points(Aearly[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=19,cex=1.25,type="o", lty=1))
    with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
                             UAearly[Trt=="PILAND"], Day[Trt=="PILAND"], LAearly[Trt=="PILAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(PILPhysGraph,points(Aearly[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o", lty=2))
    with(PILPhysGraph,arrows(Day[Trt=="PILED"],
                             UAearly[Trt=="PILED"], Day[Trt=="PILED"], LAearly[Trt=="PILED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(PILPhysGraph,points(Aearly[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=19,cex=1.25,type="o"))
    with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
                             UAearly[Trt=="PILEND"], Day[Trt=="PILEND"], LAearly[Trt=="PILEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    mtext(side = 3, text ="E. pilularis", line = 2.5,font=4, cex=1.0)
    mtext(side=3,text="Morning",line=0.5,font=1,cex=1.0)
    text(8.25,27.75, "(b)", pos=4,cex=1,font=2)
    
    #PIL A Late
    par(las=1)
    with(PILPhysGraph,plot(Alate[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,
                           ylim=range(0,29),xlim=range(0,1.05*max(Day)),type="o",lty=2,cex=1.25,
                           yaxt="n",xaxt="n"))
    axis(2,labels=FALSE,tck=0.03)
    axis(1,labels=FALSE,tck=0.03)
    with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
                             UAlate[Trt=="PILAD"], Day[Trt=="PILAD"], LAlate[Trt=="PILAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(PILPhysGraph,points(Alate[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
                             UAlate[Trt=="PILAND"], Day[Trt=="PILAND"], LAlate[Trt=="PILAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(PILPhysGraph,points(Alate[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o", lty=2))
    with(PILPhysGraph,arrows(Day[Trt=="PILED"],
                             UAlate[Trt=="PILED"], Day[Trt=="PILED"], LAlate[Trt=="PILED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(PILPhysGraph,points(Alate[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=19,cex=1.25,type="o"))
    with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
                             UAlate[Trt=="PILEND"], Day[Trt=="PILEND"], LAlate[Trt=="PILEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    legend("topleft",  expression(aC[a]~-~W, aC[a]~-~D,eC[a]~-~W ,eC[a]~-~D),
           cex=1.35,bty="n",
           pch = c(19,1,19,1),lty=c(2,1,2,1), col=c("blue","blue","red","red"))
    mtext(side=3,text="Midday",line=0.5,font=1,cex=1.0)
    text(8.25,27.75, "(c)", pos=4,cex=1,font=2)
    
    #PIL GS Daily
    par(las=1)
    with(PILPhysGraph,plot(gsDaily[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,
                           ylim=range(-0.025,0.45),xlim=range(0,1.05*max(Day)),
                           ylab="",xlab=expression(bold(Day),cex.lab=1)))
    with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
                             UgsDaily[Trt=="PILAD"], Day[Trt=="PILAD"], LgsDaily[Trt=="PILAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(PILPhysGraph,points(gsDaily[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
                             UgsDaily[Trt=="PILAND"], Day[Trt=="PILAND"], LgsDaily[Trt=="PILAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(PILPhysGraph,points(gsDaily[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o", lty=2))
    with(PILPhysGraph,arrows(Day[Trt=="PILED"],
                             UgsDaily[Trt=="PILED"], Day[Trt=="PILED"], LgsDaily[Trt=="PILED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(PILPhysGraph,points(gsDaily[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=19,cex=1.25,type="o"))
    with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
                             UgsDaily[Trt=="PILEND"], Day[Trt=="PILEND"], LgsDaily[Trt=="PILEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    par(las=3)
    mtext(side = 2, text =expression(bold(g[s])), line = 3.75,font=2.5, cex=1.0)
    mtext(side = 2, text =expression(bold((mol~m^-2~s^-1))),
          font=2,cex=0.75, line = 2.25)	
    text(8.25,0.435, "(d)", pos=4,cex=1,font=2)
    
    #PIL GS Early
    par(las=1)
    with(PILPhysGraph,plot(gsearly[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,
                           ylim=range(0,0.45),xlim=range(0,1.05*max(Day)),
                           yaxt="n",xlab=expression(bold(Day))))
    axis(2,labels=FALSE,tck=0.03)
    with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
                             Ugsearly[Trt=="PILAD"], Day[Trt=="PILAD"], Lgsearly[Trt=="PILAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(PILPhysGraph,points(gsearly[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
                             Ugsearly[Trt=="PILAND"], Day[Trt=="PILAND"], Lgsearly[Trt=="PILAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(PILPhysGraph,points(gsearly[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o",lty=2))
    with(PILPhysGraph,arrows(Day[Trt=="PILED"],
                             Ugsearly[Trt=="PILED"], Day[Trt=="PILED"], Lgsearly[Trt=="PILED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(PILPhysGraph,points(gsearly[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=19,cex=1.25,type="o"))
    with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
                             Ugsearly[Trt=="PILEND"], Day[Trt=="PILEND"], Lgsearly[Trt=="PILEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    mtext(side=1,text="DAY",line=2.75,font=2,cex=1.0)
    text(8.25,0.435, "(e)", pos=4,cex=1,font=2)
    
    #PIL GS Late
    par(las=1)
    with(PILPhysGraph,plot(gslate[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,
                           ylim=range(0,0.45),xlim=range(0,1.05*max(Day)),
                           yaxt="n",xlab=expression(bold(Day))))
    axis(2,labels=FALSE,tck=0.03)
    with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
                             Ugslate[Trt=="PILAD"], Day[Trt=="PILAD"], Lgslate[Trt=="PILAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(PILPhysGraph,points(gslate[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
                             Ugslate[Trt=="PILAND"], Day[Trt=="PILAND"], Lgslate[Trt=="PILAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(PILPhysGraph,points(gslate[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o",lty=2))
    with(PILPhysGraph,arrows(Day[Trt=="PILED"],
                             Ugslate[Trt=="PILED"], Day[Trt=="PILED"], Lgslate[Trt=="PILED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(PILPhysGraph,points(gslate[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=19,cex=1.25,type="o"))
    with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
                             Ugslate[Trt=="PILEND"], Day[Trt=="PILEND"], Lgslate[Trt=="PILEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    par(las=3)
    text(8.25,0.435, "(f)", pos=4,cex=1,font=2)
    
    
    dev.off()
    
    
    
}

