make_populnea_physiological_plots <- function() {

    #FIGURES (Eucalyptus populnea)
    POPPhysGraph<-read.csv("data/glasshouse2/Populnea_Phys.csv",sep=",", header=TRUE)
    
    n2 <- min(unique(POPPhysGraph[POPPhysGraph$n.1<6,]$Day))
    
    POPPhysGraph <- POPPhysGraph[POPPhysGraph$Day<n2, ]

    
    ################################### Plotting ######################################
    #FIGURE 3 - Eucalyptus populnea (Daily, morning and midday Asat & gs)
    pdf(paste0(outdir, "F8.2.Pop_gas_exchange.pdf"), width=8, height=6)
    #par(mfrow=c(2,3))
    #bottom,left,top,right
    par(mfrow=c(2,3), omi=c(.5, 0.75, 0.75, .5)) 
    par(mar=c(0, 0, 0, 0))
    par(xaxs="i",yaxs="i")
    #POP A Daily
    par(las=1)
    with(POPPhysGraph,plot(Adaily[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex.axis=1.25,
                           ylim=range(0,29),xlim=range(0,1.05*max(Day)),
                           ylab="",xaxt="n",cex.lab=1,cex=1.25,type="o",lty=2))
    axis(1,labels=FALSE,tck=0.03)
    with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
                             UAdaily[Trt=="POPAD"], Day[Trt=="POPAD"], LAdaily[Trt=="POPAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(POPPhysGraph,points(Adaily[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
                             UAdaily[Trt=="POPAND"], Day[Trt=="POPAND"], LAdaily[Trt=="POPAND"]
                             , length = .035, angle = 90, code = 3, col="blue")) 
    with(POPPhysGraph,points(Adaily[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o", lty=2))
    with(POPPhysGraph,arrows(Day[Trt=="POPED"],
                             UAdaily[Trt=="POPED"], Day[Trt=="POPED"], LAdaily[Trt=="POPED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(POPPhysGraph,points(Adaily[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=19,cex=1.25,type="o"))
    with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
                             UAdaily[Trt=="POPEND"], Day[Trt=="POPEND"], LAdaily[Trt=="POPEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    par(las=3)
    mtext(side = 2, text =expression(bold(A[sat])), line = 3.75,font=2.5, cex=1.0)
    mtext(side = 2, text =expression(bold((mu~mol~m^-2~s^-1))),
          font=2,cex=0.75, line = 2.25)	
    par(las=1)
    mtext(side=3,text="Daily",line=0.5,font=1,cex=1.0)
    text(0.25,27.75, "(a)", pos=4,cex=1,font=2)
    
    #POP A Early
    par(las=1)
    with(POPPhysGraph,plot(Aearly[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,
                           ylim=range(0,29),xlim=range(0,1.05*max(Day)),
                           xaxt="n",yaxt="n",cex=1.25,type="o",lty=2))
    axis(2,labels=FALSE,tck=0.03)
    axis(1,labels=FALSE,tck=0.03)
    with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
                             UAearly[Trt=="POPAD"], Day[Trt=="POPAD"], LAearly[Trt=="POPAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(POPPhysGraph,points(Aearly[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
                             UAearly[Trt=="POPAND"], Day[Trt=="POPAND"], LAearly[Trt=="POPAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(POPPhysGraph,points(Aearly[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o", lty=2))
    with(POPPhysGraph,arrows(Day[Trt=="POPED"],
                             UAearly[Trt=="POPED"], Day[Trt=="POPED"], LAearly[Trt=="POPED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(POPPhysGraph,points(Aearly[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=19,cex=1.25,type="o"))
    with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
                             UAearly[Trt=="POPEND"], Day[Trt=="POPEND"], LAearly[Trt=="POPEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    mtext(side = 3, text ="E. populnea", line = 2.5,font=4, cex=1.0)
    mtext(side=3,text="Morning",line=0.5,font=1,cex=1.0)
    text(0.25,27.75, "(b)", pos=4,cex=1,font=2)
    
    #POP A Late
    par(las=1)
    with(POPPhysGraph,plot(Alate[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,
                           ylim=range(0,29),xlim=range(0,1.05*max(Day)),type="o",lty=2,cex=1.25,
                           yaxt="n",xaxt="n"))
    axis(2,labels=FALSE,tck=0.03)
    axis(1,labels=FALSE,tck=0.03)
    with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
                             UAlate[Trt=="POPAD"], Day[Trt=="POPAD"], LAlate[Trt=="POPAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(POPPhysGraph,points(Alate[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
                             UAlate[Trt=="POPAND"], Day[Trt=="POPAND"], LAlate[Trt=="POPAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(POPPhysGraph,points(Alate[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o",lty=2))
    with(POPPhysGraph,arrows(Day[Trt=="POPED"],
                             UAlate[Trt=="POPED"], Day[Trt=="POPED"], LAlate[Trt=="POPED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(POPPhysGraph,points(Alate[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=19,cex=1.25,type="o"))
    with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
                             UAlate[Trt=="POPEND"], Day[Trt=="POPEND"], LAlate[Trt=="POPEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    legend("topright",  expression(aC[a]~-~W, aC[a]~-~D,eC[a]~-~W ,eC[a]~-~D),
           cex=1.35,bty="n",
           pch = c(19,1,19,1),lty=c(2,1,2,1), col=c("blue","blue","red","red"))
    mtext(side=3,text="Midday",line=0.5,font=1,cex=1.0)
    text(0.25,27.75, "(c)", pos=4,cex=1,font=2)
    
    #POP GS Daily
    par(las=1)
    with(POPPhysGraph,plot(gsDaily[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,
                           ylim=range(-0.025,0.45),xlim=range(0,1.05*max(Day)),
                           ylab="",xlab=expression(bold(Day),cex.lab=1)))
    with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
                             UgsDaily[Trt=="POPAD"], Day[Trt=="POPAD"], LgsDaily[Trt=="POPAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(POPPhysGraph,points(gsDaily[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
                             UgsDaily[Trt=="POPAND"], Day[Trt=="POPAND"], LgsDaily[Trt=="POPAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(POPPhysGraph,points(gsDaily[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o", lty=2))
    with(POPPhysGraph,arrows(Day[Trt=="POPED"],
                             UgsDaily[Trt=="POPED"], Day[Trt=="POPED"], LgsDaily[Trt=="POPED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(POPPhysGraph,points(gsDaily[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=19,cex=1.25,type="o"))
    with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
                             UgsDaily[Trt=="POPEND"], Day[Trt=="POPEND"], LgsDaily[Trt=="POPEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    par(las=3)
    mtext(side = 2, text =expression(bold(g[s])), line = 3.75,font=2.5, cex=1.0)
    mtext(side = 2, text =expression(bold((mol~m^-2~s^-1))),
          font=2,cex=0.75, line = 2.25)	
    text(0.25,0.435, "(d)", pos=4,cex=1,font=2)
    
    #POP GS Early
    par(las=1)
    with(POPPhysGraph,plot(gsearly[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,
                           ylim=range(0,0.45),xlim=range(0,1.05*max(Day)),
                           yaxt="n",xlab=expression(bold(Day))))
    axis(2,labels=FALSE,tck=0.03)
    with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
                             Ugsearly[Trt=="POPAD"], Day[Trt=="POPAD"], Lgsearly[Trt=="POPAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(POPPhysGraph,points(gsearly[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
                             Ugsearly[Trt=="POPAND"], Day[Trt=="POPAND"], Lgsearly[Trt=="POPAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(POPPhysGraph,points(gsearly[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o",lty=2))
    with(POPPhysGraph,arrows(Day[Trt=="POPED"],
                             Ugsearly[Trt=="POPED"], Day[Trt=="POPED"], Lgsearly[Trt=="POPED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(POPPhysGraph,points(gsearly[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=19,cex=1.25,type="o"))
    with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
                             Ugsearly[Trt=="POPEND"], Day[Trt=="POPEND"], Lgsearly[Trt=="POPEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    mtext(side=1,text="DAY",line=2.75,font=2,cex=1.0)
    text(0.25,0.435, "(e)", pos=4,cex=1,font=2)
    
    #POP GS Late
    par(las=1)
    with(POPPhysGraph,plot(gslate[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,
                           ylim=range(0,0.45),xlim=range(0,1.05*max(Day)),
                           yaxt="n",xlab=expression(bold(Day))))
    axis(2,labels=FALSE,tck=0.03)
    with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
                             Ugslate[Trt=="POPAD"], Day[Trt=="POPAD"], Lgslate[Trt=="POPAD"]
                             , length = .035, angle = 90, code = 3,col="blue"))        
    with(POPPhysGraph,points(gslate[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=19,cex=1.25,type="o",lty=1))
    with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
                             Ugslate[Trt=="POPAND"], Day[Trt=="POPAND"], Lgslate[Trt=="POPAND"]
                             , length = .035, angle = 90, code = 3,col="blue")) 
    with(POPPhysGraph,points(gslate[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o",lty=2))
    with(POPPhysGraph,arrows(Day[Trt=="POPED"],
                             Ugslate[Trt=="POPED"], Day[Trt=="POPED"], Lgslate[Trt=="POPED"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    with(POPPhysGraph,points(gslate[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=19,cex=1.25,type="o"))
    with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
                             Ugslate[Trt=="POPEND"], Day[Trt=="POPEND"], Lgslate[Trt=="POPEND"]
                             , length = .035, angle = 90, code = 3,col="red")) 
    par(las=3)
    text(0.25,0.435, "(f)", pos=4,cex=1,font=2)
    
    dev.off()
    
    
    
}