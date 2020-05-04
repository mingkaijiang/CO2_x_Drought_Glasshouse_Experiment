make_whole_plant_hydraulic_conductance_plot <- function() {
    #Eucalyptus pilularis
    PILPhys<-read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv",sep=",", header=TRUE)
    #Eucalyptus populnea
    POPPhys<-read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv",sep=",", header=TRUE)
    
    
    ################################### Plotting ######################################
    #FIGURE 7 - Eucalyptus pilularis & Eucalyptus populnea (E/(psiPD-psiMD) by Day)
    pdf("output/whole_plant_hydraulics_plot.pdf", width=8, height=6)
    #A
    #bottom,left,top,right
    par(mfrow=c(1,2), omi=c(.65, 0.75, 0.75, .65)) 
    par(mar=c(2, 1, 4, 0.5))
    par(xaxs="i",yaxs="i")
    
    #PIL E_psi Daily (All points)
    par(las=1)
    with(PILPhys,plot(E_psi[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex.axis=1.25,
                      ylim=range(0,12),xlim=range(0,1.05*max(Day)),
                      ylab="",xaxt="n",cex.lab=1,cex=1.25))
    axis(1,labels=TRUE,tck=-0.02,cex.lab=1,cex.axis=1.25,cex=1.25)
    with(PILPhys,points(E_psi[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=16,cex=1.25))
    with(PILPhys,points(E_psi[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25))
    with(PILPhys,points(E_psi[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=16,cex=1.25))
    par(las=3)
    mtext(side = 2, text =expression(bold(E/psi[pd]-psi[md]~~(liter/MPa))),
          font=2,cex=1.0, line = 3)	
    par(las=1)
    mtext(side=1,text=expression(bold(Day)),line=3,cex=1.25)
    axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
    title(main="Eucalyptus pilularis",  font.main=4,cex.main=1.1,line=0.5)
    legend("topleft",  expression(aC[a]~-~W,aC[a]~-~D,eC[a]~-~W,eC[a]~-~D),
           cex=1.025,bty="n",
           pch=c(16,1,16,1), col=c("blue","blue","red","red"))
    
    #POP E_psi Daily (All points)
    par(mar=c(2, 0.5, 4, 1))
    par(las=1)
    with(POPPhys,plot(E_psi[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex.axis=1.25,
                      ylim=range(0,7),xlim=range(0,1.05*max(Day)),
                      ylab="",yaxt="n",xaxt="n",cex.lab=1,cex=1.25))
    axis(1,labels=TRUE,tck=-0.02,cex.lab=1,cex.axis=1.25,cex=1.25)
    with(POPPhys,points(E_psi[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=16,cex=1.25))
    with(POPPhys,points(E_psi[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25))
    with(POPPhys,points(E_psi[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=16,cex=1.25))
    par(las=3)
    mtext(side = 4, text =expression(bold(E/psi[pd]-psi[md]~~(liter/MPa))),
          font=2,cex=1.0, line = 3)	
    par(las=1)
    axis(4,labels=TRUE,tck=-0.03,cex.axis=1.25)
    par(las=1)
    mtext(side=1,text=expression(bold(Day)),line=3,cex=1.25)
    axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
    title(main="Eucalyptus populnea",  font.main=4,cex.main=1.1,line=0.5)
    
    dev.off()
}



