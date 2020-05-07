make_transpiration_leaf_area_plot_DT <- function() {
   PilTLA<-read.csv("data/glasshouse2/PILtransp_leafarea.csv",sep=",", header=TRUE)
   PopTLA<-read.csv("data/glasshouse2/POPtransp_leafarea.csv",sep=",", header=TRUE)
   
   ################################### Plotting ######################################
   #FIGURE 5 - Eucalyptus pilularis & populnea (Transpiration as a function of leaf area)
   pdf(paste0(outdir, "F4.transpiration_leaf_area.pdf"), width=10, height=7.5)
   #A
   #bottom,left,top,right
   par(mfrow=c(1,2), omi=c(.65, 0.75, 0.75, .65)) 
   par(mar=c(2, 1, 4, 1))
   par(xaxs="i",yaxs="i")
   #PIL Transpiration vs. leaf area on day 1 of dry down
   par(las=1)
   with(PilTLA,plot(transp_plant[Trt=="PILAD"]~leaf_area[Trt=="PILAD"],col="red",pch=1,cex=1.25,cex.axis=1.25,xaxt="n",ylab="",
                    ylim=range(0,5),xlim=range(0,12)))
   with(PilTLA,points(transp_plant[Trt=="PILAND"]~leaf_area[Trt=="PILAND"],col="blue",pch=1,cex=1.25))
   
   with(PilTLA,points(transp_plant[Trt=="PILED"]~leaf_area[Trt=="PILED"],col="red",pch=16,lty=2,cex=1.25))
   
   with(PilTLA,points(transp_plant[Trt=="PILEND"]~leaf_area[Trt=="PILEND"],col="blue",pch=16,cex=1.25))
   
   par(las=3)
   mtext(side = 2, text =expression(bold(Transpiration~~(l~d^-1))),
         font=2,cex=1.0, line = 3)	
   par(las=1)
   mtext(side=1,text=expression(bold(Leaf~area~(m^2))),line=3,cex=1.25)
   axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
   title(main="Eucalyptus pilularis",  font.main=4,cex.main=1.1,line=0.5)
   legend("topleft",  expression(aC[a]~-~W,aC[a]~-~D,eC[a]~-~W,eC[a]~-~D),
          cex=1.025,bty="n",
          pch=c(1,1,16,16), col=c("blue","red","blue","red"))
   
   #POP Transpiration vs. leaf area on day 1 of dry down
   par(mar=c(2, 1, 4, 1))
   par(las=1)
   par(xaxs="i",yaxs="i")
   with(PopTLA,plot(transp_plant[Trt=="POPAD"]~leaf_area[Trt=="POPAD"],col="red",pch=1,cex=1.25,cex.axis=1.25,xaxt="n",ylab="",yaxt="n",
                    ylim=range(0,5),xlim=range(0,2)))
   
   with(PopTLA,points(transp_plant[Trt=="POPAND"]~leaf_area[Trt=="POPAND"],col="blue",pch=1,cex=1.25))
   
   with(PopTLA,points(transp_plant[Trt=="POPED"]~leaf_area[Trt=="POPED"],col="red",pch=16,lty=2,cex=1.25))
   
   with(PopTLA,points(transp_plant[Trt=="POPEND"]~leaf_area[Trt=="POPEND"],col="blue",pch=16,cex=1.25))
   
   par(las=3)
   mtext(side = 4, text =expression(bold(Transpiration~~(l~d^-1))),
         font=2,cex=1.0, line = 3)	
   par(las=1)
   axis(4,labels=TRUE,tck=-0.03,cex.axis=1.25)
   par(las=1)
   mtext(side=1,text=expression(bold(Leaf~area~(m^2))),line=3,cex=1.25)
   axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
   title(main="Eucalyptus populnea",  font.main=4,cex.main=1.1,line=0.5)
   
   dev.off()
}

