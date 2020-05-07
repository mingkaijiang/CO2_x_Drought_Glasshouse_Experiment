make_predawn_water_potential_with_swc_plots <- function() {
   ### read in data
   #PILPhys<-read.csv("data/glasshouse2/Drydown_gasexchange_pilularis.csv",sep=",", header=TRUE)
   #POPPhys<-read.csv("data/glasshouse2/Drydown_gasexchange_populnea.csv",sep=",", header=TRUE)
   
   
   ################################### Plotting ######################################
   
   ################################### Plotting ######################################
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
   
   # Eucalyptus pilularis & populnea (Pre-dawn leaf water potential (mean & SE) vs. Soil water content)
   pdf(paste0(outdir, "F6.predawn_leaf_water_potential_vs_swc_with_se.pdf"), 
       width=8, height=6)
   #A
   #bottom,left,top,right
   par(mfrow=c(1,2), omi=c(.65, 0.75, 0.75, .65)) 
   par(mar=c(2, 1, 4, 0.5))
   par(xaxs="i",yaxs="i")
   #PIL pre-dawn leaf water potential vs. soil water content
   par(las=1)
   with(PILPhysGraph,plot(psiPD[Trt=="PILAD"]~SWC[Trt=="PILAD"],col="red",pch=1,cex=1.25,cex.axis=1.25,xaxt="n",ylab="",
                          type="o", lty=2,ylim=range(-3,0.05),xlim=range(-0.025,1.025*max(SWC))))
   with(PILPhysGraph,arrows(SWC[Trt=="PILAD"],
                            UpsiPD[Trt=="PILAD"], SWC[Trt=="PILAD"], LpsiPD[Trt=="PILAD"]
                            , length = .035, angle = 90, code = 3,col="red"))
   with(PILPhysGraph,points(psiPD[Trt=="PILAND"]~SWC[Trt=="PILAND"],col="blue",pch=1,cex=1.25,type="o", lty=2))
   with(PILPhysGraph,arrows(SWC[Trt=="PILAND"],
                            UpsiPD[Trt=="PILAND"], SWC[Trt=="PILAND"], LpsiPD[Trt=="PILAND"]
                            , length = .035, angle = 90, code = 3,col="blue")) 
   with(PILPhysGraph,points(psiPD[Trt=="PILED"]~SWC[Trt=="PILED"],col="red",pch=16,lty=1,cex=1.25,type="o"))
   with(PILPhysGraph,arrows(SWC[Trt=="PILED"],
                            UpsiPD[Trt=="PILED"], SWC[Trt=="PILED"], LpsiPD[Trt=="PILED"]
                            , length = .035, angle = 90, code = 3,col="red")) 
   with(PILPhysGraph,points(psiPD[Trt=="PILEND"]~SWC[Trt=="PILEND"],col="blue",pch=16,cex=1.25,type="o"))
   with(PILPhysGraph,arrows(SWC[Trt=="PILEND"],
                            UpsiPD[Trt=="PILEND"], SWC[Trt=="PILEND"], LpsiPD[Trt=="PILEND"]
                            , length = .035, angle = 90, code = 3,col="blue")) 
   par(las=3)
   mtext(side = 2, text =expression(bold(psi[pd]~~(MPa))),
         font=2,cex=1.0, line = 3)	
   par(las=1)
   mtext(side=1,text=expression(bold(s.w.c.~(m^3~m^-3))),line=3,cex=1.25)
   axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
   title(main="E. pilularis",  font.main=4,cex.main=1.1,line=0.5)
   legend("topleft",  expression(aC[a]~-~W,aC[a]~-~D,eC[a]~-~W,eC[a]~-~D),
          cex=1.025,bty="n",
          pch=c(1,1,16,16), col=c("blue","red","blue","red"))
   
   #POP pre-dawn leaf water potential vs. soil water content
   par(mar=c(2, 0.5, 4, 1))
   par(las=1)
   with(POPPhysGraph,plot(psiPD[Trt=="POPAD"]~SWC[Trt=="POPAD"],col="red",pch=1,cex=1.25,cex.axis=1.25,xaxt="n",ylab="",yaxt="n",
                          type="o",lty=2,ylim=range(-4.25,0.05),xlim=range(-0.025,1.025*max(SWC))))
   with(POPPhysGraph,arrows(SWC[Trt=="POPAD"],
                            UpsiPD[Trt=="POPAD"], SWC[Trt=="POPAD"], LpsiPD[Trt=="POPAD"]
                            , length = .035, angle = 90, code = 3,col="red"))
   with(POPPhysGraph,points(psiPD[Trt=="POPAND"]~SWC[Trt=="POPAND"],col="blue",pch=1,cex=1.25,type="o", lty=2))
   with(POPPhysGraph,arrows(SWC[Trt=="POPAND"],
                            UpsiPD[Trt=="POPAND"], SWC[Trt=="POPAND"], LpsiPD[Trt=="POPAND"]
                            , length = .035, angle = 90, code = 3,col="blue")) 
   with(POPPhysGraph,points(psiPD[Trt=="POPED"]~SWC[Trt=="POPED"],col="red",pch=16,lty=2,cex=1.25,type="o"))
   with(POPPhysGraph,arrows(SWC[Trt=="POPED"],
                            UpsiPD[Trt=="POPED"], SWC[Trt=="POPED"], LpsiPD[Trt=="POPED"]
                            , length = .035, angle = 90, code = 3,col="red")) 
   with(POPPhysGraph,points(psiPD[Trt=="POPEND"]~SWC[Trt=="POPEND"],col="blue",pch=16,cex=1.25,type="o"))
   with(POPPhysGraph,arrows(SWC[Trt=="POPEND"],
                            UpsiPD[Trt=="POPEND"], SWC[Trt=="POPEND"], LpsiPD[Trt=="POPEND"]
                            , length = .035, angle = 90, code = 3,col="blue"))
   par(las=3)
   mtext(side = 4, text =expression(bold(psi[pd]~~(MPa))),
         font=2,cex=1.0, line = 3)	
   par(las=1)
   axis(4,labels=TRUE,tck=-0.03,cex.axis=1.25)
   par(las=1)
   mtext(side=1,text=expression(bold(s.w.c.~(m^3~m^-3))),line=3,cex=1.25)
   axis(1,labels=TRUE,tck=-0.03,cex.axis=1.25)
   title(main="E. populnea",  font.main=4,cex.main=1.1,line=0.5)
   
   dev.off()
   
}


