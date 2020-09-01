

#FIGURES (Eucalyptus populnea)
POPPhysGraph<-read.csv("data/Populnea_Phys2.csv",sep=",", header=TRUE)
names(POPPhysGraph)
str(POPPhysGraph)

pdf("output/original_populnea_Figure4_test2.pdf")

#FIGURE 4 - Eucalyptus populnea (Pre-Dawn and Midday LWP, SWC and Daily Transpiration)
par(mfrow=c(2,2), omi=c(.65, 0.75, 0.75, .65)) 
par(mar=c(0, 0, 0, 0))
par(xaxs="i",yaxs="i")

#POP pre-dawn leaf water potential
par(las=1)
with(POPPhysGraph,plot(psiPD[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,xaxt="n",
ylim=range(-4.5,0),xlim=range(0,1.05*max(Day))))
axis(1,labels=FALSE,tck=0.03)
with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
 UpsiPD[Trt=="POPAD"], Day[Trt=="POPAD"], LpsiPD[Trt=="POPAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(POPPhysGraph,points(psiPD[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=16,type="o",cex=1.25))
with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
UpsiPD[Trt=="POPAND"], Day[Trt=="POPAND"], LpsiPD[Trt=="POPAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(POPPhysGraph,points(psiPD[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,type="o",lty=2,cex=1.25))
with(POPPhysGraph,arrows(Day[Trt=="POPED"],
UpsiPD[Trt=="POPED"], Day[Trt=="POPED"], LpsiPD[Trt=="POPED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(POPPhysGraph,points(psiPD[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=16,type="o",cex=1.25))
with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
UpsiPD[Trt=="POPEND"], Day[Trt=="POPEND"], LpsiPD[Trt=="POPEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 2, text =expression(bold(psi[pd]~~(MPa))),
font=2,cex=1.0, line = 3)	
par(las=1)
mtext(text="Eucalyptus populnea", font=4,outer=TRUE, line=0.05) 
text(36.5,-0.15, "(a)", pos=4,cex=1,font=2)

#POP midday leaf water potential
par(las=1)
with(POPPhysGraph,plot(psiMD[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,
ylim=range(-4.5,0),xlim=range(0,1.05*max(Day)),yaxt="n",xaxt="n",
ylab="",xlab=""))
axis(4,labels=FALSE,tck=-0.03)
axis(1,labels=FALSE,tck=0.03)
with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
UpsiMD[Trt=="POPAD"], Day[Trt=="POPAD"], LpsiMD[Trt=="POPAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(POPPhysGraph,points(psiMD[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=16,cex=1.25,type="o"))
with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
UpsiMD[Trt=="POPAND"], Day[Trt=="POPAND"], LpsiMD[Trt=="POPAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(POPPhysGraph,points(psiMD[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o",lty=2))
with(POPPhysGraph,arrows(Day[Trt=="POPED"],
UpsiMD[Trt=="POPED"], Day[Trt=="POPED"], LpsiMD[Trt=="POPED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(POPPhysGraph,points(psiMD[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=16,cex=1.25,type="o"))
with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
UpsiMD[Trt=="POPEND"], Day[Trt=="POPEND"], LpsiMD[Trt=="POPEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 4, text =expression(bold(psi[md]~~(MPa))),
font=2,cex=1.0, line = 2.0)	
legend("topright",  expression(aC[a]~-~W, aC[a]~-~D,eC[a]~-~W ,eC[a]~-~D),
cex=1.35,bty="n",
 pch = c(16,1,16,1),lty=c(1,2,1,2), col=c("blue","blue","red","red"))
 text(-0.15,-0.15, "(b)", pos=4,cex=1,font=2)
 
#POP soil water content
par(las=1)
with(POPPhysGraph,plot(SWC[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,
ylim=range(1.05*max(SWC),0.00),xlim=range(0,1.05*max(Day)),tck=-0.03,cex.axis=1.25,
ylab="",xlab="",xaxt="n"))
with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
 USWC[Trt=="POPAD"], Day[Trt=="POPAD"], LSWC[Trt=="POPAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(POPPhysGraph,points(SWC[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=16,cex=1.25,type="o"))
with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
USWC[Trt=="POPAND"], Day[Trt=="POPAND"], LSWC[Trt=="POPAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(POPPhysGraph,points(SWC[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o",lty=2))
with(POPPhysGraph,arrows(Day[Trt=="POPED"],
USWC[Trt=="POPED"], Day[Trt=="POPED"], LSWC[Trt=="POPED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(POPPhysGraph,points(SWC[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=16,cex=1.25,type="o"))
with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
USWC[Trt=="POPEND"], Day[Trt=="POPEND"], LSWC[Trt=="POPEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 2, text =expression(bold(s.w.c.~(m^3~m^-3))),
font=2,cex=1, line = 3)	
par(las=1)
axis(1,labels=TRUE,tck=0.03,cex.axis=1.25)
mtext(side=1,text=expression(bold(DAY)),line=2.5,font=2.5,cex=1.0)	  
text(2,0.33, "(c)", pos=4,cex=1,font=2)

#POP daily transpiration
par(las=1)
with(POPPhysGraph,plot(transp_plant[Trt=="POPAD"]~Day[Trt=="POPAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,
ylim=range(0,1.1*max(transp_plant)),xlim=range(0,1.05*max(Day)),yaxt="n",cex.axis=1.25,tck=0.03,
ylab="",xlab="",xaxt="n"))
with(POPPhysGraph,arrows(Day[Trt=="POPAD"],
 Utransp_plant[Trt=="POPAD"], Day[Trt=="POPAD"], Ltransp_plant[Trt=="POPAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(POPPhysGraph,points(transp_plant[Trt=="POPAND"]~Day[Trt=="POPAND"],col="blue",pch=16,cex=1.25,type="o"))
with(POPPhysGraph,arrows(Day[Trt=="POPAND"],
Utransp_plant[Trt=="POPAND"], Day[Trt=="POPAND"], Ltransp_plant[Trt=="POPAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(POPPhysGraph,points(transp_plant[Trt=="POPED"]~Day[Trt=="POPED"],col="red",pch=1,cex=1.25,type="o",lty=2))
with(POPPhysGraph,arrows(Day[Trt=="POPED"],
Utransp_plant[Trt=="POPED"], Day[Trt=="POPED"], Ltransp_plant[Trt=="POPED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(POPPhysGraph,points(transp_plant[Trt=="POPEND"]~Day[Trt=="POPEND"],col="red",pch=16,cex=1.25,type="o"))
with(POPPhysGraph,arrows(Day[Trt=="POPEND"],
Utransp_plant[Trt=="POPEND"], Day[Trt=="POPEND"], Ltransp_plant[Trt=="POPEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 4, text =expression(bold(Transpiration~~(l~~d^-1))), line = 2.5,font=2.5, cex=1)
par(las=1)
axis(4,labels=TRUE,tck=-.03,cex.axis=1.25)
axis(1,labels=TRUE,tck=0.03,cex.axis=1.25)
mtext(side=1,text=expression(bold(DAY)),line=2.5,font=2.5,cex=1.0)
text(35,2.625, "(d)", pos=4,cex=1,font=2) 

dev.off()

