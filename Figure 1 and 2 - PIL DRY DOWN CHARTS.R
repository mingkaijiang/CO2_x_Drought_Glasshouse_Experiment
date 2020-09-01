
#FIGURES (Eucalyptus pilularis)
PILPhysGraph<-read.csv("data/Pilularis_Phys2.csv",sep=",", header=TRUE)
names(PILPhysGraph)
str(PILPhysGraph)

pdf("output/original_pilularis_Figure2_test2.pdf")

#FIGURE 2 - Eucalyptus pilularis (Pre-Dawn and Midday LWP, SWC and Daily Transpiration)
windows(12.5,10)
par(mfrow=c(2,2), omi=c(.65, 0.75, 0.75, .65)) 
par(mar=c(0, 0, 0, 0))
par(xaxs="i",yaxs="i")

#PIL pre-dawn leaf water potential
par(las=1)
with(PILPhysGraph,plot(psiPD[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,cex.axis=1.25,xaxt="n",
ylim=range(-3,0),xlim=range(0,1.05*max(Day))))
axis(1,labels=FALSE,tck=0.03)
with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
 UpsiPD[Trt=="PILAD"], Day[Trt=="PILAD"], LpsiPD[Trt=="PILAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(PILPhysGraph,points(psiPD[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=16,type="o",cex=1.25))
with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
UpsiPD[Trt=="PILAND"], Day[Trt=="PILAND"], LpsiPD[Trt=="PILAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(PILPhysGraph,points(psiPD[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,type="o",lty=2,cex=1.25))
with(PILPhysGraph,arrows(Day[Trt=="PILED"],
UpsiPD[Trt=="PILED"], Day[Trt=="PILED"], LpsiPD[Trt=="PILED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(PILPhysGraph,points(psiPD[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=16,type="o",cex=1.25))
with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
UpsiPD[Trt=="PILEND"], Day[Trt=="PILEND"], LpsiPD[Trt=="PILEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 2, text =expression(bold(psi[pd]~~(MPa))),
font=2,cex=1.0, line = 3)	
par(las=1)
mtext(text="Eucalyptus pilularis", font=4,outer=TRUE, line=0.05) 
text(-0.15,-0.15, "(a)", pos=4,cex=1,font=2)

#PIL midday leaf water potential
par(las=1)
with(PILPhysGraph,plot(psiMD[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,
ylim=range(-3,0),xlim=range(0,1.05*max(Day)),yaxt="n",xaxt="n",
ylab="",xlab=""))
axis(4,labels=FALSE,tck=-0.03)
axis(1,labels=FALSE,tck=0.03)
with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
UpsiMD[Trt=="PILAD"], Day[Trt=="PILAD"], LpsiMD[Trt=="PILAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(PILPhysGraph,points(psiMD[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=16,cex=1.25,type="o"))
with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
UpsiMD[Trt=="PILAND"], Day[Trt=="PILAND"], LpsiMD[Trt=="PILAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(PILPhysGraph,points(psiMD[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o",lty=2))
with(PILPhysGraph,arrows(Day[Trt=="PILED"],
UpsiMD[Trt=="PILED"], Day[Trt=="PILED"], LpsiMD[Trt=="PILED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(PILPhysGraph,points(psiMD[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=16,cex=1.25,type="o"))
with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
UpsiMD[Trt=="PILEND"], Day[Trt=="PILEND"], LpsiMD[Trt=="PILEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 4, text =expression(bold(psi[md]~~(MPa))),
font=2,cex=1.0, line = 2.0)	
legend("topright",  expression(aC[a]~-~W, aC[a]~-~D,eC[a]~-~W ,eC[a]~-~D),
cex=1.35,bty="n",
 pch = c(16,1,16,1),lty=c(1,2,1,2), col=c("blue","blue","red","red"))
 text(-0.15,-0.15, "(b)", pos=4,cex=1,font=2)
 
#PIL soil water content
par(las=1)
with(PILPhysGraph,plot(SWC[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,
ylim=range(1.1*max(SWC),0.05),xlim=range(0,1.05*max(Day)),tck=0.03,cex.axis=1.25,
ylab="",xlab="",xaxt="n"))
with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
 USWC[Trt=="PILAD"], Day[Trt=="PILAD"], LSWC[Trt=="PILAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(PILPhysGraph,points(SWC[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=16,cex=1.25,type="o"))
with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
USWC[Trt=="PILAND"], Day[Trt=="PILAND"], LSWC[Trt=="PILAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(PILPhysGraph,points(SWC[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o",lty=2))
with(PILPhysGraph,arrows(Day[Trt=="PILED"],
USWC[Trt=="PILED"], Day[Trt=="PILED"], LSWC[Trt=="PILED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(PILPhysGraph,points(SWC[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=16,cex=1.25,type="o"))
with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
USWC[Trt=="PILEND"], Day[Trt=="PILEND"], LSWC[Trt=="PILEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 2, text =expression(bold(s.w.c.~(m^3~m^-3))),
font=2,cex=1, line = 3)	
par(las=1)
axis(1,labels=TRUE,tck=0.03,cex.axis=1.25)
mtext(side=1,text=expression(bold(DAY)),line=2.5,font=2.5,cex=1.0)	  
text(-0.15,0.3275, "(c)", pos=4,cex=1,font=2)

#PIL daily transpiration
par(las=1)
with(PILPhysGraph,plot(transp_plant[Trt=="PILAD"]~Day[Trt=="PILAD"],col="blue",pch=1,cex=1.25,type="o",lty=2,
ylim=range(0,1.1*max(transp_plant)),xlim=range(0,1.05*max(Day)),yaxt="n",cex.axis=1.25,tck=0.03,
ylab="",xlab="",xaxt="n"))
with(PILPhysGraph,arrows(Day[Trt=="PILAD"],
 Utransp_plant[Trt=="PILAD"], Day[Trt=="PILAD"], Ltransp_plant[Trt=="PILAD"]
 , length = .035, angle = 90, code = 3,col="blue"))        
with(PILPhysGraph,points(transp_plant[Trt=="PILAND"]~Day[Trt=="PILAND"],col="blue",pch=16,cex=1.25,type="o"))
with(PILPhysGraph,arrows(Day[Trt=="PILAND"],
Utransp_plant[Trt=="PILAND"], Day[Trt=="PILAND"], Ltransp_plant[Trt=="PILAND"]
 , length = .035, angle = 90, code = 3,col="blue")) 
with(PILPhysGraph,points(transp_plant[Trt=="PILED"]~Day[Trt=="PILED"],col="red",pch=1,cex=1.25,type="o",lty=2))
with(PILPhysGraph,arrows(Day[Trt=="PILED"],
Utransp_plant[Trt=="PILED"], Day[Trt=="PILED"], Ltransp_plant[Trt=="PILED"]
 , length = .035, angle = 90, code = 3,col="red")) 
with(PILPhysGraph,points(transp_plant[Trt=="PILEND"]~Day[Trt=="PILEND"],col="red",pch=16,cex=1.25,type="o"))
with(PILPhysGraph,arrows(Day[Trt=="PILEND"],
Utransp_plant[Trt=="PILEND"], Day[Trt=="PILEND"], Ltransp_plant[Trt=="PILEND"]
 , length = .035, angle = 90, code = 3,col="red")) 
par(las=3)
mtext(side = 4, text =expression(bold(Transpiration~~(l~~d^-1))), line = 2.5,font=2.5, cex=1)
par(las=1)
axis(4,labels=TRUE,tck=-.03,cex.axis=1.25)
axis(1,labels=TRUE,tck=0.03,cex.axis=1.25)
mtext(side=1,text=expression(bold(DAY)),line=2.5,font=2.5,cex=1.0)
text(-0.15,4.05, "(d)", pos=4,cex=1,font=2) 

dev.off()
