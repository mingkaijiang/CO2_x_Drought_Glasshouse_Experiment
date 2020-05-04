adderrorbars <- function(x,y,SE,direction,barlen=0.075,...){
    if(direction=="up")arrows(x0=x, x1=x, y0=y, y1=y + SE, code=2, 
angle=90, length=barlen,...)
    if(direction=="down")arrows(x0=x, x1=x, y0=y, y1=y - SE, code=2, 
angle=90, length=barlen,...)
    if(direction=="left")arrows(x0=x, x1=x-SE, y0=y, y1=y, code=2, 
angle=90, length=barlen,...)
    if(direction=="right")arrows(x0=x, x1=x+SE, y0=y, y1=y, code=2, 
angle=90, length=barlen,...)

if(direction=="updown"){
    arrows(x0=x, x1=x, y0=y, y1=y + SE, code=2, 
angle=90, length=barlen,...)
    arrows(x0=x, x1=x, y0=y, y1=y - SE, code=2, 
angle=90, length=barlen,...)
}

}
windows(width=8, height=4) #, pointsize=18)
#PIL BIO
par(mfrow=c(1,2))
#PIL BIO
setwd("C:/Users/jeffwk9/Desktop/Australia Paper 2")
PIL<-read.csv("PIL_LA.csv",sep=",", header=TRUE)
#bottom,left,top and right
par(mar=c(5,5,2,2)) 
par(las=3)

bp <- with(PIL, barplot(LA,col=c("blue","red","white","white"),yaxt="n",
border=c("black","black","blue","red"),ylim=c(0,1.1*max(LA)),
names.arg=c("380","700","380","700"),cex.names=1,
ylab=expression(bold(Leaf~~area~~(m^2))),font.lab=2,cex.lab=1.25))
with(PIL,adderrorbars(x=bp, y=LA, SE=LAE,direction="updown"))
par(las=1)
axis(2,labels=TRUE,tcl=-0.5,cex.axis=1)
axis(1,labels=FALSE,tcl=-0.5,font=1,cex.axis=1)
title(main="Eucalyptus pilularis",  font.main=4,cex.main=1.25)
text(0.05,2900, "(a)", pos=4,cex=1.5,font=2)



par(las=1)

names<-"Well-watered"
name<-"Droughted"
mtext(names,1,3,adj=0.15,font=2, cex= 0.9)
mtext(name,1,3,adj=0.85,font=2, cex= 0.9)

#POP LA
setwd("C:/Users/jeffwk9/Desktop/Australia Paper 2")
POP<-read.csv("POP_LA.csv",sep=",", header=TRUE)
#bottom,left,top and right
par(mar=c(5,2,2,5)) 
par(las=3)
bp <- with(POP, barplot(LA,col=c("blue","red","white","white"),yaxt="n",
border=c("black","black","blue","red"),ylim=c(0,1.5*max(LA)),
names.arg=c("380","700","380","700"),cex.name=1
))
title(main="Eucalyptus populnea",  font.main=4,cex.main=1.25)
with(POP,adderrorbars(x=bp, y=LA, SE=LAE,direction="updown"))
par(las=1)
axis(4,labels=TRUE,tcl=-0.5,font=1,cex.axis=1)
axis(1,labels=FALSE,tcl=-0.5,cex.axis=1)
par(las=1)
names<-"Well-watered"
name<-"Droughted"
mtext(names,1,3,adj=0.15,font=2, cex= 0.9)
mtext(name,1,3,adj=0.85,font=2, cex= 0.9)


