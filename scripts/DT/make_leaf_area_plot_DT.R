make_leaf_area_plot_DT <- function() {
    
    ### read leaf area
    PIL<-read.csv("data/glasshouse2/PIL_LA.csv",sep=",", header=TRUE)
    POP<-read.csv("data/glasshouse2/POP_LA.csv",sep=",", header=TRUE)
    
    ################################### Plotting ######################################
    pdf(paste0(outdir, "leaf_area.pdf"), width=8, height=4)
    #PIL BIO
    par(mfrow=c(1,2))
    #PIL BIO
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
    
    dev.off()
}



