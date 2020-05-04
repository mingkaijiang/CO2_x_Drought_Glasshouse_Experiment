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