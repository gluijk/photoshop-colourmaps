# Photoshop curves for perceptually uniform colourmaps (VIRIDIS)
# www.overfitting.net

library(png)
library(viridis)


# Auxiliary write binary functions
escribir_bin=function(valor, fichero) {
    writeBin(0L, fichero, size=1)
    writeBin(as.integer(round(valor)), fichero, size=1)
}

escribir_point=function(x, y, fichero) {
    writeBin(0L, fichero, size=1)
    writeBin(as.integer(round(y)), fichero, size=1)
    writeBin(0L, fichero, size=1)
    writeBin(as.integer(round(x)), fichero, size=1)
}


# Viridis colourmap library
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# opt = A: magma; B: inferno; C: plasma; D: viridis; E: cividis
cmID=c("A", "B", "C", "D", "E")
cmname=c("magma", "inferno", "plasma", "viridis", "cividis")

NCOL=16  # IMPORTANT: 16 is the max number of points allowed by Photoshop
colourmap=array(0,c(NCOL,3))
for (j in 1:length(cmID)) {  # loop through Viridis colourmaps
    colour=viridis(NCOL, opt=cmID[j])
    for (i in 1:3) {
        colourmap[,i]=strtoi(paste0("0x",substr(colour, start=i*2, stop=i*2+1)))
    }
    
    # Output RGB curves
    png(paste0(cmname[j],'.png'))
    x=seq(from=0, to=1, length.out=NCOL)
    plot(x, colourmap[,1]/255, type='b', col='red',
         main=cmname[j], ylab='y', xlim=c(0,1), ylim=c(0,1))
    lines(x, colourmap[,2]/255, type='b', col='green')
    lines(x, colourmap[,3]/255, type='b', col='blue')
    abline(h=c(0,1), v=c(0,1), col='gray', lty='dotted')
    dev.off()
    
    
    # Write values in ACV curve
    
    # Adobe ACV format documentation:
    # https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577411_pgfId-1056330
    
    # Count of points in the curve (short integer from 2...19) -> NO SON 19 SINO 16
    # Curve points. Each curve point is a pair of short integers
    # where the first number is the output value (vertical coordinate on
    # the Curves dialog graph) and the second is the input value.
    # All coordinates have range 0 to 255

    # Build ACV file
    acv=file(paste0(cmname[j],'.acv'), 'wb')
    
    # Write header
    ACVVERSION=4  # version 4 = count of curves in the file
    NCURVES=4  # count of curves in the file
    escribir_bin(ACVVERSION, acv)  # ACV version
    escribir_bin(NCURVES, acv)  # count of curves
    
    # RGB NULL curve
    escribir_bin(2, acv)  # points in curve
    escribir_point(0, 0, acv)  # (0,0)
    escribir_point(255, 255, acv)  # (255,255)
    
    # R, G and B curves
    for (curve in 1:3) {
        escribir_bin(NCOL, acv)  # points in curve
        for (i in 1:NCOL) {
            escribir_point(round(255/(NCOL-1)*(i-1)), colourmap[i,curve], acv)
        }
    }
    
    close(acv)
}
