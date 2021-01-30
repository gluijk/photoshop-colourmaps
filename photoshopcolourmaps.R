# Photoshop curves for viridis and R built-in colourmaps
# www.overfitting.net
# https://www.overfitting.net/2021/01/mapas-de-color-viridis-en-photoshop.html

library(png)
library(viridis)


# Auxiliary write binary functions
writeValue=function(valor, fichero) {
    writeBin(0L, fichero, size=1)
    writeBin(as.integer(round(valor)), fichero, size=1)
}

writePoint=function(x, y, fichero) {
    writeBin(0L, fichero, size=1)
    writeBin(as.integer(round(y)), fichero, size=1)
    writeBin(0L, fichero, size=1)
    writeBin(as.integer(round(x)), fichero, size=1)
}

# Function that creates a Photoshop ACV curve file
writePhotoshopACVcurve=function(colourmap, nombreacv) {
    # Adobe Photoshop ACV curve file format:
    # https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577411_pgfId-1056330
    # Count of points in the curve (short integer from 2..16)
    # Curve points. Each curve point is a pair of short integers
    # where the first number is the output value (vertical coordinate on
    # the Curves dialog graph) and the second is the input value.
    # All coordinates have range 0 to 255
    
    # Build ACV file
    acv=file(nombreacv, 'wb')
    
    # Write header
    ACVVERSION=4  # version 4 = count of curves in the file
    NCURVES=4  # count of curves in the file: RGB, R, G and B
    writeValue(ACVVERSION, acv)  # ACV version
    writeValue(NCURVES, acv)  # count of curves
    
    # RGB curve (NULL)
    writeValue(2, acv)  # 2 points in curve
    writePoint(0, 0, acv)  # (0,0)
    writePoint(255, 255, acv)  # (255,255)
    
    # R, G and B curves
    for (curve in 1:3) {
        writeValue(NCOL, acv)  # NCOL points in curve
        for (j in 1:NCOL) {
            writePoint(round(255/(NCOL-1)*(j-1)), colourmap[j,curve], acv)
        }
    }
    
    close(acv)
}

# Matlab 'Jet' colour palette
jet.colors=colorRampPalette(
    c("#00007F", "blue", "#007FFF", "cyan",
      "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


# Viridis colourmap library:
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# plus standard built-in R colourmaps
cmname=c("magma", "inferno", "plasma", "viridis", "cividis",
         "rainbow", "heat.colors", "terrain.colors", "topo.colors",
         "cm.colors", "jet.colors")
NCOLMAPS=length(cmname)
cmfun=list()  # list containing the functions to build the colourmaps
for (i in 1:NCOLMAPS) cmfun[[i]]=get(cmname[i])


NCOL_ORG=256  # number of possible input values in a Photoshop curve
NCOL=16  # max number of points allowed in a Photoshop curve
WIDTH=(NCOL_ORG-1)/(NCOL-1)  # must be an integer

for (i in 1:NCOLMAPS) {  # loop through all colourmaps
    
    # Obtain 256 RGB values in hex and convert to int
    colourmap=t(col2rgb(cmfun[[i]](NCOL_ORG)))
    # Subsample 16 RGB values (pick 1 sample every WIDTH samples)
    colourmap=colourmap[(row(colourmap)-1) %%  WIDTH == 0]
    dim(colourmap)=c(NCOL,3)
    
    # NOTE1: doing just colourmap=t(col2rgb(cmfun[[i]](NCOL))) with NCOL=16
    # leads to offset errors in all built-in colourmaps (viridis are fine)
    # hence we prefer to subsample from NCOL_ORG=256 
    
    # NOTE2: another way to obtain the RGB values
    # colour=cmfun[[i]](NCOL)
    # for (j in 1:3) {
    #     colourmap[,j]=strtoi(paste0("0x",substr(colour,start=j*2,stop=j*2+1)))
    #  }
    
    # Plot RGB curves
    png(paste0(cmname[i],'.png'))
    x=seq(from=0, to=1, length.out=nrow(colourmap))
    plot(x, colourmap[,1]/255, type='b', col='red',
         main=cmname[i], ylab='y', xlim=c(0,1), ylim=c(0,1))
    lines(x, colourmap[,2]/255, type='b', col='green')
    lines(x, colourmap[,3]/255, type='b', col='blue')
    abline(h=c(0,1), v=c(0,1), col='gray', lty='dotted')
    dev.off()
    
    # Write Photoshop ACV curve file
    writePhotoshopACVcurve(colourmap, nombreacv=paste0(cmname[i],'.acv'))

}
