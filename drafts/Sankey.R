SankeyR <- function(inputs, losses, unit, labels, format="plot"){
########################
# SankeyR version 1.01 (updated August 10, 2010)
# is a function for creating Sankey Diagrams in R.
# See http://www.sankey-diagrams.com for excellent examples of Sankey Diagrams.
# 
# OPTIONS:
# 'inputs' is a vector of input values
# 'losses' is a vector of loss values
# 'unit' is a string of the unit
# 'labels' is a vector of the labels for inputs and losses
# 'format' is the type of plotting:
#   The default is "plot," which produces a plot in the R graphics device.
#   Current alternate options include "pdf" and "bmp," which produce
#   those file types under the name "Sankey.xxx" in the current directory.
#
# Inputs do not need to equal losses.  Any difference will be displayed
# as a discrepancy in the height of the left and right sides of the diagram.
# This capability enables the developer to examine imbalances in flows.
# Percentages are a proportion of the inputs (so, the outputs might not equal 100%).
#
# EXAMPLE:
# Try using these values for the global carbon cycle, from Schlesinger (1997):
# inputs = c(120,92)
# losses = c(45,75,90,1,6)
# unit = "GtC/yr"
# labels = c("GPP","Ocean assimilation","Ra","Rh","Ocean loss","LULCC","Fossil fuel emissions")
# SankeyR(inputs,losses,unit,labels)
#
# UPDATES:
# 8/10/10 - Added drawing for only one input.
#
# CREDITS:
# Created for R by Aaron BERDANIER
# send questions or comments to
# aaron.berdanier@gmail.com
#
# SankeyR is based strongly on drawSankey for Matlab,
# from James SPELLING, KTH-EGI-EKV (spelling@kth.se)
# http://leniwiki.epfl.ch/index.php/DrawSankey
#
# Distributed under Creative Commons Attribution Non-Commercial.
# Licensees may copy, distribute, display, and perform the work and make
# derivative works based on it only for noncommercial purposes.
#
# Aaron would appreciate notification if you modify or improve this function.
########################

# Calculate fractional losses and inputs
frLosses = losses/sum(inputs)
frInputs = inputs/sum(inputs)

# First input and last output labels
inputLabel = paste(labels[1],": ",inputs[1]," ",unit," (",round(100*frInputs[1],digits=1),"%)",sep="")
lossLabel = paste(labels[length(labels)],": ",losses[length(losses)]," ",unit," (",round(100*frLosses[length(losses)],digits=1),"%)",sep="")

########################
# Calculate position of plot axes (repeat of annotated code below; alternative: save values as vectors...)
limTop = frInputs[1]; posTop = 0.4; maxy=0
limBot = 0; posBot = 0.1
if(length(inputs)>1){
 for(j in 2:length(inputs)){
  rI = max(0.07, abs(frInputs[j]/2))
  rE = rI + abs(frInputs[j])
  newPosB = posBot + rE*sin(pi/4) + 0.01
  posBot = newPosB
  arcEx = posBot - rE*sin(seq(0,pi/4,length.out=100))
  arcEy = limBot - rE*(1-cos(seq(0,pi/4,length.out=100)))
  arcIx = posBot - rI*sin(seq(0,pi/4,length.out=100))
  arcIy = limBot - rE + rI*cos(seq(0,pi/4,length.out=100))
  phiTip = pi/4 - 2*min(0.05, 0.8*abs(frInputs[j]))/(rI + rE)
  xTip = posBot - (rE+rI)*sin(phiTip)/2
  yTip = limBot - rE + (rE+rI)*cos(phiTip)/2
  limBot = limBot - frInputs[j]
 }
}else{}
posTop = posBot + 0.4
for(i in 1:(length(losses)-1)){
  rI = max(0.07, abs(frLosses[i]/2))
  rE = rI + abs(frLosses[i])
  arcIx = posTop + rI*sin(seq(0,pi/2,length.out=100))
  arcIy = limTop + rI*(1 - cos(seq(0,pi/2,length.out=100)))
  arcEx = posTop + rE*sin(seq(0,pi/2,length.out=100));
  arcEy = (limTop + rI) - rE*cos(seq(0,pi/2,length.out=100))
  arEdge = max(0.015, rI/3)
  arTop  = max(0.04, 0.8*frLosses[i])
  arX = posTop + rI + c(0,-arEdge,frLosses[i]/2,frLosses[i]+arEdge,frLosses[i])
  arY = limTop + rI + c(0,0,arTop,0,0)
  if(max(arY)>maxy){maxy = max(arY)}else{maxy=maxy}
  limTop = limTop - frLosses[i]
  newPos = posTop + rE + 0.01
  posTop = newPos
}
newPos = max(posTop, posBot) + max(0.05*limTop,0.05)
newPos = newPos + 0.8*(limTop-limBot)

maxx = newPos
miny = (limTop-frLosses[length(frLosses)])-max(0.015,abs(frLosses[length(frLosses)]/4))
maxy = maxy*2
minx = 0

########################
# Graphics type?
if(format!="plot"){
 # Call graphics device
 plottype = switch(format, 
  "pdf" = pdf("Sankey.pdf", width=11, height=min(8.5,11*(maxy-miny)/((maxx+3)-minx))),
  "bmp" = bmp("Sankey.bmp", width=800*((maxx+3)-minx)/(maxy-miny), height=800, unit="px",res=144)
 )
}


# Create plotting window
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(0,0,type="n",xlim=c(-1.5,maxx+1.5),ylim=c(miny,maxy),xaxt="n",yaxt="n")

w = 1 # line width

# Calculate fractional losses and inputs
frLosses = losses/sum(inputs)
frInputs = inputs/sum(inputs)

# Draw back edge of first input arrow
lines(c(0.1,0,0.05,0,0.4), c(0,0,frInputs[1]/2,frInputs[1],frInputs[1]),lwd=w)

# First input label
inputLabel = paste(labels[1],": ",inputs[1]," ",unit," (",round(100*frInputs[1],digits=1),"%)",sep="")
fontsize = max(0.5,frInputs[1]*2.5)
text(0, frInputs[1]/2, inputLabel, cex=fontsize, pos=2) # try pos=4

# Set initial position for the top of the arrows
limTop = frInputs[1]; posTop = 0.4; maxy=0

# set initial position for the bottom of the arrows
limBot = 0; posBot = 0.1

###
# DRAW ARROWS FOR ADDITIONAL INPUTS
if(length(inputs)>1){
 for(j in 2:length(inputs)){

 # determine inner and outer arrow radii
  rI = max(0.07, abs(frInputs[j]/2))
  rE = rI + abs(frInputs[j])
 # push separation point forwards
  newPosB = posBot + rE*sin(pi/4) + 0.01
  lines(c(posBot,newPosB), c(limBot,limBot), lwd=w)
  posBot = newPosB
 # determine points on the external arc
  arcEx = posBot - rE*sin(seq(0,pi/4,length.out=100))
  arcEy = limBot - rE*(1-cos(seq(0,pi/4,length.out=100)))
 # determine points on the internal arc
  arcIx = posBot - rI*sin(seq(0,pi/4,length.out=100))
  arcIy = limBot - rE + rI*cos(seq(0,pi/4,length.out=100))
 # draw internal and external arcs
  lines(arcIx, arcIy, lwd=w)
  lines(arcEx, arcEy, lwd=w)

 # determine arrow point tip
  phiTip = pi/4 - 2*min(0.05, 0.8*abs(frInputs[j]))/(rI + rE)
  xTip = posBot - (rE+rI)*sin(phiTip)/2
  yTip = limBot - rE + (rE+rI)*cos(phiTip)/2
 # draw back edge of additional input arrows
  lines(c(min(arcEx),xTip,min(arcIx)), c(min(arcEy),yTip,min(arcIy)), lwd=w)

 # Draw label
  phiText = pi/2-2*min(0.05,0.8*abs(frInputs[j]))/(rI+rE)
  xText = posBot-(rE+rI)*sin(phiText)/3
  yText = limBot-rE/1.5+(rE+rI)*cos(phiText)/2
  fullLabel = paste(labels[j],": ",inputs[j]," ",unit," (",round(100*frInputs[j],digits=1),"%)",sep="")
  fontsize = max(0.5,frInputs[j]*2.5)
  text(xText, yText, fullLabel, cex=fontsize, pos=2)

 # save new bottom end of arrow
  limBot = limBot - frInputs[j]
 }

posTop = posBot + 0.4

lines(c(0.4,posTop), c(frInputs[1],frInputs[1]), lwd=w)

lines(c(posBot,posBot+(posTop-posBot)/2), c(limBot,limBot),lwd=w)

posMid=posBot+(posTop-posBot)/2

}else{
 lines(c(posBot,posBot+(posTop-posBot)/2), c(limBot,limBot),lwd=w)
 posMid=posBot+(posTop-posBot)/2
}

### 
# DRAW ARROWS OF LOSSES
for(i in 1:(length(losses)-1)){

 # Determine inner and outer arrow radii
  rI = max(0.07, abs(frLosses[i]/2))
  rE = rI + abs(frLosses[i])
 # Determine points on the internal arc
  arcIx = posTop + rI*sin(seq(0,pi/2,length.out=100))
  arcIy = limTop + rI*(1 - cos(seq(0,pi/2,length.out=100)))
 # Determine points on the internal arc
  arcEx = posTop + rE*sin(seq(0,pi/2,length.out=100));
  arcEy = (limTop + rI) - rE*cos(seq(0,pi/2,length.out=100))
 # Draw internal and external arcs
  lines(arcIx, arcIy, lwd=w)
  lines(arcEx, arcEy, lwd=w)

 # Determine arrow tip dimensions
  arEdge = max(0.015, rI/3)
  arTop  = max(0.04, 0.8*frLosses[i])
 # Determine points on arrow tip
  arX = posTop + rI + c(0,-arEdge,frLosses[i]/2,frLosses[i]+arEdge,frLosses[i])
  arY = limTop + rI + c(0,0,arTop,0,0)
  if(max(arY)>maxy){maxy = max(arY)}else{maxy=maxy}
 # Draw tip of losses arrow
  lines(arX, arY, lwd=w)

 # Draw label
  txtX = posTop + rI + frLosses[i]/2
  txtY = limTop + rI + arTop + 0.05
  fullLabel = paste(labels[i+length(inputs)],": ",losses[i]," ",unit," (",round(100*frLosses[i],digits=1),"%)",sep="")
  fontsize = max(0.5,frLosses[i]*2.5)
  text(txtX, txtY, fullLabel, cex=fontsize, pos=4, srt=35)

 # Save new position of arrow top
  limTop = limTop - frLosses[i]
 # Advance to new separation point
  newPos = posTop + rE + 0.01
 # Draw top line to new separation point
  lines(c(posTop,newPos), c(limTop,limTop), lwd=w)
 # Save new advancement point
  posTop = newPos

 # SEPARATION LINES - not implemented yet

}

###
# Push the arrow forwards a little after all side-arrows drawn
 newPos = max(posTop, posBot) + max(0.05*limTop,0.05)
# Draw lines to this new position
 lines(c(posTop,newPos), c(limTop,limTop),lwd=w)
 lines(c(posMid,newPos), c(limTop-frLosses[length(frLosses)],limTop-frLosses[length(frLosses)]),lwd=w)

# Draw final arrowhead for the output
 lines(c(newPos,newPos,newPos+max(0.04,0.8*(frLosses[length(frLosses)])),newPos,newPos), c(limTop,limTop+max(0.015,abs(frLosses[length(frLosses)]/6)),limTop-frLosses[length(frLosses)]/2,(limTop-frLosses[length(frLosses)])-max(0.015,abs(frLosses[length(frLosses)]/6)),(limTop-frLosses[length(frLosses)])),  lwd=w)

# Save final tip position
newPos = newPos + 0.8*(frLosses[length(frLosses)])

# Last loss label
lossLabel = paste(labels[length(labels)],": ",losses[length(losses)]," ",unit," (",round(100*frLosses[length(losses)],digits=1),"%)",sep="")
fontsize = max(0.5,frLosses[length(losses)]*2.5)
text(newPos+0.05, limTop-frLosses[length(frLosses)]/2, lossLabel, cex=fontsize, pos=4) # try pos=4

# Draw mid-line
if(limBot<(limTop-frLosses[length(frLosses)])){
 lines(c(posMid,posMid), c(frInputs[1],limBot),lty=2)
}else{
 lines(c(posMid,posMid), c(frInputs[1],limTop-frLosses[length(frLosses)]),lty=2)
}

if(format!="plot"){
 # Close graphics device
 dev.off()
}

}