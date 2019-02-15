

SixMinWalk<- read.csv( file='SixMinWalk.csv')

mmA <-  apply( SixMinWalk, 2, mean, na.rm=T)
sdA  <-   apply( SixMinWalk, 2, sd, na.rm=T)


par(mfrow=c(1,3), cex.axis=1.2 )
A<-  barplot(as.matrix(mmA), width=rep(0.7,4), beside = TRUE,   col = c("gray90", "gray40"), 
             xlim=c(0.3, 2.5) , ylim = c(0, 350), bty="n", axes=F,
             cex.axis=2,font.axis=2, cex.lab=1.8, font.lab=1, cex=1.5, ylab=" ", lwd=4, xlab="",axisnames = F)

axis(2, las=2, at=seq(0, 350, by=50),  mgp=c(3,0.8,0))

axis(1, at=c(A[1,1],A[2,1]), tick=F, labels= names(SixMinWalk), las=1, mgp=c(3,1,0) )

mtext("Barchart", cex=1.2, font=2, side=3, line=0.6 )

arrows(-.8-0.5,0, 5, 0, lty=1, length=0,lwd=1.5) # add the line in the bottom
mtext("Six Minute Walk Mean Distance (m)", side=2, font=1,line=3,  cex=1) # ylabel


## adding SD##
for( i in 1:2)
{
  arrows(A[i,1], mmA[i] , A[i,1], mmA[i]+ sdA[i], col =1, code=2,length = 0.2,lwd=2, angle = 90)  
  
}

set.seed(123)
points( A[1,1]+rnorm(30,0,0.1), SixMinWalk[,1], col=2, pch=16)
points( A[2,1]+rnorm(30,0,0.1), SixMinWalk[,2], col=4, pch=16)

## plot 2##

boxplot(SixMinWalk, ylim = c(0, 350), col=c('bisque', 'lightblue'), boxwex = 0.5)
mtext("Boxplot", cex=1.2, font=2, side=3, line=0.6 )

set.seed(123)
points( rnorm(30,1,0.1), SixMinWalk[,1], col=2, pch=16)
points( rnorm(30,2,0.1), SixMinWalk[,2], col=4, pch=16)

## plot 3##
set.seed(123)

plot(c( rnorm(30,1,0.1)), c(SixMinWalk[,1]), col=2, pch=16,ylim = c(0, 330),
     xlim=c(0.5, 2.5) , axes=F, ylab="", xlab="")

points(c( rnorm(30,2,0.1)), c(SixMinWalk[,2]), col=4, pch=16 )
axis(1, at= c(1,2), labels=names(SixMinWalk))
axis(2)
box()

mtext("Mean+/-SD", cex=1.2, font=2, side=3, line=0.6 )

for( i in 1:2)
{
  arrows(i-0.15, mmA[i], i+0.15,mmA[i], lwd=3, length=0)
  
  arrows(i, mmA[i] - sdA[i], i, mmA[i]+ sdA[i], col =1, 
         code=3,length = 0.2,lwd=2, angle = 90)  
 
}





