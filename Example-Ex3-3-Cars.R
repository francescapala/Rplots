#' ---
#' title: "R Graphics Devices"
#' output: word_document
#' ---
#' 
## ----setup, cache=FALSE, include=FALSE, warning = FALSE------------------
data(mtcars)
st
head(mtcars)
tail(mtcars)
?mtcars

? pdf()
n<- nrow(mtcars)

pdf('Cars_plot1.pdf', width = 10, height = 8)
 # plot1 : hp:	 Gross horsepower; ranked high to low with labels
par( mfrow=c(1,2), oma=c(10,0,0,0))

 mtcars2<- mtcars[rev(order(mtcars$hp)),]

 plot(1:n, sort(mtcars$hp), type='n' , ylim=c(0, 350), 
      xlab="", ylab="", main="")
  for (i in 1:n)
  {arrows(i,0, i,mtcars2$hp[i], length=0, lwd=2 , col=1)
   text(i+0.3,mtcars2$hp[i]-30, srt=90, 
       paste( row.names(mtcars2)[i]), cex=0.6, col=4 )}
title(main="plot1", xlab="rank", ylab="HP")
 
 #plot2
 par(mar=c(5,6,4,3))
  mtcars2<- mtcars[order(mtcars$hp),]
  counts<- as.matrix(t(mtcars2$cyl+  mtcars2$carb))
 
  A<-barplot(counts, horiz=TRUE, col='gray70',
             xlab="", ylab="", main="Order by HP" )
  axis(2, at=A,  label= row.names(mtcars2), col.axis=4, cex.axis=0.9, las=2, tick = F, mgp=c(3,0.02,0), pos=-0.2)
  barplot(as.matrix(t(mtcars2$cyl)),
          horiz=TRUE, col='gray20', xlab="", 
          ylab="", main="", add=T )
  legend('bottomright', 
         fill=c('gray20', 'gray70' ),
         legend=c('cylinders', 'carburetors') )
  
dev.off()
  
###
tiff('Carsplot2.tiff', width = 4, height = 4, units = "in", res = 300, compress='lzw')

 plot(mtcars$carb,  mtcars$qsec)
 abline(lsfit( mtcars$carb,  mtcars$qsec), lty=2, col=2)
 
dev.off()

#########
#tiff('Carsplot3.tiff', width = 4, height = 4, units = "in", res = 300, compress='lzw')

ggplot(mtcars, aes(x=carb, y=qsec)) + 
  geom_point(alpha=0.6)+
  stat_smooth(method="lm", col='red', se=FALSE)
  
#dev.off()
ggsave("plot2gg.tiff", w=5, h=5)


