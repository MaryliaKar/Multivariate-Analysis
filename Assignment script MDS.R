##first step-import data set
require(smacof)
require(MASS)

##now we need to convert similarities in our data set to dissimilarities for 
#mds analysis
##Because we have 0.00 values for similarities with the same country 
#I want to exclude them from converting to dissimilarities
Nations[Nations==0.00]=NA
Nations

#NOw convert with 0.00 being NA
?sim2diss
Nations_1 <- sim2diss(Nations, method = 9)
Nations_1#to see if we convert the similarities to dissimilarities
#Now I will turn again NA to 0.00
Nations_1[is.na(Nations_1)]=0.00
Nations_1
d<-dist(Nations_1,method = "euclidian",diag = T)

#now perform MDS
Nations_mds <- isoMDS(d)
#get the points and stress value
Nations_mds$points
Nations_mds$stress
#create the x and y axes for the plot
windows()
x= Nations_mds$points[,1]
y=Nations_mds$points[,2]

plot(x,y, xlab = "Coordinate 1", ylab = "Coordinate 2", xlim=range(Nations_mds$points[,1])*1.2,
     type="n")
text(x,y,labels=colnames(Nations_1),cex = 0.9)



#for shepard plot
Nations_sh <- Shepard(d,Nations_mds$points)
windows()
plot(Nations_sh, pch = ".", xlab = "Dissimilarity",
     ylab = "Distance", xlim = range(Nations_sh$x), 
     ylim = range(Nations_sh$x))
lines(Nations_sh$x, Nations_sh$yf, type = "S")




