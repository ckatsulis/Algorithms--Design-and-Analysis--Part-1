##  PQ3.R
require(igraph)
setwd("C:/Users/chris.katsulis/Documents/Algorithms--Design-and-Analysis--Part-1/PQ3")

dat = read.graph("test1.txt",format ="edgelist",directed=FALSE,7)

plot(dat, vertex.size=0, edge.arrow.size=0 )
plot(dat, layout=layout.kamada.kawai)
tkplot(dat, layout=layout.kamada.kawai)




dat = read.table("test1.txt",header=F,fill=T,col.names=1:50)
mincut = function(A = matrix()){
  r = dim(A)[1]
  i = round(.00001 + runif(10000,0.5,r + 0.5))
  
  
  
  
}