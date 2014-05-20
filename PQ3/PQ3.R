##  PQ3.R
require(igraph)
setwd("C:/Users/chris.katsulis/Documents/Algorithms--Design-and-Analysis--Part-1/PQ3")

dat = read.graph("test1.txt",format ="edgelist",directed=FALSE,7)

plot(dat, vertex.size=0, edge.arrow.size=0 )
plot(dat, layout=layout.kamada.kawai)
tkplot(dat, layout=layout.kamada.kawai)




dat = read.table("test1.txt",header=F,fill=T,col.names=1:10)
mincut = function(A = matrix()){
  r = dim(A)[1]
  i = round(.00001 + runif(10000,0.5,r + 0.5))
}

splice = function(A = matrix(),m = -1L){
  if ((m < 1)|A[m,2] == NA) return A
  B = A[m,]
  n = A[m,2]
  C = A[n,2:dim(A)[2]]
  
  for (i in 1:length(B)){
    if is.na(B[i]){
      B = c(B[1:i-1],C)
    }
  }
  
}