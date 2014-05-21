##  PQ3.R
require(igraph)
# setwd("C:/Users/chris.katsulis/Documents/Algorithms--Design-and-Analysis--Part-1/PQ3")
# 
# dat = read.graph("test1.txt",format ="edgelist",directed=FALSE,7)
# 
# plot(dat, vertex.size=0, edge.arrow.size=0 )
# plot(dat, layout=layout.kamada.kawai)
# tkplot(dat, layout=layout.kamada.kawai)
# 
C = splice(C,1)
dat = read.table("kargerMinCut.txt",header=F,fill=T,col.names=1:201)
dat = dat[,2:length(dat)]


mincut = function(A = matrix()){
  min = dim(A)[1]
  for (i in 1:dim(A)[1]){
    cuts = cut(A)
    if (cuts < min) min = cuts
  }
  return(min)
}


cut = function(A = matrix()){
  nodes = 1:dim(A)[1]
  
  while (length(nodes) > 2){
    rnode = sample(A[!(is.na(A[,1])),1],1)
    A = splice(A,rnode)
    nodes = which(!is.na(A[,1]))
  }
  cuts = sum(!is.na(A[nodes[1],]))
  return(list(cuts,A))
  #return(cuts)
}

splice = function(C = data.frame(),a = -1L){
  if (is.na(a)) return(C)
  r = dim(C)[1]
  l = dim(C)[2]
  A = C[a,]             #  Save A node references
  b = sample(A[!is.na(A)],1)                   #  Set paired node
  B = C[b,]           #  Save B node references
  C[b,] = NA                       #  Remove B node references in return matrix
  D = sort(cbind(A,B),na.last = T)     #  Combine
  D = D[!(D == a | D == b)]        #  Remove Self References
  D = D[1:l]    #  Append NA to remaing vector
  C[a,] = D                      #  Set new node references to a location
  
  C[C == b] = a
  return(C)
}