##  PQ3.R
require(igraph)
setwd("./")
fname = "test5.txt"
l = length(readLines(fname,warn=FALSE)) 
dat = read.table(fname,header=FALSE,fill=T,col.names=1:(l+1))
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
    A = splice(A)
    nodes = which(!is.na(A[,1]))
  }
  cuts = sum(!is.na(A[nodes[1],]))
  #return(list(cuts,A))
  return(cuts)
}

splice = function(A = data.frame()){

  nodes = which(!is.na(A[,1]))     #  Find available nodes in data structure
  n1 = sample(nodes,1)             #  Random Sample of first node (n1)
  B = as.integer(A[n1,])           #  Save n1 node references (B)
  B = B[!is.na(B)]                #  Remove NAs 
  n2 = sample(B,1)                 #  Random sample of second node (n2)
  C = as.integer(A[n2,])          #  Save n2 node references (C)
  C = C[!is.na(C)]  
  D = c(B,C)                       #  Merge nodes in B and C  
  D = D[!(D == n1 | D == n2)]       #  Remove Self References
  D = c(D,rep(NA,dim(A)[2]-length(D))) #  Append NA to remaing vector
  A[n1,] = D                      #  Set new node references to a location
  A[n2,] = NA                       #  Remove B node references in return matrix
  
  A[A == n2] = n1
  return(A)
}