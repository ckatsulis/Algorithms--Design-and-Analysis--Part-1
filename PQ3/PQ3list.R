##  PQ3.R
# 
# dat = read.graph("test1.txt",format ="edgelist",directed=FALSE,7)
# 
# plot(dat, vertex.size=0, edge.arrow.size=0 )
# plot(dat, layout=layout.kamada.kawai)
# tkplot(dat, layout=layout.kamada.kawai)
# 
fname = "kargerMinCut.txt"
l = length(readLines(fname,warn=FALSE)) 
dat = read.table(fname,header=FALSE,fill=T,col.names=1:(l+1))
dat = split(dat,dat$X1)
dat = lapply(dat,as.integer)
dat = lapply(dat,na.omit)
dat = lapply(dat,as.integer)
dat = lapply(dat,function(x) return(x[-1]))

mincut = function(A = list()){
  min = length(A)
  for (i in 1:min){
    cuts = cut(A)
    if (cuts < min) min = cuts
  }
  return(min)
}

cut = function(A = list()){
  nodes = sum(!is.na(A))
  while (nodes > 2){
    A = splice(A)
    nodes = sum(!is.na(A))
  }
  B = A[!is.na(A)]
  cuts = length(B[[1]])
  return(cuts)
}

splice = function(A = list()){

  nodes = names(A[!is.na(A)])    #  Find available nodes in data structure
  n1 = as.numeric(sample(nodes,1))            #  Random Sample of first node (n1)
  n2 = sample(A[[n1]],1)        #  Random sample of second node (n2)
  #print(n1)
  #print(n2)
  B = A[[n1]]
  #print(B)
  C = A[[n2]]
  #print(C)
  D = as.vector(c(B,C))
  D = D[!(D == n1 | D == n2)] 
  #print(D)
  A[[n1]] = D
  A
  C = C[!(C == n1)]

  for (i in C){
    D = A[[i]]
    D[D == n2] = n1
    A[[i]] = D
  }
  
  A[[n2]] = NA
  return(A)
}