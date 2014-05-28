dat = read.table("testlec.txt")

# forward graph
A = split(dat$V2,dat$V1)

# inverse graph
B = split(dat$V1,dat$V2)

C = DFSlist(B)




#Flag for visited is appending node 0 to node list
DFSlist = function(A = list(),B = list()){
  B = list()
  A <<- A
  for (i in names(A)){
    B[[i]] = as.numeric(DFS(i))
  }
  return(B)
}

DFS = function(v = integer()){
  
  verts = A[[v]]
  discovered = v
  A[[v]] = c(0,A[[v]])
  for (i in verts){
    if (!(0 %in% A[[i]])){
      d = DFS(i)
      discovered = c(discovered,d)
    }
  }
  return(discovered)
}