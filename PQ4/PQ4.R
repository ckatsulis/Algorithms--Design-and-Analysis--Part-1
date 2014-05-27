dat = read.table("testlec.txt")

# forward graph
A = split(dat$V2,dat$V1)

A# inverse graph
B = split(dat$V1,dat$V2)

#Flag for visited is appending node 0 to node list

DFSloop = function(A = list()){
  n <<- 0
  s <<- integer()
  leader <<- as.integer(rep(NA,length(A)))
  f <<- as.integer(rep(NA,length(A)))
  ##C = names(sort(unlist(lapply(DFSlist(B),length)),decreasing=T))
  
  C = as.integer(sort(names(A),decreasing=T))
  for (i in C){
    if (!(A[[i]][1] == 0))
      s <<- i
      DFSCSC(A,i)
  }
  return(leader)
}

DFSCSC = function(A = list(),i = integer()){
  leader[i] = s
  B = A[[i]]
  A[[i]] = c(0,A[[i]])  # Marked Discovered
  for (j in B){
    if (!(A[[j]][1] == 0)){
      print(A)
      print(n)
      A = DFSCSC(A,j) 
    }
  }
  n <<- n + 1
  f[i] <<- n
  return(A)
}


# Original Functions
DFSlist = function(A = list(),v = integer()){
  B = list()
  
  for (i in names(A)){
    B[[i]] = as.numeric(DFS(A,i))
  }
  return(B)
}

DFS = function(A = list(),v = integer()){
  
  verts = A[[v]]
  discovered = v
  A[[v]] = c(0,A[[v]])
  for (i in verts){
    if (!(0 %in% A[[i]])){
      d = DFS(A,i)
      discovered = c(discovered,d)
    }
  }
  return(discovered)
}