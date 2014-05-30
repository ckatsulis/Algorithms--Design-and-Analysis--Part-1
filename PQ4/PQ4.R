DFSloop = function(){
  #  Initialize Variables
  nc <<- 0  # Global node count
  s <<- integer()  # Lead node reference
  leader <<- vector() #rep(NA,length(A)) # Leader Aarray
  f <<- vector() #rep(NA,length(A))  # Finishing Time Array
  
  nodes = sort(names(A),decreasing=T)
  
  for (i in nodes){
    if (!(A[[i]][1] == -1)){
      s <<- as.integer(i)  # Assign Leader Node
      DFSCSC(as.integer(i))
    }
  }
  return(leader)
}
DFSloop = cmpfun(DFSloop)

DFSCSC = function(n = integer()){
  leader[as.character(n)] <<- s         # Assign Leader Node
  B = sort(A[[as.character(n)]],decreasing=T)
  print(B)
  A[[as.character(n)]] <<- c(-1,A[[as.character(n)]])  # Marked Discovered
  
  for (i in B){  
    if (is.null(A[[as.character(i)]][1])){

      A[[as.character(i)]] <<- -1
      leader[as.character(i)] <<- s
      nc <<- nc + 1
      f[as.character(i)] <<- nc
    }
    if (!(A[[as.character(i)]][1] == -1)){
      
      DFSCSC(i)
    }  
  }
  nc <<- nc + 1
  f[as.character(n)] <<- nc
  
  return()
}
DFSCSC = cmpfun(DFSCSC)

primegraph = function(d = integer()){
  adds = vector()
  if (d == 2){
    # inverse graph
    cn = unique(dat$V1)  #child node
    pn = unique(dat$V2)  #parent node
    adds = cn[!is.element(cn,pn)]
  }
  else if (d == 1){
    # forward graph
    pn = unique(dat$V1)  #child node
    cn = unique(dat$V2)  #parent node
    adds = cn[!is.element(cn,pn)]
  }
  A <<- split(as.integer(dat$V1),dat$V2)
  if (length(adds) > 0){
    A[[as.character(adds)]] <<- vector()
    A <<- A[as.character(sort(as.integer(names(A))))]
  }
}