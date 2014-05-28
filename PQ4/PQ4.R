dat = read.table("test5.txt")

# forward graph
A = split(dat$V2,dat$V1)

A# inverse graph
B = split(dat$V1,dat$V2)

DFSloop(B)

for (i in 1:length(A)){
  for(j in 1:length(A[[i]])){
    A[[i]][j] = f[A[[i]][j]]
    
  } 
}

names(A) = f
A = A[order(names(A))]

result = DFSloop(A)
table(result)

     
     
DFSloop = function(A = list()){
  
  #  Initialize Variables
  n <<- 0  # Global node count
  s <<- integer()  # Lead node reference
  leader <<- as.integer(rep(NA,length(A)))  # Leader Aarray
  f <<- as.integer(rep(NA,length(A)))  # Finishing Time Array

  C = sort(names(A),decreasing=T)
  
  for (i in C){
    if (!(A[[i]][1] == 0)){
      s <<- i  # Assign Leader Node
      A = DFSCSC(A,i)
    }
  }
  return(leader)
}

DFSCSC = function(A = list(),i = integer()){
  
  leader[i] <<- s         # Assign Leader Node
  B = A[[i]]
  A[[i]] = c(0,A[[i]])  # Marked Discovered
  
  for (j in B){
    if (!(A[[j]][1] == 0)){
      A = DFSCSC(A,as.integer(j) )
    }
  }
  
  n <<- n + 1
  f[i] <<- n
  
  return(A)
}





# Original Functions
# DFSlist = function(A = list(),v = integer()){
#   B = list()
#   
#   for (i in names(A)){
#     B[[i]] = as.numeric(DFS(A,i))
#   }
#   return(B)
# }
# DFS = function(A = list(),v = integer()){
#   
#   verts = A[[v]]
#   discovered = v
#   A[[v]] = c(0,A[[v]])
#   for (i in verts){
#     if (!(0 %in% A[[i]])){
#       d = DFS(A,i)
#       discovered = c(discovered,d)
#     }
#   }
#   return(discovered)
# }