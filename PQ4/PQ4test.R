dat <<- read.table("testlec.txt")
# inverse graph

primegraph(2)

DFSloop()

f <<- f[as.character(sort(as.integer(names(f))))]
leader = leader[order(names(leader))]

# forward graph
primegraph(1)


for (i in 1:length(names(A))){
  for(j in 1:length(A[[i]])){
    A[[i]][j] <- as.integer(f[as.character(A[[i]][j])])
  }
  names(A)[i] <- as.character(f[names(A[i])])
}

A <<- A[as.character(sort(as.integer(names(A))))]

result = DFSloop()
result = result[order(names(result))]
table(result)
