dat = read.table("test7.txt")
# inverse graph
A <<- split(as.integer(dat$V1),dat$V2)


result = DFSloop()


A = A[order(names(A))]
f = f[order(names(f))]
leader = leader[order(names(leader))]
# forward graph
A <<- split(as.integer(dat$V2),dat$V1)


for (i in 1:length(names(A))){
  for(j in 1:length(A[[i]])){
    A[[i]][j] <- as.integer(f[as.character(A[[i]][j])])
  }
  names(A)[i] <- as.character(f[names(A[i])])
}

result = DFSloop()
result = result[order(names(result))]
table(result)




#DFSloopcomp = cmpfun(DFSloop)
#microbenchmark(DFSloop(), DFSloopcomp(), times = 1000)
