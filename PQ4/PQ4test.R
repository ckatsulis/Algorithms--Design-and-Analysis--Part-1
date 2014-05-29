dat = read.table("test3.txt")
# inverse graph
A <<- split(as.integer(dat$V1),dat$V2)

result = DFSloop()
A = A[order(names(A))]
f = f[order(names(f))]

# forward graph
A <<- split(as.integer(dat$V2),dat$V1)
B <<- A
for (i in 1:length(names(A))){
  for(j in 1:length(A[[i]])){
    print(i)
    print(j)
    A[[i]][j] <- as.integer(f[as.character(A[[i]][j])])
  }
  names(A)[i] <- f[as.character(i)]
}


result = DFSloop()
result = result[order(names(result))]
table(result)
