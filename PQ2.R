## Programing Question #2
setwd(".")
dat = (read.table("QuickSort.txt"))
dat = as.numeric(dat$V1)


quicksort = function(A = vector()){
  n = length(A)
  if (n <= 1){
    return(A)
  }
  pivot = A[floor(n/2)]
  B = A[-floor(n/2)]
  x = vector()
  y = vector()
  for i in 1:length(B){
    if (B[i] < pivot){
      x = c(x,B[i])
    }
    else{
      y = c(y,B[i])
    }
  }
  X = quicksort(x)
  Y = quicksort(y)
  
}

partition = function(A = vector(), l = integer(), r = ){
  
}