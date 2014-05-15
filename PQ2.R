## Programing Question #2
setwd(".")
dat = read.table("QuickSort.txt")
dat = as.numeric(dat$V1)

dat = read.table(url("https://dl.dropboxusercontent.com/u/20888180/AlgI_wk2_testcases/10.txt"))
dat = as.numeric(dat$V1)

partition = function(A = vector(),l = integer(), r = integer()){
  if (length(A) <= 1) return(A)
  
  p = A[1]
  i = l + 1
  
  for (j in (l+1):r){
    if (A[j] < p){
      temp = A[j]
      A[j] = A[i]
      A[i] = temp
      i = i + 1
    }
  }
  
  A[1] = A[i-1]
  A[i-1] = p
  return(list(A,i-1))
}

# Default function
quicksort = function(A = vector()){
  l = length(A)
  if (l <= 1){
    return(A)
  }
  
  p = A[1]
  a = partition(A,1,length(A))
  A = unlist(a[1])
  loc = unlist(a[2])
  ileft = loc - 1
  iright =  loc + 1
  
  left = vector()
  right = vector()
  
  if (ileft >= 1){
    left = quicksort(A[1:ileft])
  }
  if (iright <= length(A)){
    right = quicksort(A[iright:(length(A))])
  }
  
  return(c(left,p,right))
}  

# First Element Pivot
quicksort1 = function(A = vector()){
  l = length(A)
  if (l <= 1){
    return(list(A,0))
  }
  
  n = l - 1 #increment itterations
  
  # Index Selection
  index = 1
  temp = A[1]
  A[1] = A[index]
  A[index] = temp
  
  p = A[1]
  a = partition(A,1,length(A))
  A = unlist(a[1])
  loc = unlist(a[2])
  ileft = loc - 1
  iright =  loc + 1
  
  left = vector()
  right = vector()
  
  if (ileft >= 1){
    L = quicksort1(A[1:ileft])
    left = unlist(L[1])
    n = n + unlist(L[2])
  }
  if (iright <= length(A)){
    R = quicksort1(A[iright:(length(A))])
    right = unlist(R[1])
    n = n + unlist(R[2])
  }
  
  return(list(c(left,p,right),n))
} 

# Second Element Pivot
quicksort2 = function(A = vector()){
  l = length(A)
  if (l <= 1){
    return(list(A,0))
  }
  
  n = l - 1
 
  # Index Selection
  index = length(A)
  temp = A[1]
  A[1] = A[index]
  A[index] = temp
  
  p = A[1]
  a = partition(A,1,length(A))
  A = unlist(a[1])
  loc = unlist(a[2])
  ileft = loc - 1
  iright =  loc + 1
  
  left = vector()
  right = vector()
  
  if (ileft >= 1){
    L = quicksort2(A[1:ileft])
    left = unlist(L[1])
    n = n + unlist(L[2])
  }
  if (iright <= length(A)){
    R = quicksort2(A[iright:(length(A))])
    right = unlist(R[1])
    n = n + unlist(R[2])
  }
  
  return(list(c(left,p,right),n))
} 

# Median Element Pivor
quicksort3 = function(A = vector()){
  l = length(A)
  if (l <= 1){
    return(list(A,0))
  }
  
  n = l - 1
  
  # Index Selection
  index = medianindex(length(A))
  temp = A[1]
  A[1] = A[index]
  A[index] = temp
  
  p = A[1]
  a = partition(A,1,length(A))
  A = unlist(a[1])
  loc = unlist(a[2])
  ileft = loc - 1
  iright =  loc + 1
  
  left = vector()
  right = vector()
  
  if (ileft >= 1){
    L = quicksort3(A[1:ileft])
    left = unlist(L[1])
    n = n + unlist(L[2])
  }
  if (iright <= length(A)){
    R = quicksort3(A[iright:(length(A))])
    right = unlist(R[1])
    n = n + unlist(R[2])
  }
  
  return(list(c(left,p,right),n))
}

medianindex = function(A = vector()){
  l = length(A)
  if (l == 2){
    return(1)
#    if (A[1] < A[2]){
#      return(1L)
#    }
#    else
#      return(2L)
  }
  
  first = A[1]
  middle = A[round((l+.1)/2.0)]
  last = A[l]
  medianofthree = c(first,middle,last)
  
  
  value = median(medianofthree)
  i = which(A == value)
  return(i)
}