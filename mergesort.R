mergesort = function(A = vector()){
  n = length(A)
  if (n <= 1){
    return(A)
  }
  else if (n > 1) {
    B = mergesort(A[1:floor(n/2)])
    C = mergesort(A[(floor(n/2)+1):n])
    D = mergesorted(B,C)
  }
  return(D)
}

mergesorted = function(A = vector(),B = vector()){
  C = vector()
  while ((length(A) > 0) & (length(B) > 0)){
    if (A[1] < B[1]){
      C = c(C,A[1])
      if (length(A) > 1){
        A = A[2:length(A)]
      }
      else{
        A = vector()
      }
    }
    else{
      C = c(C,B[1])
      if (length(B) > 1){
        B = B[2:length(B)]
      }
      else{
        B = vector()
      }
    }
  }
  
  if (length(A) > 0){
    C = c(C,A)
  }
  if (length(B) > 0){
    C = c(C,B)
  }
  return(C)
}

#####  Inversion Implementation
imergesort = function(A = vector(), inversions = 0L) {
  
  n = length(A)
  inv = c(inversions,0,0,0)
  if (n <= 1){
    return(list(A,0))
  }
  else if (n > 1) {
    temp = imergesort(A[1:floor(n/2)])
    B = unlist(temp[1])
    inv[2] = unlist(temp[2])
    temp = imergesort(A[(floor(n/2)+1):n])
    C = unlist(temp[1])
    inv[3] = unlist(temp[2])
    temp = imergesorted(B,C)
    D = unlist(temp[1])
    inv[4] = unlist(temp[2])
  }
  
  return(list(D,sum(inv)))
}

imergesorted = function(A = vector(),B = vector()){
  C = vector()
  n = 0
  while ((length(A) > 0) & (length(B) > 0)){
    if (A[1] < B[1]){
      C = c(C,A[1])
      if (length(A) > 1){
        A = A[2:length(A)]
      }
      else{
        A = vector()
      }
    }
    else{
      n = length(A) + n
      C = c(C,B[1])
      if (length(B) > 1){
        B = B[2:length(B)]
      }
      else{
        B = vector()
      }
    }
  }
  
  if (length(A) > 0){
    C = c(C,A)
  }
  if (length(B) > 0){
    C = c(C,B)
  }
  return(list(C,n))
}