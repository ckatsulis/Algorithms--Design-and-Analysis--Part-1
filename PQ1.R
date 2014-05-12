data = read.table("R:/Chrisk/IntegerArray.txt",sep="\t",nrows = 100000)
data = as.numeric(data$V1)
data

x = imergesort(data)
x
system.time(mergesort(data), gcFirst=TRUE)
