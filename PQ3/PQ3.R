##  PQ3.R
require(igraph)
setwd("C:/Users/chris.katsulis/Documents/Algorithms--Design-and-Analysis--Part-1/PQ3")
dat = read.table("kargerMinCut.txt",header=F,fill=T,col.names=1:42)

pairs = data.frame()
for (i in 1:dim(dat)[1]){
  j = 2
  while (!is.na(dat[i,j]) & j <= dim(dat)[2]){
    pairs = rbind(pairs,c(dat[i,1],dat[i,j]))
    j = j + 1
  }
}

dat1 = graph.data.frame(dat)

plot(dat1, vertex.size=0, edge.arrow.size=0 )
plot(dat1, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph.data.frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

## The opposite operation
get.data.frame(g, what="vertices")
get.data.frame(g, what="edges")