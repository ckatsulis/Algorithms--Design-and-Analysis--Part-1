require(igraph)
dat <<- scan("test1.txt")

G <- graph(dat)
#plot(G)
result = clusters(G,mode="strong")

plot(G)
result = str(G)[1]



