edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(edges)
str(users)

nrow(edges)*2/nrow(users)

table(users$school,users$locale)
table(users$school,users$gender)

install.packages("igraph")
library(igraph)

g = graph.data.frame(edges, FALSE, users)
g = graph.data.frame(users, FALSE, edges)
g = graph.data.frame(edges, TRUE, users)
g = graph.data.frame(users, TRUE, edges)

plot(g, vertex.size=5, vertex.label=NA)
?graph.data.frame

sort(degree(g))

V(g)$size = degree(g)/2+2
sort(V(g)$size)
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "gray"
V(g)$color[V(g)$school == "AB"] = "red"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "gray"
V(g)$color[V(g)$locale == "B"] = "red"
plot(g, vertex.label=NA)

?igraph.plotting

install.packages("rgl")
library(rgl)
rglplot(g, vertex.label=NA)

plot(g, edge.width=2, vertex.label=NA)
