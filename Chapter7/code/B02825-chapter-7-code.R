#1. Downloading social network data using public APIs
# From www.meetup.com download groups data into groups.json

library(jsonlite)
g <- fromJSON("groups.json")
groups <- g$results
head(groups)

source("rdacb.getusers.R")
#Add your apikey within quotes in the command below before executing

members <- rdacb.getusers(groups, <<apikey within double quotes>>)
library(data.table)
users <- setDT(members)[,.SD[.N > 16], by = user_id]
save(users,file="meetup_users.Rdata")

#2. Creating adjacency matrices and edge lists
install.packages("Matrix")
load("meetup_users.Rdata") 

library(Matrix)

grp.membership = sparseMatrix(users$group_id, users$user_id, x = TRUE)

adjacency = t(grp.membership) %*% grp.membership

users.edgelist <- as.data.frame(summary(adjacency))

names(users.edgelist)

users.edgelist.upper <- users.edgelist[users.edgelist$i < users.edgelist$j,]

save(users.edgelist.upper, file = "users_edgelist_upper.Rdata")

#3. Plotting social network data
install.packages("igraph")
load("users_edgelist_upper.Rdata")

edgelist.filtered <- users.edgelist.upper[users.edgelist.upper$x > 16,]
edgelist.filtered  
nrow(edgelist.filtered)
#If you get 0 rows in the above command, adjust the filter above suitably.
# save(edgelist.filtered, file="filtered_edgelist.Rdata")
uids <- unique(c(edgelist.filtered$i, edgelist.filtered$j))

i <- match(edgelist.filtered$i, uids)
j <- match(edgelist.filtered$j, uids)

nw.new <- data.frame(i, j, x = edgelist.filtered$x)

library(igraph)

g <- graph.data.frame(nw.new, directed=FALSE)
g
save(g, file = "undirected-graph.Rdata")

plot.igraph(g, vertex.size = 20)

plot.igraph(g,layout=layout.circle, vertex.size = 20)

plot.igraph(g,edge.curved=TRUE,vertex.color="pink", edge.color="black")

V(g)$size=degree(g) * 4

plot.igraph(g,edge.curved=TRUE,vertex.color="pink", edge.color="black")

color <- ifelse(degree(g) > 5,"red","blue")

size <- degree(g)*4

plot.igraph(g,vertex.label=NA,layout= layout.fruchterman.reingold,vertex.color=color,vertex.size=size)

E(g)$x

plot.igraph(g,edge.curved=TRUE,edge.color="black", edge.width=E(g)$x/5)

dg <- graph.data.frame(nw.new)

save(dg, file = "directed-graph.Rdata")

plot.igraph(dg,edge.curved=TRUE,edge.color="black", edge.width=E(dg)$x/10,vertex.label.cex=.6)

nw.weights <- nw.new

names(nw.weights) <- c("i","j","weight")

g.weights <- graph.data.frame(nw.weights, directed=FALSE)

g.weights

get.adjacency(g,type="upper")

get.adjacency(g, type = "lower", attr = "x")

y <- get.data.frame(g)

y <- get.data.frame(g,"vertices")

set.seed(2015)

g1 <- rbinom(10,1,.5)
g2 <- rbinom(10,1,.5)
g3 <- rbinom(10,1,.5)
g4 <- rbinom(10,1,.5)

membership <- data.frame(g1, g2, g3, g4)

names(membership)

rownames(membership) = c("u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10")

rownames(membership)

bg <- graph.incidence(membership)
bg

V(bg)$type

V(bg)$name

lay <- layout.bipartite(bg)

plot(bg, layout=lay, vertex.size = 20)

save(bg, file = "bipartite-graph.Rdata")

p <- bipartite.projection(bg)
p

plot(p$proj1, vertex.size = 20)
plot(p$proj2, vertex.size = 20)

#4. Computing important network metrics

load("undirected-graph.Rdata")
load("directed-graph.Rdata")
load("bipartiite-graph.Rdata")

degree(dg)
degree(g)
degree(dg, "7")
degree(dg, 9, mode = "in")
degree(dg, 9, mode = "out")

options(digits=3)
degree.distribution(bg)

betweenness(dg)
betweenness(g)
betweenness(dg, 5)
edge.betweenness(dg)
edge.betweenness(dg,10)

options(digits=3)
closeness(dg,mode="in")
closeness(dg,mode="out")
closeness(dg,mode="all")
closeness(dg)

E(dg)

neighbors(g, 1)
neighbors(bg, "u1")
V(bg)$name[neighbors(bg,"g1")]
neighborhood(dg, 1, 1)
neighborhood(dg, 2, 1)

g.new <- g + vertex(19)
g.new <- g + vertices(19, 20)

g.new <- g.new + edge(15, 20)

g.new <- delete.vertices(g.new, V(g.new)[ degree(g.new)==0 ])

g.new <- delete.vertices(g.new,12)

g.sub <- induced.subgraph(g, c(5, 10, 13, 14, 17, 11, 7))
E(dg)

eids <- c(1:2, 9:15)
dg.sub <- subgraph.edges(dg, eids)









