library(igraph)
##########################################
bipartite = sample_bipartite(5, 8, p=0.5, directed = FALSE)
color <- c("steelblue", "orange")
plot(bipartite, layout=layout_as_bipartite, vertex.color=color[V(bipartite)$type+1])
##########################################
bipartite_proj = bipartite_projection(bipartite, multiplicity=TRUE, which="true")
plot(bipartite_proj)
##########################################
# sample = read_graph("E:\\Graph\\src data\\vnet\\vnet.graphml", "graphml")
sample = erdos.renyi.game(200, p=0.5, type = "gnp")
same_group_edges = degree(sample)
n = vcount(sample)
bipartite_edges = rep(0, n)
be_in_first_group = rep(TRUE, n)
possible_move = TRUE

update_adjacents_edges_status <- function (adjacent, moved_vertex) {
  if (be_in_first_group[moved_vertex] == be_in_first_group[adjacent]) {
    bipartite_edges[adjacent] <<- bipartite_edges[adjacent] - 1
    same_group_edges[adjacent] <<- same_group_edges[adjacent] + 1
  }
  else {
    bipartite_edges[adjacent] <<- bipartite_edges[adjacent] + 1
    same_group_edges[adjacent] <<- same_group_edges[adjacent] - 1
  }
}

put_in_sets <- function(vertex_index) {
  if (same_group_edges[vertex_index] > bipartite_edges[vertex_index]) {
    possible_move <<- TRUE
    temp = bipartite_edges[vertex_index]
    bipartite_edges[vertex_index] <<- same_group_edges[vertex_index]
    same_group_edges[vertex_index] <<- temp
    adjacents <- neighbors(sample, vertex_index)
    be_in_first_group[vertex_index] <<- !be_in_first_group[vertex_index]
    sapply(adjacents, update_adjacents_edges_status, moved_vertex = vertex_index)
  }
}

while (possible_move) {
  possible_move <<- FALSE
  sapply(V(sample), put_in_sets)
}

print("Num of edges:")
print(ecount(sample))
print("Num of achived bipartite subgraph edges:")
print(sum(bipartite_edges)/2)
