library(igraph)

NO_COLOR = -1

greedy_coloring <- function(graph) {
  for (i in 1:length(V(graph)))
    V(graph)[i]$color_num = NO_COLOR;

  sorted_vertices = degree(graph)
  names(sorted_vertices) = V(graph)
  sorted_vertices = rev(sort(sorted_vertices))
  for (i in 1:length(sorted_vertices)) {
    neighbors = neighbors(graph, names(sorted_vertices[i]))
    neighbors_colors <- c()
    for (neighbor in neighbors)
      if (V(graph)[neighbor]$color_num > NO_COLOR)
        neighbors_colors = append(neighbors_colors, V(graph)[neighbor]$color_num)
    for (color in 1:length(V(graph)))
      if (! color %in% neighbors_colors) {
        V(graph)[strtoi(names(sorted_vertices[i]))]$color_num = color
        break
      }
  }
  print(max(V(graph)$color_num))
}

graph = sample_gnm(50, 500)
greedy_coloring(graph)
