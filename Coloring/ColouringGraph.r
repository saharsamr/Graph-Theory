library(igraph)
library(lpSolve)

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

integer_programing <- function(graph) {
  size = length(V(graph))
  coefficients = rep(1, size)
  coefficients = append(coefficients, rep(0, size*size))
  num_of_variables = size + size*size

  relations = c()
  margins = c()
  constraints <- matrix(ncol = num_of_variables)

  for (i in 1:num_of_variables) {
    constraint = rep(0, num_of_variables)
    constraint[i] = 1
    rbind(constraints, rep(1, num_of_variables)) # variables >= 0 constraint
    relations = append(relations, ">=")
    margins = append(margins, 0)
    constraint = rep(0, num_of_variables)
    constraint[i] = 1
    rbind(constraints, rep(1, num_of_variables)) # variables <= 1 constraint
    relations = append(relations, "<=")
    margins = append(margins, 1)
  }

  for (i in 1:size) # vertex <-> color
    for (j in 1:size) { # color
      constraint <- rep(0, num_of_variables)
      constraint[j] = -1
      constraint[size+(i-1)*size+j] = 1
      rbind(constraints, constraint) # x(ik) - y(k) <= 0 constraint
      relations = append(relations, "<=")
      margins = append(margins, 0)
    }

  for (i in 1:size) {
    constraint <- rep(0, num_of_variables)
    constraint[size+(i-1)*size+1:size+(i-1)*size+size] = rep(1, size)
    rbind(constraints, constraint) # sigma(x(ik)) = 1 constraint
    relations = append(relations, "=")
    margins = append(margins, 1)
  }

  for (edge in E(graph)) {
    incidents = ends(graph, edge, names = FALSE)
    for (j in 1:size) {
      constraint = rep(0, num_of_variables)
      constraint[size+(incidents[1,1]-1)*size+j] = 1
      constraint[size+(incidents[1,2]-1)*size+j] = 1
      rbind(constraints, constraint) # x(ik) + x(jk) <= 1 constraint
      relations = append(relations, "<=")
      margins = append(margins, 1)
    }
  }

  lp = lp(direction="min", objective.in=coefficients, const.mat=constraints, const.dir=relations, const.rhs=margins,
        transpose.constraints=TRUE, int.vec=1:num_of_variables)
  print(lp)
}

graph = sample_gnm(50, 500)
greedy_coloring(graph)
integer_programing(graph)
