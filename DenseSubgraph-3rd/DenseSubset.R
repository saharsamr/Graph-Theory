library(igraph)
####################################################
# sample = read_graph("E:\\Graph\\src data\\vnet\\vnet.graphml", "graphml")
sample = erdos.renyi.game(200, p=0.5, type = "gnp")
max_density = 0
d = degree(sample)
n = vcount(sample)
e = ecount(sample)
while(n > 0) {
  x <- floor(e/n)
  while(x > 0 && n > 0) {
    to_remove_vertices = which(d <= x)
    sample = delete_vertices(sample, to_remove_vertices)
    d <- degree(sample)
    n = n - length(to_remove_vertices)
    e = ecount(sample)
    if (length(to_remove_vertices) == 0){
      break
    }
    x <- floor(e/n)
  }
  if( n > 0 && max_density < (e/n)){
    max_density = ((e/n))
    most_dense = sample
  }
  min_index = which.min(d)
  sample = delete_vertices(sample, c(min_index))
  d = degree(sample)
  n = n - 1
}

print("maximum density:")
print(max_density)
plot(most_dense)
#######################################################
# sample = sample = read_graph("E:\\Graph\\src data\\vnet\\vnet.graphml", "graphml")
sample = erdos.renyi.game(200, p=0.5, type = "gnp")
n = vcount(sample)
max_triangle_dense = 0
most_dense = sample
while (n > 0){
  triangles = count_triangles(sample, V(sample))
  to_remove_vertices = which(triangles == 0)
  delete_vertices(sample, to_remove_vertices)
  triangles <- triangles[triangles != 0]
  n = n - length(to_remove_vertices)
  density = sum(triangles)/3/n
  if(max_triangle_dense < density){
    max_triangle_dense = density
    most_dense = sample
  }
  min_index = which.min(triangles)
  to_remove_vertices = c(min_index)
  delete_vertices(sample, to_remove_vertices)
  n = n - 1
}

print("maximum triangle density:")
print(max_triangle_dense)
plot(most_dense)
