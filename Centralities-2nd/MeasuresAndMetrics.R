library(igraph)
library(htmlTable)
library(ggplot2)
######################################
g = read_graph("E:\\Graph\\data\\gnet.graphml", "graphml")
vertices = V(g)
undirected_g = as.undirected(g, mode=c("mutual"))
undirected_vertices = V(undirected_g)

#Degree Centrality- Undirected
extracted_feature_vector = degree(g, vertices, mode="all")
undirected_top10_degree = rev(names(tail(sort(extracted_feature_vector), 10)))
#Degree Centrality- Degree
extracted_feature_vector = degree(g, vertices, mode="out")
directed_top10_degree = rev(names(tail(sort(extracted_feature_vector), 10)))

#Closness Centrality- Undirected
extracted_feature_vector = closeness(g, vertices, mode="all")
undirected_top10_closeness = rev(names(tail(sort(extracted_feature_vector), 10)))
#Closeness Centrality- Directed
extracted_feature_vector = closeness(g, vertices, mode="out")
directed_top10_closeness = rev(names(tail(sort(extracted_feature_vector), 10)))

#Alpha Centrality- Undirected
extracted_feature_vector = alpha_centrality(undirected_g, undirected_vertices)
undirected_top10_alpha = rev(names(tail(sort(extracted_feature_vector), 10)))
#Alpha Centrality- Directed
extracted_feature_vector = alpha_centrality(g, vertices)
directed_top10_alpha = rev(names(tail(sort(extracted_feature_vector), 10)))

#Autority Score- Undirected
extracted_feature_vector = authority_score(undirected_g)$vector
undirected_top10_authority_score = rev(names(tail(sort(extracted_feature_vector), 10)))
#Authority Score- Directed
extracted_feature_vector = authority_score(g)$vector
directed_top10_authority_score = rev(names(tail(sort(extracted_feature_vector), 10)))

#Betweenness- Undirected
extracted_feature_vector = betweenness(undirected_g, undirected_vertices, directed=FALSE)
undirected_top10_betweenness = rev(names(tail(sort(extracted_feature_vector), 10)))
#Betweenness- Directed
extracted_feature_vector = betweenness(g, vertices, directed=TRUE)
directed_top10_betweenness = rev(names(tail(sort(extracted_feature_vector), 10)))

#Power Centrality- Undirected
extracted_feature_vector = power_centrality(undirected_g, undirected_vertices)
undirected_top10_power = rev(names(tail(sort(extracted_feature_vector), 10)))
#Power Centrality- Directed
extracted_feature_vector = power_centrality(g, vertices)
directed_top10_power = rev(names(tail(sort(extracted_feature_vector), 10)))

#Eigenvector Centrality- Undirected
extracted_feature_vector = eigen_centrality(g, directed=FALSE)$vector
undirected_top10_eigenvector = rev(names(tail(sort(extracted_feature_vector), 10)))
#Eigenvector Centrality- Directed
extracted_feature_vector = eigen_centrality(g, directed=TRUE)$vector
directed_top10_eigenvector = rev(names(tail(sort(extracted_feature_vector), 10)))

#Subgraph Centrality- Undirected
extracted_feature_vector = subgraph_centrality(undirected_g)
undirected_top10_subgraph = rev(names(tail(sort(extracted_feature_vector), 10)))
#Subgraph Centrality0 Directed
extracted_feature_vector = subgraph_centrality(g)
directed_top10_subgraph = rev(names(tail(sort(extracted_feature_vector), 10)))

output_table <- matrix(list(undirected_top10_degree, undirected_top10_closeness, undirected_top10_alpha,
        undirected_top10_power, undirected_top10_subgraph, undirected_top10_betweenness,
        undirected_top10_eigenvector, undirected_top10_authority_score,
        directed_top10_degree, directed_top10_closeness, directed_top10_alpha,
        directed_top10_power, directed_top10_subgraph, directed_top10_betweenness,
        directed_top10_eigenvector, directed_top10_authority_score),
      ncol=2, dimnames = list(c("Degree centrality", "Closeness centrality", "Alpha centrality",
                                "Power centrality", "Subgraph centrality", "Betweenness",
                                "Eigen centrality", "Authority score"),
                              c("Undirected graph", "Directed graph"))) %>%
                              htmlTable
print(output_table)
# ######################################
got_g = read_graph("E:\\Graph\\data\\got.graphml", "graphml")
got_vertices = V(got_g)
par(mfrow=c(4,2))

#Edge betweenness
clusters = cluster_edge_betweenness(got_g)
plot(clusters, got_g)
#Fast greedy
clusters = cluster_fast_greedy(got_g)
plot(clusters, got_g)
#Lable prop
clusters = cluster_label_prop(got_g)
plot(clusters, got_g)
#Louvain
clusters = cluster_louvain(got_g)
plot(clusters, got_g)
#Optimal
clusters = cluster_optimal(got_g)
plot(clusters, got_g)
#Spinglass
clusters = cluster_spinglass(got_g)
plot(clusters, got_g)
#Walktrap
clusters = cluster_walktrap(got_g)
plot(clusters, got_g)
#Leading eigen
# clusters = cluster_leading_eigen(got_g)
# plot(clusters, got_g)
# ######################################

#Jaccard
varys_index = as.numeric(V(got_g)["Varys"])
similarity_scores = similarity(got_g, method="jaccard")[, varys_index]
jaccard_indexes = rev(match(tail(sort(similarity_scores), 10), similarity_scores))
#Dice
varys_index = as.numeric(V(got_g)["Varys"])
similarity_scores = similarity(got_g, method="dice")[, varys_index]
dice_indexes = rev(match(tail(sort(similarity_scores), 10), similarity_scores))
#Invlogweighted
varys_index = as.numeric(V(got_g)["Varys"])
similarity_scores = similarity(got_g, method="invlogweighted")[, varys_index]
invlogweighted_indexes = rev(match(tail(sort(similarity_scores), 10), similarity_scores))

output_table1 <- matrix(list(names(V(got_g)[jaccard_indexes]), names(V(got_g)[dice_indexes]),
                            names(V(got_g)[invlogweighted_indexes])),
      ncol=1, dimnames = list(c("Jaccard", "Dice", "Invlogweighted"))) %>%
                              htmlTable
print(output_table1)
########################################

# adj = as_adjacency_matrix(got_g)
# d = degree(got_g)
# transition_matrix = t(replicate(length(d), d))
# transition_matrix = adj/(transition_matrix + 1)
# r_limit = 2000
# for (i in 1:r_limit){
#
# }
#
# print(transition_matrix)
