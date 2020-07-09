install.packages("")
library(tidyverse)
library(readr)
library(igraph)

# get friends full series edgelist
edgefile_url <- "https://github.com/keithmcnulty/friends_analysis/blob/master/data/friends_full_series_edgelist.RDS?raw=true"
download.file(edgefile_url, "edgelist.RDS")
edgelist <- readRDS("edgelist.RDS")

# top 10 rows of edgelist df
knitr::kable(edgelist %>% head(10))

################################################################################

# persons of interest
friends <- c("Phoebe", "Monica", "Rachel", "Joey", "Ross", "Chandler")

# df without poi
edgelist_without <- edgelist %>% dplyr::filter(!(from %in% friends & to %in% friends))

# as matrix
edgelist_matrix <- as.matrix(edgelist_without[ ,c("from", "to")])

################################################################################

# graph object
friends_graph <- igraph::graph_from_edgelist(edgelist_matrix, directed = FALSE) %>% 
  igraph::set.edge.attribute("weight", value = edgelist_without$weight)

# plt of graph object
plot(friends_graph)

################################################################################

# run louvain clustering algorithm with edge weights
louvain_partition <- igraph::cluster_louvain(friends_graph, weights = E(friends_graph)$weight)

# assign communities to graph
friends_graph$community <- louvain_partition$membership

# see how many communities there are
paste(length(unique(friends_graph$community)), "communities")

################################################################################

# cacluate betweenness centrality for communities in graph
communities <- data.frame()
for (i in unique(friends_graph$community)) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(friends_graph, v = which(friends_graph$community == i))
  # get size of each subgraph
  size <- igraph::gorder(subgraph)
  # get betweenness centrality
  btwn <-  igraph::betweenness(subgraph)
  communities <- communities %>% 
    dplyr::bind_rows(
      data.frame(community = i,
                 n_characters = size,
                 most_important = names(which(btwn == max(btwn)))
      )
    )
}
# community information
knitr::kable(communities %>% 
               dplyr::select(community, n_characters, most_important))

################################################################################

# Degree centrality  for each community
top_five <- data.frame()
for (i in unique(friends_graph$community)) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(friends_graph, v = which(friends_graph$community == i))
  # for larger communities
  if (igraph::gorder(subgraph) > 20) {
    # get degree
    degree <-  igraph::degree(subgraph)
    # get top ten degrees
    top <- names(head(sort(degree, decreasing = TRUE), 5))
    result <- data.frame(community = i, rank = 1:5, character = top)
  } else {
    result <- data.frame(community = NULL, rank = NULL, character = NULL)
  }
  top_five <- top_five %>% 
    dplyr::bind_rows(result)
}

knitr::kable(
  top_five %>% 
    tidyr::pivot_wider(names_from = rank, values_from = character)
)

################################################################################

# give our nodes some properties, incl scaling them by degree and coloring them by community
V(friends_graph)$size <- 3
V(friends_graph)$frame.color <- "white"
V(friends_graph)$color <- friends_graph$community
V(friends_graph)$label <- V(friends_graph)$name
V(friends_graph)$label.cex <- 1.5

# also color edges according to their starting node
edge.start <- ends(friends_graph, es = E(friends_graph), names = F)[,1]
E(friends_graph)$color <- V(friends_graph)$color[edge.start]
E(friends_graph)$arrow.mode <- 0

# only label central characters
v_labels <- which(V(friends_graph)$name %in% friends)

for (i in 1:length(V(friends_graph))) {
  if (!(i %in% v_labels)) {
    V(friends_graph)$label[i] <- ""
  }
}

plot(friends_graph)

################################################################################

l2 <- layout_with_mds(friends_graph)
plot(friends_graph, rescale = T, layout = l2, main = "'Friends' Network - All Seasons")
