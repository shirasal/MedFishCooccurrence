source("R/packages.R")
source("R/functions.R")
source('R/models.R', echo = TRUE)

# Overall networks --------------------------------------------------------

library(ggraph)

plot_graph <- function(guild_mod, plot_title){
  net_cols <- c(neg = '#FF3333', pos = '#3399CC')
  net <- igraph::graph.adjacency(guild_mod$graph, weighted = T, mode = "undirected")
  weights <- igraph::E(net)$weight
  deg <- igraph::degree(net, mode = "all")
  ggraph(net, layout = "circle") + 
    geom_edge_link(aes(width = weights, color = weights < 0), lineend = "round", linejoin = "round") +
    scale_edge_width(range = c(0, 3)) +
    scale_edge_color_manual(values = c(net_cols[["pos"]], net_cols[["neg"]])) +
    geom_node_point(aes(size = deg), col = "grey", alpha = 0.5) +
    geom_node_text(aes(label = str_replace(name, "\\.", "\\ ")), repel = TRUE, check_overlap = TRUE, 
                   point.padding = unit(0.2, "lines"), fontface = "italic") +
    theme(legend.position = "none",
          aspect.ratio = 1,
          panel.background = element_blank())
}

plot_graph(poisson_models$grps_pois, "Groupers")
# ggsave("figures/groupers_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(poisson_models$dip_pois, "Seabreams")
# ggsave("figures/diplodus_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(poisson_models$herb_pois, "Herbivores")
# ggsave("figures/herb_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")
