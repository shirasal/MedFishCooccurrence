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
    ggtitle(plot_title) +
    theme(legend.position = "none",
          aspect.ratio = 1,
          panel.background = element_blank())
}

plot_graph(poisson_models$grps_pois, "Groupers")
# ggsave("figures/groupers_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(poisson_models$dip_pois, "Seabreams")
# ggsave("figures/seabreams_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")

plot_graph(poisson_models$herb_pois, "Herbivores")
# ggsave("figures/herb_network_overall.png", device = "png", dpi = 300, width = 4, unit = "in")


# Network gradients -------------------------------------------------------

guilds <- list(groupers = colnames(species_mats$grps_mat)[1:4],
               diplodus = colnames(species_mats$dip_mat)[1:4],
               herbivores = colnames(species_mats$herb_mat)[1:4])

plotMRF_net_cont <- function(data, MRF_mod, node_names, covariate){
  #### Function to create network graphs
  create_netgraph  <- function(matrix, node_names, predictor_value){
    
    # Create the adjacency network graph
    comm.net <- igraph::graph.adjacency(matrix, weighted = T, mode = "undirected")
    # Specify edge colours
    cols <- c(neg = "#FF3333", pos = "#3399CC")
    
    igraph::E(comm.net)$color <- ifelse(igraph::E(comm.net)$weight < 0,
                                        cols[["neg"]],
                                        cols[["pos"]])
    igraph::E(comm.net)$width <- abs(igraph::E(comm.net)$weight*20)
    igraph::V(comm.net)$label <- str_replace(node_names, "\\.", "\\ ")
    igraph::V(comm.net)$color <- grDevices::adjustcolor("grey", alpha.f = .6)
    
    # Create the network plot
    net.plot <- plot(comm.net,
                     layout = igraph::layout.circle,
                     vertex.label.cex = 1.2,
                     vertex.frame.color = grDevices::adjustcolor("grey", alpha.f = .6),
                     vertex.shape = "circle",
                     vertex.label.family = "sans",
                     vertex.label.font = 3,
                     vertex.label.color = "black")
    return(net.plot)
  }
  
  interaction_coefficients <- MRF_mod$graph
  
  #### Specify default parameter settings ####
  dimnames(interaction_coefficients) <- list(node_names, node_names)
  
  #### Extract indirect effect matrix that matches the covariate name ####
  indirect_coef_names <- names(MRF_mod$indirect_coefs)
  which_matrix_keep <- grepl(covariate, indirect_coef_names)
  covariate_matrix <- MRF_mod$indirect_coefs[which_matrix_keep]
  covariate_matrix <- as.matrix(covariate_matrix[[1]][[1]])
  baseinteraction_matrix <- as.matrix(MRF_mod$graph)
  
  #### Extract quantiles of observed values for the covariate ####
  observed_cov_values <- as.vector(data[[paste(covariate)]])
  observed_cov_quantiles <- quantile(observed_cov_values,
                                     probs = c(0, 0.5, 1), na.rm = T)
  
  #If number of unique values is low, quantiles may be identical. Instead,
  #generate a sequence of 10 simulated values from the observed min to the observed max
  if(length(unique(observed_cov_quantiles)) < 3){
    observed_cov_quantiles <- quantile(seq(min(observed_cov_values),
                                           max(observed_cov_values),
                                           length.out = 10),
                                       probs = c(0, 0.5, 1), na.rm = T)
  }
  
  #### Create a gridded plot object to plot the three networks
  graphics::par(mfrow = c(1, length(observed_cov_quantiles)), mar = c(0,3,0,3))
  cont.cov.mats <- lapply(observed_cov_quantiles, function(j){
    pred_values <- (covariate_matrix * j) + baseinteraction_matrix
    net.plot <- create_netgraph(matrix = pred_values, node_names = node_names, predictor_value = j)
  })
}

png(filename = "figures/groupers_net_temp.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(species_mats$grps_mat, poisson_models$grps_pois, node_names = guilds$groupers, covariate = "temp")
dev.off()

png(filename = "figures/seabreams_net_temp.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(species_mats$dip_mat, poisson_models$dip_pois, node_names = guilds$diplodus, covariate = "temp")
dev.off()

png(filename = "figures/herbivores_net_temp.png", res = 150, width = 12, height = 3.7, units = "in")
plotMRF_net_cont(species_mats$herb_mat, poisson_models$herb_pois, node_names = guilds$herbivores, covariate = "temp")
dev.off()



plotMRF_net_factor <- function(data, MRF_mod, node_names, covariate){
  #### Function to create network graphs
  create_netgraph  <- function(matrix, node_names, predictor_value){
    
    # Create the adjacency network graph
    comm.net <- igraph::graph.adjacency(matrix, weighted = T, mode = "undirected")
    # Specify edge colours
    cols <- c(neg = "#FF3333", pos = "#3399CC")
    
    igraph::E(comm.net)$color <- ifelse(igraph::E(comm.net)$weight < 0,
                                        cols[["neg"]],
                                        cols[["pos"]])
    igraph::E(comm.net)$width <- abs(igraph::E(comm.net)$weight*20)
    igraph::V(comm.net)$label <- str_replace(node_names, "\\.", "\\ ")
    igraph::V(comm.net)$color <- grDevices::adjustcolor("grey", alpha.f = .6)
    
    # Create the network plot
    # graphics::par(mar = c(0,3,0,3))
    net.plot <- plot(comm.net,
                     layout = igraph::layout.circle,
                     vertex.label.cex = 1.2,
                     vertex.frame.color = grDevices::adjustcolor("grey", alpha.f = .6),
                     vertex.shape = "circle",
                     vertex.label.family = "sans",
                     vertex.label.font = 3,
                     vertex.label.color = "black",
                     main = predictor_value)
    return(net.plot)
  }
  
  interaction_coefficients <- MRF_mod$graph
  
  #### Specify default parameter settings ####
  dimnames(interaction_coefficients) <- list(node_names, node_names)
  
  #### Extract indirect effect matrix that matches the covariate name ####
  indirect_coef_names <- names(MRF_mod$indirect_coefs)
  which_matrix_keep <- grepl(covariate, indirect_coef_names)
  covariate_matrix <- MRF_mod$indirect_coefs[which_matrix_keep]
  covariate_matrix <- as.matrix(covariate_matrix[[1]][[1]])
  baseinteraction_matrix <- as.matrix(MRF_mod$graph)
  
  #### Extract quantiles of observed values for the covariate ####
  observed_cov_values <- as.vector(data[[paste(covariate)]])
  observed_cov_unique <- as.numeric(unique(observed_cov_values, na.rm = T))
  
  #### Create a gridded plot object to plot the three networks
  graphics::par(mfrow = c(1, length(observed_cov_unique)), mar = c(1,3,1,3))
  cont.cov.mats <- lapply(observed_cov_unique, function(j){
    pred_values <- (covariate_matrix * j) + baseinteraction_matrix
    net.plot <- create_netgraph(matrix = pred_values, node_names = node_names, predictor_value = as.logical(j))
  })
}

png(filename = "figures/groupers_net_mpa.png", res = 150, width = 13, height = 7.38, units = "in")
plotMRF_net_factor(species_mats$grps_mat, poisson_models$grps_pois, guilds$grouper, covariate = "mpa")
dev.off()

png(filename = "figures/seabreams_net_mpa.png", res = 150, width = 13, height = 7.38, units = "in")
plotMRF_net_factor(species_mats$dip_mat, poisson_models$dip_pois, guilds$diplodus, covariate = "mpa")
dev.off()

png(filename = "figures/herbivores_net_mpa.png", res = 150, width = 13, height = 7.38, units = "in")
plotMRF_net_factor(species_mats$herb_mat, poisson_models$herb_pois, guilds$herbivores, covariate = "mpa")
dev.off()


