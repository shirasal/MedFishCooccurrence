# source("R/packages.R")

### Summarise the relative importance of each type of covariate for each species
rel_imp_sum <- function(guild_mod){
  env_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>%  # Take the taxa model and apply the following:
                         filter(Variable %in% env_cov) %>%  # Filter by relevant covariates
                         summarise(n = sum(Rel_importance))) %>% # Summarise Rel_importance column
    unlist() %>% # Take out of the list
    enframe(name = "species", value = "env_rel_imp") %>%  # Rearrange
    mutate(species = str_sub(string = species, end = -3)) # Fix species names
  
  mpa_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                            filter(Variable %in% mpa_cov) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "mpa_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  biotic_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                            filter(!(Variable %in% env_cov | Variable %in% mpa_cov | str_detect(string = Variable, pattern = "_"))) %>%
                            summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "biotic_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_bio_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                             filter(str_detect(string = Variable, pattern = "temp_")) %>% 
                             summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "env_bio_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  mpa_bio_relimp <- sapply(guild_mod$key_coefs, FUN = function(x) x %>% 
                                filter(str_detect(string = Variable, pattern = "mpa_")) %>% 
                                summarise(n = sum(Rel_importance))) %>% 
    unlist() %>%
    enframe(name = "species", value = "mpa_bio_rel_imp") %>% 
    mutate(species = str_sub(string = species, end = -3))
  
  env_relimp %>%
    left_join(mpa_relimp, by = "species") %>%
    left_join(biotic_relimp, by = "species") %>%
    left_join(env_bio_relimp, by = "species") %>%
    left_join(mpa_bio_relimp, by = "species")
}

### Plot relative importance of covariates by covariate for each species, within guild:
plot_relimp <- function(rel_imp_df, guild_col, guild_name){
  rel_imp_df %>%
    pivot_longer(2:length(.)) %>%
    rename(covariate = name, rel_imp = value) %>%
    mutate(species = str_replace_all(species, "\\.", "\\ ")) %>%
    mutate(facet.title = case_when(covariate == "env_rel_imp" ~ "Environment",
                                   covariate == "mpa_rel_imp" ~ "MPA",
                                   covariate == "biotic_rel_imp" ~ "Biotic Associations",
                                   covariate == "env_bio_rel_imp" ~ "Temp * Biotic",
                                   covariate == "mpa_bio_rel_imp" ~ "MPA * Biotic")) %>%
    mutate(facet.title = fct_relevel(facet.title,
                                     "Environment", "MPA", "Biotic Associations",
                                     "Temp * Biotic", "MPA * Biotic")) %>%
    ggplot() +
    aes(x = species, y = rel_imp) +
    stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = guild_colours[[guild_col]]) +
    facet_wrap(~facet.title, nrow = 1) +
    labs(subtitle = guild_name, y = "Relative Importance (prop.)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"), strip.placement = "outside",
          axis.title.x = element_blank(), 
          strip.text.x = element_text(size = 12, face = "bold"),
          plot.margin = margin(.2,1,.2,1, "cm"))
}

### Plot overall (mean) networks
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

### Plot networks for temperature gradient (continuous)
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

### Plot networks for MPA (factor)
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
