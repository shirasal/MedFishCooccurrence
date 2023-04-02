# source("R/packages.R")

# Relative importance summary ---------------------------------------------
# Summarise the relative importance of each type of covariate for each species
rel_imp_sum <- function(guild_mod){
  lapply(guild_mod$key_coefs, function(x) {
    if (nrow(x) == 0) return(tibble(cov_type = c("env", "mpa", "bio", "temp_bio", "mpa_bio"),
                                    sum_rel_imp = 0))
    x %>% 
      mutate(cov_type = case_when(Variable %in% env_cov ~ "env",
                                  Variable %in% mpa_cov ~ "mpa",
                                  !(Variable %in% env_cov | Variable %in% mpa_cov | str_detect(string = Variable, pattern = "_")) ~ "bio",
                                  str_detect(string = Variable, pattern = "temp_") ~ "temp_bio",
                                  str_detect(string = Variable, pattern = "mpa_") ~ "mpa_bio")) %>% 
      group_by(cov_type) %>%  # Filter by relevant covariates
      summarise(sum_rel_imp = sum(Rel_importance))}) %>% # Summarise Rel_importance column
    bind_rows(.id = "species") %>% 
    pivot_wider(id_cols = species, names_from = cov_type, values_from = sum_rel_imp, values_fill = 0)
}

# Plot relative importance of covariates by covariate for each species, within guild:
plot_relimp <- function(rel_imp_df, guild_col, guild_name){
  # Tibble for the facet names and their order:
  cov_titles <- tibble(covariate = c("env", "mpa", "bio", "temp_bio", "mpa_bio"),
                       facet.title = factor(c("Environment", "MPA", "Biotic Associations",
                                              "Temp * Biotic", "MPA * Biotic"),
                                            levels = c("Environment", "MPA", "Biotic Associations",
                                                       "Temp * Biotic", "MPA * Biotic")))
  # Organise data
  rel_imp_df %>%
    pivot_longer(2:length(.)) %>%
    rename(covariate = name, rel_imp = value) %>%
    mutate(species = str_replace_all(species, "\\.", "\\ ")) %>%
    group_by(species) %>% nest() %>% 
    mutate(new_data = map(data, function(x) right_join(x, cov_titles, by = "covariate"))) %>% 
    select(-data) %>% unnest(cols = c(new_data)) %>% replace_na(list(rel_imp = 0)) %>% 
    # Plot:
    ggplot() +
    aes(x = species, y = rel_imp) +
    stat_summary(geom = "bar", fun = mean, position = "dodge",  fill = guild_colours[[guild_col]]) +
    facet_wrap(~facet.title, nrow = 1) +
    labs(title = guild_name, y = "Relative Importance (prop.)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"), strip.placement = "outside",
          axis.title.x = element_blank(), 
          strip.text.x = element_text(size = 12, face = "bold"),
          plot.margin = margin(.2,1,.2,1, "cm"),
          plot.title = element_text(size = 16))
}


# Plot networks -----------------------------------------------------------

# Create network graph - this function is used in the following function `plotMRF_net`
create_netgraph = function(matrix, node_names, cutoff, predictor_value){
  
  # Create the adjacency network graph
  comm.net <- igraph::graph.adjacency(matrix, weighted = T, mode = "undirected")
  
  # Specify edge colours
  cols <- c(neg = "#FF3333", pos = "#3399CC")
  igraph::E(comm.net)$color <- ifelse(igraph::E(comm.net)$weight < 0,
                                      cols[["neg"]],
                                      cols[["pos"]])
  # Remove edges (links) under cutoff value
  comm.net <- igraph::delete.edges(comm.net, which(abs(igraph::E(comm.net)$weight) <= cutoff))
  # Set edge width according to association coefficients
  igraph::E(comm.net)$width <- abs(igraph::E(comm.net)$weight) * 1.75
  # Set vertex label to the shortened species name
  igraph::V(comm.net)$label <- str_replace(node_names, ".*\\.", paste0(substr(node_names, start = 1, stop = 1), ". "))
  
  # Create the network plot
  graphics::par(mar = c(0, 3, 0, 3))
  net.plot <- plot(comm.net,
                   layout = igraph::layout.circle,
                   vertex.label.cex = 1.6,
                   vertex.shape = "none",
                   vertex.label.family = "sans",
                   vertex.label.font = 3,
                   vertex.label.color = "black",
                   edge.label = round(igraph::E(comm.net)$weight, 2),
                   edge.label.cex = 1.5,
                   edge.label.family = "sans",
                   edge.label.color = "darkgray")
  return(net.plot)
}

# For type="all" the covariate doesn't matter, can by any
# 'type' options: "all" = mean network; "cont" = continuous gradient (such as temp); "factor" = like MPA
plotMRF_net <- function(data, MRF_mod, node_names, covariate,
                        main, cutoff, plot, type){
  
  if(MRF_mod$mod_type == "MRFcov"){
    plot_booted_coefs <- FALSE
  } else {
    plot_booted_coefs <- TRUE
  }
  
  if(missing(main)){
    main <- ""
  }
  
  if(missing(cutoff)){
    cutoff <- 0
  }
  
  # If plot_booted_coefs = FALSE, extract graph
  if(!plot_booted_coefs){
    ## Extract model coefficients
    interaction_coefficients <- MRF_mod$graph
    
    ## Specify default parameter settings
    if(missing(node_names)){
      node_names <- colnames(interaction_coefficients)
    }
    dimnames(interaction_coefficients) <- list(node_names, node_names)
    
    ## Extract indirect effect matrix that matches the covariate name
    indirect_coef_names <- names(MRF_mod$indirect_coefs)
    which_matrix_keep <- grepl(covariate, indirect_coef_names)
    covariate_matrix <- MRF_mod$indirect_coefs[which_matrix_keep]
    covariate_matrix <- as.matrix(covariate_matrix[[1]][[1]])
    baseinteraction_matrix <- as.matrix(MRF_mod$graph)
    
  } else {
    
    # If plot_booted_coefs = TRUE, extract and plot mean coefficients
    ## Extract model coefficients
    coef_matrix <- MRF_mod$direct_coef_means
    interaction_coefficients <- coef_matrix[, 2:(nrow(coef_matrix) + 1)]  +
      (Reduce(`+`, MRF_mod$indirect_coef_mean) /
         length(MRF_mod$indirect_coef_mean))
    
    ## Specify default parameter settings
    if(missing(node_names)){
      node_names <- rownames(coef_matrix)
    }
    dimnames(interaction_coefficients) <- list(node_names, node_names)
    
    ## Extract indirect effect matrix that matches the covariate name
    indirect_coef_names <- names(MRF_mod$indirect_coef_mean)
    which_matrix_keep <- grepl(covariate, indirect_coef_names)
    covariate_matrix <- MRF_mod$indirect_coef_mean[which_matrix_keep][[1]]
    rownames(covariate_matrix) <- node_names
    colnames(covariate_matrix) <- node_names
    baseinteraction_matrix <- interaction_coefficients
    
  }
  
  # Plot networks along a continuous gradient (like temperature)
  if (type == "cont") {
    ## Extract quantiles of observed values for the covariate to create gradient
    observed_cov_values <- as.vector(data[[paste(covariate)]])
    observed_cov_quantiles <- quantile(observed_cov_values,
                                       probs = c(0, 0.5, 1), na.rm = T)
    
    ## If number of unique values is low, quantiles may be identical. Instead,
    ## generate a sequence of 10 simulated values from the observed min to the observed max
    if(length(unique(observed_cov_quantiles)) < 3){
      observed_cov_quantiles <- quantile(seq(min(observed_cov_values),
                                             max(observed_cov_values),
                                             length.out = 10),
                                         probs = c(0, 0.5, 1), na.rm = T)
    }
    
    ## Create a gridded plot object to plot the three networks
    graphics::par(mfrow = c(1, length(observed_cov_quantiles)))
    cont.cov.mats <- lapply(observed_cov_quantiles, function(j){
      pred_values <- (covariate_matrix * j) + baseinteraction_matrix
      net.plot <- create_netgraph(matrix = pred_values,
                                  node_names = node_names,
                                  cutoff = cutoff,
                                  predictor_value = NULL)
    })
    
  }
  
  # Plot networks for each factor value (like MPA: TRUE and FALSE)
  if (type == "factor") {
    ## Extract unique values of the factor covariate
    observed_cov_values <- as.vector(data[[paste(covariate)]])
    observed_cov_unique <- as.numeric(unique(observed_cov_values, na.rm = T))
    
    ## If number of unique values is low, quantiles may be identical, throw warning:
    if(length(unique(observed_cov_values)) < 2){
      warning("Less than 2 quantiles, network may be identical") 
    }
    
    ## Create a gridded plot object to plot the unique networks
    graphics::par(mfrow = c(1, length(observed_cov_unique)))
    cont.cov.mats <- lapply(observed_cov_unique, function(j){
      pred_values <- (covariate_matrix * j) + baseinteraction_matrix
      net.plot <- create_netgraph(matrix = pred_values, cutoff = cutoff, node_names = node_names, 
                                  predictor_value = as.logical(j))
    })
  }
  
  # Plot a single network, based on mean species association coefficients (regardless of covariates)
  if (type == "all") {
    graphics::par(mfrow = c(1,1))
    pred_values <-  0 * covariate_matrix + baseinteraction_matrix
    net.plot <- create_netgraph(matrix = pred_values, cutoff = cutoff, node_names = node_names, 
                                predictor_value = T)
  }
  
}



# CV function -------------------------------------------------------------

# To over

cv_MRF_diag_spatial <- function (data, coords, symmetrise, n_nodes, n_cores, sample_seed, n_folds, 
                                 n_fold_runs, n_covariates, compare_null, family, plot = TRUE, 
                                 cached_model, cached_predictions, mod_labels = NULL) 
{
  if (!(family %in% c("gaussian", "poisson", "binomial"))) 
    stop("Please select one of the three family options:\n         \"gaussian\", \"poisson\", \"binomial\"")
  if (missing(coords))
    stop("Please set of coordinates")
  if (missing(symmetrise)) {
    symmetrise <- "mean"
  }
  if (missing(compare_null)) {
    compare_null <- FALSE
  }
  if (missing(n_folds)) {
    n_folds <- 10
  }
  else {
    if (sign(n_folds) == 1) {
      n_folds <- ceiling(n_folds)
    }
    else {
      stop("Please provide a positive integer for n_folds")
    }
  }
  if (missing(n_fold_runs)) {
    n_fold_runs <- n_folds
  }
  else {
    if (sign(n_fold_runs) == 1) {
      n_fold_runs <- ceiling(n_fold_runs)
    }
    else {
      stop("Please provide a positive integer for n_fold_runs")
    }
  }
  if (missing(n_cores)) {
    n_cores <- 1
  }
  else {
    if (sign(n_cores) != 1) {
      stop("Please provide a positive integer for n_cores")
    }
    else {
      if (sfsmisc::is.whole(n_cores) == FALSE) {
        stop("Please provide a positive integer for n_cores")
      }
    }
  }
  if (missing(n_nodes)) {
    warning("n_nodes not specified. using ncol(data) as default, assuming no covariates", 
            call. = FALSE)
    n_nodes <- ncol(data)
    n_covariates <- 0
  }
  else {
    if (sign(n_nodes) != 1) {
      stop("Please provide a positive integer for n_nodes")
    }
    else {
      if (sfsmisc::is.whole(n_nodes) == FALSE) {
        stop("Please provide a positive integer for n_nodes")
      }
    }
  }
  if (missing(n_covariates)) {
    n_covariates <- ncol(data) - n_nodes
  }
  else {
    if (sign(n_covariates) != 1) {
      stop("Please provide a positive integer for n_covariates")
    }
    else {
      if (sfsmisc::is.whole(n_covariates) == FALSE) {
        stop("Please provide a positive integer for n_covariates")
      }
    }
  }
  if (missing(sample_seed)) {
    sample_seed <- ceiling(runif(1, 0, 1e+05))
  }
  if (missing(cached_model)) {
    cat("Generating node-optimised Conditional Random Fields model", 
        "\n", sep = "")
    if (family == "binomial") {
      mrf <- MRFcov_spatial(data = data, coords = coords, symmetrise = symmetrise, 
                    n_nodes = n_nodes, n_cores = n_cores, family = "binomial")
      if (compare_null) {
        cat("\nGenerating Markov Random Fields model (no covariates)", 
            "\n", sep = "")
        mrf_null <- MRFcov_spatial(data = data[, 1:n_nodes], coords = coords, 
                           symmetrise = symmetrise, n_nodes = n_nodes, 
                           n_cores = n_cores, family = "binomial")
      }
    }
    if (family == "poisson") {
      mrf <- MRFcov_spatial(data = data, coords = coords, symmetrise = symmetrise, 
                    n_nodes = n_nodes, n_cores = n_cores, family = "poisson")
      if (compare_null) {
        cat("\nGenerating Markov Random Fields model (no covariates)", 
            "\n", sep = "")
        mrf_null <- MRFcov_spatial(data = data[, 1:n_nodes], coords = coords, 
                                   symmetrise = symmetrise, n_nodes = n_nodes, 
                           n_cores = n_cores, family = "poisson")
      }
    }
    if (family == "gaussian") {
      mrf <- MRFcov_spatial(data = data, coords = coords, symmetrise = symmetrise, 
                    n_nodes = n_nodes, n_cores = n_cores, family = "gaussian")
      if (compare_null) {
        cat("\nGenerating Markov Random Fields model (no covariates)", 
            "\n", sep = "")
        mrf_null <- MRFcov_spatial(data = data[, 1:n_nodes], coords = coords, 
                                   symmetrise = symmetrise, n_nodes = n_nodes, 
                           n_cores = n_cores, family = "gaussian")
      }
    }
  }
  else {
    mrf <- cached_model$mrf
    if (compare_null) {
      mrf_null <- cached_model$mrf_null
    }
  }
  if (family == "binomial") {
    folds <- caret::createFolds(rownames(data), n_folds)
    all_predictions <- if (missing(cached_predictions)) {
      predict_MRF(data, mrf)
    }
    else {
      cached_predictions$predictions
    }
    cv_predictions <- lapply(seq_len(n_folds), function(k) {
      test_data <- data[folds[[k]], ]
      predictions <- all_predictions[[2]][folds[[k]], 
      ]
      true_pos <- (predictions == test_data[, c(1:n_nodes)])[test_data[, 
                                                                       c(1:n_nodes)] == 1]
      false_pos <- (predictions == 1)[predictions != test_data[, 
                                                               c(1:n_nodes)]]
      true_neg <- (predictions == test_data[, c(1:n_nodes)])[test_data[, 
                                                                       c(1:n_nodes)] == 0]
      false_neg <- (predictions == 0)[predictions != test_data[, 
                                                               c(1:n_nodes)]]
      pos_pred <- sum(true_pos, na.rm = TRUE)/(sum(true_pos, 
                                                   na.rm = TRUE) + sum(false_pos, na.rm = TRUE))
      neg_pred <- sum(true_neg, na.rm = TRUE)/(sum(true_neg, 
                                                   na.rm = TRUE) + sum(false_neg, na.rm = TRUE))
      sensitivity <- sum(true_pos, na.rm = TRUE)/(sum(true_pos, 
                                                      na.rm = TRUE) + sum(false_neg, na.rm = TRUE))
      specificity <- sum(true_neg, na.rm = TRUE)/(sum(true_neg, 
                                                      na.rm = TRUE) + sum(false_pos, na.rm = TRUE))
      tot_pred <- (sum(true_pos, na.rm = TRUE) + sum(true_neg, 
                                                     na.rm = TRUE))/(length(as.matrix(test_data[, 
                                                                                                c(1:n_nodes)])))
      list(mean_pos_pred = mean(pos_pred, na.rm = TRUE), 
           mean_neg_pred = mean(neg_pred, na.rm = TRUE), 
           mean_tot_pred = mean(tot_pred, na.rm = TRUE), 
           mean_sensitivity = mean(sensitivity, na.rm = TRUE), 
           mean_specificity = mean(specificity, na.rm = TRUE))
    })
    plot_dat <- purrr::map_df(cv_predictions, magrittr::extract, 
                              c("mean_pos_pred", "mean_tot_pred", "mean_sensitivity", 
                                "mean_specificity"))
    if (compare_null) {
      all_predictions <- if (missing(cached_predictions)) {
        predict_MRF(data[, 1:n_nodes], mrf_null)
      }
      else {
        cached_predictions$null_predictions
      }
      cv_predictions_null <- lapply(seq_len(n_folds), 
                                    function(k) {
                                      test_data <- data[folds[[k]], 1:n_nodes]
                                      predictions <- all_predictions[[2]][folds[[k]], 
                                      ]
                                      true_pos <- (predictions == test_data)[test_data == 
                                                                               1]
                                      false_pos <- (predictions == 1)[predictions != 
                                                                        test_data]
                                      true_neg <- (predictions == test_data)[test_data == 
                                                                               0]
                                      false_neg <- (predictions == 0)[predictions != 
                                                                        test_data]
                                      pos_pred <- sum(true_pos, na.rm = TRUE)/(sum(true_pos, 
                                                                                   na.rm = TRUE) + sum(false_pos, na.rm = TRUE))
                                      neg_pred <- sum(true_neg, na.rm = TRUE)/(sum(true_neg, 
                                                                                   na.rm = TRUE) + sum(false_neg, na.rm = TRUE))
                                      sensitivity <- sum(true_pos, na.rm = TRUE)/(sum(true_pos, 
                                                                                      na.rm = TRUE) + sum(false_neg, na.rm = TRUE))
                                      specificity <- sum(true_neg, na.rm = TRUE)/(sum(true_neg, 
                                                                                      na.rm = TRUE) + sum(false_pos, na.rm = TRUE))
                                      tot_pred <- (sum(true_pos, na.rm = TRUE) + 
                                                     sum(true_neg, na.rm = TRUE))/(length(as.matrix(test_data)))
                                      list(mean_pos_pred = mean(pos_pred, na.rm = TRUE), 
                                           mean_neg_pred = mean(neg_pred, na.rm = TRUE), 
                                           mean_tot_pred = mean(tot_pred, na.rm = TRUE), 
                                           mean_sensitivity = mean(sensitivity, na.rm = TRUE), 
                                           mean_specificity = mean(specificity, na.rm = TRUE))
                                    })
      plot_dat_null <- purrr::map_df(cv_predictions_null, 
                                     magrittr::extract, c("mean_pos_pred", "mean_tot_pred", 
                                                          "mean_sensitivity", "mean_specificity"))
      if (is.null(mod_labels)) {
        plot_dat$model <- "CRF"
        plot_dat_null$model <- "MRF (no covariates)"
      }
      else {
        plot_dat$model <- mod_labels[1]
        plot_dat_null$model <- mod_labels[2]
      }
      plot_dat <- rbind(plot_dat, plot_dat_null)
      if (plot) {
        plot_binom_cv_diag_optim(plot_dat, compare_null = TRUE)
      }
      else {
        return(plot_dat)
      }
    }
    else {
      if (plot) {
        plot_binom_cv_diag_optim(plot_dat, compare_null = FALSE)
      }
      else {
        return(plot_dat)
      }
    }
  }
  if (family == "gaussian") {
    folds <- caret::createFolds(rownames(data), n_folds)
    cv_predictions <- lapply(seq_len(n_folds), function(k) {
      test_data <- data[folds[[k]], ]
      predictions <- predict_MRF(test_data, mrf)
      Rsquared <- vector()
      MSE <- vector()
      for (i in seq_len(ncol(predictions))) {
        Rsquared[i] <- cor.test(unlist(test_data[, i]), predictions[, 
                                                                    i])[[4]]
        MSE[i] <- mean((unlist(test_data[, i]) - predictions[, 
                                                             i])^2)
      }
      list(Rsquared = mean(Rsquared, na.rm = T), MSE = mean(MSE, 
                                                            na.rm = T))
    })
    plot_dat <- purrr::map_df(cv_predictions, magrittr::extract, 
                              c("Rsquared", "MSE"))
    if (compare_null) {
      cv_predictions_null <- lapply(seq_len(n_folds), 
                                    function(k) {
                                      test_data <- data[folds[[k]], 1:n_nodes]
                                      predictions <- predict_MRF(test_data, mrf_null)
                                      Rsquared <- vector()
                                      MSE <- vector()
                                      for (i in seq_len(ncol(predictions))) {
                                        Rsquared[i] <- cor.test(unlist(test_data[, i]), 
                                                                predictions[, i])[[4]]
                                        MSE[i] <- mean((test_data[, i] - predictions[, 
                                                                                     i])^2)
                                      }
                                      list(Rsquared = mean(Rsquared, na.rm = T), 
                                           MSE = mean(MSE, na.rm = T))
                                    })
      plot_dat_null <- purrr::map_df(cv_predictions_null, 
                                     magrittr::extract, c("Rsquared", "MSE"))
      if (is.null(mod_labels)) {
        plot_dat$model <- "CRF"
        plot_dat_null$model <- "MRF (no covariates)"
      }
      else {
        plot_dat$model <- mod_labels[1]
        plot_dat_null$model <- mod_labels[2]
      }
      plot_dat <- rbind(plot_dat, plot_dat_null)
      if (plot) {
        plot_gauss_cv_diag_optim(plot_dat, compare_null = TRUE)
      }
      else {
        return(plot_dat)
      }
    }
    else {
      if (plot) {
        plot_gauss_cv_diag_optim(plot_dat, compare_null = FALSE)
      }
      else {
        return(plot_dat)
      }
    }
  }
  if (family == "poisson") {
    folds <- caret::createFolds(rownames(data), n_folds)
    cv_predictions <- lapply(seq_len(n_folds), function(k) {
      test_data <- data[folds[[k]], ]
      predictions <- predict_MRF(test_data, mrf)
      Deviance <- vector()
      MSE <- vector()
      for (i in seq_len(ncol(predictions))) {
        preds_log <- log(test_data[, i]/predictions[, 
                                                    i])
        preds_log[is.infinite(preds_log)] <- 0
        test_data_wzeros <- test_data
        test_data_wzeros[predictions[, i] == 0, i] <- 0
        Deviance[i] <- mean(2 * sum(test_data_wzeros[, 
                                                     i] * preds_log - (test_data_wzeros[, i] - 
                                                                         predictions[, i])))
        MSE[i] <- mean((test_data[, i] - predictions[, 
                                                     i])^2)
      }
      list(Deviance = mean(Deviance, na.rm = T), MSE = mean(MSE, 
                                                            na.rm = T))
    })
    plot_dat <- purrr::map_df(cv_predictions, magrittr::extract, 
                              c("Deviance", "MSE"))
    if (compare_null) {
      cv_predictions_null <- lapply(seq_len(n_folds), 
                                    function(k) {
                                      test_data <- data[folds[[k]], 1:n_nodes]
                                      predictions <- predict_MRF(test_data, mrf_null)
                                      Deviance <- vector()
                                      MSE <- vector()
                                      for (i in seq_len(ncol(predictions))) {
                                        preds_log <- log(test_data[, i]/predictions[, 
                                                                                    i])
                                        preds_log[is.infinite(preds_log)] <- 0
                                        test_data_wzeros <- test_data
                                        test_data_wzeros[predictions[, i] == 0, 
                                                         i] <- 0
                                        Deviance[i] <- mean(2 * sum(test_data_wzeros[, 
                                                                                     i] * preds_log - (test_data_wzeros[, i] - 
                                                                                                         predictions[, i])))
                                        MSE[i] <- mean((test_data[, i] - predictions[, 
                                                                                     i])^2)
                                      }
                                      list(Deviance = mean(Deviance, na.rm = T), 
                                           MSE = mean(MSE, na.rm = T))
                                    })
      plot_dat_null <- purrr::map_df(cv_predictions_null, 
                                     magrittr::extract, c("Deviance", "MSE"))
      if (is.null(mod_labels)) {
        plot_dat$model <- "CRF"
        plot_dat_null$model <- "MRF (no covariates)"
      }
      else {
        plot_dat$model <- mod_labels[1]
        plot_dat_null$model <- mod_labels[2]
      }
      plot_dat <- rbind(plot_dat, plot_dat_null)
      if (plot) {
        plot_poiss_cv_diag_optim(plot_dat, compare_null = TRUE)
      }
      else {
        return(plot_dat)
      }
    }
    else {
      if (plot) {
        plot_poiss_cv_diag_optim(plot_dat, compare_null = FALSE)
      }
      else {
        return(plot_dat)
      }
    }
  }
}


cv_MRF_diag_rep_spatial <- function (data, symmetrise, coords, n_nodes, n_cores, sample_seed, n_folds, 
                                n_fold_runs, n_covariates, compare_null, family, plot = TRUE) 
{
  if (!(family %in% c("gaussian", "poisson", "binomial"))) 
    stop("Please select one of the three family options:\n         \"gaussian\", \"poisson\", \"binomial\"")
  if (missing(symmetrise)) {
    symmetrise <- "mean"
  }
  if (missing(compare_null)) {
    compare_null <- FALSE
  }
  if (missing(n_folds)) {
    if (nrow(data) < 50) {
      n_folds <- 2
      warning("nrow(data) is less than 50, using 2-fold validation by default")
    }
    else {
      if (nrow(data) < 100) {
        n_folds <- 5
        warning("nrow(data) is less than 100, using 5-fold validation by default")
      }
      else {
        n_folds <- 10
        warning("n_folds missing, using 10-fold validation by default")
      }
    }
  }
  else {
    if (sign(n_folds) == 1) {
      n_folds <- ceiling(n_folds)
    }
    else {
      stop("Please provide a positive integer for n_folds")
    }
  }
  if (missing(n_fold_runs)) {
    n_fold_runs <- n_folds
  }
  else {
    if (sign(n_fold_runs) == 1) {
      n_fold_runs <- ceiling(n_fold_runs)
    }
    else {
      stop("Please provide a positive integer for n_fold_runs")
    }
  }
  if (missing(n_cores)) {
    n_cores <- 1
  }
  else {
    if (sign(n_cores) != 1) {
      stop("Please provide a positive integer for n_cores")
    }
    else {
      if (sfsmisc::is.whole(n_cores) == FALSE) {
        stop("Please provide a positive integer for n_cores")
      }
    }
  }
  if (missing(n_nodes)) {
    warning("n_nodes not specified. using ncol(data) as default, assuming no covariates", 
            call. = FALSE)
    n_nodes <- ncol(data)
    n_covariates <- 0
  }
  else {
    if (sign(n_nodes) != 1) {
      stop("Please provide a positive integer for n_nodes")
    }
    else {
      if (sfsmisc::is.whole(n_nodes) == FALSE) {
        stop("Please provide a positive integer for n_nodes")
      }
    }
  }
  if (missing(n_covariates)) {
    n_covariates <- ncol(data) - n_nodes
  }
  else {
    if (sign(n_covariates) != 1) {
      stop("Please provide a positive integer for n_covariates")
    }
    else {
      if (sfsmisc::is.whole(n_covariates) == FALSE) {
        stop("Please provide a positive integer for n_covariates")
      }
    }
  }
  if (missing(sample_seed)) {
    sample_seed <- ceiling(runif(1, 0, 1e+05))
  }
  cat("Generating node-optimised Conditional Random Fields model", 
      "\n", sep = "")
  if (family == "binomial") {
    invisible(utils::capture.output(mrf <- MRFcov_spatial(data = data, 
                                                          symmetrise = symmetrise, n_nodes = n_nodes, n_cores = n_cores, 
                                                          family = "binomial")))
    if (compare_null) {
      cat("\nGenerating Markov Random Fields model (no covariates)", 
          "\n", sep = "")
      invisible(utils::capture.output(mrf_null <- MRFcov_spatial(data = data[, 1:n_nodes], coords = coords,
                                                                 symmetrise = symmetrise, n_nodes = n_nodes, 
                                                         n_cores = n_cores, family = "binomial")))
    }
  }
  if (family == "poisson") {
    invisible(utils::capture.output(mrf <- MRFcov_spatial(data = data, coords = coords,
                                                  symmetrise = symmetrise, n_nodes = n_nodes, n_cores = n_cores, 
                                                  family = "poisson")))
    if (compare_null) {
      cat("\nGenerating Markov Random Fields model (no covariates)", 
          "\n", sep = "")
      invisible(utils::capture.output(mrf_null <- MRFcov_spatial(data = data[,1:n_nodes], coords = coords,
                                                                 symmetrise = symmetrise, n_nodes = n_nodes, 
                                                         n_cores = n_cores, family = "poisson")))
    }
  }
  if (family == "gaussian") {
    invisible(utils::capture.output(mrf <- MRFcov_spatial(data = data, coords = coords,
                                                  symmetrise = symmetrise, n_nodes = n_nodes, n_cores = n_cores, 
                                                  family = "gaussian")))
    if (compare_null) {
      cat("\nGenerating Markov Random Fields model (no covariates)", 
          "\n", sep = "")
      invisible(utils::capture.output(mrf_null <- MRFcov_spatial(data = data[,1:n_nodes], coords = coords,
                                                         symmetrise = symmetrise, n_nodes = n_nodes, 
                                                         n_cores = n_cores, family = "gaussian")))
    }
  }
  cached_model <- list(mrf = mrf)
  if (compare_null) {
    cached_model$mrf_null <- mrf_null
  }
  cat("\nCalculating model predictions of the supplied data", 
      "\n", sep = "")
  cat("Generating CRF predictions ...", "\n", sep = "")
  cached_predictions <- list(predictions = predict_MRF(data, 
                                                       cached_model$mrf))
  if (compare_null) {
    cat("Generating null MRF predictions ...", "\n", sep = "")
    cached_predictions$null_predictions <- predict_MRF(data[, 
                                                            1:n_nodes], cached_model$mrf_null)
  }
  cat("\nCalculating predictive performance across test folds", 
      "\n", sep = "")
  repped_cvs <- lapply(seq_len(n_fold_runs), function(x) {
    cat("Processing cross-validation run ", x, " of ", n_fold_runs, 
        " ...\n", sep = "")
    cv_MRF_diag_spatial(data = data, coords = coords, n_nodes = n_nodes, n_folds = n_folds, 
                   n_cores = n_cores, family = family, compare_null = compare_null, 
                   plot = FALSE, cached_model = cached_model, cached_predictions = cached_predictions, 
                   sample_seed = sample_seed)
  })
  plot_dat <- do.call(rbind, repped_cvs)
  if (plot) {
    if (family == "gaussian") {
      plot_gauss_cv_diag_optim(plot_dat, compare_null = compare_null)
    }
    if (family == "poisson") {
      plot_poiss_cv_diag_optim(plot_dat, compare_null = compare_null)
    }
    if (family == "binomial") {
      plot_binom_cv_diag_optim(plot_dat, compare_null = compare_null)
    }
  }
  else {
    return(plot_dat)
  }
}
