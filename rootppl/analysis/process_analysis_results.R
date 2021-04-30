logz_plot = function(
  tree,
  birchdir = c("/home/viktor/ownCloud/miking-dppl/rootppl/results/birds-e2-20000"),
  resdir = c("/home/viktor/ownCloud/miking-dppl/rootppl/results/birds-e2-20000"),
  #models = c("birch-clads2", "clads2-d-λμασ", "clads2-factor"),
  models = c("birch-clads2", "clads2-d-λμασ"),
  iter_size
  
) {
  trees = tree
  prepare_tree = function(treenr) {
    do.call(rbind, lapply(models, function(m) {
      # find m in results
      logzfile = paste0(resdir, "/", trees[treenr], "-", m, ".cu.logz")
      logz = tryCatch(jsonlite::fromJSON(logzfile),
                      error = function(e) {
                        as.numeric(readLines(logzfile))
                      })
      data.frame(logz = logz, model = rep(m, length(logz)), runs = rep(length(logz), length(logz)))
    }))
  }
  
  plot_tree = function(df) {
    df$model = factor(df$model, levels = models, ordered = TRUE)
    ggplot2::ggplot(df, ggplot2::aes(model, logz)) + ggplot2::geom_boxplot() + ggplot2::theme(axis.text = ggplot2::element_text(size = 8)) + ggplot2::ggtitle(paste(tree, "Plot: log Z Sweeps: ", length(df$logz)/length(models)))
  }
  
  plot_tree(prepare_tree(1))
  
}



#' A global parameter plot vs concept paper results
#'
#' @param tree name of phylogenetic tree
#' @param resdir directory where raw results are storred
#' @param global_parameter which global parameter do we want to plot
#' @param iter_size Number of samples per sweep
#' @param Ni  # Ni - automatic computation of total number of sweeps
#' @param m name of model
#'
#' @return
#' @export
#'
#' @examples
global_parameter_plot = function(tree,
                                 resdir = c("/home/viktor/ownCloud/miking-dppl/rootppl/results/birds-e2-20000"),                                           global_parameter = "λ",
                                 iter_size = 1000, Ni = -1,
                                 m = "clads2-d-λμασ") {
  # read logz data
  logzfile = paste0(resdir, "/", tree, "-", m, ".cu.logz")
  logz = tryCatch(jsonlite::fromJSON(logzfile),
                  error = function(e) {
                    as.numeric(readLines(logzfile))
                  })
  data.frame(logz = logz, model = rep(m, length(logz)), runs = rep(length(logz), length(logz)))
  
  # read the birch data
  birch_dat = read.table(paste0(resdir, "/", tree, "-birch-pdf_", global_parameter, ".csv"), sep = ";")
  birch_dat$treatment = rep("birch", nrow(birch_dat))
  names(birch_dat) = c(global_parameter, "pdf", "treatment")
  # 
  
  # read the rootppl data (all sweeps)
  db = read.table(paste0(resdir, "/", tree, ".csv"), sep = ",")
  names(db) = c( "λ", "μ", "epsilon", "log_α", "σ2", "log_weight")
  # how many iterations in total, Ni = number of iterations
  if (Ni == -1) Ni = nrow(db)/iter_size
  
  # renormalize weights based on log Z (Importance Sampling)
  logweightlogz = unlist(lapply(0:(Ni-1), function(iteration) {
    # add the normalizing log-weight (i.e. multiply by the exp(weight)) to
    # sweep (iteration)
    db[(iteration*iter_size + 1):((iteration + 1)*iter_size), "log_weight"] + logz[iteration + 1]
  }))
  unnormalized_weight = exp(logweightlogz - max(logweightlogz))
  db$weight = unnormalized_weight/sum(unnormalized_weight)
  
  # Plot rootPPL 
  # + ggplot2::geom_histogram(ggplot2::aes(y = ..density..), binwidth = 0.05, position = "dodge")
  # +  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), binwidth = 0.05, position = "dodge") 
  colors <- c("RootPPL" = 1, "Birch" = 2)
  atitle = paste(tree, "Plot:", global_parameter, "Sweeps:", Ni)
  ggplot2::ggplot(db, ggplot2::aes_string(x=global_parameter) ) + ggplot2::geom_density(ggplot2::aes(color = "RootPPL")) +  ggplot2::ggtitle(atitle)  + ggplot2::geom_line(data = birch_dat, ggplot2::aes_string(x = global_parameter, y = "pdf", color = '"Birch"')) + ggplot2::theme_light() + ggplot2::scale_color_manual(values = colors)
  #+ ylim(0,5) + ggplot2::xlim(0, 4.0)
  #theme(panel.background = element_blank()) +
  
  # compute quantiles for rootppl data
  # rbind(birch_dat, do.call(rbind, lapply(0:(Ni-1), function(iteration) {
  #  
  #   data.frame(
  #              quantile = birch_dat$quantile,
  #              value = Hmisc::wtd.quantile(db[(iteration*iter_size + 1):((iteration + 1)*iter_size),][[global_parameter]], probs= birch_dat$quantile, weights = db$weight, normwt = TRUE),
  #              "treatment" = iteration)
  # 
  # })))
  
  
  
  
}



#' Plots the distribution of the factors
#'
#' @param tree name of phylogenetic tree
#' @param resdir directory where raw results are storred
#' @param iter_size Number of samples per sweep
#' @param Ni  # Ni - automatic computation of total number of sweeps
#' @param m name of model
#'
#' @return
#' @export
#'
#' @examples
factors_plot = function(tree,
                        resdir = c("/home/viktor/ownCloud/miking-dppl/rootppl/results/birds-e2-20000"), 
                        iter_size = 1000, Ni = -1,
                        m = "clads2-d-λμασ") {
  # read logz data
  logzfile = paste0(resdir, "/", tree, "-", m, ".cu.logz")
  logz = tryCatch(jsonlite::fromJSON(logzfile),
                  error = function(e) {
                    as.numeric(readLines(logzfile))
                  })
  data.frame(logz = logz, model = rep(m, length(logz)), runs = rep(length(logz), length(logz)))
  
  # read the rootppl data (all sweeps)
  db = read.table(paste0(resdir, "/", tree, "-factors.csv"), sep = ",")
  weight_index = ncol(db)
  # how many iterations in total, Ni = number of iterations
  if (Ni == -1) Ni = nrow(db)/iter_size
  
  # renormalize weights based on log Z (Importance Sampling)
  logweightlogz = unlist(lapply(0:(Ni-1), function(iteration) {
    # add the normalizing log-weight (i.e. multiply by the exp(weight)) to
    # sweep (iteration)
    db[(iteration*iter_size + 1):((iteration + 1)*iter_size), weight_index] + logz[iteration + 1]
  }))
  unnormalized_weight = exp(logweightlogz - max(logweightlogz))
  db$weight = unnormalized_weight/sum(unnormalized_weight)
  
  # compute mean and variance and unique as a function of factor number
  unique_samples = sapply(2:(weight_index - 1), function(nr) {
    length(unique(db[,nr]))
  })
  
  # map_factor = sapply(2:(weight_index - 1), function(nr) {
  #   tryCatch({
  #     map_estimate(db[,nr])  
  #   }, error = function(e) 
  #     {
  #       return(NA)
  #     })
  #   
  # })
  
  # var_factor = sapply(1:(weight_index - 1), function(nr) {
  #   var(db[,nr])
  # })
  
  db1 = data.frame(nr = 2:(weight_index - 1), log(unique_samples))
  
  ggplot2::ggplot(data = db1, ggplot2::aes(x = nr, y = log.unique_samples.)) + ggplot2::geom_point() + ggplot2::ylab("log(# unique samples)") + ggplot2::xlab("Factor Nr.") + ggplot2::ggtitle(tree, "Factor plot")
  
}

process_tree = function(tree,...) {
  p_lambda = global_parameter_plot(tree, global_parameter="λ", ...)
  p_mu = global_parameter_plot(tree, global_parameter="μ", ...)
  p_log_alpha = global_parameter_plot(tree, global_parameter="log_α", ...)
  p_sigma_squared = global_parameter_plot(tree, global_parameter="σ2", ...)
  p_logz = logz_plot(tree, ...)
  p_factors = factors_plot(tree, ...)
  g = gridExtra::grid.arrange(p_lambda, p_log_alpha, p_logz,
                   p_mu, p_sigma_squared, p_factors,
                   nrow = 2, ncol = 3)
  ggplot2::ggsave(paste0(tree, ".svg"), g, width = 16, height = 9)
}


args <- commandArgs(trailingOnly = TRUE)
resdir = args[[1]]
iter_size = args[[2]]
cat("First argument results dir; second argument number of samples per sweep.")
models = c("birch-clads2", "clads2-d-λμασ")
trees = c("Accipitridae", "BC7", "P20b", "TitTyranRest")

for (tree in trees) {
  process_tree(tree, resdir = resdir, iter_size = 1000)
}



