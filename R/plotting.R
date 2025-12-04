#' Show clusters in parameter space
#'
#'
#' @param wc parameter values as matrix
#' @param x,y variables names (as string) to map to x and y axis
#' @param interest index values for the intersting points
#' @param bmID index values of benchmarks
#' @param col color vector according to cluster assignment
#' @param cond row numbers of points used for conditioning
#' @param groups grouping assignments used to make alphahull
#' @param pal pallete used for group colouring of alphahull
#' @param a alpha value for alpha hull
#' @param showalpha boolean value to calculate and show alpha hulls
#' @return ggplot
#' @importFrom rlang .data
#' @keywords internal
plotWC <- function(wc, x, y, interest, bmID, col, cond = NULL, groups = NULL, pal = NULL, a = 0.2, showalpha = TRUE) {
  if (is.null(cond)) {
    cond <- 1:nrow(wc)
  }

  p <- ggplot2::ggplot(wc[cond, ], ggplot2::aes(.data[[x]], .data[[y]]))

  if (!is.null(groups) & showalpha) {
    for (group in unique(groups)) {
      group_indices <- intersect(cond, which(groups == group))
      data <- unique(wc[group_indices, c(x, y)])
      if (nrow(data) > 3) {
        hull <- tryCatch(alphahull::ahull(data, alpha = a), error = function(e) {
          warning(paste("alphahull failed for group", group, "no hull will be plotted"), call. = )
          return(NULL)
        })
        if (is.null(hull)) {
          next
        }
        edges <- hull$ashape$edges
        segment_df <- data.frame(
          x = edges[, 3],
          y = edges[, 4],
          xend = edges[, 5],
          yend = edges[, 6],
          group = as.factor(group)
        )
        p <- p + ggplot2::geom_segment(
          data = segment_df,
          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend), colour = pal[group]
        )
      }
    }
  }
  p + ggplot2::geom_point(color = col[cond]) +
    ggplot2::geom_point(data = wc[interest, ], shape = 1, size = 3) +
    ggplot2::geom_point(data = wc[bmID, ], shape = 5, size = 3) +
    ggplot2::theme_bw() + ggplot2::labs(title = "Cluster assignment in linked space") +
    ggplot2::theme(aspect.ratio = 1, legend.position = "none")
}

#' Make coordinate plot
#'
#'
#' @param coord coordinate representation of points
#' @param x,y variables names (as string) to map to x and y axis
#' @param wc parameter values as matrix
#' @param obs observable to plot
#' @param cond row numbers of points used for conditioning
#' @return ggplot
#'
#' @importFrom rlang .data
#' @keywords internal
plotObs <- function(coord, x, y, wc, obs, cond = NULL) {
  if (is.null(cond)) {
    cond <- 1:nrow(wc)
  }
  dat <- coord[cond, ]
  dat %>%
    tourr::rescale() %>%
    tibble::as_tibble() %>%
    cbind(wc[cond, ]) %>%
    ggplot2::ggplot(ggplot2::aes(.data[[x]], .data[[y]], color = .data[[obs]])) +
    ggplot2::geom_point() +
    ggplot2::guides(color = "none") +
    ggplot2::scale_color_viridis_c() +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste0("Centered coordinate values for ", obs)) +
    ggplot2::theme(aspect.ratio = 1)
}

#' Plot sigma bins in parameter space
#'
#'
#' @param wc parameter values as matrix
#' @param interest logical vector showing that points are intersting
#' @param bmID index values for the benchmark points
#' @param sigmabins binning in sigma
#' @param x,y variables names (as string) to map to x and y axis
#' @param binName name for title
#' @param cond row numbers of points used for conditioning
#' @param colourSet RColorBrewer set for colouring
#' @importFrom rlang .data
#' @return ggplot
#' @keywords internal
plotSigBin <- function(wc, interest, bmID, sigmabins, x, y, binName, cond = NULL, colourSet = "Set2") {
  if (is.null(cond)) {
    cond <- 1:nrow(wc)
  }
  palSig <- RColorBrewer::brewer.pal(length(unique(sigmabins)), colourSet)
  colSig <- palSig[sigmabins]
  ggplot2::ggplot(wc[cond, ], ggplot2::aes(.data[[x]], .data[[y]])) +
    ggplot2::geom_point(color = colSig[cond]) +
    ggplot2::geom_point(data = wc[interest, ], shape = 1, size = 3) +
    ggplot2::geom_point(data = wc[bmID, ], shape = 5, size = 3) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste(binName, "bins in linked space")) +
    ggplot2::theme(aspect.ratio = 1, legend.position = "none")
}

#' Plot chi2
#'
#'
#' @param wc parameter values as matrix
#' @param chi2 vector with chi2 values
#' @param x,y variables names (as string) to map to x and y axis
#' @param scoreName name for title
#' @param cond row numbers of points used for conditioning
#' @return ggplot
#' @importFrom rlang .data
#' @keywords internal
plotChi2 <- function(wc, chi2, x, y, scoreName = NULL, cond = NULL) {
  if (is.null(cond)) {
    cond <- 1:nrow(wc)
  }
  dplyr::mutate(wc[cond, ], chi2 = chi2[cond]) %>% ggplot2::ggplot(ggplot2::aes(.data[[x]], .data[[y]], color = .data[["chi2"]])) +
    ggplot2::geom_point() +
    ggplot2::guides(color = "none") +
    ggplot2::scale_color_viridis_c() +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste(scoreName, "values in linked space")) +
    ggplot2::theme(aspect.ratio = 1)
}

#' Make parallel coordinate plot
#'
#' @param coord coordinate representation of points
#' @param groups grouping from clustering is numeric or can be made numeric by as.numeric
#' @param benchmarkIds index values of benchmarks
#' @param filt  filter of groups
#' @param c centre
#' @param s rescale (default=TRUE)
#' @param a alpha transarancy for drawing non-benchmark points (default=0.2)
#' @param pal pallete for colour assignment
#' @return ggplot
#' @keywords internal
plotPC <- function(coord, groups, benchmarkIds, filt, c = TRUE, s = TRUE, a = 0.2, pal = NULL) {
  alphalvl <- rep(a, nrow(coord))
  alphalvl[benchmarkIds] <- 1
  if (is.null(pal)) {
    pal <- RColorBrewer::brewer.pal(8, "Dark2")
  }
  scale(coord, center = c, scale = s) %>%
    tibble::as_tibble() %>%
    tibble::add_column(gr = as.factor(groups)) %>%
    tibble::add_column(alphalvl = alphalvl) %>%
    dplyr::filter(.data$gr %in% filt) %>%
    ggpcp::pcp_select(-c(.data$gr, .data$alphalvl)) %>%
    ggpcp::pcp_arrange() %>%
    ggplot2::ggplot(ggpcp::aes_pcp()) +
    ggpcp::geom_pcp_axes() +
    ggpcp::geom_pcp(ggplot2::aes(colour = .data$gr, alpha = .data$alphalvl)) +
    ggplot2::scale_color_manual(values = pal[sort(as.numeric(filt))]) +
    ggplot2::labs(title = "Parallel coordinate plot") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "none"
    )
}

#' Plot heatmap with dendrogram
#'
#' @param dat coordinate representation of points
#' @param fit result from hclust
#' @param k number of clusters
#' @param pal color palette
#' @return plot
#' @keywords internal
plotHeatmap <- function(dat, fit, k, pal) {
  dendo <- stats::as.dendrogram(fit) %>%
    dendextend::set("branches_lwd", 3) %>%
    dendextend::color_branches(k = k, col = pal)

  stats::heatmap(dat, Rowv = dendo, Colv = rev(dendo), scale = "none")
}


#' Plot selected cluster statistics
#'
#' @param dist distances
#' @param fit result from hclust
#' @param chivals vector of chi2 values
#' @param stat cluster statistic to draw
#' @param kmax maximum number of clusters to appear in the plot
#' @return ggplot
#' @keywords internal
plotCstat <- function(dist, fit, chivals, stat, kmax = 8) {
  cstats <- getClusterStats(dist, fit, chivals, kmax)
  ggplot2::ggplot(cstats, ggplot2::aes({{ "k" }}, {{ stat }})) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "# clusters", y = cstat_names[[stat]]) +
    ggplot2::theme_bw()
}

#' Plot dimension reduction plot
#'
#' @param coord1 coordinates in space 1
#' @param coord2 coordinates in space 2
#' @param d_mat1 distance matrix in space 1
#' @param d_mat2 distance matrix in space 2
#' @param data either "space1" or "space2"
#' @param colouring either "clustering", "user", "bins" or "score"
#' @param dimReduction function to calculate dimension reduction with $Y being the new n x 2 matrix
#' @param algorithm name for algorithm used for labeling plot
#' @param group grouping of points from clustering
#' @param score score values and bins
#' @param user_group user defined grouping
#' @param pch factor with 2 levels 1 will be plotted as a circle 2 will be plotted as an o
#' @param seed sets the seed
#'
#' @returns plotly plot
#' @keywords internal
#'
plotDimRed <- function(coord1, coord2, d_mat1, d_mat2, data, colouring, dimReduction, algorithm, group, score, user_group, pch, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  colour <- switch(colouring,
    "clustering"  = group,
    "user"        = user_group,
    "bins"        = score$bins,
    "score"       = rank(score$score)
  )
  pal <- switch(colouring,
    "clustering"  = RColorBrewer::brewer.pal(length(unique(colour)), "Dark2"),
    "user"        = RColorBrewer::brewer.pal(length(unique(colour)), "Set3"),
    "bins"        = RColorBrewer::brewer.pal(length(unique(colour)), "Set2"),
    "score"       = viridis::viridis(length(colour))
  )
  dim.title <- paste(algorithm, "embedding")
  dim.axislab <- algorithm

  if (data == "space1") {
    mat <- dimReduction(mat = coord1, dist = d_mat1)$Y
  } else {
    mat <- dimReduction(mat = coord2, dist = d_mat2)$Y
  }
  colnames(mat) <- c("dim1", "dim2")

  plotly::plot_ly(as.data.frame(mat),
    x = ~dim1, y = ~dim2,
    color = as.factor(colour),
    symbol = pch, symbols = c("circle", "o"),
    colors = pal, marker = list(showscale = FALSE)
  ) %>%
    plotly::add_trace(type = "scatter", mode = "markers") %>%
    plotly::layout(
      title = dim.title,
      xaxis = list(
        title = paste(dim.axislab, 1),
        scaleanchor = "y"
      ),
      yaxis = list(title = paste(dim.axislab, 2)),
      showlegend = FALSE
    )
}


#' Generate a specified plot outside the GUI
#'
#' An interface to generate a specific graph seen when using the GUI.
#' Settings include: metric, linkage, k, plotType, for details
#' see the vignette on using this function.
#'
#' @param space1 dataframe of variables in cluster space
#' @param cov covariance matrix for space 1
#' @param covInv inverse covariance matrix for space 1
#' @param exp reference point in space 1
#' @param space2 dataframe of variables in linked space
#' @param space2.cov covariance matrix for space 2
#' @param space2.covInv inverse covariance matrix for space 2
#' @param space2.exp reference point in space 2
#' @param settings list specifying parameters usually selected in the app
#' @param user_dist user defined distances
#' @param getCoordsSpace1 function to calculate coordinates in space 1
#' @param getCoordsSpace2 function to calculate coordinates in space 2
#' @param getScore function to calculate scores and bins
#'
#' @returns ggplot, plotly or detourr plot depending on settings$plotType
#' @export
#' @examples
#' makePlots(
#'   space1 = Bikes$space1,
#'   settings = list(
#'     plotType = "WC", x = "hum", y = "temp", k = 4, metric = "euclidean",
#'     linkage = "ward.D2", WCa = 0.5, showalpha = TRUE
#'   ), cov = cov(Bikes$space1),
#'   space2 = Bikes$space2, getScore = outsidescore(Bikes$other$res, "Residual")
#' )
#'
#' makePlots(
#'   space1 = Bikes$space1,
#'   settings = list(
#'     plotType = "tour", k = 4, metric = "euclidean", linkage = "ward.D2",
#'     tourspace = "space1", colouring = "clustering", out_dim = 2, tour_path = "grand",
#'     display = "scatter", radial_start = NULL, radial_var = NULL, slice_width = NULL, seed = 2025
#'   ),
#'   cov = cov(Bikes$space1), space2 = Bikes$space2,
#'   getScore = outsidescore(Bikes$other$res, "Residual")
#' )
#'
makePlots <- function(space1, settings, cov = NULL, covInv = NULL, exp = NULL, space2 = NULL,
                      space2.cov = NULL, space2.covInv, space2.exp = NULL, user_dist = NULL,
                      getCoordsSpace1 = normCoords, getCoordsSpace2 = normCoords, getScore = NULL) {
  n <- nrow(space1)
  cond <- 1:n
  x <- settings$x
  y <- settings$y

  coord <- getCoordsSpace2(space1, cov, covInv, exp)
  try(coord2 <- getCoordsSpace2(space2, space2.cov, space2.covInv, space2.exp))
  dists <- getDists(coord, settings$metric, user_dist)
  try(dists2 <- getDists(coord2, settings$metric, NULL))
  fit <- stats::hclust(dists, settings$linkage)
  groups <- stats::cutree(fit, k = settings$k)
  lvl <- unique(groups[stats::order.dendrogram(stats::as.dendrogram(fit))])
  groups <- as.numeric(factor(groups, levels = lvl))
  # score fuction
  if (!is.null(getScore)) {
    value <- try(getScore(space1, cov, covInv, exp, space2, space2.cov, space2.exp, settings$k))
    if (!is.null(value$score)) {
      scorecol <- viridis::viridis(n)[rank(value$score)]
    }
    if (!is.null(value$bins)) {
      palSig <- RColorBrewer::brewer.pal(length(unique(value$bins)), "Set2")
      colSig <- palSig[value$bins]
    }
  }
  # colours
  pal <- RColorBrewer::brewer.pal(settings$k, "Dark2")
  col <- pal[groups]
  benchmarks <- getBenchmarkInformation(as.matrix(dists), groups)
  pch <- rep(20, n)
  pch[value$is.interest] <- 2
  # make plots
  if (settings$plotType == "PC") {
    return(plotPC(coord, groups, benchmarks$id, settings$filt, c = settings$centre, s = settings$scale))
  } else if (settings$plotType == "WC") {
    return(plotWC(space2, x, y, value$is.interest, benchmarks$id, col, groups = groups, pal = pal, a = settings$WCa, showalpha = settings$showalpha))
  } else if (settings$plotType == "chi2") {
    return(plotChi2(space2, value$score, x, y, value$scoreName, cond))
  } else if (settings$plotType == "sigBins") {
    return(plotSigBin(
      space2, value$is.interest, benchmarks$id, value$bins,
      x, y, value$binName, cond, "Set2"
    ))
  } else if (settings$plotType == "heatmap") {
    return(plotHeatmap(as.matrix(dists), fit, settings$k, pal))
  } else if (settings$plotType %in% names(cstat_names)) {
    return(plotCstat(
      dists, fit, computeChi2(space1, covInv, exp),
      settings$plotType
    ))
  } else if (settings$plotType == "Obs") {
    return(plotObs(coord, x, y, space2, settings$obs, cond))
  } else if (settings$plotType == "dimRed") {
    return(plotDimRed(
      coord, coord2, as.matrix(dists), as.matrix(dists2),
      settings$dimspace, settings$colouring, settings$dimReduction,
      settings$algorithm, groups, value, settings$user_group, pch, settings$seed
    ))
  } else if (settings$plotType == "tour") {
    return(tourMaker(
      coord, coord2, groups, value, settings$user_group,
      settings$tourspace, settings$colouring, settings$out_dim, settings$tour_path, settings$display,
      settings$radial_start, settings$radial_var, settings$slice_width, settings$seed
    ))
  }

  "plotType unknown"
}
