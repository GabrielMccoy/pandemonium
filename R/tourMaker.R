#' function to make tours
#'
#' @param coord1 coordinate matrix in space 1
#' @param coord2 coordinate matrix in space 2
#' @param group grouping assignment
#' @param score score assignments
#' @param user_group user defined grouping
#' @param tourspace space to show tour of
#' @param colouring colouring to use in plot
#' @param out_dim dimension of output tour
#' @param tour_path tour path and type to use, one of ("grand","cmass","holes","lda","pda","dcor","spline","radial","anomaly")
#' @param radial_start projection to use as start of radial tour, one of ("random","cmass","holes","lda","pda","dcor","spline")
#' @param radial_var variable to remove by radial tour
#' @param display display type, one of ("scatter","slice")
#' @param slice_width width of slice
#' @param final_frame if true returns the final frame as a ggplot2 plot otherwise returns detour object
#' @param seed sets the seed
#'
#'
#' @importFrom rlang .data
#' @returns detour or ggplot2
#' @keywords internal
#'
tourMaker <- function(coord1, coord2, group, score, user_group,
                      tourspace, colouring, out_dim, tour_path, display,
                      radial_start = NULL, radial_var = NULL, slice_width = NULL,
                      final_frame = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(final_frame)){
    final_frame <- FALSE
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
  projection <- switch(tourspace,
    "space1" = colnames(coord1),
    "space2" = colnames(coord2)
  )
  data <- switch(tourspace,
    "space1" = as.data.frame(coord1),
    "space2" = as.data.frame(coord2)
  )
  data$colour <- colour
  if (tour_path == "radial") {
    if (radial_start == "random") {
      radial_view <- tourr::basis_random(length(projection))
    } else {
      radial_tour <- switch(radial_start,
        "cmass"  = tourr::guided_tour(tourr::cmass()),
        "holes"  = tourr::guided_tour(tourr::holes()),
        "lda"    = tourr::guided_tour(tourr::lda_pp(colour)),
        "pda"    = tourr::guided_tour(tourr::pda_pp(colour)),
        "dcor"   = tourr::guided_tour(tourr::dcor2d()),
        "spline" = tourr::guided_tour(tourr::splines2d()),
      )
      hist <- tourr::save_history(data[projection], tour_path = radial_tour, max_bases = 1000)
      view <- drop(hist[, , dim(hist)[3]])
      attr(view, "class") <- NULL
      attr(view, "data") <- NULL
      radial_view <- view
    }
  }
  if (is.null(radial_var)) {
    radial_var <- 1
  }
  tourPath <- switch(tour_path,
    "grand" = tourr::grand_tour(out_dim),
    "cmass" = tourr::guided_tour(tourr::cmass(), out_dim),
    "holes" = tourr::guided_tour(tourr::holes(), out_dim),
    "lda" = tourr::guided_tour(tourr::lda_pp(colour), out_dim),
    "pda" = tourr::guided_tour(tourr::pda_pp(colour), out_dim),
    "dcor" = tourr::guided_tour(tourr::dcor2d(), out_dim),
    "spline" = tourr::guided_tour(tourr::splines2d(), out_dim),
    "radial" = tourr::radial_tour(radial_view, radial_var),
    "anomaly" = tourr::guided_anomaly_tour(tourr::anomaly_index())
  )
  angles <- if (tour_path == "radial") 1 / 2 else 1
  tour_bases <- if (tour_path %in% c("cmass", "holes", "lda", "pda", "dcor", "spline", "anomaly")) 1000 else 20

  if (display == "slice") {
    d <- detourr::detour(data, detourr::tour_aes(projection = tidyselect::all_of(projection), colour = "colour")) %>%
      detourr::tour_path(tourPath, fps = 60, aps = angles, max_bases = tour_bases) %>%
      detourr::show_slice(palette = pal, slice_relative_volume = as.numeric(slice_width))
  } else {
    d <- detourr::detour(data, detourr::tour_aes(projection = tidyselect::all_of(projection), colour = "colour")) %>%
      detourr::tour_path(tourPath, fps = 60, aps = angles, max_bases = tour_bases) %>%
      detourr::show_scatter(palette = pal)
  }
  if (final_frame) {
    d_plot <- tourr::render_proj(tourr::center(dplyr::select(data,!dplyr::matches("colour"))), d$x$projectionMatrices[length(d$x$projectionMatrices)][[1]])
    return(ggplot2::ggplot() +
        ggplot2::geom_point(data=d_plot$data_prj, ggplot2::aes(x=.data$P1, y=.data$P2, colour=as.factor(colour))) +
        ggplot2::geom_segment(data=d_plot$axes, ggplot2::aes(x=.data$x1, y=.data$y1, xend=.data$x2, yend=.data$y2)) +
        ggplot2::annotate(
          "text",
          x = d_plot$axes$x2,
          y = d_plot$axes$y2,
          label = projection
          )+
        ggplot2::xlim(-1,1) + ggplot2::ylim(-1, 1) +
        ggplot2::scale_colour_discrete(name="", palette=pal) +
        ggplot2::theme_bw() +
        ggplot2::theme(aspect.ratio=1,
            axis.text=ggplot2::element_blank(),
            axis.title=ggplot2::element_blank(),
            axis.ticks=ggplot2::element_blank(),
            panel.grid=ggplot2::element_blank(),
            legend.position = "none",
            panel.border=ggplot2::element_blank()))
  }
  d
}
