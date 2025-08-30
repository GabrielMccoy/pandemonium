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
#' @param tour_path tour path and type to use, one of ("grand","cmass","holes","lda","pda","dcor","origin","spline","radial","mahalanobis","anomaly")
#' @param radial_start projection to use as start of radial tour, one of ("random","cmass","holes","lda","pda","dcor","origin","spline","mahalanobis")
#' @param radial_var variable to remove by radial tour
#' @param display display type, one of ("scatter","slice")
#' @param slice_width width of slice
#'
#' @returns detour
#' @export
#'
tourMaker<- function(coord1, coord2, group, score, user_group,
                     tourspace, colouring, out_dim, tour_path, display,
                     radial_start=NULL, radial_var=NULL, slice_width=NULL){
  set.seed(2025)
  colour<- switch(colouring,
                      "clustering"  = group,
                      "user"        = user_group,
                      "bins"        = score$bins,
                      "score"       = rank(score$score))
  pal <-    switch(colouring,
                       "clustering"  = RColorBrewer::brewer.pal(length(unique(colour)),"Dark2"),
                       "user"        = RColorBrewer::brewer.pal(length(unique(colour)), "Set3"),
                       "bins"        = RColorBrewer::brewer.pal(length(unique(colour)),"Set2"),
                       "score"       = viridis::viridis(length(colour)))
  projection <- switch(tourspace,
                               "space1" = colnames(coord1),
                               "space2" = colnames(coord2))
  data       <- switch(tourspace,
                               "space1" = as.data.frame(coord1),
                               "space2" = as.data.frame(coord2))
  data$colour <- colour
  if (tour_path == "radial"){
    if (tour_path == "random") {
      radial_view <- tourr::basis_random(length(projection))
    }else {
      radial_tour <- switch(radial_start,
                                "cmass"  = tourr::guided_tour(tourr::cmass()),
                                "holes"  = tourr::guided_tour(tourr::holes()),
                                "lda"    = tourr::guided_tour(tourr::lda_pp(colour)),
                                "pda"    = tourr::guided_tour(tourr::pda_pp(colour)),
                                "dcor"   = tourr::guided_tour(tourr::dcor2d()),
                                "spline" = tourr::guided_tour(tourr::splines2d()),
                                "origin" = tourr::guided_tour(origin_dist()),
                                "mahalanobis" = tourr::guided_tour(origin_dist_mahalanobis()),
      )
      hist <- tourr::save_history(data, tour_path = radial_tour, max_bases = 1000)
      view <- drop(hist[,,dim(hist)[3]])
      attr(view,"class") <- NULL
      attr(view,"data")  <- NULL
      radial_view <- view
    }
  }
  if (is.null(radial_var)){
    radial_var<-1
  }
  tourPath <- switch(tour_path,
                         "grand"  = tourr::grand_tour(out_dim),
                         "cmass"  = tourr::guided_tour(tourr::cmass(),out_dim),
                         "holes"  = tourr::guided_tour(tourr::holes(),out_dim),
                         "lda"    = tourr::guided_tour(tourr::lda_pp(colour),out_dim),
                         "pda"    = tourr::guided_tour(tourr::pda_pp(colour),out_dim),
                         "dcor"   = tourr::guided_tour(tourr::dcor2d(),out_dim),
                         "spline" = tourr::guided_tour(tourr::splines2d(),out_dim),
                         "origin" = tourr::guided_tour(origin_dist(),out_dim),
                         "radial" = tourr::radial_tour(radial_view, radial_var),
                         "mahalanobis" = tourr::guided_tour(origin_dist_mahalanobis(),out_dim),
                         "anomaly"= tourr::guided_anomaly_tour(tourr::anomaly_index())
  )
  angles <- if (tour_path == "radial") 1 / 2 else 1
  tour_bases <- if(tour_path %in% c("cmass","holes","lda","pda","dcor","origin","spline","mahalanobis","anomaly")) 1000 else 20

  if (display=="slice"){
    d <- detourr::detour(data, detourr::tour_aes(projection = tidyselect::all_of(projection), colour = "colour")) %>%
      detourr::tour_path(tourPath, fps = 60, aps = angles, max_bases = tour_bases) %>%
      detourr::show_slice(palette = pal, slice_relative_volume = as.numeric(slice_width))
  } else{
    d <- detourr::detour(data, detourr::tour_aes(projection = tidyselect::all_of(projection), colour = "colour")) %>%
      detourr::tour_path(tourPath, fps = 60, aps = angles, max_bases = tour_bases) %>%
      detourr::show_scatter(palette = pal)
  }
  d
}
