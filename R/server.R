
#' Shiny app for exploring clustering solutions
#'
#' Opening the GUI to cluster the data points based on the predictions.
#' Coordinates and distances are computed on the fly, or can be entered
#' in the function call.(modified)
#'
#' @param df data frame of data assumes space 1 but variables can be changed once loaded
#' @param cov  covariance matrix (optional)
#' @param is.inv is the covariance matrix an inverse default FALSE
#' @param exp observable reference value (e.g. experimental measurement)
#' @param space2 dataframe assumed to be in space 2 but can be changed when loaded
#' @param space2.cov  covariance matrix (optional)
#' @param space2.exp observable reference value (e.g. experimental measurement)
#' @param group grouping assignments
#' @param label point labels
#' @param user_dist input distance matrix (optional)
#' @param dimReduction named list functions used for dimension reduction.
#' @param getCoords named list containing functions to calculate coordinates
#' @param getScore named list contaiining functions to calculate scores to be plotted as bins and continuous value.
#'
#' @importFrom rlang .data
#' @export
#'
pandemonium = function (df, cov = NULL, is.inv = FALSE, exp = NULL, space2 = NULL, space2.cov = NULL, space2.exp = NULL, group = NULL, label = NULL, user_dist = NULL, dimReduction = list(tSNE = tSNE, umap = umap), getCoords = list(normal = normCoords), getScore = NULL)
{
  stopifnot(
    is.null(space2)||nrow(df)==nrow(space2),
    is.null(cov)||(ncol(df)==nrow(cov)&&ncol(df)==ncol(cov)),
    is.null(exp)||ncol(df)==length(exp)||(ncol(df)==nrow(exp)&&ncol(exp)==1),
    is.null(label)||nrow(df)==length(label)||(nrow(df)==nrow(label)&&ncol(label)==1),
    is.null(group)||nrow(df)==length(group)||nrow(df)==nrow(group),
    is.null(space2.cov)||(ncol(space2)==nrow(space2.cov)&&ncol(space2)==ncol(space2.cov)),
    is.null(space2.exp)||ncol(space2)==length(space2.exp)||(ncol(space2)==nrow(space2.exp)&&ncol(space2.exp)==1)
    )
  if (any(colnames(df)%in%colnames(space2))||any(colnames(space2)%in%colnames(df))){
    stop("column names need to be unique across both spaces")
  }
  numeric.colnames <- c()
  nonnumeric.colnames <- c()
  options(warn = 1)
  for (var in colnames(df)){
    if (is.numeric(df[[var]])){
      numeric.colnames <- append(numeric.colnames,var)
    } else {
      warning(paste(var,"in df is non numeric and has been removed"))
      nonnumeric.colnames <- append(nonnumeric.colnames,var)
    }
  }
  for (var in colnames(space2)){
    if (is.numeric(space2[[var]])){
      numeric.colnames <- append(numeric.colnames,var)
    } else {
      warning(paste(var,"in space2 is non numeric and has been removed"))
      nonnumeric.colnames <- append(nonnumeric.colnames,var)
    }
  }

  df.colnames <- colnames(df)
  space2.colnames <- colnames(space2)
  full.colnames <- c(df.colnames,space2.colnames)
  group.colnames <- colnames(group)

  if (any(is.na(df),is.na(space2))){
    filt <- rowSums(is.na(cbind(df,space2)))
    if (requireNamespace("VIM", quietly = TRUE)) {
      warning(paste(sum(filt),"NA entries observed, kNN imputation has been used to replace these"))
      if (any(is.na(df))) {df <- VIM::kNN(df[df.colnames %in% numeric.colnames])}
      if (any(is.na(space2))) {space2 <- VIM::kNN(space2[space2.colnames %in% numeric.colnames])}
    }else{
      warning(paste(sum(filt),"NA entries observed, rows have been removed for containing na values, install VIM for automatic imputation"))
      df <- df[filt==0,]
      if (!is.null(space2)) {space2 <- space2[filt==0,]}
      if (!is.null(group))  {group  <- group[filt==0,]}
      if (!is.null(label))  {label  <- label[filt==0,]}
    }
  }


  rv <- shiny::reactiveValues()
  rv$load.app <- FALSE
  rv$detouring <- FALSE
  rv$radialWarn <-0
  rv$colour.choice <- c("clustering")
  n <- nrow(df)
  if(is.inv & !is.null(cov)) {
    covInv <- cov
    cov <- solve(covInv)}

  lab.choice <-if (is.null(label)){c(choose="",full.colnames)} else {c(choose="",full.colnames,"label")}
  group.choice <- c(group.colnames,full.colnames)

  tmp <- tempdir()

  server <- function(input, output, session) {

    if (!is.null(user_dist)) {
      shiny::updateSelectInput(session, "metric", choices = c("euclidean",
                                                              "maximum", "manhattan", "canberra", "binary",
                                                              "minkowski", "user"))
      shiny::updateSelectInput(session, "metricA", choices = c("euclidean",
                                                               "maximum", "manhattan", "canberra", "binary",
                                                               "minkowski", "user"))
      shiny::updateSelectInput(session, "metricB", choices = c("euclidean",
                                                               "maximum", "manhattan", "canberra", "binary",
                                                               "minkowski", "user"))
    }
    if(!is.null(space2)){
      shiny::updateSelectizeInput(session,"space1",choices = numeric.colnames, selected = df.colnames)
      shiny::updateSelectizeInput(session,"space2",choices = numeric.colnames, selected = space2.colnames)
    } else {
      shiny::updateSelectizeInput(session,"space1",choices = numeric.colnames)
      shiny::updateSelectizeInput(session,"space2",choices = numeric.colnames)
    }
    shiny::updateSelectizeInput(session,"label",choices = lab.choice, selected = "label")
    shiny::updateSelectInput(session,"coord_space1",choices = names(getCoords))
    shiny::updateSelectInput(session,"coord_space2",choices = names(getCoords))
    shiny::updateSelectInput(session,"coordA",choices = names(getCoords))
    shiny::updateSelectInput(session,"coordB",choices = names(getCoords))
    shiny::updateSelectInput(session,"algorithm1",choices = names(dimReduction))
    shiny::updateSelectInput(session,"algorithm2",choices = names(dimReduction))
    shiny::hideTab("navbar","1",session)

    shiny::observeEvent(input$space1,{
      shiny::updateSelectizeInput(session,"space2",choices = numeric.colnames[!(numeric.colnames %in% c(input$space1,input$group,input$label))], selected = input$space2)
      shiny::updateSelectizeInput(session,"group",choices = c(choose="",group.choice[!(group.choice %in% c(input$space1,input$space2,input$label))]), selected = input$group)
      shiny::updateSelectizeInput(session,"label",choices = lab.choice[!(lab.choice %in% c(input$space1,input$space2,input$group))], selected = input$label)
    }, ignoreInit = T, ignoreNULL = F)
    shiny::observeEvent(input$space2,{
      shiny::updateSelectizeInput(session,"space1",choices = numeric.colnames[!(numeric.colnames %in% c(input$space2,input$group,input$label))], selected = input$space1)
      shiny::updateSelectizeInput(session,"group",choices = c(choose="",group.choice[!(group.choice %in% c(input$space1,input$space2,input$label))]), selected = input$group)
      shiny::updateSelectizeInput(session,"label",choices = lab.choice[!(lab.choice %in% c(input$space1,input$space2,input$group))], selected = input$label)
    }, ignoreInit = T, ignoreNULL = F)
    shiny::observeEvent(input$group,{
      shiny::updateSelectizeInput(session,"space1",choices = numeric.colnames[!(numeric.colnames %in% c(input$space2,input$group,input$label))], selected = input$space1)
      shiny::updateSelectizeInput(session,"space2",choices = numeric.colnames[!(numeric.colnames %in% c(input$space1,input$group,input$label))], selected = input$space2)
      shiny::updateSelectizeInput(session,"label",choices = lab.choice[!(lab.choice %in% c(input$space1,input$space2,input$group))], selected = input$label)
    }, ignoreInit = T, ignoreNULL = F)
    shiny::observeEvent(input$label,{
      shiny::updateSelectizeInput(session,"space1",choices = numeric.colnames[!(numeric.colnames %in% c(input$space2,input$group,input$label))], selected = input$space1)
      shiny::updateSelectizeInput(session,"space2",choices = numeric.colnames[!(numeric.colnames %in% c(input$space1,input$group,input$label))], selected = input$space2)
      shiny::updateSelectizeInput(session,"group",choices = c(choose="",group.choice[!(group.choice %in% c(input$space1,input$space2,input$label))]), selected = input$group)
    }, ignoreInit = T, ignoreNULL = F)

    shiny::observeEvent(input$group,{
      if (is.null(input$group)){
        rv$tmp.group <- NULL
        shinyFeedback::hideFeedback("group",session)
      } else{
        group.df <-
          dplyr::bind_cols(
            dplyr::select(df, tidyselect::any_of(input$group)),
            if (!is.null(space2)) {dplyr::select(space2, tidyselect::any_of(input$group))},
            if (!is.null(group)) {dplyr::select(group,  tidyselect::any_of(input$group))}
          ) %>%
          dplyr::select(tidyselect::all_of(input$group))

        rv$temp.group <- interaction(group.df)
        n.level <-nlevels(rv$temp.group)

        if (n.level>12){
          shinyFeedback::hideFeedback("group",session)
          shinyFeedback::feedbackWarning("group", T, text = paste("number of groups made:",n.level,"WARNING max 12"), session = session)
        } else {
          shinyFeedback::hideFeedback("group",session)
          shinyFeedback::feedbackSuccess("group", T, text = paste("number of groups made:",n.level), session = session)
        }
      }
    }, ignoreNULL = F)

    shiny::observeEvent(input$app.load,{
      rv$space1 <-
        dplyr::bind_cols(
          dplyr::select(df,     tidyselect::any_of(input$space1)),
          if (!is.null(space2)) {dplyr::select(space2, tidyselect::any_of(input$space1))}
        ) %>%
        dplyr::select(tidyselect::all_of(input$space1))

      rv$space2 <-
        dplyr::bind_cols(
          dplyr::select(df,     tidyselect::any_of(input$space2)),
          if (!is.null(space2)) {dplyr::select(space2, tidyselect::any_of(input$space2))}
        ) %>%
        dplyr::select(tidyselect::all_of(input$space2))

      if(all(input$label!="")){
        if (input$label == "label"){
          rv$label <- as.vector(as.matrix(label))
        } else {
            rv$label <-
              dplyr::bind_cols(
                dplyr::select(df,     tidyselect::any_of(input$label)),
                if (!is.null(space2)) {dplyr::select(space2, tidyselect::any_of(input$label))}
              )
          }

      }else{ rv$label <- ""}

      if (all(input$space1 %in% df.colnames) & !is.null(cov)){
        space1.filt <- which(df.colnames %in% input$space1)
        rv$cov1  <- cov[space1.filt,space1.filt]
      } else {
        rv$cov1 <- cov(rv$space1)
      }

      if (is.inv & setequal(df.colnames,input$space1)) {rv$covInv1 <- covInv}
      else {rv$covInv1 <- try(solve(rv$cov1))}

      if (all(input$space2 %in% df.colnames)){
        space2.filt <- which(space2.colnames %in% input$space2)
        rv$cov2  <- cov[space2.filt,space2.filt]
      } else {
        rv$cov2 <- cov(rv$space2)
      }
      rv$covInv2 <- try(solve(rv$cov2))

      if (is.null(input$group)){
        rv$colour.choice <- intersect(rv$colour.choice,c("clustering","score","bins"))
        shiny::updateCheckboxInput(session,"usegroupB", value = F)

      } else{
        rv$colour.choice <- unique(c(rv$colour.choice,"user"))
      }

      rv$load.app <- TRUE

      rv$ndf <- ncol(rv$space2)
      rv$cond <- 1:n
      if (setequal(df.colnames,input$space1) & !is.null(exp)) {
        rv$exp <- data.frame(value = exp)
      } else if (all(input$space1 %in% df.colnames) & !is.null(exp)){
          rv$exp <- data.frame(value = exp[space1.filt,])
      } else {
          rv$exp <- data.frame(value = colMeans(rv$space1))
      }
      if (setequal(space2.colnames,input$space2) & !is.null(space2.exp)) {
        rv$exp2 <- data.frame(value = space2.exp)
      } else if (all(input$space1 %in% df.colnames) & !is.null(space2.exp)){
        rv$exp2 <- data.frame(value = space2.exp[space2.filt,])
      } else {
        rv$exp2 <- data.frame(value = colMeans(rv$space2))
      }
      rv$user.group <- rv$temp.group
      if (!is.null(rv$user.group)){
        rv$user.pal <- RColorBrewer::brewer.pal(nlevels(rv$user.group), "Set3")
        rv$pcol <- rv$user.pal[rv$user.group]
      }
      shiny::updateSelectInput(session,"px", choices = input$space2)
      shiny::updateSelectInput(session,"py", choices = input$space2, selected = input$space2[2])
      shiny::updateSelectInput(session,"pxA", choices = input$space2)
      shiny::updateSelectInput(session,"pyA", choices = input$space2, selected = input$space2[2])
      shiny::updateSelectInput(session,"pxB", choices = input$space2)
      shiny::updateSelectInput(session,"pyB", choices = input$space2, selected = input$space2[2])

      rv$space1names <- input$space1
      rv$space2names <- input$space2

      rv$coord1 <- try(getCoords[[input$coord_space1]](df=rv$space1, cov=rv$cov1, covInv=rv$covInv1, exp=rv$exp))
      rv$coord2 <- try(getCoords[[input$coord_space2]](df=rv$space2, cov=rv$cov2, covInv=rv$covInv2, exp=rv$exp2))
      shiny::showTab("navbar","1",select = TRUE,session)

    }, priority = 1)

    shiny::observeEvent(c(input$metric, rv$coord1, rv$coord2,
                          input$linkage, rv$space1, rv$space2, input$kC,
                          input$app.load), {
      shiny::req(rv$load.app)
      rv$kC <- as.numeric(input$kC)
      if (!is.null(getScore)){
        rv$value<- getScore(space1=rv$space1, cov=rv$cov1, covinv=rv$covInv1, exp=rv$exp, space2=rv$space2, space2.cov=rv$cov2, space2.exp=rv$space2.exp, k=rv$kC)
        if (!is.null(rv$value$score)){
          rv$scorecol <- viridis::viridis(n)[rank(rv$value$score)]
          rv$colour.choice <- unique(c(rv$colour.choice,"score"))
        }
        if(!is.null(rv$value$bins)){
          rv$palSig <- RColorBrewer::brewer.pal(length(unique(rv$value$bins)), "Set2")
          rv$colSig <- rv$palSig[rv$value$bins]
          rv$colour.choice <- unique(c(rv$colour.choice,"bins"))
        }
        }

      dists1 <- tryCatch(getDists(rv$coord1, input$metric, user_dist), error = function(e){
        warning("Distances have failed to calculate, coordinates may not have been calculated", call. = F)
      })
      rv$d_mat <- as.matrix(dists1)

      dists2 <- tryCatch(getDists(rv$coord2, input$metric, NULL), error = function(e){
        warning("Distances have failed to calculate, coordinates may not have been calculated", call. = F)
      })
      rv$d_mat2 <- as.matrix(dists2)


      rv$fit <- stats::hclust(dists1, input$linkage)
      rv$cstats <- getClusterStats(dists1,rv$fit, computeChi2(rv$space1, rv$covInv1, rv$exp), 8)
      groups <- stats::cutree(rv$fit, k = rv$kC)
      rv$lvl <- unique(groups[stats::order.dendrogram(stats::as.dendrogram(rv$fit))])
      rv$groups <- as.numeric(factor(groups, levels = rv$lvl))
      shiny::updateSelectizeInput(session,"pc.filt",choices = unique(rv$groups),selected = unique(rv$groups))
      rv$pal <- RColorBrewer::brewer.pal(rv$kC, "Dark2")
      rv$col <- rv$pal[rv$groups]
      rv$benchmarks <- getBenchmarkInformation(rv$d_mat, rv$groups)
      a <- round(max(c(rv$benchmarks$r,0.1)),1)
      shiny::updateSliderInput(session,"alpha", max = a, value = a/2, step = a/100)
      rv$bpDists <- getClusterDists(rv$d_mat, rv$groups, rv$benchmarks)
      rv$pointcol <- rv$col
      rv$pointcol[rv$value$is.interest] <- "black"
      rv$pch <- rep(1, n)
      rv$pch[rv$value$is.interest] <- 2
    }, priority = 80, ignoreInit = T)
    shiny::observeEvent(rv$colour.choice,{
      shiny::updateSelectInput(session,"colouring1","Colouring", choices = rv$colour.choice)
      shiny::updateSelectInput(session,"colouring2","Colouring", choices = rv$colour.choice)
      shiny::updateSelectInput(session,"dimred.colour1","Colouring", choices = rv$colour.choice)
      shiny::updateSelectInput(session,"dimred.colour2","Colouring", choices = rv$colour.choice)
    }, priority = -1)

    output$heatmap <- shiny::renderPlot({
      shiny::req(rv$load.app)
      plotHeatmap(rv$d_mat, rv$fit, rv$kC, rv$pal)
    }, height = 325)
    output$chi2 <- shiny::renderPlot({
      shiny::req(rv$load.app,rv$value)
      plotChi2(rv$space2, rv$value$score, input$px, input$py, rv$value$scoreName, rv$cond)
    }, height = 325)
    output$clusterstats <- shiny::renderPlot({
      shiny::req(rv$load.app)
      rv$cstats %>% tidyr::pivot_longer(cols = .data$within.cluster.ss:.data$dmin,
                                        names_to = "stat") %>%
        ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(x = .data$k, y = .data$value)) +
        ggplot2::xlab("# clusters") + ggplot2::ylab("") +
        ggplot2::facet_wrap(~stat, ncol = 3, scales = "free_y",
                            labeller = cstat_labeller()) + ggplot2::theme_bw()
    }, height = 600)
    output$scalar <- shiny::renderPlot({
      shiny::req(rv$load.app)
      dat <- rv$coord1[rv$cond, ]
      dat %>% tourr::rescale() %>% tibble::as_tibble() %>%
        cbind(rv$space2[rv$cond, ]) %>%
        tidyr::pivot_longer(cols = rv$space1names,
                            names_to = "observable") %>%
        dplyr::mutate(observable = factor(.data$observable,
                                          levels = rv$space1names)) %>%
        ggplot2::ggplot(ggplot2::aes_string(input$px,input$py, color = "value")) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~observable, scales = "free",
                            ncol = 7) + ggplot2::guides(colour = "none") +
        ggplot2::scale_colour_viridis_c() + ggplot2::theme_bw() +
        ggplot2::ggtitle("Centered coordinate values for all observables") +
        ggplot2::theme(aspect.ratio = 1)
    })
    output$pc <- shiny::renderPlot({
      shiny::req(rv$load.app)
      plotPC(rv$coord1, rv$groups, rv$benchmarks$id, input$pc.filt, input$pc.centre, input$pc.scale, a=0.2, rv$pal)
    })
    output$wc <- shiny::renderPlot({
      shiny::req(rv$load.app)
      plotWC(rv$space2, input$px, input$py, rv$value$is.interest, rv$benchmarks$id,
                rv$col, rv$cond, groups = rv$groups, pal = rv$pal, a = input$alpha, showalpha = input$plotHulls)
    }, height = 325)
    output$sigbins <- shiny::renderPlot({
      shiny::req(rv$load.app,rv$value)
      plotSigBin(rv$space2, rv$value$is.interest, rv$benchmarks$id, rv$value$bins,
                    input$px, input$py, rv$value$binName, rv$cond, "Set2")
    }, height = 325)
    output$distText <- shiny::renderText(
      paste0("Average distance: ",round(mean(as.vector(rv$d_mat)), 1),
             ", Maximum distance: ",round(max(as.vector(rv$d_mat)), 1))
      )
    output$hist <- shiny::renderPlot({
      shiny::req(rv$load.app)
      groups<-rv$groups
      dist_vec <- as.vector(rv$d_mat)
      gr1 <- rep(groups,each = n)
      gr2 <- rep(groups,times = n)
      dist_tib <- tibble::tibble(dist = dist_vec,gr1 = as.factor(gr1), gr2 = as.factor(gr2)) %>%
        dplyr::mutate(match = dplyr::if_else(gr1 == gr2, "within", "between"))
      ggplot2::ggplot(dist_tib,
                            ggplot2::aes(x = .data$dist, y = ggplot2::after_stat(.data$density*.data$width))) +
        ggplot2::geom_histogram(data = dplyr::select(dist_tib, -gr1, -gr2, -match),
                                fill = NA, color = "grey",position = "identity") +
        ggplot2::geom_histogram(mapping = ggplot2::aes(color = gr1),
                                fill = NA, position = "identity") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::facet_grid(gr1 ~ match) + ggplot2::theme_bw() +
        ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::ggtitle("Distribution of distances within and between clusters") +
        ggplot2::theme(legend.position = "none", strip.text.y = ggplot2::element_blank())
    })
    output$benchmarks = DT::renderDT({
      shiny::req(rv$load.app)
      dt <- rv$space2[rv$benchmarks$id, ] %>% tibble::add_column(cluster = rv$benchmarks$group) %>%
        tibble::add_column(r = rv$benchmarks$r) %>% tibble::add_column(d = rv$benchmarks$d) %>%
        tibble::add_column(score = rv$value$score[rv$benchmarks$id])
      DT::formatRound(DT::datatable(dt[, c(rv$ndf + 1,
                                           1:rv$ndf, (rv$ndf + 2):ncol(dt))], rownames = FALSE),
                      2:ncol(dt), digits = 2) %>% DT::formatStyle("cluster",
                                                                  backgroundColor = DT::styleEqual(rv$benchmarks$group,
                                                                                                   unique(rv$col)))
    })
    output$benchmarks.grouping <- DT::renderDT({
      shiny::req(rv$load.app)
      group.benchmarks <- getBenchmarkInformation(rv$d_mat, as.numeric(rv$user.group))
      dt <- rv$space2[group.benchmarks$id, ] %>% tibble::add_column(cluster = unique(as.character(rv$user.group))) %>%
        tibble::add_column(r = group.benchmarks$r) %>% tibble::add_column(d = group.benchmarks$d) %>%
        tibble::add_column(score = rv$value$score[group.benchmarks$id])
      DT::formatRound(DT::datatable(dt[, c(rv$ndf + 1,
                                           1:rv$ndf, (rv$ndf + 2):ncol(dt))], rownames = FALSE),
                      2:ncol(dt), digits = 2) %>% DT::formatStyle("cluster",
                                                                  backgroundColor = DT::styleEqual(unique(as.character(rv$user.group)),
                                                                                                   unique(rv$pcol)))
    })

    #tour options
    shiny::observeEvent(c(input$colouring1,rv$groups,rv$user.group,rv$value$score),{
      rv$tour1<- colourHelper(input$colouring1,rv)
    })
    shiny::observeEvent(c(input$colouring2,rv$groups,rv$user.group,rv$value$score),{
      rv$tour2<- colourHelper(input$colouring2,rv)
    })
    shiny::observeEvent(c(input$tour1data,rv$space1names,rv$space2names),{
      rv$tour1projection <- switch(input$tour1data,
                                   "space1" = rv$space1names,
                                   "space2" = rv$space2names,
                                   "space1 PCA" = paste0("pc", 1:min(5, ncol(rv$space1))),
                                   "space2 PCA" = paste0(" pc", 1:min(5, ncol(rv$space2))))
      shiny::updateSelectInput(session,"select_radial_obs",choices = rv$tour1projection, selected = rv$tour1projection[[1]])
      rv$tour1data       <- switch(input$tour1data,
                                   "space1" = rv$coord1,
                                   "space2" = rv$coord2,
                                   "space1 PCA" = rv$pca1,
                                   "space2 PCA" = rv$pca2)
    },priority = 1)
    shiny::observeEvent(c(input$tour2data,rv$space1names,rv$space2names),{
      rv$tour2projection <- switch(input$tour2data,
                                   "space1" = rv$space1names,
                                   "space2" = rv$space2names,
                                   "space1 PCA" = paste0("pc", 1:min(5, ncol(rv$space1))),
                                   "space2 PCA" = paste0(" pc", 1:min(5, ncol(rv$space2))))
      shiny::updateSelectInput(session,"select_radial_par",choices = rv$tour2projection, selected = rv$tour2projection[[1]])
      rv$tour2data       <- switch(input$tour2data,
                                   "space1" = rv$coord1,
                                   "space2" = rv$coord2,
                                   "space1 PCA" = rv$pca1,
                                   "space2 PCA" = rv$pca2)
    },priority = 1)

    shiny::observeEvent(c(input$render1),{
      if (input$render1 == "2D"){
        rv$tour.dim1 <- 2
        shiny::updateSelectInput(session,"tour_type_obs", choices =
                                   c("grand","cmass","holes","lda","pda","dcor","spline","radial","anomaly"))
      } else {
        rv$tour.dim1 <- 3
        shiny::updateSelectInput(session,"tour_type_obs", choices =
                                   c("grand","cmass","holes","lda","pda","dcor"))
      }
    },priority = 1)
    shiny::observeEvent(c(input$render2),{
      if (input$render2 == "2D"){
        rv$tour.dim2 <- 2
        shiny::updateSelectInput(session,"tour_type_param", choices =
                                   c("grand","cmass","holes","lda","pda","dcor","spline","radial","anomaly"))
      } else {
        rv$tour.dim2 <- 3
        shiny::updateSelectInput(session,"tour_type_param", choices =
                                   c("grand","cmass","holes","lda","pda","dcor"))
      }
    },priority = 1)

    shiny::observeEvent(c(input$radial_start_par, rv$space1_tour, input$slice_par,input$tour_type_param,rv$tour2data,rv$tour2),{
      if (input$tour_type_param == "radial"){
        if (input$radial_start_par == "random") {
          rv$radial_view_par <- tourr::basis_random(length(rv$tour2projection))
        }else {
          radial_tour_par <- switch(input$radial_start_par,
                                    "cmass"  = tourr::guided_tour(tourr::cmass()),
                                    "holes"  = tourr::guided_tour(tourr::holes()),
                                    "lda"    = tourr::guided_tour(tourr::lda_pp(rv$tour2$colour)),
                                    "pda"    = tourr::guided_tour(tourr::pda_pp(rv$tour2$colour)),
                                    "dcor"   = tourr::guided_tour(tourr::dcor2d()),
                                    "spline" = tourr::guided_tour(tourr::splines2d()),
          )
          hist <- tourr::save_history(rv$tour2data, tour_path = radial_tour_par, max_bases = 1000)
          view <- drop(hist[,,dim(hist)[3]])
          attr(view,"class") <- NULL
          attr(view,"data")  <- NULL
          rv$radial_view_par <- view
        }
      }
    },priority = 1)
    shiny::observeEvent(c(input$radial_start_obs, rv$space1_tour, input$slice_obs,input$tour_type_obs,rv$tour1data,rv$tour1),{
      if (input$tour_type_obs=="radial"){
        if (input$radial_start_obs == "random") {
          rv$radial_view_obs <- tourr::basis_random(length(rv$tour1projection))
        }else {
          radial_tour_obs <- switch(input$radial_start_obs,
                                    "cmass"  = tourr::guided_tour(tourr::cmass()),
                                    "holes"  = tourr::guided_tour(tourr::holes()),
                                    "lda"    = tourr::guided_tour(tourr::lda_pp(rv$tour1$colour)),
                                    "pda"    = tourr::guided_tour(tourr::pda_pp(rv$tour1$colour)),
                                    "dcor"   = tourr::guided_tour(tourr::dcor2d()),
                                    "spline" = tourr::guided_tour(tourr::splines2d()),
          )
          hist <- tourr::save_history(rv$tour1data, tour_path = radial_tour_obs, max_bases = 1000)
          view <- drop(hist[,,dim(hist)[3]])
          attr(view,"class") <- NULL
          attr(view,"data")  <- NULL
          rv$radial_view_obs <- view
        }
      }
    },priority = 1)
    shiny::observeEvent(c(input$tour_type_param, input$select_radial_par,input$ellc_param,
                          rv$radial_view_par,rv$tour2projection,rv$tour.dim2
                          ,rv$coord1,rv$coord2,rv$pca1,rv$pca2,
                          rv$coord_red1$Y,rv$coord_red2$Y,rv$tour1,rv$tour2), {
      if (is.null(input$select_radial_par)){
        rad.var<-1
        shinyFeedback::feedbackWarning("select_radial_par", T, text = "Warning: a radial variable must be selected", session = session)
        rv$radialWarn <- rv$radialWarn+1
      }else{
        rad.var <- which(rv$tour2projection %in% input$select_radial_par)
        shinyFeedback::hideFeedback("select_radial_par", session = session)
      }
      rv$paramType <- switch(input$tour_type_param,
                             "grand"  = tourr::grand_tour(rv$tour.dim2),
                             "cmass"  = tourr::guided_tour(tourr::cmass(),rv$tour.dim2),
                             "holes"  = tourr::guided_tour(tourr::holes(),rv$tour.dim2),
                             "lda"    = tourr::guided_tour(tourr::lda_pp(rv$tour2$colour),rv$tour.dim2),
                             "pda"    = tourr::guided_tour(tourr::pda_pp(rv$tour2$colour),rv$tour.dim2),
                             "dcor"   = tourr::guided_tour(tourr::dcor2d(),rv$tour.dim2),
                             "spline" = tourr::guided_tour(tourr::splines2d(),rv$tour.dim2),
                             "radial" = tourr::radial_tour(rv$radial_view_par, rad.var),
                             "anomaly"= tourr::guided_anomaly_tour(tourr::anomaly_index(),ellipse = rv$cov2, ellc=as.numeric(input$ellc_param))
      )
      rv$detour2.angles <- if (input$tour_type_param == "radial") 1 / 2 else 1
      rv$tour_bases1 <- if(input$tour_type_param %in% c("cmass","holes","lda","pda","dcor","spline","anomaly")) 1000 else 20
    },priority = 1)
    shiny::observeEvent(c(input$tour_type_obs,input$select_radial_obs,input$ellc_obs,
                          rv$radial_view_obs,rv$tour1projection,rv$tour.dim1,
                          rv$coord1,rv$coord2,rv$pca1,rv$pca2,
                          rv$coord_red1$Y,rv$coord_red2$Y,rv$tour1,rv$tour2),{
      if (is.null(input$select_radial_obs)){
        rad.var<-1
        shinyFeedback::feedbackWarning("select_radial_obs", T, text = "Warning: a radial variable must be selected", session = session)
        rv$radialWarn <- rv$radialWarn+1
      }else{
        rad.var <- which(rv$tour1projection %in% input$select_radial_obs)
        shinyFeedback::hideFeedback("select_radial_obs", session = session)
      }
      rv$obsType <- switch(input$tour_type_obs,
                           "grand"  = tourr::grand_tour(rv$tour.dim1),
                           "cmass"  = tourr::guided_tour(tourr::cmass(),rv$tour.dim1),
                           "holes"  = tourr::guided_tour(tourr::holes(),rv$tour.dim1),
                           "lda"    = tourr::guided_tour(tourr::lda_pp(rv$tour1$colour),rv$tour.dim1),
                           "pda"    = tourr::guided_tour(tourr::pda_pp(rv$tour1$colour),rv$tour.dim1),
                           "dcor"   = tourr::guided_tour(tourr::dcor2d(),rv$tour.dim1),
                           "spline" = tourr::guided_tour(tourr::splines2d(),rv$tour.dim1),
                           "radial" = tourr::radial_tour(rv$radial_view_obs, rad.var),
                           "anomaly"= tourr::guided_anomaly_tour(tourr::anomaly_index(), ellipse = rv$cov1, ellc = as.numeric(input$ellc_obs))
      )
      rv$detour1.angles <- if (input$tour_type_obs == "radial") 1 / 2 else 1
      rv$tour_bases2 <- if(input$tour_type_obs %in% c("cmass","holes","lda","pda","dcor","spline","anomaly")) 1000 else 20
    },priority = 1)
    shiny::observeEvent(c(input$slice_obs,input$slw_obs,rv$space1_tour,input$ellc_obs,rv$ell_obs,rv$tour1),{
      if (input$slice_obs) {
        rv$displayobs <- function(x){
          return(detourr::show_slice(x, palette = rv$tour1$pal, slice_relative_volume = as.numeric(input$slw_obs)))
        }
      }
      else {
        rv$displayobs <- function(x){
          return(detourr::show_scatter(x, palette = rv$tour1$pal, alpha=0.6)) # pch = rv$space1_tour$pch, ellipse = rv$ell_obs, ellc = as.numeric(input$ellc_obs)
        }
      }
    })
    shiny::observeEvent(c(input$slice_par,input$slw_par,rv$space1_tour,input$ellc_param,rv$ell_par,rv$tour2),{
      if(input$slice_par){
        rv$displayparam <- function(x){
          return(detourr::show_slice(x, palette = rv$tour2$pal, slice_relative_volume = as.numeric(input$slw_par)))
        }
      }
      else {
        rv$displayparam <- function(x){
          return(detourr::show_scatter(x, palette = rv$tour2$pal, alpha=0.6)) # pch = rv$space2_tour$pch, ellipse = rv$ell_par, ellc = as.numeric(input$ellc_param)
        }
      }
    })

    #dimension reduction options
    shiny::observeEvent(c(input$dimred.colour1,rv$groups,rv$user.group,rv$value$score),{
      rv$dimRed1<- colourHelper(input$dimred.colour1,rv)
    })
    shiny::observeEvent(c(input$dimred.colour2,rv$groups,rv$user.group,rv$value$score),{
      rv$dimRed2<- colourHelper(input$dimred.colour2,rv)
    })


    shiny::observeEvent(c(rv$coord1,rv$coord2,input$algorithm1,input$red1.data),{
      shiny::req(rv$load.app)
      if ((input$algorithm1==input$algorithm2)&&(input$red1.data==input$red2.data)){
        rv$coord_red1 <- rv$coord_red2
      } else {
        if(input$red1.data=="space1"){
          rv$coord_red1 <- try(dimReduction[[input$algorithm1]](mat = rv$coord1, dist = rv$d_mat))
        } else {
          rv$coord_red1 <- try(dimReduction[[input$algorithm1]](mat = rv$coord2, dist = rv$d_mat2))
        }
      }

      tryCatch(colnames(rv$coord_red1$Y) <- c("dim1A","dim2A"), error = function(e){
        warning("dimension reduction has not returned correctly formatted data", call. = F)
      })
    },ignoreInit = T)
    shiny::observeEvent(c(rv$coord1,rv$coord2,input$algorithm2,input$red2.data),{
      shiny::req(rv$load.app)
      if ((input$algorithm1==input$algorithm2)&&(input$red1.data==input$red2.data)){
        rv$coord_red2 <- rv$coord_red1
      } else {
        if(input$red2.data=="space1"){
          rv$coord_red2 <- try(dimReduction[[input$algorithm2]](mat = rv$coord1, dist = rv$d_mat))
        } else {
          rv$coord_red2 <- try(dimReduction[[input$algorithm2]](mat = rv$coord2, dist = rv$d_mat2))
        }
      }

      tryCatch(colnames(rv$coord_red2$Y) <- c("dim1B","dim2B"), error = function(e){
        warning("dimension reduction has not returned correctly formatted data", call. = F)
      })
    },ignoreInit = T)

    shiny::observeEvent(c(rv$coord1,rv$coord2),{
      rv$pca1 <- stats::prcomp(rv$coord1)$x[, 1:min(5,ncol(rv$space1))]
      colnames(rv$pca1) <- paste0("pc", 1:min(5, ncol(rv$space1)))
      rv$pca2 <- stats::prcomp(rv$coord2)$x[, 1:min(5,ncol(rv$coord2))]
      colnames(rv$pca2) <- paste0(" pc", 1:min(5, ncol(rv$coord2)))
    })

    tour.data <- shiny::reactive({shiny::req(rv$coord1,rv$coord2)
      data <- dplyr::bind_cols(rv$coord1,rv$coord2,rv$pca1,rv$pca2,rv$coord_red1$Y,rv$coord_red2$Y)
      data$colour1<- rv$tour1$colour
      data$colour2<- rv$tour2$colour
      data$label1 <- rv$tour1$label
      data$label2 <- rv$tour2$label
      data
    })



    shared.data <- crosstalk::SharedData$new(tour.data)


    output$dimRed1 <- plotly::renderPlotly({
      shiny::req(rv$load.app)
      shiny::validate(
        shiny::need(try(!is.null(shared.data$origData()[c("dim1A","dim2A")])),
                    "data cannot be plotted select a different dimension reduction algorithm or data")
      )
      dim.title   <- paste(input$algorithm1, "embedding")
      dim.axislab <- input$algorithm1
      dim.text    <- rv$dimRed1$label

      plotly::plot_ly(shared.data, x = ~dim1A, y = ~dim2A,
                      color = as.factor(rv$dimRed1$colour), symbol = rv$pch, symbols = c("circle","o"),
                      text = dim.text, colors = rv$dimRed1$pal,
                      marker = list(showscale = FALSE)) %>%
        plotly::add_trace(type = "scatter", mode = "markers") %>%
        plotly::layout(title = dim.title,
                       xaxis = list(title = paste(dim.axislab,1), scaleanchor = "y", range = rv$lim),
                       yaxis = list(title = paste(dim.axislab,2), range = rv$lim),
                       showlegend = FALSE) %>%
        plotly::highlight(on = "plotly_selected", off = "plotly_deselect", selected = plotly::attrs_selected(opacity=1))
    })

    output$dimRed2 <- plotly::renderPlotly({
      shiny::req(rv$load.app)
      shiny::validate(
        shiny::need(try(!is.null(shared.data$origData()[c("dim1B","dim2B")])),
                    "data cannot be plotted select a different dimension reduction algorithm or data")
      )
      dim.title   <- paste(input$algorithm2, "embedding")
      dim.axislab <- input$algorithm2
      dim.text    <- rv$dimRed2$label

      plotly::plot_ly(shared.data, x = ~dim1B, y = ~dim2B,
                      color = as.factor(rv$dimRed2$colour), symbol = rv$pch, symbols = c("circle","o"),
                      text = dim.text, colors = rv$dimRed2$pal,
                      marker = list(showscale = FALSE)) %>%
        plotly::add_trace(type = "scatter", mode = "markers") %>%
        plotly::layout(title = dim.title,
                       xaxis = list(title = paste(dim.axislab,1), scaleanchor = "y", range = rv$lim),
                       yaxis = list(title = paste(dim.axislab,2), range = rv$lim),
                       showlegend = FALSE) %>%
        plotly::highlight(on = "plotly_selected", off = "plotly_deselect", selected = plotly::attrs_selected(opacity=1))
    })

    output$tourImg1 <- detourr::shinyRenderDetour({
      shiny::validate(
        shiny::need(try(!is.null(shared.data$origData()[rv$tour1projection])),"data cannot be plotted select different data")
      )

      detourr::detour(shared.data, detourr::tour_aes(projection = tidyselect::all_of(rv$tour1projection), colour = "colour1", label = I(.data$label1))) %>% #.data may be deprecated in this way
        detourr::tour_path(rv$obsType, fps = 60, aps = rv$detour1.angles, max_bases = rv$tour_bases2) %>%
        rv$displayobs()
    })
    output$tourImg2 <- detourr::shinyRenderDetour({
      shiny::validate(
        shiny::need(try(!is.null(shared.data$origData()[rv$tour2projection])),"data cannot be plotted select different data")
      )
      rv$detouring <- TRUE
      detourr::detour(shared.data, detourr::tour_aes(projection = tidyselect::all_of(rv$tour2projection), colour = "colour2", label = I(.data$label2))) %>% #.data may be deprecated in this way
        detourr::tour_path(rv$paramType, fps = 60, aps = rv$detour2.angles, max_bases = rv$tour_bases1) %>%
        rv$displayparam()
    })

    shiny::observeEvent(rv$detouring,{
      shiny::outputOptions(output, "tourImg1", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "tourImg2", suspendWhenHidden = FALSE)
    }, once = TRUE, ignoreInit = TRUE)

    shiny::observeEvent(c(input$metric,input$linkage,input$kC,input$usemainA),{
      shiny::req(input$usemainA)
      shiny::updateSelectInput(session,"metricA",selected = input$metric)
      shiny::updateSelectInput(session,"linkageA",selected = input$linkage)
      shiny::updateSelectInput(session,"kA",selected = input$kC)
    })

    shiny::observeEvent(c(input$metricA, input$linkageA,
                          input$linkageB, input$metricB,
                          input$coordA, input$coordB,
                          input$kA, input$kB, rv$space1),{
      shiny::req(rv$load.app)
      kA <- as.numeric(input$kA)
      kB <- as.numeric(input$kB)
      coordA <- getCoords[[input$coordA]](df=rv$space1, cov=rv$cov1, covInv=rv$covInv1, exp=rv$exp)
      coordB <- getCoords[[input$coordB]](df=rv$space1, cov=rv$cov1, covInv=rv$covInv1, exp=rv$exp)
      distsA <- getDists(coordA, input$metricA, user_dist)
      d_matA <- as.matrix(distsA)
      distsB <- getDists(coordB, input$metricB, user_dist)
      d_matB <- as.matrix(distsB)
      fitA <- stats::hclust(distsA, input$linkageA)
      fitB <- stats::hclust(distsB, input$linkageB)
      groupsA <- stats::cutree(fitA, kA)
      groupsB <- stats::cutree(fitB, kB)
      palA <- RColorBrewer::brewer.pal(kA, "Dark2")
      groupsA <- as.numeric(factor(groupsA, levels = unique(groupsA[stats::order.dendrogram(stats::as.dendrogram(fitA))])))
      rv$colA <- palA[groupsA]
      palB <- RColorBrewer::brewer.pal(kB, "Set2")
      groupsB <- as.numeric(factor(groupsB, levels = unique(groupsB[stats::order.dendrogram(stats::as.dendrogram(fitB))])))
      rv$colB <- palB[groupsB]
      output$tableAB <- shiny::renderPlot({
        if (input$usegroupB) {
         groupsB <- rv$user.group
        palB <- unique(rv$pcol)
        }
        table(groupsA, groupsB) %>% as.data.frame() %>%
          ggplot2::ggplot(ggplot2::aes(groupsA, groupsB)) +
          ggplot2::geom_tile(ggplot2::aes(fill = .data$Freq)) +
          ggplot2::geom_text(ggplot2::aes(label = .data$Freq)) +
          ggplot2::scale_fill_distiller(palette = "RdGy") + ggplot2::theme_minimal() +
          ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::theme(legend.position = "none",
         axis.text.x = ggplot2::element_text(colour = palA,
             face = "bold", size = 15),
         axis.text.y = ggplot2::element_text(colour = palB,
             face = "bold", size = 15))
       }, height = 325)

       output$heatmapA <- shiny::renderPlot({
         plotHeatmap(d_matA, fitA, kA, palA)
       }, height = 325)
       output$heatmapB <- shiny::renderPlot({
         plotHeatmap(d_matB, fitB, kB, palB)
       }, height = 325)
    }, ignoreInit = T)

    output$wcA <- shiny::renderPlot({
      shiny::req(rv$load.app)
      plotWC(rv$space2, input$pxA, input$pyA, rv$value$is.interest, c(), rv$colA, rv$cond)
    }, height = 325)
    output$wcB <- shiny::renderPlot({
      shiny::req(rv$load.app)
      if (input$usegroupB) {
        col <- rv$pcol
      } else {
        col <- rv$colB
      }
      plotWC(rv$space2, input$pxB, input$pyB, rv$value$is.interest, c(), col, rv$cond)
    }, height = 325)

  }
  shiny::shinyApp(UI(), server)
}

