#' Generating layout for the graphical interface.
#'
#' (modified)
#'
#' @return shiny ui
#' @keywords internal
#'
UI <- function() {
  shiny::fluidPage(
    theme = shinythemes::shinytheme("simplex"),
    shinyFeedback::useShinyFeedback(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        #pc_vars .option[data-value='1'], #pc_vars .item[data-value='1']{
          background: #1B9E77 !important;
          color: white !important;
        }
        #pc_vars .option[data-value='2'], #pc_vars .item[data-value='2']{
          background: #D95F02 !important;
          color: white !important;
        }
        #pc_vars .option[data-value='3'], #pc_vars .item[data-value='3']{
          background: #7570B3 !important;
          color: white !important;
        }
        #pc_vars .option[data-value='4'], #pc_vars .item[data-value='4']{
          background: #E7298A !important;
          color: white !important;
        }
        #pc_vars .option[data-value='5'], #pc_vars .item[data-value='5']{
          background: #66A61E !important;
          color: white !important;
        }
        #pc_vars .option[data-value='6'], #pc_vars .item[data-value='6']{
          background: #E6AB02 !important;
          color: white !important;
        }
        #pc_vars .option[data-value='7'], #pc_vars .item[data-value='7']{
          background: #A6761D !important;
          color: white !important;
        }
        #pc_vars .option[data-value='8'], #pc_vars .item[data-value='8']{
          background: #666666 !important;
          color: white !important;
        }
                      "))
    ),
    shiny::navbarPage("pandemonium",
      id = "navbar",
      shiny::tabPanel("Data",
        value = "0",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::hr(),
            shiny::selectInput("space1", "Cluster Space", c(), multiple = TRUE, selectize = TRUE),
            shiny::selectInput("coord_space1", "Coordinate Funcion", c())
          ),
          shiny::column(
            6,
            shiny::hr(),
            shiny::selectInput("space2", "Linked Space", c(), multiple = TRUE, selectize = TRUE),
            shiny::selectInput("coord_space2", "Coordinate Funcion", c())
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::hr(),
            shiny::selectInput("group", "Flags or Groupings", c(choose = ""), multiple = TRUE, selectize = TRUE)
          ),
          shiny::column(
            6,
            shiny::hr(),
            shiny::selectInput("label", "Label", c(choose = ""), selectize = TRUE)
          )
        ),
        shiny::actionButton("app.load", "load data"),
        shiny::uiOutput("notloadedwarning")
      ),
      shiny::tabPanel("Analysis",
        value = "1",
        shiny::tabsetPanel(
          id = "tabset",
          shiny::tabPanel(
            "Input",
            shiny::fluidPage(
              shiny::h3(),

              # Sidebar with a drop down input for number of bins
              shiny::sidebarLayout(
                shiny::sidebarPanel(
                  shiny::selectInput("linkage", "Linkage",
                    choices = c(
                      "complete", "single", "average", "median",
                      "centroid", "mcquitty", "ward.D", "ward.D2"
                    ),
                    selected = "ward.D2"
                  ),
                  shiny::selectInput("metric", "Distance measure",
                    choices = c(
                      "euclidean", "maximum", "manhattan",
                      "canberra", "binary"
                    )
                  ),
                  shiny::selectInput("kC", "Number of clusters", choices = 2:8, selected = 4),
                  shiny::selectInput("px", "x", choices = c()),
                  shiny::selectInput("py", "y", choices = c()),
                  shiny::checkboxInput("plotHulls", "Show alpha hulls"),
                  shiny::conditionalPanel("input.plotHulls == true", shiny::sliderInput("alpha", "alpha value", min = 0.1, max = 10, step = 0.1, value = 0.2))
                ),

                # Show a plot of the generated distribution
                shiny::mainPanel(
                  shiny::splitLayout(
                    cellWidths = c("50%", "50%"),
                    cellArgs = list(style = "height: 325px"),
                    shiny::imageOutput("heatmap"),
                    shiny::imageOutput("wc")
                  ),
                  shiny::splitLayout(
                    cellWidths = c("50%", "50%"),
                    cellArgs = list(style = "height: 325px"),
                    shiny::imageOutput("chi2"),
                    shiny::imageOutput("sigbins")
                  )
                )
              )
            )
          ),
          shiny::tabPanel(
            "Benchmarks",
            shiny::fluidPage(
              shiny::h3(),
              shiny::conditionalPanel('input.group != ""', shiny::h3("Cluster Assignment")),
              DT::DTOutput("benchmarks"),
              shiny::hr(),
              shiny::conditionalPanel('input.group != ""', shiny::h3("User Grouping")),
              shiny::conditionalPanel('input.group != ""', DT::DTOutput("benchmarks.grouping"))
            )
          ),
          shiny::tabPanel(
            "Distance breakdown",
            shiny::fluidPage(
              shiny::h3(),
              shiny::textOutput("distText"),
              shiny::plotOutput("hist", width = "100%", height = "600px")
            )
          ),
          shiny::tabPanel(
            "Coordinates",
            shiny::fluidPage(
              shiny::h3(),
              shiny::plotOutput("scalar"),
              shiny::hr(),
              shiny::plotOutput("pc"),
              shiny::splitLayout(
                shiny::checkboxInput("pc.centre", "centre", value = TRUE),
                shiny::checkboxInput("pc.scale", "scale", value = TRUE),
                shiny::tags$div(
                  id = "pc_vars",
                  shiny::selectizeInput("pc.filt", "grouping selection", 1:4, 1:4, multiple = TRUE)
                )
              )
            )
          ),
          shiny::tabPanel(
            "Dimension reduction",
            shiny::fluidPage(
              shiny::h3(),
              shiny::fluidRow(
                shiny::column(
                  6,
                  plotly::plotlyOutput("dimRed1", width = "100%", height = "500px"),
                  shiny::selectInput("algorithm1", "algorithm", c()),
                  shiny::selectInput("red1.data", "data", c("Cluster Space", "Linked Space"), selected = "Cluster Space"),
                  shiny::selectInput("dimred.colour1", "colour",
                    c("clustering", "sigma bins", "chi squared", "user"),
                    selected = "clustering"
                  )
                ),
                shiny::column(
                  6,
                  plotly::plotlyOutput("dimRed2", width = "100%", height = "500px"),
                  shiny::selectInput("algorithm2", "algorithm", c()),
                  shiny::selectInput("red2.data", "data", c("Cluster Space", "Linked Space"), selected = "Linked Space"),
                  shiny::selectInput("dimred.colour2", "colour",
                    c("clustering", "sigma bins", "chi squared", "user"),
                    selected = "clustering"
                  )
                )
              )
            )
          ),
          shiny::tabPanel(
            "Tour display",
            shiny::fluidPage(
              shiny::h3(),
              # page with tour displays in observable and parameter space
              shiny::fluidRow(
                shiny::column(
                  6,
                  detourr::detourOutput("tourImg1", width = "100%", height = "400px")
                ),
                shiny::column(
                  6,
                  detourr::detourOutput("tourImg2", width = "100%", height = "400px")
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  # tour1 controls
                  shiny::selectInput("colouring1", "Colouring",
                    choices =
                      c("clustering", "sigma bins", "chi squared", "user"), selected = "clustering"
                  ),
                  shiny::selectInput("tour1data", "data",
                    choices =
                      c("Cluster Space", "Linked Space", "Cluster PCA", "Linked PCA"),
                    selected = "Cluster Space"
                  ),
                  shiny::hr(),
                  shiny::selectInput("render1", "Display type",
                    choices = c("2D", "3D"), selected = "2D"
                  ),
                  shiny::uiOutput("holdButton1"),
                  shiny::selectInput("tour_type_1", "tour type",
                    choices =
                      c("grand", "cmass", "holes", "lda", "pda", "dcor", "spline", "radial", "anomaly"), selected = "grand"
                  ),
                  # radial variable selection only shown if radial selected
                  shiny::conditionalPanel(
                    "input.tour_type_1 == 'radial' ",
                    shiny::selectInput("start1", "starting view", choices = c("random"), selected = "random"),
                    shiny::selectInput("select_radial_1", "radial variable", c(), multiple = TRUE)
                  ),
                  # ellipse scaling only shown if anomaly selected. larger value means only further points
                  shiny::conditionalPanel(
                    "input.tour_type_1 == 'anomaly' ",
                    shiny::selectInput("ellc_1", "ellc", 1:30, selected = 1)
                  ),
                  # slice display selection width only shown if slice selected
                  shiny::checkboxInput("slice_1", "Use Slice"),
                  shiny::conditionalPanel(
                    "input.slice_1 == true",
                    shiny::numericInput("slw_1", "slice width", value = 0.2, min = 0, max = 1)
                  ),
                  shiny::actionButton("buildTour1", "Build Tour"),
                  shiny::conditionalPanel(
                    "input.tour2data ==input.tour1data",
                    shiny::actionButton("copyTour1", "Copy next tour")
                  )
                ),
                shiny::column(
                  6,
                  # tour2 controls
                  shiny::selectInput("colouring2", "Colouring",
                    choices =
                      c("clustering", "sigma bins", "chi squared", "user"), selected = "clustering"
                  ),
                  shiny::selectInput("tour2data", "data",
                    choices =
                      c("Cluster Space", "Linked Space", "Cluster PCA", "Linked PCA"),
                    selected = "Linked Space"
                  ),
                  shiny::hr(),
                  shiny::selectInput("render2", "Display type",
                    choices = c("2D", "3D"), selected = "2D"
                  ),
                  shiny::uiOutput("holdButton2"),
                  shiny::selectInput("tour_type_2", "tour type",
                    choices =
                      c("grand", "cmass", "holes", "lda", "pda", "dcor", "spline", "radial", "anomaly"), selected = "grand"
                  ),
                  # radial variable selection only shown if radial selected
                  shiny::conditionalPanel(
                    "input.tour_type_2 == 'radial' ",
                    shiny::selectInput("start2", "starting view", choices = c("random"), selected = "random"),
                    shiny::selectInput("select_radial_2", "radial variable", c(), multiple = TRUE)
                  ),
                  # ellipse scaling only shown if anomaly selected. larger value means only further points
                  shiny::conditionalPanel(
                    "input.tour_type_2 == 'anomaly' ",
                    shiny::selectInput("ellc_2", "ellc", 1:30, selected = 1)
                  ),
                  # slice display selection width only shown if slice selected
                  shiny::checkboxInput("slice_2", "Use Slice"),
                  shiny::conditionalPanel(
                    "input.slice_2 == true",
                    shiny::numericInput("slw_2", "slice width", value = 0.2, min = 0, max = 1)
                  ),
                  shiny::actionButton("buildTour2", "Build Tour"),
                  shiny::conditionalPanel(
                    "input.tour2data ==input.tour1data",
                    shiny::actionButton("copyTour2", "Copy next tour")
                  )
                )
              )
            )
          ),
          shiny::tabPanel(
            "Comparison",
            shiny::fluidPage(
              shiny::h3(),

              # Sidebar with a dropdown input for number of bins
              shiny::fluidRow(
                shiny::column(4, shiny::fluidRow(
                  shiny::column(
                    6,
                    shiny::selectInput("coordA", "coord", c()),
                    shiny::selectInput(
                      "linkageA", "Linkage A",
                      choices = c(
                        "complete", "single",
                        "average", "median",
                        "centroid", "mcquitty",
                        "ward.D", "ward.D2"
                      ),
                      selected = "complete"
                    ),
                    shiny::selectInput("metricA", "Distance measure A",
                      choices = c(
                        "euclidean", "maximum", "manhattan",
                        "canberra", "binary"
                      )
                    ),
                    shiny::selectInput("kA", "Number of clusters A", choices = 2:8),
                    shiny::selectInput("pxA", "x", choices = c()),
                    shiny::selectInput("pyA", "y", choices = c()),
                    shiny::checkboxInput("usemainA", "Use Main Choices")
                  ),
                  shiny::column(
                    6,
                    shiny::selectInput("coordB", "coord", c()),
                    shiny::selectInput(
                      "linkageB", "Linkage B",
                      choices = c(
                        "complete", "single",
                        "average", "median",
                        "centroid", "mcquitty",
                        "ward.D", "ward.D2"
                      ),
                      selected = "complete"
                    ),
                    shiny::selectInput("metricB", "Distance measure B",
                      choices = c(
                        "euclidean", "maximum", "manhattan",
                        "canberra", "binary"
                      )
                    ),
                    shiny::selectInput("kB", "Number of clusters B", choices = 2:8),
                    shiny::selectInput("pxB", "x", choices = c()),
                    shiny::selectInput("pyB", "y", choices = c()),
                    shiny::conditionalPanel(
                      'input.group != ""',
                      shiny::checkboxInput("usegroupB", "User Grouping")
                    ),
                  ),
                  shiny::plotOutput("tableAB")
                )),


                # Show a plot of the generated distribution
                shiny::column(
                  8,
                  shiny::splitLayout(
                    cellWidths = c("50%", "50%"),
                    cellArgs = list(style = "height: 325px"),
                    shiny::plotOutput("heatmapA"),
                    shiny::plotOutput("heatmapB")
                  ),
                  shiny::splitLayout(
                    cellWidths = c("50%", "50%"),
                    cellArgs = list(style = "height: 325px"),
                    shiny::plotOutput("wcA"),
                    shiny::plotOutput("wcB")
                  )
                )
              )
            )
          ),
          shiny::tabPanel(
            "Statistics",
            shiny::fluidPage(shiny::plotOutput("clusterstats"))
          )
        )
      )
    )
  )
}
