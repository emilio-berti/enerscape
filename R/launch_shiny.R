#' Shiny application for enerscape

launch_shiny <- function() {
  options(shiny.maxRequestSize = 1024^3)
  shiny::shinyApp(
    # ui ---------------
    ui = shiny::fluidPage(
      shiny::column(3, offset = 4,
                    shiny::titlePanel("",
                                      "Enerscape")),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(2,
                      shiny::numericInput(
                        "mass",
                        "Body mass (kg)",
                        0,
                        min = 0,
                        max = NA,
                        step = NA,
                        width = NULL
                      )
        ),
        shiny::column(4,
                      shiny::selectInput(
                        "neigh",
                        "Neighbors cells for calculations",
                        selected = "4",
                        choices = c("4" = 4,
                                    "8" = 8,
                                    "16" = 16)
                      )),
        shiny::column(6,
                      shiny::br(),
                      shiny::actionButton("compute",
                                          "Compute energy landscape",
                                          style='padding:20px; font-size:150%'),
        ),
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(6,
                      shiny::fileInput(
                        "dem",
                        "Digital elevation model",
                        multiple = FALSE,
                        accept = NULL,
                        width = NULL,
                        buttonLabel = "Upload",
                        placeholder = "No file selected"
                      )
        ),
        shiny::column(6,
                      shiny::br(),
                      shiny::downloadButton(
                        "download",
                        "Download energy landscape",
                      )
        ),
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(6,
                      shiny::h3("Digital elevation model (m)"),
                      shiny::plotOutput(
                        "dem",
                        width = "100%",
                        height = "400px",
                        click = NULL,
                        dblclick = NULL,
                        hover = NULL,
                        brush = NULL,
                        inline = FALSE
                      )
        ),
        shiny::column(6,
                      shiny::h3("Energy landscape (kcal)"),
                      shiny::plotOutput(
                        "enerscape",
                        width = "100%",
                        height = "400px",
                        click = NULL,
                        dblclick = NULL,
                        hover = NULL,
                        brush = NULL,
                        inline = FALSE
                      )
        )
      )
    ),
    # server ----------------
    server = server <- function(input, output) {
      compute <- shiny::reactive(input$compute)
      mass <- shiny::reactive(input$mass)
      neigh <- shiny::reactive(input$neigh)
      dem_file <- shiny::reactive(input$dem)
      dem <- shiny::reactive({
        if (!is.null(dem_file())) {
          raster::raster(dem_file()$datapath)
        }
      })
      e <- shiny::reactive({
        if (!is.null(dem_file())) {
          raster::extent(dem())[1]
        }
      })
      en <- shiny::reactive({
        if (compute()) {
          enerscape(dem(), mass(), unit = "kcal", neigh = neigh())
        }
      })
      # output --------------------
      output$dem <- shiny::renderPlot({
        if (!is.null(dem_file())) {
          raster::plot(dem())
        }
      })
      output$enerscape <- shiny::renderPlot({
        if (mass() != 0 & !is.null(dem_file())) {
          if (compute()) {
            raster::plot(en()$rasters$Work, col = grDevices::topo.colors(100))
          }
        }
      })
      output$download <- shiny::downloadHandler(
        filename = function() {
          "enerscape.tif"
        },
        content = function(file) {
          raster::writeRaster(en()$rasters$Work,
                              file,
                              overwrite = TRUE)
        }
      )
    }
  )
}
