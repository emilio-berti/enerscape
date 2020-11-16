## define server for shiny
server <- function(input, output) {
  compute <- reactive(input$compute)
  mass <- reactive(input$mass)
  dem_file <- reactive(input$dem)
  dem <- reactive({
    if (!is.null(dem_file())) {
      raster::raster(dem_file()$datapath)
    }
  })
  e <- reactive({
    if (!is.null(dem_file())) {
      raster::extent(dem())[1]
    }
  })
  en <- reactive({
    if (compute()) {
      enerscape::enerscape(dem(), mass(), units = "kcal")
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
        raster::plot(en()$work, col = topo.colors(100))
      }
    }
  })
  output$download <- downloadHandler(
        filename = function() {
          "enerscape.tif"
        },
        content = function(file) {
          raster::writeRaster(en()$work,
                            file,
                            overwrite = TRUE)
        }
  )
}
