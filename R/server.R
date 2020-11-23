#' server for shiny
#'
# server <- function(input, output) {
#   compute <- shiny::reactive(input$compute)
#   mass <- shiny::reactive(input$mass)
#   dem_file <- shiny::reactive(input$dem)
#   dem <- shiny::reactive({
#     if (!is.null(dem_file())) {
#       raster::raster(dem_file()$datapath)
#     }
#   })
#   e <- shiny::reactive({
#     if (!is.null(dem_file())) {
#       raster::extent(dem())[1]
#     }
#   })
#   en <- shiny::reactive({
#     if (compute()) {
#       enerscape::enerscape(dem(), mass(), units = "kcal")
#     }
#   })
#   # output --------------------
#   output$dem <- shiny::renderPlot({
#     if (!is.null(dem_file())) {
#       raster::plot(dem())
#     }
#   })
#   output$enerscape <- shiny::renderPlot({
#     if (mass() != 0 & !is.null(dem_file())) {
#       if (compute()) {
#         raster::plot(en()$work, col = topo.colors(100))
#       }
#     }
#   })
#   output$download <- shiny::downloadHandler(
#         filename = function() {
#           "enerscape.tif"
#         },
#         content = function(file) {
#           raster::writeRaster(en()$work,
#                             file,
#                             overwrite = TRUE)
#         }
#   )
# }
