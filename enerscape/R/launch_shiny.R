launch_shiny <- function() {
  options(shiny.maxRequestSize = 1024^3)
  shiny::shinyApp(ui, server)
}
