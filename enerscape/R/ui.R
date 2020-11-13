## define UI for shiny
ui <- shiny::fluidPage(
  shiny::column(3, offset = 4,
                titlePanel("",
                           "Enerscape")),
  shiny::br(),
  shiny::fluidRow(
    shiny::column(6,
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
)
