library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(
    useShinyjs(),
    includeCSS("www/style.css"),
    tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
    ),
    # Loading message
    div(id = "loading-content",
        h5("Preparing the graphical representation..."),
        img(src = "loading_2.gif")
    ),
    # The main app code goes here
    hidden(
      div(
        id = "app-content",
        div(id="toolbar",
            fluidRow(
              column(
                3,
                uiOutput("taxonLevel"),
                # this div is not showed, this is just a workaround to load the files in a reactive environment
                div(style = "display: none;",
                    checkboxInput(
                      "taxa_are_rows", label = "", value = T
                    ))
              ),
              column(
                5,
                selectizeInput(
                  "category",
                  choices = NULL,
                  label = "Design Factor",
                  options = list(placeholder = 'Choose design factor to calculate differential abundance'),
                  width = "100%"
                )
              ),
              column(4,
                     div(
                       style = "padding-left: 1em;",
                       fluidRow(
                         strong("Download")
                       ),
                       fluidRow(
                         div(
                           style = "padding-top: 0.45em;",
                           disabled(downloadButton("btnDownloadPNG", class="btnToolbar", label = "", tooltip="Download plot as PNG")),
                           disabled(downloadButton("btnDownloadSVG", class="btnToolbar", label = "", tooltip="Download plot as SVG")),
                           disabled(downloadButton("btnDownloadEPS", class="btnToolbar", label = "", tooltip="Download plot as EPS")),
                           disabled(downloadButton("btnDownloadCSV", class="btnToolbar", label = "", tooltip="Download data as CSV"))
                         )
                       )
                     )
              )
            ), # end fluidRow 1 toolbar
            # hidden(
            div(
              # style="width: 90%;float:right;",
                fluidRow(
                  column(4,
                         selectizeInput(
                           "factor1",
                           choices = NULL,
                           label = "Factor 1",
                           options = list(placeholder = 'First choose Design Factor'),
                           width = "100%"
                         )
                  ),
                  column(1,
                         actionButton("exchangeBtn", style="width: 100%; margin-top: 25px", icon=icon("exchange"), label = "")
                  ),
                  column(4,
                         selectizeInput(
                           "factor2",
                           choices = NULL,
                           label = "Factor 2",
                           options = list(placeholder = 'First choose Design Factor'),
                           width = "100%"
                         )
                  ),
                  column(2,
                    actionButton("go", style="background-color: #4e81bd; color: white; border-style: none; margin-top: 25px", label="Run Analysis")
                  )
                )# end fluidRow factors
            ), # div row factors
            fluidRow(
              column(9,
                div(
                  style = "position:relative;",
                  uiOutput("InputErrors", style="text-align: center;")
                )
              )
            )
            # )# end hidden factors
        ), # end div toolbar
         div(id="chartArea",
          div(id="chartContent",
              fluidRow(
                column(12,
                       div(
                         style = "position:relative;",
                         uiOutput("mainChart", style="text-align: center;"),
                         # plotOutput("mainChart",
                         #            hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                         #            click = clickOpts("plot_click"), width = "100%"),
                         uiOutput("hover_info")
                       ) 
                )
              ),
              fluidRow(
                column(12,
                       dataTableOutput("datatableOutput")
                )
              )
            ),
          hidden(
            div(id="chartLoading", style="text-align: center;",
                # br(),br(),br(),br(),
                h5("Running DESeq2 to calculate differential abundance. This could take a while..."),
                # img(src = "spinner.gif", id = "loading-spinner")
                img(src = "spinner.gif", id = "loading-spinner")
            )
          )
         ) # end div chart area
      ) # end div id = "app-content",
    ) # end hidden
  ) # end fluidPage
) # end shinyUI

