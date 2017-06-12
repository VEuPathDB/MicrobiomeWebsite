library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(
    useShinyjs(),
    includeCSS("www/style.css"),
    # Loading message
    div(id = "loading-content",
        h5("Preparing the graphical representation..."),
        img(src = "new_loading.gif")
    ),
    # The main app code goes here
    hidden(
      div(
        id = "app-content",
        div(id="toolbar",
            fluidRow(
              column(
                3,
                selectInput(
                  "taxonLevel",
                  label = "Taxonomic level",
                  choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                  selected = "Genus",
                  width = '100%'
                ),
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
                  label = "Metadata Design Factor",
                  options = list(placeholder = 'Loading...'),
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
                           downloadButton("btnDownloadPNG", class="btnToolbar", label = "", tooltip="Download plot as PNG"),
                           downloadButton("btnDownloadSVG", class="btnToolbar", label = "", tooltip="Download plot as SVG"),
                           downloadButton("btnDownloadEPS", class="btnToolbar", label = "", tooltip="Download plot as EPS"),
                           downloadButton("btnDownloadCSV", class="btnToolbar", label = "", tooltip="Download data as CSV")
                         )
                       )
                     )
              )
            ), # end fluidRow 1 toolbar
            # hidden(
              fluidRow(
                column(6,
                       selectizeInput(
                         "factor1",
                         choices = NULL,
                         label = "Factor 1",
                         options = list(placeholder = 'Loading...'),
                         width = "100%"
                       )
                ),
                column(6,
                       selectizeInput(
                         "factor2",
                         choices = NULL,
                         label = "Factor 2",
                         options = list(placeholder = 'Loading...'),
                         width = "100%"
                       )
                )
              )# end fluidRow factors
            # )# end hidden factors
        ), # end div toolbar
        div(id="chartArea",
            fluidRow(
              column(12,
                     div(
                       style = "position:relative",
                       plotOutput("mainChart",
                                  hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                                  click = clickOpts("plot_click"), width = "100%"),
                       uiOutput("hover_info")
                     ) 
              )
            ),
            fluidRow(
              column(12,
                     dataTableOutput("datatableOutput")
              )
            )
        ), # end div chart area
        hidden(
          div(id="chartAreaLoading",
              h5("Running DESeq2 to calculate differential abundance. This could take a while..."),
              img(src = "spinner.gif", id = "loading-spinner")
          )
        )
      ) # end div id = "app-content",
    ) # end hidden
  ) # end fluidPage
) # end shinyUI