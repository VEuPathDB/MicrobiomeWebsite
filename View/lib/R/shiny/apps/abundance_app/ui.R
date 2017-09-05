# Declaring the packages
library(shiny)
library(shinyjs)

jsCode <- "shinyjs.shinyScrollTo = function(id){
    window.scrollTo(0, document.getElementById(id).offsetTop);
}"

# The shiny ui function build the HTML final page showed in the shiny app
shinyUI(
  fluidPage(
    useShinyjs(),
    includeCSS("www/style.css"),
    includeCSS("../common/tooltip/tooltip.css"),
    includeScript("www/script.js"),
    # extendShinyjs(text = jsCode),
    # Loading message
    div(id = "loading-content",
        h5("Preparing graphical representation..."),
        img(src = "loading_microbiome.gif")
    ),
    # The main app code goes here
    hidden(
    div(
    id = "app-content",
         fluidRow(
           column(
           3,
           selectInput(
             "taxonLevel",
             label = "Taxonomic level",
             choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
             selected = "Phylum",
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
             label = "Split by sample details",
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
         ),
       tabsetPanel(
         id = "tabs",
         tabPanel(
           id="firstTab",
           title = "Overview",
           value = "bySample",
           hidden(
             div(id="chartLoading", style="text-align: center;",
                 h5("Formatting plot..."),
                 img(src = "spinner.gif", id = "loading-spinner")
             )),
           div(
             id="overviewTabContent",
               fluidRow(column(
                 12,
                 div(style = "position:relative",
                     # div(onclick="shinyjs.newScrollTo('div_sample_datatable')",
                       uiOutput("overviewChart"),
                       # ),
                     uiOutput("overviewTooltip"))
               )),
               fluidRow(column(
                 12,
                   dataTableOutput("overviewDatatable")
               ))
           )
         ), # end tabPanel bySample
         tabPanel(
           title = "Top Taxa Comparison",
           value = "byTopOTU",
           hidden(
             div(id="topTabLoading", style="text-align: center;",
                 h5("Formatting plot..."),
                 img(src = "spinner.gif", id = "loading-spinner")
             )),
           div(
             id="topTabContent",
             fluidRow(
               column(12,
                      # div(style = "position:relative",
                      uiOutput("chartByTopOTU"),
                      uiOutput("hoverByTopOTU")
                      # )
               )),
             fluidRow(
               column(12,
                      DT::dataTableOutput("by_top_otu_datatable")
               )
             )
           )
         ),# end tabPabel byTopOTU
         tabPanel(
           title = "Single Taxon",
           value = "byOTU",
             fluidRow(
               column(
                 12,
                   selectizeInput(
                     "filterOTU",
                     choices = NULL,
                     label = "Search OTU",
                     width = "100%",
                     options = list(placeholder = 'Loading...')
                   )
               )
              ),
           hidden(
             div(id="singleOtuLoading", style="text-align: center;",
                 h5("Formatting plot..."),
                 img(src = "spinner.gif", id = "loading-spinner")
             )),
           div(
             id="singleOtuContent",
             fluidRow(
               column(
                 12,
                  hidden(uiOutput("result_tests", class="shell-wrap"))
               )
             ),
             fluidRow(
               column(12,
                      # div(style = "position:relative",
                        uiOutput("chartByOTU"),
                        uiOutput("hoverByOTU")
                      # )
                    )),
             fluidRow(column(
               12,
               dataTableOutput("by_otu_datatable")
             ))
           )
         )# end tabPabel byOTU
       ) # end tabSetPanel
    ) # end div id = "app-content",
    ) # end hidden
  ) # end fluidPage
) # end shinyUI