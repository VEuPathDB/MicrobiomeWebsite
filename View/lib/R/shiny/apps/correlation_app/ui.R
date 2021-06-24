
library(shiny)
library(shinyjs)

# The shiny ui function build the HTML final page showed in the shiny app
shinyUI(
  fluidPage(
    # #pvalueCutoff.form-control {margin-top: 0px; padding-top:0px;}
    tags$head(tags$style(HTML("div.checkbox {margin-top: 0px; margin-bottom: 0px;}"))),
    useShinyjs(),
    includeCSS("www/style.css"),
    tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
    ),
    #includeCSS("www/hint.min.css"),
    #includeScript("www/script.js"),
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
           2,
           uiOutput("taxonLevel"),
           # this div is not showed, this is just a workaround to load the files in a reactive environment
           div(style = "display: none;",
               checkboxInput(
                 "taxa_are_rows", label = "", value = T
               ))
         ),
         column(
           4,
           uiOutput("corType")
         ),
        column(2,
          actionButton("go", style="margin-top: 25px; background-color: #4e81bd; color: white; border-style: none;", label = "Run Analysis")
        ),
         column(3,
            div(
              # style = "padding-left: 0.1em;",
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
         ), # end fluidRow toolbar
           div(id="chartLoading", style="text-align: center;",
               h5("Calculating correlation. This could take a while..."),
               img(src = "spinner.gif", id = "loading-spinner")
           ),
           div(
             id="divContent", style="text-align: center;",
               fluidRow(column(
                 12,
                 div(
                  uiOutput("correlationChart"),
                  uiOutput("hover_info")
                )
               )),
               # fluidRow(column(
               #   12,
               #   HTML("<span class='hint--bottom  hint--rounded' aria-label='We have rounded corners for you'>Hmm...So you don't like sharp edges?</span>")
               # )),
             fluidRow(column(
                style='padding-top:40px',
                12,
                   dataTableOutput("datatableOutput")
              ))
            ), # end divContent
            div(id="noPlot", syle="text-align: center;",
              h5(class="alert alert-warning","Click Run Analysis to make a new plot")
            )
    ) # end div id = "app-content",
    ) # end hidden
  ) # end fluidPage
) # end shinyUI

