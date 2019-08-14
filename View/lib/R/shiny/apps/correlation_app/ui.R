# Declaring the packages
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
         hidden(
         column(3,
          div(
            style="padding-left: 2.2em;",
            fluidRow(
              strong("Visualization type")
            ),
            fluidRow(
              div(
                style = "padding-top: 0.65em;",
                radioButtons("plotTypeRadio",
                             label=NULL,
                             choices = c("Dot plot" = "dotplot",
                                         "Heatmap" = "heatmap"),
                             selected = c("dotplot"), inline=T)
              )
            )
          )
        ) ),
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
        fluidRow(
          column(12,
            uiOutput("pvalueCutoff"),
            tags$style(type="text/css", "#pvalueCutoff {height:10px;}")
          )
        ),
           div(id="chartLoading", style="text-align: center;",
               h5("Calculating correlation. This could take a while..."),
               img(src = "spinner.gif", id = "loading-spinner")
           ),
           div(
             id="divContent",
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
                 12,
                   dataTableOutput("datatableOutput")
               ))
            ) # end divContent
    ) # end div id = "app-content",
    ) # end hidden
  ) # end fluidPage
) # end shinyUI
