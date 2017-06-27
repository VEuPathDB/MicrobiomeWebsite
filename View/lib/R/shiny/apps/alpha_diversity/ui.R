library(shiny)
library(shinyjs)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

shinyUI(
	fluidPage(
	  useShinyjs(),
	  includeCSS("../common/style.css"),
	  includeCSS("www/style.css"),
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
	    column(4,
	           checkboxGroupInput("measuresCheckbox", label="Measure(s)",
	                              choices = c("Shannon" = "Shannon",
	                                          "Simpson" = "Simpson",
	                                          "Chao1" = "Chao1",
	                                          "ACE" = "ACE",
	                                          "Fisher" = "Fisher"),
	                              selected = c("Shannon", "Simpson"), inline=T),
	           div(style = "display: none;",
	               checkboxInput("taxa_are_rows", label = "", value = T) )
	    ),
	    column(3, 
	           # alignCenter(
	             # div(align="center",
	             fluidRow(
	               strong("Visualization type")
	             ),
	             fluidRow(
	               div(
	                 style = "padding-top: 0.65em;",
	                 radioButtons("plotTypeRadio",
	                              label=NULL,
	                                           choices = c("Boxplot" = "boxplot",
	                                                       "Dot plot" = "dotplot"),
	                                           selected = c("boxplot"), inline=T)
	               )
	             )
	             # )
	           # )
	    ),
	    column(3,
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
	  ),
	  tabsetPanel(
	    id = "tabs",
	    tabPanel(
	      id="firstTab",
	      title = "Alpha Diversity Overview",
	      value = "firstTab",
	      hidden(
      div(id="chartLoading", style="text-align: center;",
          # br(),br(),br(),br(),
          h5("Calculating the alpha diversity. This could take a while..."),
          img(src = "spinner.gif", id = "loading-spinner")
      )),
	      div(
	        id="allSamplesArea",
	        fluidRow(
	          column(12,
	                 uiOutput("allSamplesChart"),
	                 uiOutput("uiHoverAllSamples")
	          )
	        ),
	        fluidRow(
	          column(12,
	                 dataTableOutput("allSamplesDt")
	          )
	        )
	      )
	    ), # end tabPanel bySample
	    tabPanel(
	      id="secondTab",
	      value = "secondTab",
	      title = "Explore metadata",
	      fluidRow(
	        column(10,
	               selectizeInput(
	                 "category",
	                 choices = NULL,
	                 label = " Select one or more metadata to explore the alpha diversity",
	                 options = list(placeholder = 'Loading...'),
	                 multiple = T,
	                 width = "100%"
	               )
          ),
	        column(2,
	               actionButton("doneButton", "Generate plot", class="btn-info", style="margin-top: 25px;", width = "100%")
	         )
	      ),
	      hidden(
	        div(id="metadataLoading", style="text-align: center;",
	            h5("Formatting plot..."),
	            img(src = "spinner.gif", id = "loading-spinner")
	        )
	      ),
	      div(id="metadataContent",
	          fluidRow(
	            column(12,
	                   uiOutput("result_tests", class="shell-wrap")
	            )
	          ),
	          fluidRow(
	            column(12,
	                   uiOutput("byMetadataChart"),
	                   uiOutput("uiHoverByMetadata")
	            )
	          ),
	          fluidRow(
	            column(12,
	                   dataTableOutput("byMetadataDt")
	            )
	          )
	       )
	    ) # end tabPanel secondTab
	  )
	  # fluidRow(
	  #   column(8,
	  #          selectizeInput(
	  #            "category",
	  #            choices = NULL,
	  #            label = "Split by metadata",
	  #            options = list(placeholder = 'Loading...'),
	  #            multiple = TRUE,
	  #            width = "100%"
	  #          )
	  #   )
	  # ),
	  # fluidRow(
	  #   column(12,
	  #          uiOutput("result_tests", class="shell-wrap")
	  #   )
	  # ),
	  # fluidRow(
	  #   column(12
	  #          
	  #   )
	  # ),
	  # fluidRow(
	  #   column(12,
	  #          dataTableOutput("sample_subset")
	  #   )
	  # )
	    ) # end div id = "app-content",
	  ) # end hidden
	) # end fluidPage
) # end shinyUI