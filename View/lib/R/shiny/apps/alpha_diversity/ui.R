library(shiny)
library(shinyjs)

shinyUI(
	fluidPage(
	  useShinyjs(),
	  includeCSS("www/style.css"),
	  # Loading message
	  div(id = "loading-content",
	      h5("We are preparing the graphical representation..."),
	      img(src = "loading.gif")
	  ),
	  # The main app code goes here
	  hidden(
	    div(
	      id = "app-content",
	  fluidRow(
	    column(5, 
	           checkboxGroupInput("measureCheckBox", label="Measure(s)",
	                              choices = c("Chao1" = "Chao1",
	                                          "ACE" = "ACE",
	                                          "Shannon" = "Shannon",
	                                          "Simpson" = "Simpson",
	                                          "Fisher" = "Fisher"),
	                              selected = c("Shannon", "Simpson"), inline=T),
	           div(style = "display: none;",
	               checkboxInput("taxa_are_rows", label = "", value = T) )
	    ),
	    column(3,
	           radioButtons("plotTypeRadio", label="Visualization type",
	                        choices = c("Boxplot" = "boxplot",
	                                    "Dot plot" = "dotplot"),
	                        selected = c("boxplot"), inline=T)
	    ),
	    column(4,
	           div(
	             style = "",
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
	  fluidRow(
	    column(6,
	           # uiOutput("category")
	           selectizeInput(
	             "category",
	             choices = NULL,
	             label = "Split by metadata",
	             options = list(placeholder = 'Loading...'),
	             width = "100%"
	           )
	    )
	  ),
	  fluidRow(
	    column(12,
	           uiOutput("result_tests", class="shell-wrap")
	    )
	  ),
	  fluidRow(
	    column(12,
	           div(
	             style = "position:relative",
	             plotOutput("abundanceChart",
	                        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
	                        click = clickOpts("plot_click"), width = "100%"),
	             uiOutput("hover_info")
	           ) 
	    )
	  ),
	  fluidRow(
	    column(12,
	           dataTableOutput("sample_subset")
	    )
	  )
	    ) # end div id = "app-content",
	  ) # end hidden
	) # end fluidPage
) # end shinyUI