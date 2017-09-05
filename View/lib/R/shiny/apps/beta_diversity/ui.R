library(shiny)
library(shinyjs)

shinyUI( 
	fluidPage(
	  useShinyjs(),
	  includeCSS("www/style.css"),
	  includeCSS("../common/tooltip/tooltip.css"),
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
	        4,
	        selectInput("distance", "Distance Method",
	                    c(
	                      "Bray-Curtis" = "bray",
	                      "Jensen-Shannon Divergence"="jsd",
	                      "Jaccard" = "jaccard",
	                      "Canberra" = "canberra",
	                      "Kulczynski"="kulczynski",
	                      "Horn"="horn",
	                      "Mountford"="mountford"
	                    )),
	        # this div is not showed, this is just a workaround to load the files in a reactive environment
	        div(style = "display: none;",
	            checkboxInput(
	              "taxa_are_rows", label = "", value = T
	            ))
	      ),
	      column(
	        3,
	        selectizeInput(
	          "category",
	          choices = NULL,
	          label = "Color by",
	          options = list(placeholder = 'Loading...'),
	          width = "100%"
	        )
	      ),
	      column(
	        3,
	        selectizeInput(
	          "categoryShape",
	          choices = NULL,
	          label = "Shape by",
	          options = list(placeholder = 'Loading...'),
	          width = "100%"
	        )
	      ),
	      column(2,
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
	      hidden(
	        div(id="chartLoading", style="text-align: center;",
	          h5("Generating plot..."),
	          img(src = "spinner.gif", id = "loading-spinner")
	      )),
	      
        div(id="betaLoading", style="text-align: center;",
            h5("Calculating beta diversity..."),
            img(src = "spinner.gif", id = "loading-beta")
        ),
	    hidden(
	      div(
	       id="contentArea",
  	      fluidRow(column(12,
        	  div(
        	    style = "position:relative",
        	    uiOutput("betadiversityChart"),
        	               # hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
        	               # click = clickOpts("plot_click"), width = "100%"),
        	    uiOutput("hover_info")
        	    )
  	        )
  	      )
	      )
	    )
	    ) # end div id = "app-content",
	  ) # end hidden
	  ) # end fluidPage
) # end shinyUI