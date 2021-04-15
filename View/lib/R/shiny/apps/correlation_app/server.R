
library(shiny)
library(data.table)
library(ggplot2)
library(shinyjs)
library(httr)
library(plotly)
source("../../functions/wdkDataset.R")
source("../../lib/ggplot_ext/eupath_default.R")
source("../../lib/mbiome/mbiome-reader.R")
source("../../lib/mbiome/mbiome-stats.R")
source("../../lib/config.R")

#source("~/Documents/functions/wdkDataset.R")
 source("../../lib/ggplot_ext/eupath_default.R")
 source("../../lib/ggplot_ext/eupath_functions.R")
 source("../../lib/config.R")
 source("../../lib/mbiome/mbiome-reader.R")

#options(shiny.fullstacktrace = TRUE)

shinyServer(function(input, output, session) {
  mstudy_obj <- NULL
  mstats <- NULL
  cor_result<-NULL
  column_x<-NULL
  column_y<-NULL
  hash_colors <- NULL
  max_point_size <- 10
  plot_margin <- 100

  # variables to define some plot parameters
  NUMBER_TAXA <- 10
  MAX_SAMPLES_NO_RESIZE <- 10
  MIN_HEIGHT_AFTER_RESIZE <- 12

  NO_METADATA_SELECTED <- "No Metadata Selected"

  oldw <- getOption("warn")

  ggplot_object <- NULL
  ggplot_build_object <- NULL

  properties <- NULL
  propUrl <- NULL

  shinyjs::hide(id="chartLoading")

  load_properties <- reactive({
    if(is.null(propUrl)) {
      propUrl <<- getPropertiesUrl(session)
      properties <<- try(fread(propUrl))

      if (length(properties) > 0) {
        if (grepl("Error", properties)) {
          properties <<- NULL
        }
      } else {
	properties <<- NULL
      }
    }
  })

  load_microbiome_data <- reactive({
     
     taxon_level <- input$taxonLevel
     cor_type<-input$corType

    if(is.null(mstudy_obj)){

      mstudy_obj <<- import.eupath(
        taxa_abundance_path = getWdkDatasetFile('TaxaRelativeAbundance.tab', session, FALSE, dataStorageDir),
        sample_path = getWdkDatasetFile('Characteristics.tab', session, FALSE, dataStorageDir),
        datasets_path = getWdkDatasetFile('Datasets.tab', session, FALSE, dataStorageDir),
        aggregate_by = input$taxonLevel
      )

      mstats <<- MicrobiomeStats$new(mstudy_obj)

    }

    otus<-mstudy_obj$get_otus_by_level(input$taxonLevel)
    

    if(identical(cor_type, "tm")){
      column_y<<-taxon_level
      column_x<<-"metadata"
    }else if(identical(cor_type, "mm")){
      column_y<<-"metadata.1"
      column_x<<-"metadata.2"
    }else{
      column_y<<-paste0(taxon_level,".1")
      column_x<<-paste0(taxon_level,".2")
    }
    options(warn=-1)
    cor_result<<-mstats$calculate_correlation(taxon_level, cor_type)
    options(warn=oldw)

    mstudy_obj
  })

  output$taxonLevel <- renderUI({
    load_properties()
    if (is.null(properties)) {
      mySelected <- "Species"
    } else {
      mySelected <- properties$selected[properties$input == "input$taxonLevel"]
    }

    selectInput(
             "taxonLevel",
             label = "Taxonomy level",
             choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
             selected = mySelected,
             width = '100%'
           )
  })

  output$corType <- renderUI({
    load_properties()
    if (is.null(properties)) {
      mySelected <- "tm"
    } else {
      mySelected <- properties$selected[properties$input == "input$corType"]
    }

    selectInput(
             "corType",
             label = "Correlation type",
             #"Metadata vs Metadata"="Metadata"
             choices = c("Taxon vs Sample Detail"="tm",
                         "Sample Detail vs Sample Detail"="mm"),
             selected = mySelected,
             width = '100%'
           )
  })


  observeEvent({input$taxonLevel
               input$corType}, {

    text <- paste0("input\tselected\n",
                   "input$taxonLevel\t", input$taxonLevel, "\n",
                   "input$corType\t", input$corType, "\n"
            )

    PUT(propUrl, body = "")
    PUT(propUrl, body = text)

    # Clear plot and associated vars, ui elements
    clearPlot()

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  correlationChartFunction <- function(){}

observeEvent(input$go, {

    logjs('go buton pressed')

    shinyjs::hide(id="divContent")
    shinyjs::hide("noPlot")
    shinyjs::show("chartLoading")

    # Clear variables
    generated_plot <- NULL

    # Clear plot and associated vars, ui elements
    clearPlot()

    # Compute correlation 
    mstudy <<- load_microbiome_data()

    # Create plot
    generated_plot <- generatePlot()

    # The following must be set *outside* of the generatePlot function or the resizing does not work properly
    if(nrow(cor_result)>0){
      if(rows_in_plot<MAX_SAMPLES_NO_RESIZE){
          generated_plot<-plotlyOutput("plotWrapper",
            width = "100%", height = "700px"
          )
        }else{
          generated_plot<-plotlyOutput("plotWrapper",
            width = "800px", # Changes width of main svg and wrapper
            height=paste0(rows_in_plot*max_point_size*4+plot_margin,"px")
          )
      }
    }

    output$correlationChart <- renderUI({generated_plot})

    shinyjs::hide("chartLoading")
    shinyjs::show(id="divContent")


  }, ignoreInit = TRUE) # End go button observeEvent



  # Clear the plot by removing ui elements resetting data
  clearPlot <- reactive({

    input$taxonLevel
    input$corType

    shinyjs::hide(id="divContent")
    shinyjs::show(id="noPlot")

    # Reset variables
    cor_result <<- NULL
    output$correlationChart <- NULL
    output$plotWrapper <- NULL
    output$datatableOutput <- NULL

    # Remove ui elements
    removeUI(selector = ".svg-container", multiple=TRUE, immediate=TRUE)
    removeUI(selector = "#plotWrapper", multiple=TRUE, immediate=TRUE)

  })


# Create necessary vars for correlation plot
  generatePlot <- reactive({

      input$go
      logjs("generating plot")

      isolate(cor_type <- input$corType)
      isolate(category <- input$category)
      isolate(taxon_level<-input$taxonLevel)

      result_to_show<-NULL

      if (is.null(taxon_level)) {
        return()
      }

      if(!identical(category, "")){

        # The following left for if we want to reinstate a p-value cutoff in the future.
        # p <-input$pvalueCutoff
        # result <- subset(cor_result, pvalue<p)
        result <- cor_result


        cols <- colnames(result)[1:2]
        result[,(cols):= lapply(.SD, as.factor), .SDcols = cols]  # Note factors helpful for plot formatting

        if(nrow(result)>0){

          chart<-ggplot(result, aes_string(x=cols[2], y=cols[1]))+
              geom_point(aes_string(size="size", colour ="rho"))+
              scale_size(range = c(1, max_point_size), guide = 'none')+
              theme_eupath_default()+
              scale_colour_gradient2(high="#d8b365", mid="#f0f0f0", low="#5ab4ac")+
              scale_y_discrete(limits = rev(as.character(unique(result[[cols[1]]]))))+
              labs(x="Sample Details", y=taxon_level, colour="Spearman rho")+
              guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5))+
              theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "top",
                axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))#+
              #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
          

          # Calculate the number of rows in the resulting plot for later plot sizing.
          if(identical(cor_type, "tm")){
            rows_in_plot <<- uniqueN(result[[taxon_level]])
          } else if(identical(cor_type, "mm")) {
            rows_in_plot <<- uniqueN(result[, metadata.1])
          }

          # Calculate new plot height. Not exact because I'm unsure of the map from max_point_size as a ggpplot input to circle diameter in the output svg.
          new_height <- rows_in_plot*max_point_size*4
          chart$height <- new_height
          

          output$plotWrapper<-renderPlotly({
            ggplotly(chart) %>% layout(xaxis=list(side="top")) %>% plotly:::config(displaylogo = FALSE)
          })

          # Populate and render data table
          cols_to_show<-result[, !"size", with=FALSE]
          output$datatableOutput<-renderDataTable(cols_to_show)

        }else{
          output$datatableOutput<-renderDataTable(NULL)
          result_to_show<-h5(class="alert alert-warning", "There is no correlation with the selected parameters.")
        } # end if(nrow(result)>0)

      }else{
        output$datatableOutput<-renderDataTable(NULL)
      } # end !identical(category, "")
    
      result_to_show

  })  # end generatePlot


  # download buttons
  downloadButtons <- function(){}

  output$btnDownloadPNG <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      png(file, width=1200,height=800,units="px")
      print(ggplot_object)
      dev.off()
    }
  )

  output$btnDownloadEPS <- downloadHandler(
    filename = "plot.eps",
    content = function(file) {
      setEPS()
      postscript(file, width=16,height=10.67, family = "Helvetica")
      print(ggplot_object)
      dev.off()
    }
  )

  output$btnDownloadSVG <- downloadHandler(
    filename = "plot.svg",
    content = function(file) {
      svg(file, width=16,height=10.67)
      print(ggplot_object)
      dev.off()
    }
  )

  output$btnDownloadCSV <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(ggplot_object$data, file)
    }
  )

  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
})

