
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
  max_point_size = 10
  plot_margin <- 100

  # variables to define some plot parameters
  NUMBER_TAXA <- 10
  MAX_SAMPLES_NO_RESIZE <- 40
  MIN_HEIGHT_AFTER_RESIZE <- 12

  NO_METADATA_SELECTED <- "No Metadata Selected"

  oldw <- getOption("warn")

  ggplot_object <- NULL
  ggplot_build_object <- NULL

  properties <- NULL
  propUrl <- NULL

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
    cor_type<-input$corType
    taxon_level<-input$taxonLevel
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
      mySelected <- "Taxon"
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

  # output$pvalueCutoff <- renderUI({
  #   load_properties()
  #   if (is.null(properties)) {
  #     mySelected <- 0.05
  #   } else {
  #     mySelected <- properties$selected[properties$input == "input$pvalueCutoff"]
  #   }

  #   sliderInput("pvalueCutoff", 
  #               "Filter pvalue", 
  #               min = 0, max = 1, 
  #               value = mySelected, 
  #               step = 0.01, 
  #               width = "100%",
  #               ticks=T)
  # })

  observeEvent({input$taxonLevel
               input$corType}, {

    text <- paste0("input\tselected\n",
                   "input$taxonLevel\t", input$taxonLevel, "\n",
                   "input$corType\t", input$corType, "\n"
            )

    PUT(propUrl, body = "")
    PUT(propUrl, body = text)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  correlationChartFunction <- function(){}
  output$correlationChart <- renderUI({
    if (is.null(input$taxonLevel)) {
      return()
    }

    shinyjs::hide("divContent")
    shinyjs::show("chartLoading")
    mstudy <- load_microbiome_data()

    result_to_show<-NULL
    if(!identical(input$category, "")){
      quantity_samples <- mstudy$sample_table$get_sample_count()
      isolate(cor_type<-input$corType)
      isolate(taxon_level<-input$taxonLevel)

      # p <-input$pvalueCutoff
      p <- 0.05

      # result <- subset(cor_result, pvalue<p)
      result <- cor_result

      cols <- colnames(cor_result)[1:2]
      result[,(cols):= lapply(.SD, as.factor), .SDcols = cols]

      # Turn to data frame for heatmaply

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
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "top")+
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

       #  }else{
        #   chart<-ggplot(result, aes_string(x=cols[2], y=cols[1], fill= "rho"))+
        #     geom_tile()+
        #     theme_eupath_default(legend.position = "bottom")+
        #     scale_fill_gradient(low = "red", high = "blue")+
        #     labs(x="Sample Details", y=taxon_level, fill="Spearman rho")+
        #     guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))
        # }
        ggplot_object<<-chart
        # ggplot_build_object<<-ggplot_build(chart)
        ggplot_build_object <<- chart

        output$plotWrapper<-renderPlotly({
          ggplotly(chart, height=37400) %>% layout(xaxis=list(side="top")) %>% plotly:::config(displaylogo = FALSE)
        })

        # NEW!
        quantity_samples <- uniqueN(result[[taxon_level]])
        logjs(quantity_samples)

        #if(quantity_samples<MAX_SAMPLES_NO_RESIZE){
        #  result_to_show<-plotlyOutput("plotWrapper",
        #     width = "100%", height = "500px"
        #   )
        #}else{
          result_to_show<-plotlyOutput("plotWrapper",
            width = "100%",
            height = paste0(quantity_samples*max_point_size*4 + plot_margin,"px")
          )
        #}
        cols_to_show<-result[, !"size", with=FALSE]
        # output$datatableOutput<-renderDataTable(cols_to_show)
      }else{
        # output$datatableOutput<-renderDataTable(NULL)
        result_to_show<-h5(class="alert alert-warning", "There is no correlation with the selected parameters.")
      }
    }else{
      # output$datatableOutput<-renderDataTable(NULL)
    }

    shinyjs::hide("chartLoading", anim = TRUE, animType = "slide")
    shinyjs::show("divContent")
    result_to_show
  })

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

