# Declaring the packages
library(shiny)
library(data.table)
library(ggplot2)
library(httr)
source("../../functions/wdkDataset.R")
source("../../lib/ggplot_ext/eupath_default.R")
source("../../lib/tooltip/abundance_tt.R")
source("../../lib/mbiome/mbiome-reader.R")
source("../../lib/mbiome/mbiome-stats.R")
source("../../lib/config.R")

shinyServer(function(input, output, session) {
  mstudy_obj <- NULL
  mstats <- NULL
  cor_result<-NULL
  column_x<-NULL
  column_y<-NULL
  hash_colors <- NULL

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

      if (grepl("Error", properties)) {
        properties <<- NULL
      }
    }
  })

  load_microbiome_data <- reactive({
    if(is.null(mstudy_obj)){

abundance_file <- getWdkDatasetFile('TaxaRelativeAbundance.tab', session, FALSE, dataStorageDir)
sample_file <- getWdkDatasetFile('Characteristics.tab', session, FALSE, dataStorageDir)

      mstudy_obj <<- import.eupath(
        taxa_abundance_path = abundance_file,
        sample_path = sample_file,
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

  output$pvalueCutoff <- renderUI({
    load_properties()
    if (is.null(properties)) {
      mySelected <- 0.05
    } else {
      mySelected <- properties$selected[properties$input == "input$pvalueCutoff"]
    }

    sliderInput("pvalueCutoff", 
                "Filter pvalue", 
                min = 0, max = 1, 
                value = mySelected, 
                step = 0.01, 
                width = "100%",
                ticks=T)
  })

  observeEvent({input$taxonLevel
               input$corType
               input$pvalueCutoff}, {

    text <- paste0("input\tselected\n",
                   "input$taxonLevel\t", input$taxonLevel, "\n",
                   "input$corType\t", input$corType, "\n",
                   "input$pvalueCutoff\t", input$pvalueCutoff
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

      p <-input$pvalueCutoff

      result <- subset(cor_result, pvalue<p)

      cols <- colnames(cor_result)[1:2]
      result[,(cols):= lapply(.SD, as.factor), .SDcols = cols]

      if(nrow(result)>0){
        if(identical(input$plotTypeRadio, "dotplot")){
          chart<-ggplot(result, aes_string(x=cols[2], y=cols[1]))+
            geom_point(aes_string(color="rho", size="size"))+
            scale_size(range = c(3, 9), guide = 'none')+
            theme_eupath_default(legend.position = "bottom")+
            scale_colour_gradient(low = "red", high = "blue")+
            labs(x="Sample Details", y=taxon_level, color="Spearman rho")+
            guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5))
        }else{
          chart<-ggplot(result, aes_string(x=cols[2], y=cols[1], fill= "rho"))+
            geom_tile()+
            theme_eupath_default(legend.position = "bottom")+
            scale_fill_gradient(low = "red", high = "blue")+
            labs(x="Sample Details", y=taxon_level, fill="Spearman rho")+
            guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))
        }
        ggplot_object<<-chart
        ggplot_build_object<<-ggplot_build(chart)

        output$plotWrapper<-renderPlot({
          chart
        })

        quantity_samples<-nrow(result)

        if(quantity_samples<MAX_SAMPLES_NO_RESIZE){
          result_to_show<-plotOutput("plotWrapper",
             hover = hoverOpts("plot_hover", delay = 100, delayType = "throttle"),
             click = clickOpts("plot_click"),
             dblclick = dblclickOpts("plot_dblclick"),
             width = "100%", height = "500px"
           )
        }else{
          result_to_show<-plotOutput("plotWrapper",
            hover = hoverOpts("plot_hover", delay = 100, delayType = "throttle"),
            click = clickOpts("plot_click"), width = "100%",
            height = quantity_samples*MIN_HEIGHT_AFTER_RESIZE
          )
        }
        cols_to_show<-result[, !"size", with=FALSE]
        output$datatableOutput<-renderDataTable(cols_to_show)
                                                # options = list(
                                                #   order = list(list(1, 'desc'))
                                                # )
      }else{
        output$datatableOutput<-renderDataTable(NULL)
        result_to_show<-h5(class="alert alert-warning", "There is no correlation with the selected parameters.")
      }
    }else{
      output$datatableOutput<-renderDataTable(NULL)
    }

    shinyjs::hide("chartLoading", anim = TRUE, animType = "slide")
    shinyjs::show("divContent")
    result_to_show
  })

  hovers <- function(){}
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    if (is.null(hover) || is.null(hover$x) || is.null(hover$y) || round(hover$x) <0 || round(hover$y)<0 ) {
      return(NULL)
    }
    isolate(taxon_level<-input$taxonLevel)
    isolate(plot_type<-input$plotTypeRadio)
    if(identical(plot_type, "heatmap")){
      point2<-horizontal_points(ggplot_object$data, hover)
      point_x=point2$point_x
    }else{
      point<-nearPoints(ggplot_object$data, hover, xvar = column_x, yvar=column_y, allRows = T, addDist = T)
      point_x=round(hover$x)
      point2<-subset(point, dist_<10)
    }
    # return(NULL)
    cols <- c(column_y, column_x, "rho", "pvalue")

    if(nrow(point2)>0){
      if(nrow(point2)>1){
        data_to_show <- point2[nrow(point2):1,cols,with=F]
        middle_point <- (round(hover$y+0.5)+round(hover$y-0.5))/2
        point_tooltip(hover = hover, df = data_to_show, point_x=point_x,
                    point_y = middle_point, left_offset = 30, right_offset = 33,
                    top_offset = -62*nrow(point2))
      }else{
        data_to_show <- point2[,cols,with=F]
        if(!point2$selected_){
          # when the point is not selected we need to discover the exact y point
          # we just match the taxon selected with the index level of that taxon
          row_num<-match(point2[[column_y]],levels(point[[column_y]]))
          point_tooltip(hover = hover, df = data_to_show, point_x=point_x,
                  point_y = row_num, left_offset = 30, right_offset = 33,
                  top_offset = -68)
        }else{
          point_tooltip(hover = hover, df = data_to_show, point_x=point_x,
                      point_y = round(hover$y), left_offset = 30, right_offset = 33,
                      top_offset = -68)
        }
      }
    }else{
      return(NULL)
    }
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
