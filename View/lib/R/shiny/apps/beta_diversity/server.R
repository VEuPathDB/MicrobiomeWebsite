# Declaring the packages
library(shiny)
library(ggplot2)
source("../../functions/wdkDataset.R")
library(data.table)
library(phyloseq)
library(httr)
library(gtools)
library(plotly)
source("../../functions/ebrc_functions.R")
source("../../lib/ggplot_ext/eupath_default.R")
source("../../lib/mbiome/mbiome-reader.R")
source("../../lib/config.R")

shinyServer(function(input, output, session) {
  # Declaring some global variables
  # df_abundance, df_sample and df_sample.formatted are declared global to avoid 
  # multiple file reading in the reactive section
  mstudy_obj <- NULL
  
  hash_colors <- NULL
  
  metadata_list <- NULL
  
  # variables to define some plot parameters
  NUMBER_TAXA <- 10
  WIDTH <- global_width
  MAX_SAMPLES_NO_RESIZE <- 30
  MIN_HEIGHT_AFTER_RESIZE <- 9
  
  NO_METADATA_SELECTED <- "Select sample details"
  
  
  eupath_pallete<-c("#6a3d9a", "#cab2d6", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99", "#33a02c", "#b2df8a", "#1f78b4", "#a6cee3")
  
  # end new variables
  
  
  df_abundance <- NULL
  df_sample <- NULL

  phyloseqobj <- NULL
  ordination_obj <- NULL
  
  columns <- NULL
  hash_sample_names<- NULL
  hash_count_samples <- NULL
  ggplot_object<-NULL
  ggplot_build_object<-NULL
  ggplot_data <- NULL

  propUrl <- NULL
  properties <- NULL

  load_microbiome_data <- reactive({
    if (is.null(propUrl)) {
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

    if(is.null(mstudy_obj)){
      mstudy_obj <<- import.eupath(
        taxa_abundance_path = getWdkDatasetFile('TaxaRelativeAbundance.tab', session, FALSE, dataStorageDir),
        sample_path = getWdkDatasetFile('Characteristics.tab', session, FALSE, dataStorageDir),
        datasets_path = getWdkDatasetFile('Datasets.tab', session, FALSE, dataStorageDir),
        aggregate_by = "Species"
      )
     
      if (is.null(properties)) {
        mySelectedCategory <- NO_METADATA_SELECTED
        mySelectedShape <- NO_METADATA_SELECTED
      } else {
        mySelectedCategory <- properties$selected[properties$input == "input$category"]
        mySelectedShape <- properties$selected[properties$input == "input$categoryShape"]
      }
 
      filtered_categories <- mstudy_obj$get_filtered_categories()
      #message("filtered_categories: ", filtered_categories)
      updateSelectizeInput(session, "category",
                           choices = c(NO_METADATA_SELECTED, filtered_categories),
                           selected = mySelectedCategory,
                           options = list(placeholder = 'Select sample details'))#,
                           #server = TRUE)
      updateSelectizeInput(session, "categoryShape",
                           choices = c(NO_METADATA_SELECTED, filtered_categories),
                           selected = mySelectedShape,
                           options = list(placeholder = 'Select sample details'))#,
                           #server = TRUE)
    }
    
    mstudy_obj
  })

  output$distance <- renderUI({
    load_microbiome_data()
    if (is.null(properties)) {
      mySelected <- NULL
    } else {
      mySelected <- properties$selected[properties$input == "input$distance"]
    }

    selectInput("distance", 
                label = "Distance Method",
                choices = c("Bray-Curtis" = "bray",
                            "Jensen-Shannon Divergence"="jsd",
                            "Jaccard" = "jaccard",
                            "Canberra" = "canberra",
                            "Kulczynski"="kulczynski",
                            "Horn"="horn",
                            "Mountford"="mountford"),
                selected = mySelected)

  })

  observeEvent({input$distance
               input$category
               input$categoryShape}, {

    text <- paste0("input\tselected\n",
                   "input$distance\t", input$distance, "\n",
                   "input$category\t", input$category, "\n",
                   "input$categoryShape\t", input$categoryShape
            )

    PUT(propUrl, body = "")
    PUT(propUrl, body = text)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$distance,{
    mstudy_obj <- load_microbiome_data()
    distance <- input$distance
    if(!identical(distance, "")){
      shinyjs::hide("contentArea")
      shinyjs::show("betaLoading")
      
      phyloseq_obj <- mbiome2phyloseq(mstudy_obj, "Species")
      dist <- distance(phyloseq_obj, method = distance)
      ordination_obj <<- ordinate(phyloseq_obj, method = "PCoA", distance = dist)
      shinyjs::hide("betaLoading")
      shinyjs::show("contentArea")
    }
  })
  
  output$betadiversityChart <- renderUI({
    mstudy_obj <- load_microbiome_data()
    categoryColor<-input$category
    categoryShape<-input$categoryShape
    distance<-input$distance
    result_to_show<-NULL
    
    if(identical(categoryColor, "") & identical(categoryShape, "") ){
      result_to_show <- h5(class="alert alert-warning", "Please choose either color or shape.")
    }else if( !is.null(ordination_obj) ) {
      shapes <- c(19,17,15,4,8,5:13)
      quantity_shape <- length(mstudy_obj$sample_table$get_unique_details(categoryShape))
      
      shinyjs::hide("contentArea")
      shinyjs::show("chartLoading")
      
      eigvec = ordination_obj$values$Relative_eig
      fracvar = eigvec[1:6] / sum(eigvec)
      percvar = round(100*fracvar, 1)
      ps_data<-data.frame(ordination_obj$vectors)
      ps_data$SampleName<-row.names(ps_data)
      
      categoryColor <- ifelse(identical(categoryColor,""), NO_METADATA_SELECTED, categoryColor)
      categoryShape <- ifelse(identical(categoryShape,""), NO_METADATA_SELECTED, categoryShape)
      
      if(identical(categoryColor, NO_METADATA_SELECTED) & identical(categoryShape, NO_METADATA_SELECTED)){
        
        chart<-ggplot(ps_data, aes(Axis.1, Axis.2))+
          geom_point(aes(size = 4, alpha= 0.5))+
          guides(size=FALSE, alpha=F)
        
      }else if(!identical(categoryColor, NO_METADATA_SELECTED) & identical(categoryShape, NO_METADATA_SELECTED)){
        sample_details<-mstudy_obj$get_metadata_as_column(categoryColor)
        
        merged<-merge(sample_details, ps_data, by="SampleName")
        colnames(merged)<-c("SampleName", "colorCategory", colnames(ps_data)[1:(length(ps_data)-1)])
       
        if (is.character(merged$colorCategory)) {
          merged$colorCategory <- factor(merged$colorCategory, levels=mixedsort(levels(as.factor(merged$colorCategory))))
        }
        if (is.character(merged$colorCategory)){
          colourGuide = guide_legend(keywidth = 1.7, keyheight = 1.7,override.aes = list(size=5))
        } else {
          colourGuide = guide_colourbar()
        }
        chart<-ggplot(merged, aes(Axis.1, Axis.2))+
          theme_eupath_default(
            legend.title.align=0.4,
            legend.title = element_text(colour="black", face="bold")
          )+
          geom_point(aes(size = 4, alpha= 0.5, color=colorCategory))+
          
          guides(shape=FALSE, size=FALSE, alpha=F, colour = colourGuide)+
          labs(color=categoryColor)
        
      }else if(identical(categoryColor, NO_METADATA_SELECTED) & !identical(categoryShape, NO_METADATA_SELECTED)){
        sample_details<-mstudy_obj$get_metadata_as_column(categoryShape)
        
        merged<-merge(sample_details, ps_data, by="SampleName")
        
        colnames(merged)<-c("SampleName", "categoryShape", colnames(ps_data)[1:(length(ps_data)-1)])
       
        #bin shape col if numeric
        #TODO figure how this handles for categorical numeric vars. these should be set to factor before now
        if (is.numeric(merged$categoryShape)) {
          if (uniqueN(merged$categoryShape) > 10) {
                merged$categoryShape <- rcut_number(merged$categoryShape)
              } else {
            merged$categoryShape <- as.factor(merged$categoryShape)
          }
        } else if (is.character(merged$categoryShape)) {
          merged$categoryShape <- factor(merged$categoryShape, levels=mixedsort(levels(as.factor(merged$categoryShape))))
        } 
        
        chart<-ggplot(merged, aes(Axis.1, Axis.2))+
          geom_point(aes(size = 4, alpha= 0.5, shape=categoryShape))+
          guides(colour=FALSE, size=FALSE, alpha=F)+
          scale_shape_manual(values=shapes[1:quantity_shape])+
          guides(shape = guide_legend(override.aes = list(size = 5)))+
          labs(shape=categoryShape)
        
      }else{
        if(identical(categoryColor, categoryShape)){
          sample_details<-mstudy_obj$get_metadata_as_column(categoryColor)
          merged<-merge(sample_details, ps_data, by="SampleName")
          colnames(merged)<-c("SampleName", "categoryColor", colnames(ps_data)[1:(length(ps_data)-1)])
          
          #bin shape col if numeric
          if (is.numeric(merged$categoryColor)) {
            if (uniqueN(merged$categoryColor) > 10) {
                  merged$categoryColor <- rcut_number(merged$categoryColor)
            } else {
              merged$categoryColor <- as.factor(merged$categoryColor)
            }
          } else if (is.character(merged$categoryColor)) {
          merged$categoryColor <- factor(merged$categoryColor, levels=mixedsort(levels(as.factor(merged$categoryColor))))
          }
          if (is.character(merged$colorCategory) ){
            colourGuide = guide_legend(keywidth = 1.7, keyheight = 1.7,override.aes = list(size=5))
          } else {
            colourGuide = guide_colourbar()
          }
          chart<-ggplot(merged, aes(Axis.1, Axis.2))+
            geom_point(aes(size = 4, alpha= 0.5, color=categoryColor, shape=categoryColor))+
            guides(size=FALSE, alpha=F, colour = colourGuide )+
            # guides(size=FALSE, alpha=F)+
            scale_shape_manual(values=shapes[1:quantity_shape])+
            labs(color=categoryColor, shape=categoryColor)
        }else{
          categories<-c(categoryColor, categoryShape)
          sample_details<-mstudy_obj$get_metadata_as_column(categories)
          merged<-merge(sample_details, ps_data, by="SampleName")
          colnames(merged)<-c("SampleName", "colorCategory", "shapeCategory", colnames(ps_data)[1:(length(ps_data)-1)])
          
          #bin shape col if numeric
          #TODO figure how this handles for categorical numeric vars. these should be set to factor before now
          if (is.numeric(merged$shapeCategory)) {
            if (uniqueN(merged$shapeCategory) > 10) {
                  merged$shapeCategory <- rcut_number(merged$shapeCategory)
            } else {
              merged$shapeCategory <- as.factor(merged$shapeCategory)
            }
          } else if (is.character(merged$shapeCategory)) {
          merged$shapeCategory <- factor(merged$shapeCategory, levels=mixedsort(levels(as.factor(merged$shapeCategory))))
          }

        if (uniqueN(merged$colorCategory) > 10){
          colourGuide = guide_colourbar()
        } else {
          colourGuide = guide_legend(keywidth = 1.7, keyheight = 1.7,override.aes = list(size=5))
        }
        chart<-ggplot(merged, aes(Axis.1, Axis.2))+
            geom_point(aes(size = 4, alpha= 0.5, color=colorCategory, shape=shapeCategory))+
            # guides(size=FALSE, alpha=F, colour = guide_legend(order = 1), shape = guide_legend(order = 2))+
            guides(size=FALSE, alpha=F, colour = colourGuide,
                   shape = guide_legend(order = 2, override.aes = list(size = 5)))+
            
            
            scale_shape_manual(values=shapes[1:quantity_shape])+
            labs(color=categoryColor, shape=paste(categoryShape))
          
          fill = guide_legend(keywidth = 1.7, keyheight = 1.7)
        }
      }
      
      chart<-chart+
        xlab(sprintf("Axis.1 [ %.1f %%]", percvar[1]))+
        ylab(sprintf("Axis.2 [ %.1f %%]", percvar[2]))+
        theme_eupath_default(
          legend.title = element_text(face="bold", size=rel(0.9)),
          legend.text = element_text(size=rel(0.9))
        )
      
      ggplot_object <<- chart
      ggplot_build_object <<- ggplot_build(chart)
      
      output$betaChartWrapper<-renderPlotly({
        ggplotly(chart) %>% plotly:::config(displaylogo = FALSE)
      })
      
      result_to_show<-plotlyOutput("betaChartWrapper",
                                 width = paste0(WIDTH,"px"),
                                 height = "500px"
      )
      
      shinyjs::hide("chartLoading", anim = TRUE, animType = "slide")
      shinyjs::show("contentArea")
    }
    
    result_to_show
    
  })


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
        # setEPS()
        # postscript(file, width=16,height=10.67, family = "Helvetica")
        cairo_ps(file, width=16,height=10.67)
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
