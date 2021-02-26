# Declaring the packages
library(shiny)
library(ggplot2)
source("../../functions/wdkDataset.R")
library(data.table)
library(httr)
library(gtools)
library(plotly)
source("functions.R")
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
  
  global_otus<-NULL
  selected_levels<-NULL
  # variables to define some plot parameters
  NUMBER_TAXA <- 10
  WIDTH <- global_width
  MAX_SAMPLES_NO_RESIZE <- 40
  MIN_HEIGHT_AFTER_RESIZE <- 9.5
  
  NO_METADATA_SELECTED <- "Click to select sample details"
  
  ggplot_object <- NULL
  ggplot_build_object <- NULL
  
  ggplot_by_top_otu_object <- NULL
  ggplot_build_by_top_otu_object <- NULL
  
  ggplot_by_otu_object <- NULL
  ggplot_build_by_otu_object <- NULL
  
  eupath_pallete<-c("#999999", "#6a3d9a", "#cab2d6", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99", "#33a02c", "#b2df8a", "#1f78b4", "#a6cee3")
  
  hash_colors<-NULL
  
  propUrl <- NULL
  properties <- NULL

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

      if (is.null(properties)) {
        mySelected <- NO_METADATA_SELECTED
      } else {
        mySelected <- properties$selected[properties$input == "input$category"]
      }
 
      updateSelectizeInput(session, "category",
                           choices = c(NO_METADATA_SELECTED,
                                       mstudy_obj$get_filtered_categories()),
                           selected = mySelected,
                           options = list(placeholder = 'Choose metadata to split the chart'),
                           server = TRUE)
    }
    
    mstudy_obj
  })

  #to use propUrl, need to create all ui from server file.
  output$taxLevel <- renderUI({
    load_properties()
    if (is.null(properties)) {
      mySelected <- "Phylum"
    } else {
      mySelected <- properties$selected[properties$input == "input$taxonLevel"]
    }

    selectInput(
      "taxonLevel",
      label = "Taxonomic level",
      choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
      selected = mySelected,
      width = '100%'
    )
  })

  observeEvent({input$taxonLevel
               input$category
               input$filterOTU}, {

    text <- paste0("input\tselected\n",
                   "input$taxonLevel\t", input$taxonLevel, "\n",
                   "input$category\t", input$category, "\n",
                   "input$filterOTU\t", input$filterOTU
            )

    PUT(propUrl, body = "")
    PUT(propUrl, body = text)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$taxonLevel,{
    mstudy<-load_microbiome_data()
    
    taxon_level <- input$taxonLevel
    if(!identical(taxon_level, "")){
      # output$overviewDatatable <- renderDataTable(NULL)
      global_otus <<- mstudy$get_otus_by_level(taxon_level)
      selected_levels <<- get_columns_taxonomy(taxon_level)
      
      hash_colors <<- eupath_pallete
      top_ten<-mstudy$get_top_n_by_mean(taxon_level, NUMBER_TAXA)
      ordered<-c(mstudy$otu_table$get_ordered_otu(NUMBER_TAXA))
      rev_ordered<-c("Other", rev(ordered))
      names(hash_colors) <<- rev_ordered
     
      if (is.null(properties)) {
        mySelected <- global_otus[1]
      } else { 
        if (input$taxonLevel != properties$selected[properties$input == "input$taxonLevel"]) {
          mySelected <- global_otus[1]
        } else {
          mySelected <- properties$selected[properties$input == "input$filterOTU"]
        }
      }

      updateSelectizeInput(session, "filterOTU",
                           choices = global_otus,
                           selected = mySelected,
                           options = list(placeholder = 'Choose a OTU'),
                           server = TRUE)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
#  overviewChart <- function(){}
#  
#  output$overviewChart <- renderUI({
#    mstudy <- load_microbiome_data()
#    category<-input$category
#    taxon_level<-input$taxonLevel
#    result_to_show<-NULL
#    if(!identical(input$category, "")){
#      shinyjs::hide("divContent")
#      shinyjs::show("chartLoading")
#      quantity_samples <- mstudy$get_sample_count()
#      #TODO test for input without 10 different taxa
#      top_ten<-mstudy$get_top_n_by_mean(taxon_level, NUMBER_TAXA)
#      ordered<-c(mstudy$otu_table$get_ordered_otu(NUMBER_TAXA))
#      rev_ordered<-c("Other", rev(ordered))
#      wrapped_labels<-wrap_column(rev_ordered)
#      top_ten[[taxon_level]]<-factor(top_ten[[taxon_level]], levels=rev_ordered)
#      if(identical(category, NO_METADATA_SELECTED)){
#        chart<-ggplot(top_ten, aes_string(x="SampleName", y="Abundance", fill=taxon_level))+
#          geom_bar(stat="identity", position="stack", color="black")+
#          theme_eupath_default(
#            legend.title.align=0.4,
#            legend.title = element_text(colour="black", size=rel(1), face="bold"),
#            axis.text.y=element_blank(),
#            axis.ticks.y = element_blank()
#          )+
#          scale_fill_manual(values=eupath_pallete, name=taxon_level,
#                            labels = c(wrapped_labels),
#                            guide = guide_legend(reverse=TRUE, keywidth = 1.7, keyheight = 1.7)
#                          )+
#          labs(x="Samples", y="Phylogenetic Relative Abundance")+
#          coord_flip(expand=F)
#      }else{
#        dt_metadata<-mstudy$get_metadata_as_column(category)
#        dt_metadata<-merge(dt_metadata, top_ten, by="SampleName")
#      
#        #colnames(dt_metadata)[2] <- make.names(category)
#        col_renamed <- make.names(category)
#        all_columns <- colnames(dt_metadata)
#        all_columns[2]<-col_renamed
#        colnames(dt_metadata)<-all_columns
#        
#        #check for numeric category and bin
#        #TODO figure how this handles for categorical numeric vars. these should be set to factor before now
#        if (is.numeric(dt_metadata[[col_renamed]])) {
#	  if (uniqueN(dt_metadata[[col_renamed]]) > 10) {
#            dt_metadata[[col_renamed]] <- rcut_number(dt_metadata[[col_renamed]])
#	  } else {
#	    dt_metadata[[col_renamed]] <- as.factor(dt_metadata[[col_renamed]])
#	  }
#        } else if (is.character(dt_metadata[[col_renamed]])) {
#          dt_metadata[[col_renamed]] <- factor(dt_metadata[[col_renamed]], levels=mixedsort(levels(as.factor(dt_metadata[[col_renamed]]))))
#        } 
#
#        chart<-ggplot(dt_metadata, aes_string(x="SampleName", y="Abundance", fill=taxon_level))+
#          geom_bar(stat="identity", position="stack", color="black")+
#	  facet_wrap(as.formula(paste("~ ",col_renamed)), ncol=1, scales='free_y')+
#          #facet_grid(rows = col_renamed, scales='free_y', space='free_y')+
#	  theme(panel.spacing = unit(-.85, "lines")) +
#          theme_eupath_default(
#            legend.title.align=0.4,
#            legend.title = element_text(colour="black", size=rel(1), face="bold"),
#            axis.text.y=element_blank(),
#            axis.ticks.y = element_blank()
#          )+
#          scale_fill_manual(values=eupath_pallete, name=taxon_level,
#                            labels = c(wrapped_labels),
#                            guide = guide_legend(reverse=TRUE, keywidth = 1.7, keyheight = 1.7)
#          )+
#          labs(x="Samples", y="Phylogenetic Relative Abundance")+
#          coord_flip(expand=F)
#      }
#      
#      ggplot_object<<-chart
#      ggplot_build_object<<-ggplot_build(chart)
#
#      output$plotWrapper<-renderPlotly({
#        ggplotly(chart) %>% plotly:::config(displaylogo = FALSE)
#      })
#      
#      if(quantity_samples<MAX_SAMPLES_NO_RESIZE){
#        result_to_show<-plotlyOutput("plotWrapper",
#               width = paste0(WIDTH,"px"), height = "500px"
#             )
#      }else{
#        result_to_show<-plotlyOutput("plotWrapper",
#           height = quantity_samples*MIN_HEIGHT_AFTER_RESIZE
#         )
#      }
#      shinyjs::hide("chartLoading", anim = TRUE, animType = "slide")
#      shinyjs::show("divContent")
#    }
#    result_to_show
#  })
#  

  topAbundance <- function(){}

  output$chartByTopOTU <- renderUI({
    mstudy <- load_microbiome_data()
    category<-input$category
    taxon_level<-input$taxonLevel
    
    result_to_show<-NULL
    
    if(!identical(category, "")){
      
      shinyjs::hide("topTabContent")
      shinyjs::show("topTabLoading")
      
      quantity_samples <- mstudy$get_sample_count()
      
      top_ten<-mstudy$get_top_n_by_mean(taxonomy_level = taxon_level, n = NUMBER_TAXA, 
                                        add_other = F)
      
      ordered<-mstudy$otu_table$get_ordered_otu(NUMBER_TAXA)
      
      top_ten[[taxon_level]]<-factor(top_ten[[taxon_level]], levels=ordered)
      
      if(identical(category, NO_METADATA_SELECTED)){
        chart<-ggplot(top_ten, aes_string(x=taxon_level, y="Abundance"))+
          geom_boxplot()+
          theme_eupath_default(
            legend.title = element_text(colour="black", size=rel(1), face="bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )+
          labs(x=taxon_level, y="Relative Abundance")+
          scale_y_continuous(limits = c(0, 1))+
          scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
      }else{ # end if identical(category, NO_METADATA_SELECTED)
        dt_metadata<-mstudy$get_metadata_as_column(category)
        dt_metadata<-merge(dt_metadata, top_ten, by="SampleName")
        
        col_renamed <- make.names(category)
        all_columns <- colnames(dt_metadata)
        all_columns[2]<-col_renamed
        colnames(dt_metadata)<-all_columns
       
        #check for numeric category and bin
        #TODO figure how this handles for categorical numeric vars. these should be set to factor before now
        if (is.numeric(dt_metadata[[col_renamed]])) {
          if (uniqueN(dt_metadata[[col_renamed]]) > 10) {
            dt_metadata[[col_renamed]] <- rcut_number(dt_metadata[[col_renamed]])
	  } else {
	    dt_metadata[[col_renamed]] <- as.factor(dt_metadata[[col_renamed]])
	  }
        } else if (is.character(dt_metadata[[col_renamed]])) {
          dt_metadata[[col_renamed]] <- factor(dt_metadata[[col_renamed]], levels=mixedsort(levels(as.factor(dt_metadata[[col_renamed]]))))
        }
 
        chart<-ggplot(dt_metadata, aes_string(x=taxon_level, y="Abundance", fill=col_renamed))+
          geom_boxplot()+
          theme_eupath_default(
            legend.title = element_text(colour="black", face="bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )+
          guides(fill = guide_legend(keywidth = 1.7, keyheight = 1.7))+
          labs(x=taxon_level, y="Relative Abundance",fill=category)+
          scale_y_continuous(limits = c(0, 1))+
          scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
        
        # calculating stats
        samples_with_details <- mstudy$sample_table$get_samples_details(category)
        unique_details <- mstudy$sample_table$get_unique_details(category)
        data_frame_table <- data.frame()
        
        if(length(unique_details)==2){
          # the merged solution is to avoid comparing empty vectors
          # now we only have all samples being compared and if there is no taxa for
          # that particular sample we place 0
          for(i in 1:length(ordered)){
            taxa_data <- subset(dt_metadata, get(taxon_level)==ordered[i], select=c("SampleName","Abundance"))
            merged<-merge(samples_with_details, taxa_data, by="SampleName", all = TRUE)
            set(merged,which(is.na(merged[[4L]])),4L,0)
            result<-wilcox.test(merged$Abundance ~ merged$Value, conf.level = 0.95)
            df<-data.frame(a=ordered[i], "W"=result$statistic, "P-value"=result$p.value)
            data_frame_table<-rbind(data_frame_table, df)
          }
          colnames(data_frame_table)<-c(taxon_level, "W", "P-Value")
          
	  data_frame_table[, 3] <- lapply(data_frame_table[, 3], round, 3)
          # data_frame_table[,3]<-format(data_frame_table[,3], scientific = F)
          
          sketch <- tags$table(
            tags$thead(
              tags$tr(
                tags$th(style="text-align:center;", rowspan = 2, taxon_level),
                tags$th(style="text-align:center;",colspan = 2, 'Wilcoxon rank sum test')
              ),
              tags$tr(
                tags$th(style="text-align:center;","W"),
                tags$th(style="text-align:center;","P-Value")
              )
            )
          )
          
          data_frame_table[,taxon_level] <- paste0("<a class='link_table'onclick='goToOtuTab(\"",data_frame_table[,taxon_level],"\")'>",data_frame_table[,taxon_level],"</a>")
          output$by_top_otu_datatable <- DT::renderDataTable(data_frame_table, escape = F, selection = 'none',
                                                             container = sketch, rownames = FALSE)
          
          
        }else{
          for(i in 1:length(ordered)){
            taxa_data <- subset(dt_metadata, get(taxon_level)==ordered[i], select=c("SampleName","Abundance"))
            merged<-merge(samples_with_details, taxa_data, by="SampleName", all = TRUE)
            # TODO replace other ways to change 0 to the follow line
            set(merged,which(is.na(merged[[4L]])),4L,0)
            result<-kruskal.test(merged$Value ~ merged$Abundance)
            df<-data.frame("a"=ordered[i], "chi-squared"=unname(result$statistic), "df"=result$parameter, "P-value"=result$p.value)
            data_frame_table<-rbind(data_frame_table, df)
          }
          colnames(data_frame_table)<-c(taxon_level, "chi-squared", "df", "P-Value")
          data_frame_table[, c(2,4)] <- lapply(data_frame_table[, c(2,4)], round, 3)
	  # data_frame_table[,4]<-format(data_frame_table[,4], scientific = F)
          
          sketch <- tags$table(
            tags$thead(
              tags$tr(
                tags$th(style="text-align:center;", rowspan = 2, taxon_level),
                tags$th(style="text-align:center;",colspan = 3, 'Kruskal-Wallis rank sum test')
              ),
              tags$tr(
                tags$th(style="text-align:center;","chi-squared"),
                tags$th(style="text-align:center;","df"),
                tags$th(style="text-align:center;","P-Value")
              )
            )
          )
          
          data_frame_table[,taxon_level] <- paste0("<a class='link_table'onclick='goToOtuTab(\"",data_frame_table[,input$taxonLevel],"\")'>",data_frame_table[,taxon_level],"</a>")
          output$by_top_otu_datatable <- DT::renderDataTable(data_frame_table, escape = F, selection = 'none',
                                                             container = sketch, rownames = FALSE)
        
        }
      } # end else: if(identical(category, NO_METADATA_SELECTED))
      
      ggplot_by_top_otu_object <<- chart
      ggplot_build_by_top_otu_object <<- ggplot_build(chart)
 
      output$topOtuPlotWrapper<-renderPlotly({
        ggplotly(chart) %>% layout(boxmode = "group") %>% plotly:::config(displaylogo = FALSE)
      })
      
      result_to_show<-plotlyOutput("topOtuPlotWrapper", width = "100%", height = "500px")
      
      shinyjs::hide("topTabLoading", anim = TRUE, animType = "slide")
      shinyjs::show("topTabContent")
    }

   
    result_to_show
  })
  
  abundanceByOTU <- function(){}
  output$chartByOTU <- renderUI({
    
    mstudy <- load_microbiome_data()
    category<-input$category
    otu_picked <- input$filterOTU
    isolate(taxon_level<-input$taxonLevel)
    result_to_show<-NULL
    
    if(is.null(hash_colors)){
      random_color<-sample(eupath_pallete)
      localPallete <- c(random_color[1], "#363636")
    }else{
      if(is.na(match(otu_picked, names(hash_colors)))){
        localPallete <- c("#363636", hash_colors[["Other"]])
      }else{
        localPallete <- c("#363636", hash_colors[[otu_picked]])
      }
    }
    
    if(!identical(category, "") & !identical(otu_picked, "") & !identical(taxon_level,"")){
      shinyjs::hide("singleOtuContent")
      shinyjs::show("singleOtuLoading")
      
      quantity_samples <- mstudy$get_sample_count()
      other<-mstudy$otu_table$OTHER_TEXT
      if(identical(category, NO_METADATA_SELECTED)){
        shinyjs::hide("result_tests")
        data_to_show <- mstudy$get_single_otu(taxon_level, otu_picked, T, T)
        
        data_to_show[[taxon_level]]<-factor(data_to_show[[taxon_level]],
                                            levels=c(other, otu_picked))
        
        chart<-ggplot(data_to_show, aes_string(x="SampleName", y="Abundance", fill=taxon_level))+
          geom_bar(stat = "identity", color="black")+
          theme_eupath_default(
            legend.title.align=0.4,
            legend.title = element_text(colour="black", size=rel(1), face="bold")
          )+
          scale_fill_manual(values=localPallete, labels=c(other, otu_picked), name=taxon_level,
                            guide = guide_legend(reverse=T, keywidth = 1.7, keyheight = 1.7))+
          labs(x="Samples", y=paste(otu_picked,"Relative Abundance"))+
          coord_flip(expand=F)
        
        output$singleOtuBarplotWrapper<-renderPlotly({
          ggplotly(chart) %>% plotly:::config(displaylogo = FALSE)
        })
        
        if(quantity_samples<MAX_SAMPLES_NO_RESIZE){
          result_to_show<-plotlyOutput("singleOtuBarplotWrapper", width = paste0(WIDTH,"px"),
                                     height = "500px"
                                     )
        }else{
          result_to_show<-plotlyOutput("singleOtuBarplotWrapper", width = paste0(WIDTH,"px"),
                                     height = quantity_samples*MIN_HEIGHT_AFTER_RESIZE
                                     )
        }
      }else{ # end if(identical(category, NO_METADATA_SELECTED))
        otu_data <- mstudy$get_single_otu(taxon_level, otu_picked, F)
        metadata_as_column <- mstudy$get_metadata_as_column(category)
        
        col_renamed <- make.names(category)
        colnames(metadata_as_column)<-c("SampleName", col_renamed)
        
        #check for numeric category and bin
        #TODO figure how this handles for categorical numeric vars. these should be set to factor before now
        if (is.numeric(metadata_as_column[[col_renamed]])) {
	  if (uniqueN(metadata_as_column[[col_renamed]]) > 10) {
            metadata_as_column[[col_renamed]] <- rcut_number(metadata_as_column[[col_renamed]])
	  } else {
	    metadata_as_column[[col_renamed]] <- as.factor(metadata_as_column[[col_renamed]])
	  }
        } else if (is.character(metadata_as_column[[col_renamed]])) {
          metadata_as_column[[col_renamed]] <- factor(metadata_as_column[[col_renamed]], levels=mixedsort(levels(as.factor(metadata_as_column[[col_renamed]]))))
        }

        # to plot we don't show samples with 0 abundance
        merged_to_plot<-merge(otu_data, metadata_as_column, by="SampleName")
        # to calculate the statistics we work with all samples
        merged_to_stats<-merge(metadata_as_column, otu_data, by="SampleName", all=T)
        
        chart<-ggplot(merged_to_plot, aes_string(x=col_renamed, y="Abundance"))+geom_boxplot()+
          theme_eupath_default()+
          labs(x=stringi::stri_trans_totitle(category), y=paste(otu_picked, "Relative Abundance"))
        
        output$singleOtuPlotWrapper<-renderPlotly({
          ggplotly(chart) %>% layout(boxmode = "group") %>% plotly:::config(displaylogo = FALSE)
        })
        
        result_to_show<-plotlyOutput("singleOtuPlotWrapper", width = paste0(WIDTH,"px"),
                                   height = "500px"
                        #           hover = hoverOpts("singleOtuTooltip", delay = 60, delayType = "throttle")
        )
        
        merged_to_stats[is.na(merged_to_stats)] <- 0
        unique_details <- mstudy$sample_table$get_unique_details(category)
        
        quantity <- length(unique_details)
        abundance_col <- ncol(merged_to_stats)
        category_col <- 2
        if(quantity==2){
          result<-wilcox.test(merged_to_stats[[abundance_col]]~merged_to_stats[[category_col]], conf.int = T, conf.level = 0.95)
          html_formatted<-HTML(sprintf("<ul class=\"shell-body\"> <li>Wilcoxon rank sum test: W = %f, p-value = %.8f</li></ul>",
                                       result$statistic, result$p.value))
        }else{
          result<-kruskal.test(merged_to_stats[[category_col]]~merged_to_stats[[abundance_col]])
          html_formatted<-HTML(sprintf("<ul class=\"shell-body\"> <li>Kruskal-Wallis rank sum test: chi-squared = %f, df = %f, p-value = %.8f</li></ul>",
                                       result$statistic, result$parameter, result$p.value))
        }
        shinyjs::show("result_tests")
        output$result_tests <-renderUI({html_formatted})
      } # end else (if(identical(category, NO_METADATA_SELECTED)))
      
      ggplot_by_otu_object <<- chart
      ggplot_build_by_otu_object <<- ggplot_build(chart)
 
      shinyjs::hide("singleOtuLoading", anim = TRUE, animType = "slide")
      shinyjs::show("singleOtuContent")
    }else{
      shinyjs::hide("result_tests")
      output$by_otu_datatable <- renderDataTable(NULL)
    }
    result_to_show
  })
  
  
  downloads<-function(){}
  output$btnDownloadPNG <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      png(file, width=1200,height=800,units="px")
      if(identical(input$tabs, "bySample")){
        print(ggplot_object)
      }else if(identical(input$tabs, "byOTU")){
        print(ggplot_by_otu_object)
      }else{
        print(ggplot_by_top_otu_object)
      }
      dev.off()
    }
  )
  
  output$btnDownloadEPS <- downloadHandler(
    filename = "plot.eps",
    content = function(file) {
      setEPS()
      postscript(file, width=16,height=10.67)
      if(identical(input$tabs, "bySample")){
        print(ggplot_object)
      }else if(identical(input$tabs, "byOTU")){
        print(ggplot_by_otu_object)
      }else{
        print(ggplot_by_top_otu_object)
      }
      dev.off()
    }
  )
  
  output$btnDownloadSVG <- downloadHandler(
    filename = "plot.svg",
    content = function(file) {
      svg(file, width=16,height=10.67)
      if(identical(input$tabs, "bySample")){
        print(ggplot_object)
      }else if(identical(input$tabs, "byOTU")){
        print(ggplot_by_otu_object)
      }else{
        print(ggplot_by_top_otu_object)
      }
      dev.off()
    }
  )
  
  output$btnDownloadCSV <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      if(identical(input$tabs, "bySample")){
        write.csv(ggplot_object$data, file)
      }else if(identical(input$tabs, "byOTU")){
        write.csv(ggplot_by_otu_object$data, file)
      }else{
        write.csv(ggplot_by_top_otu_object$data, file)
      }
    }
  )
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
})
