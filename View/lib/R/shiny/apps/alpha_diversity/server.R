library(shiny)
library(ggplot2)
library(phyloseq)
library(data.table)
source("../../lib/wdkDataset.R")
source("../common/ggplot_extension.R")
source("../common/hover_tooltips.R")
source("../common/config.R")

shinyServer(function(input, output, session) {
  # Declaring some global variables
  # df_abundance, df_sample and df_sample.formatted are declared global to avoid 
  # multiple file reading in the reactive section
  df_abundance <- NULL
  df_sample <- NULL
  df_sample.formatted <- NULL
  
  richness_object <- NULL
  
  
  phyloseq_obj <- NULL
  
  # global objects to read in more than one function
  columns <- NULL
  hash_sample_names<- NULL
  hash_count_samples <- NULL
  
  ggplot_object<-NULL
  ggplot_data <- NULL
  ggplot_build_object <- NULL
  
  ggplot_object_mt<-NULL
  ggplot_data_mt <- NULL
  ggplot_build_object_mt <- NULL
  
  abundance_otu <- NULL
  abundance_taxa <- NULL
  
  maximum_samples_without_resizing <- 65
  minimum_height_after_resizing <- 8.5
  create_phyloseq <- reactive({
    if(is.null(df_abundance)){
      df_abundance <<-
      read.csv(
        getWdkDatasetFile('TaxaRelativeAbundance.tab', session, FALSE, dataStorageDir),
          sep = "\t",
          col.names = c("Sample","Taxon", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "RelativeAbundance", "AbsoluteAbundance", "EmptyColumn"),
          colClasses = c("character", "integer", "character", "character", "character", "character", "character", "character", "character", "numeric", "integer", "character")
        )
      
      # Change with the Characteristics file
      df_sample <<-
      read.csv(
        getWdkDatasetFile('Characteristics.tab', session, FALSE, dataStorageDir),
          sep = "\t",
          col.names = c("SampleName", "Source", "Property", "Value", "Type", "Filter", "EmptyColumn"),
          colClasses = c("character", "character", "character", "character", "character", "character", "character")
        )
      
      df_sample.formatted <<- dcast(data = df_sample,formula = SampleName~Property, value.var = "Value")
      
      properties <- unique(df_sample[,c("Property","Type")], MARGIN=1)
      properties <- subset(properties, Type != "string")
      
      for(i in 1:nrow(properties)){
        if(identical(properties[i,"Type"], "number")){
          df_sample.formatted[,properties[i,"Property"]]<<-factor(df_sample.formatted[,properties[i,"Property"]],
                                                                  levels=sort(as.numeric(unique(df_sample.formatted[,properties[i,"Property"]]))))
          # df_sample.formatted[,properties[i,"Property"]]<<-as.numeric(df_sample.formatted[,properties[i,"Property"]])
        }
      }
      rownames(df_sample.formatted) <<- df_sample.formatted[,1]
      columns <<- colnames(df_sample.formatted)
      corrected_columns <-  make.names(columns)
      colnames(df_sample.formatted) <<- corrected_columns
      names(corrected_columns) <- columns
      
      hash_sample_names <<- corrected_columns
      
      SAMPLE <- sample_data(df_sample.formatted)
      
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus+Species~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      if(ncol(df_abundance.formatted) == 8){
        OTU_MATRIX <- df_abundance.formatted[,8, drop=F]
      }else{
        OTU_MATRIX <- df_abundance.formatted[,8:ncol(df_abundance.formatted)]
      }
      
      OTU = otu_table(OTU_MATRIX, taxa_are_rows = input$taxa_are_rows)
      TAX_MATRIX <- df_abundance.formatted[,1:7]
      TAX_MATRIX <- as.matrix(TAX_MATRIX)
      TAX <- tax_table(TAX_MATRIX)
      SAMPLE <- sample_data(df_sample.formatted)
      
      categories <- columns
      new_columns <- 0
      k <- 1
      phyloseq_obj<<-phyloseq(OTU, TAX, SAMPLE)
      
      min_factors<-1000
      
      for(i in 1:length(columns)){
        unique_factors <- as.factor(df_sample.formatted[[hash_sample_names[[columns[i]]]]])
        if(length(levels(unique_factors)) > 1){
          new_columns[k] <- paste0(columns[i], " (",length(levels(unique_factors)), ")")
          hash_count_samples[[new_columns[k]]] <<- columns[i]
          
          if(length(levels(unique_factors)) < min_factors){
            min_factors <- length(levels(unique_factors))
            column_factors<-new_columns[k]
            levels_factors<-levels(unique_factors)
          }
          k <- k+1
        }
      }
      columns <<- new_columns
      updateSelectizeInput(session, "category",
                           choices = c(columns[2:length(columns)]),
                           # selected = column_factors,
                           selected = NULL,
                           options = list(
                             placeholder = 'Click here to select'
                             # onInitialize = I('function() { this.setValue(""); }')
                           ),
                           server = TRUE)
      
      all_measures <- c("Chao1", "ACE", "Shannon", "Simpson", "Fisher")
      richness_object <<- estimate_richness(phyloseq_obj, measures = all_measures)
      richness_object$SampleName <<- gsub("\\.", "\\-", rownames(richness_object))
    }
    phyloseq_obj
  })
  
  allSamples <- function(){}
  output$allSamplesChart <- renderUI({
    physeq <- create_phyloseq()
    isolate(category<-input$category)
    # reactive values
    measures<-input$measuresCheckbox
    plotRadio <- input$plotTypeRadio
    if(length(measures)==0){
      output$allSamplesDt <- renderDataTable(NULL)
      h5(class="alert alert-danger", "Please choose at least one alpha diversity measure.")  
    }
    else{
      quantity_samples <- length(sample_names(physeq))
      output$allSamplesDt <- renderDataTable(NULL)
      
      if(quantity_samples <= maximum_samples_without_resizing | identical(plotRadio, "boxplot")){
        plotOutput("allSamplesWrapper", width = "100%", height = "400px",
                   hover = hoverOpts("hoverAllSamples"))
      }else{
        plotOutput("allSamplesWrapper", width = "100%", height = quantity_samples*minimum_height_after_resizing,
                   hover = hoverOpts("hoverAllSamples"))
        # plotOutput("allSamplesWrapper", hover = hoverOpts("plot_hoverByOTU", delay = 60, delayType = "throttle"), width = "100%", height = quantity_samples*minimum_height_after_resizing)
      }
    }
  })
  
  output$allSamplesWrapper <- renderPlot({
    physeq <- create_phyloseq()
    
    measures<-input$measuresCheckbox
    plotRadio <- input$plotTypeRadio
    if(length(measures)==0){
      return(NULL)
    }
    
    shinyjs::hide("allSamplesArea")
    shinyjs::show("chartLoading")
    # static values
    measures_with_se <- c("Chao1","ACE")
    names(measures_with_se)<-c("se.chao1","se.ACE")
    
    have_measures_se<-measures_with_se %in% measures
    if(any(have_measures_se)){
      indexes_which_is_true<-which(have_measures_se)
      ses = names(measures_with_se)[indexes_which_is_true]
      rich <- richness_object[,c("SampleName", measures,ses)]
    }else{
      rich <- richness_object[,c("SampleName", measures)]
      ses = NULL
    }
    
    rich<-rich[seq(dim(rich)[1],1),]
    
    rich$SampleName<-factor(rich$SampleName, levels=rich$SampleName)
    
    data_melted<-melt(rich, id.vars = c("SampleName"),  measure.vars=measures)
    
    if(length(ses)>0){
      se_melted <-melt(rich, id.vars = c("SampleName"),  measure.vars=ses)
      
      se_melted[,"variable"]<-sapply(se_melted[,"variable"], function(x){
        measures_with_se[x]
      })
      colnames(se_melted)<-c("SampleName", "variable", "se")
      data_melted<-merge(data_melted,se_melted,by=c("SampleName", "variable"), all.x=T)
      data_melted[is.na(data_melted$se), "se"]<-0
    }else{
      data_melted$se<-0
    }
    
    if(identical(plotRadio, "dotplot")){
      chart <- ggplot(data_melted, aes_string(x="value", y="SampleName"))+
        geom_errorbarh(aes(xmax=value + se, xmin=value - se), height = .1)+
        facet_wrap(~ variable, scales="free_x")+
        geom_point(shape = 21, alpha=1, colour = "grey", fill = "black", size = 3, stroke = 1.5)+
        theme_eupath_default(
          panel.border = element_rect(colour="black", size=1, fill=NA),
          axis.text.y = element_text(size=rel(0.8),face="bold")
        )+
        labs(x="Alpha Diversity",  y="Samples")
    } # end if is dotplot
    else{
      chart<-ggplot(data_melted, aes(variable, value))+geom_boxplot()+
        facet_wrap(~ variable, scales="free")+
        theme_eupath_default(
          panel.border = element_rect(colour="black", size=1, fill=NA),
          axis.text.x = element_blank(),
          axis.ticks.x =  element_blank()
        )+
        labs(x="All Samples", y="Alpha Diversity")
    }
    
    number_columns <- ncol(rich)
    output$allSamplesDt = renderDataTable(
      rich,
      options = list(
        order = list(list(0, 'desc'))
      )
    )
    
    shinyjs::hide("chartLoading", anim = TRUE, animType = "fade")
    shinyjs::show("allSamplesArea")
    print("entrouuuu")
    ggplot_object<<-chart
    ggplot_build_object<<-ggplot_build(chart)
    ggplot_data<<-chart$data
    chart
  })
  
  byMetadata <- function(){}
  output$byMetadataChart <- renderUI({
    physeq <- create_phyloseq()
    
    # reactive values
    measures<-input$measuresCheckbox
    plotRadio <- input$plotTypeRadio
    category <- category_button()
    if(length(measures)==0){
      output$byMetadataDt <- renderDataTable(NULL)
      h5(class="alert alert-danger", "Please choose at least one alpha diversity measure.")  
    }else if(is.null(category)){
      # print("achou bebe")
      output$byMetadataDt <- renderDataTable(NULL)
      h5(class="alert alert-warning", "Please choose at least one metadata.")  
    }
    else{
      quantity_samples <- length(sample_names(physeq))
      # output$byMetadataDt <- renderDataTable(NULL)
      
      if(quantity_samples <= maximum_samples_without_resizing | identical(plotRadio, "boxplot")){
        plotOutput("byMetadataChartWrapper", width = "100%", height = "400px",
                   hover = hoverOpts("hoverByMetadata"))
      }else{
        plotOutput("byMetadataChartWrapper", width = "100%", height = quantity_samples*minimum_height_after_resizing,
                   hover = hoverOpts("hoverByMetadata"))
      }
    }
  })
  
  category_button <- eventReactive(input$doneButton, {
    input$category
  })
  
  output$byMetadataChartWrapper <- renderPlot({
    physeq <- create_phyloseq()
    
    measures<-input$measuresCheckbox
    plotRadio <- input$plotTypeRadio
    category <- category_button()
    
    category_to_show <- hash_count_samples[category]
    category_column <- hash_sample_names[category_to_show]
    
    if(length(measures)==0 | is.null(category)){
      output$byMetadataDt <- renderDataTable(NULL)
      return(NULL)
    }
    
    shinyjs::hide("metadataContent")
    shinyjs::show("metadataLoading")
    
    output$result_tests <- renderUI(NULL)
    
    measures_with_se <- c("Chao1","ACE")
    names(measures_with_se)<-c("se.chao1","se.ACE")
    
    have_measures_se<-measures_with_se %in% measures
    
    if(any(have_measures_se)){
      indexes_which_is_true<-which(have_measures_se)
      ses = names(measures_with_se)[indexes_which_is_true]
      rich <- richness_object[,c("SampleName", measures,ses)]
    }else{
      rich <- richness_object[,c("SampleName", measures)]
      ses = NULL
    }
    
    rich<-rich[seq(dim(rich)[1],1),]
    
    rich$SampleName<-factor(rich$SampleName, levels=rich$SampleName)
    
    df_sample_selected <- df_sample.formatted[,c("SampleName", category_column)]
    richness_merged <- merge(df_sample_selected, rich, by = "SampleName")
   
    data_melted<-melt(richness_merged, id.vars = c("SampleName", category_column),  measure.vars=measures)
    
    if(length(ses)>0){
      se_melted <-melt(rich, id.vars = c("SampleName"),  measure.vars=ses)
      
      se_melted[,"variable"]<-sapply(se_melted[,"variable"], function(x){
        measures_with_se[x]
      })
      colnames(se_melted)<-c("SampleName", "variable", "se")
      data_melted<-merge(data_melted,se_melted,by=c("SampleName", "variable"), all.x=T)
      data_melted[is.na(data_melted$se), "se"]<-0
    }else{
      data_melted$se<-0
    }
    
    chart<-ggplot(data_melted, aes_string(category_column[1], "value"))+
      theme_eupath_default(
        panel.border = element_rect(colour="black", size=1, fill=NA),
        axis.text.y = element_text(size=rel(0.8),face="bold")
      )+
      scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))+
      labs(x=paste(category_to_show), y="Alpha Diversity")
    
    
    if(identical(plotRadio, "dotplot")){
      chart<-chart+
        geom_point(shape = 21, alpha=1, colour = "grey", fill = "black", size = 3, stroke = 1.5)
        # geom_errorbar(aes(ymax=value + se, ymin=value - se), height = .1) # error
    } # end if is dotplot
    else{
      chart<-chart+
        geom_boxplot()
    }
    
    if(length(category_column)==1){
      chart <- chart+facet_grid(as.formula(paste("variable ~ .")), scales = "free_y")
    }else{
      joined_categories <- paste(category_column[2:length(category_column)], collapse = " ~ ")
      chart <- chart+
        facet_grid(as.formula(paste("variable ~",joined_categories)), scales = "free_y")
    }
    
    ggplot_object_mt<<-chart
    ggplot_build_object_mt<<-ggplot_build(chart)
    ggplot_data_mt<<-chart$data
    
    output$result_tests <- renderUI(runStatisticalTests(category_column, measures, ggplot_data_mt))
    output$byMetadataDt = renderDataTable(
      richness_merged,
      options = list(
        order = list(list(0, 'desc'))
      )
    )
    
    
    shinyjs::hide("metadataLoading", anim = TRUE, animType = "fade")
    shinyjs::show("metadataContent")
    
    chart
  })
  
  hovers <- function(){}
  output$uiHoverByMetadata <- renderUI({
    hover <- input$hoverByMetadata
    physeqobj <- create_phyloseq()
    isolate(typeRadio<-input$plotTypeRadio)
    isolate(measure<-input$measureCheckBox)
    
    if (is.null(hover$x) || round(hover$x) <0 || round(hover$y)<0 || is.null(hover$y))
      return(NULL)
    
    isolate(category<-input$category)
    category_to_show <- hash_count_samples[category]
    category_column <- hash_sample_names[category_to_show]
    
    max_col <- max(ggplot_build_object_mt$layout$panel_layout$COL)
    
    tooltip_direction<-"right"
    if(max_col == 1){
      middle <- (hover$domain$right-hover$domain$left)/2
      if(hover$x>middle){
        tooltip_direction<-"left"
      }
    }else{
      panel_layout <- ggplot_build_object_mt$layout$panel_layout
      last_category <- category_column[length(category_column)]
      panel_hover <- panel_layout[panel_layout[last_category] == hover$panelvar1, ]
      if(panel_hover$COL[1] == max_col){
        middle <- (hover$domain$right-hover$domain$left)/2
          if(hover$x>middle){
            tooltip_direction<-"left"
          }
      }
    }
    
    variable_number <- ifelse(length(category)==1, 1, 2)
    variable_index<-paste0("panelvar",variable_number)
    
    panel_filtered <- subset(ggplot_build_object_mt$layout$panel_layout, variable==hover[[variable_index]])
    length_category<-length(category)
    
    hover_format <- "<b>%s:</b>&nbsp;%s<br>"
    
    # PROBLEMA AQUI
    x_labels <- ggplot_data_mt[,category_column[1]]
    x_labels <- levels(factor(x_labels))
    hover_category<-sprintf(hover_format, category_to_show[1], x_labels[round(hover$x)])
    
    if(length_category>1){
      hover_category <- paste0(hover_category, sprintf(hover_format, category_to_show[length_category], hover$panelvar1))
      panel_filtered <- panel_filtered[panel_filtered[category_column[length_category]] == hover$panelvar1, ]
      
      if(length_category>=3){
        for(i in 1:(length_category-2)){
          panel_category_index<-paste0("panelvar",length_category+i-1)
          panel_filtered <- panel_filtered[panel_filtered[category_column[i+1]] == hover[[panel_category_index]], ]
          
          hover_category <- paste0(hover_category, sprintf(hover_format, category_to_show[i+1], hover[[panel_category_index]]))
          
        } 
      }
    }
    
    
    if(identical(typeRadio, "dotplot") ){
      near_points <- nearPoints(ggplot_data_mt, hover)
      if(nrow(near_points)>0){
        alpha_and_sample <- ""
        for(i in 1:nrow(near_points)){
          if(!is.na(near_points[i,"se"]) && near_points[i,"se"]!=0){
            alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"SampleName"],
                                       "<br><b>Alpha diversity: </b>", sprintf("%.3f [SE: %.5f]",near_points[i,"value"],
                                                                               near_points[i,"se"]),"<br>")
          }else{
            alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"SampleName"],
                                       "<br><b>Alpha diversity: </b>", sprintf("%.3f",near_points[i,"value"]),"<br>")
          }
        }# end for near_points
        text<-sprintf("<strong>Measure:</strong> %s<br>%s%s", near_points[1,"variable"], hover_category, alpha_and_sample)
        return(get_simple_hover(hover, text, tooltip_direction, offset_right = 20))
      }else{
        return(NULL)
      }
    }else{
      gg_data<-subset(ggplot_build_object_mt$data[[1]], PANEL==panel_filtered$PANEL & x==round(hover$x))
      
      if(nrow(gg_data)>0){
        if(hover$x<gg_data$xmin | hover$x > gg_data$xmax){
          return(NULL)
        }
        line_data<-subset(gg_data, hover$x>=xmin & hover$x<=xmax)
        
        alpha_and_sample <- sprintf("<b>Measure:</b>&nbsp;%s<br>%s
                                  &nbsp;&nbsp;<b>min: </b>%f - <b>max: </b>%f<br>
                                  &nbsp;&nbsp;<b>25th percentile: </b>%f<br>
                                  &nbsp;&nbsp;<b>Median: </b>%f<br>
                                  &nbsp;&nbsp;<b>75th percentile: </b>%f<br>",panel_filtered$variable,hover_category,
                                    line_data[["ymin"]],line_data[["ymax"]],line_data[["lower"]],line_data[["middle"]],line_data[["upper"]])
        
        return(get_simple_hover(hover, alpha_and_sample, tooltip_direction, offset_right=20))
        # boxplot_hover <- get_single_boxplot_hover(hover, line_data$xmax, line_data$x, line_data$ymin, line_data$lower, line_data$middle, line_data$upper, line_data$ymax)
        # return(HTML(boxplot_hover))
      }else{
        return(NULL)
      }
    }
  })
  
  
  output$uiHoverAllSamples <- renderUI({
    hover <- input$hoverAllSamples
    physeqobj <- create_phyloseq()
    isolate(typeRadio<-input$plotTypeRadio)
    isolate(measure<-input$measureCheckBox)

    if (is.null(hover$x) || round(hover$x) <0 || round(hover$y)<0 || is.null(hover$y))
      return(NULL)
    
    # to define if the tooltip will be in the right or left side
    max_col <- max(ggplot_build_object$layout$panel_layout$COL)
    # panel_hover <- subset(ggplot_build_object$layout$panel_layout, variable==hover$panelvar1)
    tooltip_direction <- "right"
    
    if(max_col == 1){
      middle <- (hover$domain$right+hover$domain$left)/2
      if(hover$x>middle){
        tooltip_direction<-"left"
      }
    }else{
      panel_layout <- ggplot_build_object$layout$panel_layout
      panel_layout <- subset(panel_layout, variable==hover$panelvar1)
      if(panel_layout$COL[1]==max_col){
        middle <- (hover$domain$right-hover$domain$left)/2
        if(hover$x>middle){
          tooltip_direction<-"left"
        }
      }
    }
    
    # print(tooltip_direction)
    if(identical(typeRadio, "dotplot") ){
      
      near_points <- nearPoints(ggplot_data, hover)
      
      # print(panel_number)
      if(nrow(near_points)>0){
        alpha_and_sample <- ""
        for(i in 1:nrow(near_points)){
          if(!is.na(near_points[i,"se"]) && near_points[i,"se"]!=0){
            alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"SampleName"],
                                       "<br><b>Alpha diversity: </b>", sprintf("%.3f [SE: %.5f]",near_points[i,"value"],
                                                                               near_points[i,"se"]),"<br>")
          }else{
            alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"SampleName"],
                                       "<br><b>Alpha diversity: </b>", sprintf("%.3f",near_points[i,"value"]),"<br>")
          }
        }# end for near_points
        text<-sprintf("<strong>Measure:</strong> %s<br>%s", near_points[1,"variable"], alpha_and_sample)
        
        return(get_simple_hover(hover, text, tooltip_direction))
        
      } # end if nearPoints > 0
    } # end if it is dotplot
    else{
      panel_number <- subset(ggplot_build_object$layout$panel_layout, variable==hover$panelvar1, select="PANEL")
      gg_data<-subset(ggplot_build_object$data[[1]], PANEL==panel_number[[1]])
      
      if(nrow(gg_data)==1){
        if(hover$x<gg_data$xmin | hover$x > gg_data$xmax){
          return(NULL)
        }
        line_data<-subset(gg_data, hover$x>=xmin & hover$x<=xmax)
        
        alpha_and_sample <- sprintf("<b>Measure:</b>&nbsp;%s<br>
                                  &nbsp;&nbsp;<b>min: </b>%f - <b>max: </b>%f<br>
                                  &nbsp;&nbsp;<b>25th percentile: </b>%f<br>
                                  &nbsp;&nbsp;<b>Median: </b>%f<br>
                                  &nbsp;&nbsp;<b>75th percentile: </b>%f<br>",hover$panelvar1,
                                    line_data[["ymin"]],line_data[["ymax"]],line_data[["lower"]],line_data[["middle"]],line_data[["upper"]])
        
        return(get_simple_hover(hover, alpha_and_sample, tooltip_direction))
      } # end if nrow(gg_data)==1
      
      return(NULL)
    } # end else it is dotplot
  })
  
  
  
  
  # download buttons
  downloadButtons <- function(){}
  
  output$btnDownloadPNG <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      isolate(selected_tab<-input$tabs)
      png(file, width=1200,height=800,units="px")
      if(identical(selected_tab, "firstTab")){
        print(ggplot_object)
      }else{
        print(ggplot_object_mt)
      }
      dev.off()
    }
  )
  
  output$btnDownloadEPS <- downloadHandler(
    filename = "plot.eps",
    content = function(file) {
      isolate(selected_tab<-input$tabs)
      setEPS()
      postscript(file, width=16,height=10.67, family = "Palatino")
      if(identical(selected_tab, "firstTab")){
        print(ggplot_object)
      }else{
        print(ggplot_object_mt)
      }
      dev.off()
    }
  )
  
  output$btnDownloadSVG <- downloadHandler(
    filename = "plot.svg",
    content = function(file) {
      isolate(selected_tab<-input$tabs)
      svg(file, width=16,height=10.67)
      if(identical(selected_tab, "firstTab")){
        print(ggplot_object)
      }else{
        print(ggplot_object_mt)
      }
      dev.off()
    }
  )
  
  output$btnDownloadCSV <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      if(identical(selected_tab, "firstTab")){
        write.csv(ggplot_object$data, file)
      }else{
        write.csv(ggplot_object_mt$data, file)
      }
    }
  )
  
  
  runStatisticalTests <- function(category, measures, gg_data){
      html_formatted<-"<ul class=\"shell-body\"> %s</ul>"
      
      if(length(category)==1){
        levels_df <- levels(factor(gg_data[[category]]))
        if(length(levels_df)==2){
          html_formatted<-sprintf(html_formatted, "<li>Wilcoxon rank sum test:%s</li>")
        }else{
          html_formatted<-sprintf(html_formatted, "<li>Kruskal-Wallis rank sum test:%s</li>")
        }
        
        text <- ""
        for(i in 1:length(measures)){
          df<-subset(gg_data, variable==measures[i])
          df_to_run <- df[,c(category,"value")]
          if(length(levels_df)==2){
            suppressWarnings(
                result<-wilcox.test(df_to_run[,2] ~ df_to_run[,1])
              )
            text<-paste0(text, sprintf("<br>[%s]: W = %f, p-value = %.8f", measures[i], result$statistic, result$p.value))
          }else{
            suppressWarnings(
              result<-kruskal.test(df_to_run[,1] ~ df_to_run[,2])
            )
            text<-paste0(text, sprintf("<br>[%s]: chi-squared = %f, df = %f, p-value = %.8f", measures[i], result$statistic, result$parameter, result$p.value))
          }
        }
        html_formatted<-HTML(sprintf(html_formatted, text))
      }else{
        html_formatted <- ""
      }
      html_formatted
    }
  # 
  # observeEvent(input$plot_click, {
  #   click <- input$plot_click
  #   if (is.null(click$y))
  #     return(NULL)
  #   
  #   if(identical(input$category, "All Samples")){
  #     output_table <- subset(ggplot_data, ggplot_data$variable==click$panelvar1, select=c("SampleName", "variable", "value"))
  #     colnames(output_table)<-c("Sample", "Measure", "Alpha Diversity")
  #   }else{
  #     column_category<-hash_sample_names[[hash_count_samples[[input$category]]]]
  #     original_category<-hash_count_samples[[input$category]]
  #     
  #     output_table <- subset(ggplot_data, ggplot_data$variable==click$panelvar1, select=c("SampleName", "variable", "value", column_category))
  #     colnames(output_table)<-c("Sample", "Measure", "Alpha Diversity", original_category)
  #   }
  #   
  #   output$sample_subset <- renderDataTable(output_table, 
  #                                           options = list(
  #                                             order = list(list(2, 'desc'))
  #                                           )
  #   )
  # })
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}) # end shinyServer
