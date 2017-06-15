library(shiny)
library(ggplot2)
library(phyloseq)
library(data.table)
source("config.R")
source("../../lib/wdkDataset.R")

shinyServer(function(input, output, session) {
  # Declaring some global variables
  # df_abundance, df_sample and df_sample.formatted are declared global to avoid 
  # multiple file reading in the reactive section
  df_abundance <- NULL
  df_sample <- NULL
  df_sample.formatted <- NULL
  
  # global objects to read in more than one function
  columns <- NULL
  hash_sample_names<- NULL
  hash_count_samples <- NULL
  
  ggplot_object<-NULL
  ggplot_data <- NULL
  ggplot_build_object <- NULL
  
  abundance_otu <- NULL
  abundance_taxa <- NULL
  
  physeq <- reactive({
    
    # start.time <- Sys.time()
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
    merged_phyloseq<-phyloseq(OTU, TAX, SAMPLE)
    
    for(i in 1:length(columns)){
      unique_factors <- as.factor(sample_data(merged_phyloseq)[[hash_sample_names[[columns[i]]]]]) 
      if(length(levels(unique_factors)) > 1){
        new_columns[k] <- paste0(columns[i], " (",length(levels(unique_factors)), ")")
        hash_count_samples[[new_columns[k]]] <<- columns[i]
        k <- k+1
      }
    }
    columns <<- new_columns
    updateSelectizeInput(session, "category",
                         choices = c("All Samples", columns[2:length(columns)]),
                         selected = "All Samples",
                         options = list(placeholder = 'Type the category to split'),
                         server = TRUE)
    merged_phyloseq
  })
  
  output$abundanceChart <- renderPlot({
    category<-input$category
    measures<-input$measureCheckBox
    if(identical(category, "")){
      return(NULL)
    }
    if(length(measures)==0){
      updateCheckboxGroupInput(session, "measureCheckBox", selected = c("Shannon"))
      chart<- NULL
      ggplot_data<<-NULL
      ggplot_data <<- NULL
      ggplot_build_object<<-NULL
      return(NULL)
    }
    plotRadio <- input$plotTypeRadio
    physeqobj <- physeq()
    if(identical(category, "All Samples")){
      shinyjs::hide("result_tests")
      output$result_tests <- renderUI(NULL)
      if(identical(plotRadio, "dotplot")){
        chart <- plot_richness(physeqobj, measures = measures)+
          geom_point(size = 4, alpha= 0.5)+coord_fixed()+
          theme(
            axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
            axis.text.x = element_blank(),
            axis.ticks.x =  element_blank(),
            text = element_text(family = "Palatino", size=13, face="bold", color="black"),
            panel.border = element_rect(colour="black", size=1, fill=NA),
            strip.text.x = element_text(family = "Palatino", size=13, face="bold", color="black"),
            strip.background = element_rect(fill="#F3F2F2")
          )+
          labs(x="All Samples", y="Alpha Diversity Measure")
      }else{
        
        rich <- estimate_richness(physeqobj, measures = measures)
        rich$SampleName <- gsub("\\.", "\\-", rownames(rich))
        data_melted<-melt(rich, id.vars = c("SampleName"),  measure.vars=measures)
        
        abundance_otu <<- data_melted
        # print(str(data_melted))
        chart<-ggplot(data_melted, aes(variable, value))+geom_boxplot()+
          facet_wrap(~ variable, scales="free")+
          theme(
            axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
            axis.text.x = element_blank(),
            axis.ticks.x =  element_blank(),
            text = element_text(family = "Palatino", size=13, face="bold", color="black"),
            panel.border = element_rect(colour="black", size=1, fill=NA),
            strip.text.x = element_text(family = "Palatino", size=13, face="bold", color="black"),
            strip.background = element_rect(fill="#F3F2F2")
          )+
          labs(x="All Samples", y="Alpha Diversity Measure")
      }
    } # end if(identical(category, "All Samples")){
    else{
      category_column<-hash_sample_names[[hash_count_samples[[category]]]]
      if(identical(plotRadio, "dotplot")){
        chart <- plot_richness(physeqobj, x=category_column, measures = measures)+
          theme(
            axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            text = element_text(family = "Palatino", size=13, face="bold", color="black"),
            panel.border = element_rect(colour="black", size=1, fill=NA),
            strip.text.x = element_text(family = "Palatino", size=13, face="bold", color="black"),
            strip.background = element_rect(fill="#F3F2F2")
            )+
          # scale_x_discrete(labels = abbreviate)+
          geom_point(size = 4, alpha= 0.5)
        # output$result_tests <- renderUI("riariariaria")
      } # end if dotplot
      else{
        rich <- estimate_richness(physeqobj, measures = measures)
        rich$SampleName <- gsub("\\.", "\\-", rownames(rich))
        
        df_sample_selected <- df_sample.formatted[,c("SampleName", category_column)]
        richness_merged <- merge(df_sample_selected, rich, by.x = "SampleName", by.y = "SampleName")
        
        data_melted<-melt(richness_merged, id.vars = c("SampleName", category_column),  measure.vars=measures)
        abundance_otu<<-data_melted
        
        chart<-ggplot(data_melted, aes_string(x=category_column, y="value", group=category_column))+geom_boxplot()+
          facet_wrap(as.formula("~ variable "), scales = "free_y", ncol = 2)+
          theme(
            axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            text = element_text(family = "Palatino", size=13, face="bold", color="black"),
            panel.border = element_rect(colour="black", size=1, fill=NA),
            strip.text.x = element_text(family = "Palatino", size=13, face="bold", color="black"),
            strip.background = element_rect(fill="#F3F2F2")
          )+
          # scale_x_discrete(labels = abbreviate)+
          labs(x=stringi::stri_trans_totitle(hash_count_samples[[category]]), y="Alpha Diversity Measure")
      } # end else dotplot
      
      quantity <- gsub("^(.+)\\s\\((\\d+)\\)$", "\\2", category, perl=T)
      quantity<-as.numeric(quantity)
      
      if(quantity==2){
        output$result_tests <- renderUI(format_results(category_column, T, measures, chart$data))
      }else{
        output$result_tests <- renderUI(format_results(category_column, F, measures, chart$data))
      }
      shinyjs::show("result_tests")
    } # end else (identical(category, "All Samples")){
    ggplot_object<<-chart
    ggplot_data <<- chart$data
    ggplot_build_object <<- ggplot_build(chart)
    
    output$sample_subset <- renderDataTable(NULL)
    chart
  })
  
  
  format_results <- function(category,wilcoxon, measures, gg_data){
    html_formatted<-""
    
    if(wilcoxon==T){
      html_formatted<-"<ul class=\"shell-body\"> <li>Wilcoxon rank sum test:%s</li></ul>"
      text <- ""
      
      for(i in 1:length(measures)){
        df<-subset(ggplot_data, variable==measures[i])
        df_to_run <- df[,c(category,"value")]
        result<-wilcox.test(df_to_run[,2] ~ df_to_run[,1])
        text<-paste0(text, sprintf("<br>[%s]: W = %f, p-value = %.8f", measures[i], result$statistic, result$p.value))
      }
      html_formatted<-HTML(sprintf(html_formatted, text))
    }else{
      html_formatted<-"<ul class=\"shell-body\"> <li>Kruskal-Wallis rank sum test:%s</li></ul>"
      text <- ""
      for(i in 1:length(measures)){
        df<-subset(ggplot_data, variable==measures[i])
        df_to_run <- df[,c(category,"value")]
        result<-kruskal.test(df_to_run[,1] ~ df_to_run[,2])
        text<-paste0(text, sprintf("<br>[%s]: chi-squared = %f, df = %f, p-value = %.8f", measures[i], result$statistic, result$parameter, result$p.value))
      }
      html_formatted<-HTML(sprintf(html_formatted, text))
    }
    html_formatted
  }
  
  # output$result_tests <-renderUI({
  #   html_formatted<-""
  #   measures<-input$measureCheckBox
  #   category <- input$category
  #   if(identical(category, "") || identical(category, "All Samples")){
  #     return(NULL)
  #   }
  #   if(length(measures)==0){
  #     updateCheckboxGroupInput(session, "measureCheckBox", selected = c("Shannon"))
  #     chart<- NULL
  #     ggplot_data<<-NULL
  #     ggplot_data <<- NULL
  #     ggplot_build_object<<-NULL
  #   }
  #   physeqobj<-physeq()
  #   
  #   quantity <- gsub("^(.+)\\s\\((\\d+)\\)$", "\\2", category, perl=T)
  #   quantity<-as.numeric(quantity)
  #   
  #   if(quantity==2){
  #     html_formatted<-"<ul class=\"shell-body\"> <li>Wilcoxon rank sum test:%s</li></ul>"
  #     text <- ""
  #     
  #     for(i in 1:length(measures)){
  #       df<-subset(ggplot_data, variable==measures[i])
  #       df_to_run <- df[,c(2,4)]
  #       result<-wilcox.test(df_to_run[,2] ~ df_to_run[,1])
  #       text<-paste0(text, sprintf("<br>[%s]: W = %f, p-value = %.8f", measures[i], result$statistic, result$p.value))
  #     }
  #     html_formatted<-HTML(sprintf(html_formatted, text))
  #   }else{
  #     html_formatted<-"<ul class=\"shell-body\"> <li>Kruskal-Wallis rank sum test:%s</li></ul>"
  #     text <- ""
  #     for(i in 1:length(measures)){
  #       df<-subset(ggplot_data, variable==measures[i])
  #       df_to_run <- df[,c(2,4)]
  #       result<-kruskal.test(df_to_run[,1] ~ df_to_run[,2])
  #       # print(input$measureCheckBox[i])
  #       # print(result)
  #       text<-paste0(text, sprintf("<br>[%s]: chi-squared = %f, df = %f, p-value = %.8f", measures[i], result$statistic, result$parameter, result$p.value))
  #     }
  #     html_formatted<-HTML(sprintf(html_formatted, text))
  #   }
  #   html_formatted
  # })
  
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
      postscript(file, width=16,height=10.67, family = "Palatino")
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
  
  # output$category <- renderUI({
  #   lvls <- columns
  #   lvls[1] <- "All Samples"
  #   selectInput("category", label = "Category: ", 
  #               choices = lvls, selected = 1)
  #   
  # })
  
  output$hover_info <- renderUI({
    
    # if(is.null(ggplot_object)){
    #   return(NULL)
    # }
    
    hover <- input$plot_hover
    physeqobj <- physeq()
    lvls <- rownames(sample_data(physeqobj))
    
    if (is.null(hover$x) || round(hover$x) <0 || round(hover$y)<0 || is.null(hover$y))
      return(NULL)
    
    left_pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <-
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <-
      paste0(
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); "
        ,
        "left:",
        left_px + 2,
        "px; top:",
        top_px + 2,
        "px;"
      )
    
    if(identical(input$plotTypeRadio, "dotplot") ){
      near_points <- nearPoints(ggplot_data, hover)

      if(identical(input$category, "All Samples")){
        if(nrow(near_points) > 0){
          lvls <- rownames(sample_data(physeqobj))
          hover_sample <- lvls[round(hover$x)]
          alpha_and_sample <- ""
          for(i in 1:nrow(near_points)){
            if(!is.na(near_points[i,"se"]) && near_points[i,"se"]!=0){
              alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"samples"],
                                         "<br><b>Alpha diversity: </b>", sprintf("%.3f [SE: %.5f]",near_points[i,"value"],
                                                                                 near_points[i,"se"]),"<br>")
            }else{
              alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"samples"],
                                         "<br><b>Alpha diversity: </b>", sprintf("%.3f",near_points[i,"value"]),"<br>")
            }
          }
          wellPanel(style = style,
                    tags$b("Measure: "),
                    hover$panelvar1,
                    br(),
                    HTML(alpha_and_sample)
          )
        }
      }else{
        if(nrow(near_points) > 0){
          category_value <- sample_data(physeqobj)[,hash_sample_names[[hash_count_samples[[input$category]]]]]
          lvls <- levels(as.factor(category_value[[1]]))
          hover_category <- lvls[round(hover$x)]
          alpha_and_sample <- ""
          for(i in 1:nrow(near_points)){
            if(!is.na(near_points[i,"se"]) && near_points[i,"se"]!=0){
              alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"samples"],
                                         "<br><b>Alpha diversity: </b>", sprintf("%.3f [SE: %.5f]",near_points[i,"value"],
                                                                                 near_points[i,"se"]),"<br>")
            }else{
              alpha_and_sample <- paste0(alpha_and_sample, "<b>Sample: </b>",near_points[i,"samples"],
                                         "<br><b>Alpha diversity: </b>", sprintf("%.3f",near_points[i,"value"]),"<br>")
            }
          }
          wellPanel(style = style,
                    tags$b("Measure: "),
                    hover$panelvar1,
                    br(),
                    tags$b(paste(hash_count_samples[[input$category]], ": ")),
                    hover_category,
                    br(),
                    HTML(alpha_and_sample)
          )
        }
      }
    }else{ # end if(identical(input$plotTypeRadio, "dotplot") ){
      gg_data <- ggplot_build_object$data[[1]]
      subBoxplot<-subset(gg_data,hover$x>=xmin & hover$x<=xmax & hover$y>=ymin & hover$y<=ymax & PANEL==match(hover$panelvar1, input$measureCheckBox))
      alpha_and_sample <- NULL
      if(identical(input$category, "All Samples")){
        if(nrow(subBoxplot)>0){
          alpha_and_sample <- ""
          for(i in 1:nrow(subBoxplot)){
            alpha_and_sample <- paste0(alpha_and_sample,
                                       sprintf("<b>Alpha diversity values:</b><br>&nbsp;&nbsp;<b>min: </b>%f - <b>max: </b>%f<br>
                                               &nbsp;&nbsp;<b>25th percentile: </b>%f<br>&nbsp;&nbsp;<b>Median: </b>%f<br>&nbsp;&nbsp;<b>75th percentile: </b>%f<br>",
                                               subBoxplot[i,"ymin"],subBoxplot[i,"ymax"],subBoxplot[i,"lower"],subBoxplot[i,"middle"],subBoxplot[i,"upper"]) )
          }
          
        }else{
          near_points <- nearPoints(ggplot_data, hover)
          if(nrow(near_points) > 0){
            alpha_and_sample <- ""
            for(i in 1:nrow(near_points)){
              alpha_and_sample <- paste0(alpha_and_sample,
                                         sprintf("<b>Sample: </b>%s<br><b>Alpha diversity: </b>%.3f<br>",
                                                 near_points[i,"SampleName"],near_points[i,"value"]))
            }
          }
        }
        # return(NULL)
      }else{ # if(identical(input$category, "All Samples")){
        column_category<-hash_sample_names[[hash_count_samples[[input$category]]]]
        original_category<-hash_count_samples[[input$category]]
        all_groups <- levels(as.factor(ggplot_data[,column_category]))
        if(nrow(subBoxplot)>0){
          alpha_and_sample<-""
          for(i in 1:nrow(subBoxplot)){
            alpha_and_sample <- paste0(alpha_and_sample,
                                     sprintf("<b>Alpha diversity values:</b><br>&nbsp;&nbsp;<b>min: </b>%f - <b>max: </b>%f<br>
                                             &nbsp;&nbsp;<b>25th percentile: </b>%f<br>&nbsp;&nbsp;<b>Median: </b>%f<br>&nbsp;&nbsp;<b>75th percentile: </b>%f<br><b>%s: </b>%s<br>",
                                             subBoxplot[i,"ymin"],subBoxplot[i,"ymax"],subBoxplot[i,"lower"],subBoxplot[i,"middle"], subBoxplot[i,"upper"], original_category, all_groups[subBoxplot[i,"group"]]) )
          }
        }else{
          near_points <- nearPoints(ggplot_data, hover)
          if(nrow(near_points) > 0){
            alpha_and_sample <- ""
            for(i in 1:nrow(near_points)){
              alpha_and_sample <- paste0(alpha_and_sample,
                                         sprintf("<b>Sample: </b>%s<br><b>Alpha diversity: </b>%.3f<br><b>%s</b>: %s<br>",
                                                 near_points[i,"SampleName"],near_points[i,"value"], original_category, near_points[i,column_category]))
            }
          }
        }
      }# else
      if(!is.null(alpha_and_sample)){
        wellPanel(style = style,
                  tags$b("Measure: "),
                  hover$panelvar1,
                  br(),
                  HTML(alpha_and_sample)
        )
      }else{
        return(NULL)
      }
    }
  })
  
  observeEvent(input$plot_click, {
    click <- input$plot_click
    if (is.null(click$y))
      return(NULL)
    
    if(identical(input$category, "All Samples")){
      output_table <- subset(ggplot_data, ggplot_data$variable==click$panelvar1, select=c("SampleName", "variable", "value"))
      colnames(output_table)<-c("Sample", "Measure", "Alpha Diversity")
    }else{
      column_category<-hash_sample_names[[hash_count_samples[[input$category]]]]
      original_category<-hash_count_samples[[input$category]]
      
      output_table <- subset(ggplot_data, ggplot_data$variable==click$panelvar1, select=c("SampleName", "variable", "value", column_category))
      colnames(output_table)<-c("Sample", "Measure", "Alpha Diversity", original_category)
    }
    
    output$sample_subset <- renderDataTable(output_table, 
                                            options = list(
                                              order = list(list(2, 'desc'))
                                            )
    )
  })
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
}) # end shinyServer
