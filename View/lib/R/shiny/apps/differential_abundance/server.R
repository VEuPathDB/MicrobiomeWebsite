library(shiny)
library(ggplot2)
library(phyloseq)
library(data.table)
source("../../lib/wdkDataset.R")
library(DESeq2)
source("../common/ggplot_extension.R")
source("../common/config.R")
source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  df_abundance <- NULL
  df_sample <- NULL
  df_sample.formatted <- NULL
  
  # global objects to read in more than one function
  columns <- NULL
  hash_sample_names<- NULL
  hash_count_samples <- NULL
  ggplot_object <- NULL
  ggplot_build_object <- NULL
  
  SAMPLE <- NULL
  OTU<-NULL
  TAX<-NULL
  
  loaded_category <- FALSE
  
  hash_colors <- NULL
  
  abundance_otu <- NULL
  abundance_otu_relative <- NULL
  abundance_taxa <- NULL
  
  maximum_samples_without_resizing <- 65
  minimum_height_after_resizing <- 6
  
  physeq <- reactive({
    # Change with the file with abundances
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
      
      rownames(df_sample.formatted) <<- df_sample.formatted[,1]
      
      columns <<- colnames(df_sample.formatted)
      
      corrected_columns <-  make.names(columns)
      colnames(df_sample.formatted) <<- corrected_columns
      names(corrected_columns) <- columns 
      
      hash_sample_names <<- corrected_columns
      
      SAMPLE <<- sample_data(df_sample.formatted)
      
      new_columns <- 0
      k <- 1
      first_with_two<-0
      levels_factors_two<-0
      levels_factors_three<-0
      first_with_three<-0
      
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
      
      levels_factors<-sort(levels_factors)
      updateSelectizeInput(session, "category",
                           choices = c(columns[2:length(columns)]),
                           selected = column_factors,
                           options = list(placeholder = 'Choose the metadata to calculate the differential abundance'),
                           server = T)
      updateSelectizeInput(session, "factor1",
                           choices = levels_factors,
                           selected = levels_factors[1],
                           options = list(placeholder = 'Choose the first factor'),
                           server = T)
      updateSelectizeInput(session, "factor2",
                           choices = levels_factors,
                           selected = levels_factors[2],
                           options = list(placeholder = 'Choose the second factor'),
                           server = T)
    } # end if df_abundance is not null
    
    if(identical(input$taxonLevel, "Phylum")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      df_abundance_form_relative <- dcast(data = df_abundance,formula = Kingdom+Phylum~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 3
      column_tax <- 2
    }else if(identical(input$taxonLevel, "Class")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      df_abundance_form_relative <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 4
      column_tax <- 3
    }else if(identical(input$taxonLevel, "Order")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      df_abundance_form_relative <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 5
      column_tax <- 4
    }else if(identical(input$taxonLevel, "Family")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      df_abundance_form_relative <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 6
      column_tax <- 5
    }else if(identical(input$taxonLevel, "Genus")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      df_abundance_form_relative <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 7
      column_tax <- 6
    }else{
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus+Species~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      df_abundance_form_relative <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus+Species~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 8
      column_tax <- 7
    }
    
    # output$by_sample_datatable <- renderDataTable(NULL)
    abundance_otu <<- df_abundance.formatted[,column_otu:ncol(df_abundance.formatted)]
    abundance_otu_relative <<- df_abundance_form_relative[,column_otu:ncol(df_abundance.formatted)]
    OTU <<- otu_table(abundance_otu, taxa_are_rows = input$taxa_are_rows)
    
    abundance_taxa <<- df_abundance.formatted[,1:column_tax]
    abundance_taxa <<- fix_taxonomy_names(abundance_taxa)
    
    TAX <<- tax_table(as.matrix(abundance_taxa))
    
    hash_colors<<-NULL
    merged_phyloseq <- phyloseq(OTU, TAX, SAMPLE)
    merged_phyloseq
  })
  
  observeFunctions <- function(){}
  
  observeEvent(input$exchangeBtn, {
    isolate(factor1<-input$factor1)
    isolate(factor2<-input$factor2)
    updateSelectizeInput(session, "factor1", selected = factor2, server = F)
    updateSelectizeInput(session, "factor2", selected = factor1, server = F)
  })
  
  observeEvent(input$category, {
    category<-input$category
    if(!identical(category, "")){
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      levels_category<-sort(unique(SAMPLE[[category_column]]))
      updateSelectizeInput(session, "factor1",
                           choices = levels_category,
                           selected = levels_category[1],
                           options = list(placeholder = 'Choose the first factor'),
                           server = F)
      updateSelectizeInput(session, "factor2",
                           choices = levels_category,
                           selected = levels_category[2],
                           options = list(placeholder = 'Choose the second factor'),
                           server = F)
    }
  })
  
  mainChart <- function(){}
  output$mainChart <- renderUI({
    physeq1<-physeq()
    
    isolate(category<-input$category)
    
    factor1<-input$factor1
    factor2<-input$factor2
    taxon_level<-input$taxonLevel
    if(!identical(category, "") & !identical(factor1, "") & !identical(factor2, "")){
      if(identical(factor1, factor2)){
        output$datatableOutput<-renderDataTable(NULL)
        shinyjs::disable("btnDownloadPNG")
        shinyjs::disable("btnDownloadSVG")
        shinyjs::disable("btnDownloadEPS")
        shinyjs::disable("btnDownloadCSV")
        return(
          h5(class="alert alert-danger", "Please choose different factors to calculate the differential abundance.")  
        )
      }else{
        deseq_result <- runDeseq()
        
        if(!is.null(deseq_result)){
          # the x axis will be from -max_fold_change to +max_fold_change
          max_fold_change<-max(abs(deseq_result$log2FoldChange))
          category_column <- hash_count_samples[[category]]
          
          limits_plot<-c(levels(deseq_result[[taxon_level]]),"","")
          
          chart<-ggplot(deseq_result, aes_string(x="log2FoldChange", y=taxon_level, color="Phylum")) +
            annotate("text", x = 0, y = nrow(deseq_result)+1, size=5, colour = "red", parse=T,
                     label = sprintf("atop(bold(\"%s - %s vs %s\"))", category_column, factor2, factor1) )+
            xlim(-max_fold_change, max_fold_change)+
            # ylim(0, nrow(deseq_result)+1.5)+
            geom_segment(aes_string(x = 0, y = taxon_level, xend = "log2FoldChange", yend = taxon_level), color = "grey50") +
            geom_point(aes(size = log10(0.5+1/pvalue)))+
            scale_size(range = c(3, 9), guide = 'none')+
            # theme_eupath(legend.position = "right", legend.direction="vertical")+
            theme_eupath(legend.position = "bottom")+
            scale_y_discrete(position = "right", limits=limits_plot, breaks = levels(deseq_result[[taxon_level]]) )+
            guides(colour = guide_legend(override.aes = list(size=8)))
          
          ggplot_object<<-chart
          
          ggplot_build_object<<-ggplot_build(chart)
          shinyjs::enable("btnDownloadPNG")
          shinyjs::enable("btnDownloadSVG")
          shinyjs::enable("btnDownloadEPS")
          shinyjs::enable("btnDownloadCSV")
          
          output$plotWrapper<-renderPlot({
            chart
          })
          
          result_to_show<-plotOutput("plotWrapper", hover = hoverOpts("plot_hover"))
          
        }else{ 
          # output$datatableOutput<-renderDataTable(NULL)
          # output$mainChart<-renderUI(NULL)
          # ggplot_object<<-NULL
          # ggplot_build_object<<-NULL
          shinyjs::disable("btnDownloadPNG")
          shinyjs::disable("btnDownloadSVG")
          shinyjs::disable("btnDownloadEPS")
          shinyjs::disable("btnDownloadCSV")
          result_to_show<-h5(class="alert alert-warning", "Sorry, but there is no OTU with differential abundance using your search parameters.")
        }
        result_to_show
      }
    }else{
      if(!identical(category, "") & (identical(factor1,"")|identical(factor2,"") )){
        output$datatableOutput<-renderDataTable(NULL)
        ggplot_object<<-NULL
        ggplot_build_object<<-NULL
      }
    }
  })
  
  runDeseq <- reactive({
    isolate(
      category<-input$category
    )
    
    factor1<-input$factor1
    factor2<-input$factor2
    taxon_level <- input$taxonLevel
    
    chart<-NULL
    shinyjs::hide("chartContent")
    shinyjs::show("chartLoading")
    if(!identical(factor1, factor2)){
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      
      df_sample_filter <- SAMPLE[,category_column]
      
      df_sample_filter<-df_sample_filter[df_sample_filter[,category_column]==factor1 | df_sample_filter[,category_column]==factor2]
      
      new_physeq_obj<-phyloseq(OTU, TAX, sample_data(df_sample_filter))
      new_physeq_obj <- prune_samples(sample_sums(new_physeq_obj) > 500, new_physeq_obj)
      
      # creating factor with levels in the same order as the select input factors
      sample_data(new_physeq_obj)[[category_column]] <- factor(sample_data(new_physeq_obj)[[category_column]], levels=c(factor1,factor2))
      
      diagdds = phyloseq2Deseq2(new_physeq_obj, as.formula(paste0("~", category_column)))
      # calculate geometric means prior to estimate size factors
      gm_mean = function(x, na.rm=TRUE){
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
      }
      
      geoMeans = apply(counts(diagdds), 1, gm_mean)
      diagdds = estimateSizeFactors(diagdds, geoMeans = geoMeans)
      diagdds = DESeq(diagdds, fitType="local", quiet=T)
      
      #investigating test result
      res = results(diagdds)
      
      res = res[order(res$padj, na.last=NA), ]
      alpha = 0.01
      sigtab = res[(res$padj < alpha), ]
      
      if(nrow(sigtab)>0){
        sigtab = cbind(as(sigtab, "data.frame"), as(TAX[rownames(sigtab), ], "matrix"))
        
        sigtab<-sigtab[order(sigtab$log2FoldChange),]
        sigtab[,taxon_level]<-factor(sigtab[,taxon_level], levels=sigtab[,taxon_level])
        
        cols_to_show<-sigtab[,c("baseMean", "log2FoldChange", "pvalue", taxon_level)]
        
        output$datatableOutput<-renderDataTable(cols_to_show,
                                                options = list(
                                                  order = list(list(1, 'desc'))
                                                )
        )
        # log data transformation by https://stats.stackexchange.com/questions/83914/how-to-log-transform-data-with-a-large-number-of-zeros
        chart<-sigtab
      }else{
        output$datatableOutput<-renderDataTable(NULL)
      }
    }
    shinyjs::hide("chartLoading")
    shinyjs::show("chartContent")
    chart
  })
  
  
  hovers <- function(){}
  
  output$hover_info <- renderUI({
    
    hover <- input$plot_hover
    
    if (is.null(hover$x) || is.null(hover$y) || is.null(ggplot_object))
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
        "position:absolute; background-color: rgba(245, 245, 245, 0.85); z-index:1000;",
        "left:",
        left_px + 2,
        "px; top:",
        top_px + 2,
        "px;"
      )
    near_points <- nearPoints(ggplot_object$data, hover)
    if(nrow(near_points)>0){
      isolate(taxa_level<-input$taxonLevel)
      isolate(category<-input$category)
      isolate(factor1<-input$factor1)
      isolate(factor2<-input$factor2)
      
      text_hover<-""
      if(nrow(near_points) == 1){
        filtered_taxa <- abundance_taxa[abundance_taxa[,taxa_level] == near_points[1, taxa_level],]
        filtered_otu <- abundance_otu_relative[rownames(filtered_taxa),]
        
        
        
        otu_for_plot <- as.data.frame(t(filtered_otu))
        otu_for_plot$SampleName <- rownames(otu_for_plot)
        otu_for_plot_filtered<-otu_for_plot[otu_for_plot[,1]>0,]
        
        colnames(otu_for_plot_filtered)<-c("Abundance", "SampleName")
        
        category_column<-hash_sample_names[[hash_count_samples[[category]]]]
        df_sample_selected <- df_sample.formatted[,c("SampleName", category_column)]
        df_sample_selected <- df_sample_selected[df_sample_selected[,category_column] == factor1 | df_sample_selected[,category_column] == factor2,]
        
        
        data_merged <- merge(df_sample_selected, otu_for_plot_filtered, by = "SampleName", all.y=F)
        data_merged[,category_column] <- factor(data_merged[,category_column], levels=c(factor1,factor2))
        
        chart<-ggplot(data_merged, aes_string(x=hash_sample_names[[hash_count_samples[[input$category]]]], y="Abundance"))+geom_boxplot()+
          theme(
            axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            text = element_text(family = "Palatino", size=13, face="bold", color="black"),
            panel.border = element_rect(colour="black", size=1, fill=NA),
            strip.text.x = element_text(family = "Palatino", size=13, face="bold", color="black"),
            strip.background = element_rect(fill="#F3F2F2")
          )+
          labs(x=stringi::stri_trans_totitle(hash_count_samples[[category]]), y="Relative Abundance")
        
        output$plotHover<-renderPlot(chart)
        
        text_hover <- paste0(text_hover, sprintf("<b>%s: </b>", taxa_level), near_points[1, taxa_level],
                       "<br><b>log2FoldChange: </b>", sprintf("%.3f",near_points[1,"log2FoldChange"]),
                       "<br><b>pvalue: </b>", sprintf("%0.6g",near_points[1,"pvalue"]) )
      }else{
        # for(i in 1:nrow(near_points)){
        # } 
      }
      wellPanel(style = style,
        HTML(text_hover),
        plotOutput("plotHover", width = 250, height = 200)
      )
    }else{
      return(NULL)
    }
    
  })
  
  
  downloads <- function(){}
  
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
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
})
