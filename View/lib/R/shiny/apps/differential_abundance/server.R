library(shiny)
library(ggplot2)
source("../../functions/wdkDataset.R")
library(phyloseq)
library(data.table)
library(DESeq2)
library(httr)
library(plotly)
library(dplyr)
source("../../functions/ebrc_functions.R")
source("../../lib/ggplot_ext/eupath_default.R")
source("../../lib/ggplot_ext/eupath_functions.R")
source("../../lib/config.R")
source("../../lib/mbiome/mbiome-reader.R")
source("functions.R")

shinyServer(function(input, output, session) {
  mstudy_obj <- NULL 
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

  physeq <- reactive({
    # Change with the file with abundances
    if(is.null(df_abundance)){
      mstudy_obj <<- import.eupath(
        taxa_abundance_path = getWdkDatasetFile('TaxaRelativeAbundance.tab', session, FALSE, dataStorageDir),
        sample_path = getWdkDatasetFile('Characteristics.tab', session, FALSE, dataStorageDir),
        datasets_path = getWdkDatasetFile('Datasets.tab', session, FALSE, dataStorageDir)
      )
      df_abundance <<- as.data.frame(mstudy_obj$otu_table$otu_dt)
      df_sample <<- as.data.frame(mstudy_obj$sample_table$sample_dt) 

      df_sample.formatted <<- dcast(data = df_sample,formula = SampleName~Property, value.var = "Value")
      
      rownames(df_sample.formatted) <<- df_sample.formatted[,1]
      
      columns <<- colnames(df_sample.formatted)
      
      corrected_columns <-  make.names(columns)
      colnames(df_sample.formatted) <<- corrected_columns
      names(corrected_columns) <- columns 
      
      #convert to number based on df_sample
      myNums <- unique(df_sample$Property[df_sample$Type == "number" & df_sample$Filter == "range"])
      myNums <- make.names(myNums)
      df_sample.formatted[myNums] <- sapply(df_sample.formatted[myNums], as.numeric)

      hash_sample_names <<- corrected_columns
      
      new_columns <- 0
      k <- 1
      min_factors<-1000
      
      for(i in 1:length(columns)){
        #bin numbers into two groups to compare
        if (is.numeric(df_sample.formatted[[hash_sample_names[[columns[i]]]]])) {
	  if (uniqueN(df_sample.formatted[[hash_sample_names[[columns[i]]]]]) > 10) {
            df_sample.formatted[[hash_sample_names[[columns[i]]]]] <- rcut_number(df_sample.formatted[[hash_sample_names[[columns[i]]]]], 2)
	  } else {
	    df_sample.formatted[[hash_sample_names[[columns[i]]]]] <- as.factor(df_sample.formatted[[hash_sample_names[[columns[i]]]]])
	  }
        }

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
     
      SAMPLE <<- sample_data(df_sample.formatted)  
      columns <<- new_columns
      
      if (is.null(properties)) {
        message("column_factors: ", column_factors)
        mySelectedCategory <- character(0) # column_factors
        mySelectedFactor1 <- character(0) #levels_factors[1]
        mySelectedFactor2 <- character(0) #levels_factors[2]
      } else {
        mySelectedCategory <- properties$selected[properties$input == "input$category"]
        dontUseProps <- FALSE
        if (is.null(input$category)) {
           dontUseProps <- TRUE 
        } else if (input$category != mySelectedCategory) {
           dontUseProps <- TRUE
        }

        if (dontUseProps) {
          mySelectedFactor1 <- levels_factors[1]
          mySelectedFactor2 <- levels_factors[2]
        } else {
          mySelectedFactor1 <- properties$selected[properties$input == "input$factor1"]
          mySelectedFactor2 <- properties$selected[properties$input == "input$factor2"]
        }
      }

      levels_factors<-sort(levels_factors)
      updateSelectizeInput(session, "category",
                           choices = c(columns[2:length(columns)]),
                           selected = mySelectedCategory,
                           options = list(placeholder = 'Choose metadata to calculate differential abundance'),
                           server = T)
      
      
      if(length(levels_factors)>2){
        choices_level <- c(paste("not",levels_factors[1]), levels_factors)
        selected_level<-choices_level[1]
        updateSelectizeInput(session, "factor1",
                             choices = c("Not Factor 2", levels_factors),
                             selected = mySelectedFactor1,
                             options = list(placeholder = 'Choose the first factor'),
                             server = T)
        updateSelectizeInput(session, "factor2",
                             choices = c("Not Factor 1", levels_factors),
                             selected = mySelectedFactor2,
                             options = list(placeholder = 'Choose the second factor'),
                             server = T)
        
      }else{
        updateSelectizeInput(session, "factor1",
                             choices = levels_factors,
                             selected = mySelectedFactor1,
                             options = list(placeholder = 'Choose the first factor'),
                             server = T)
        updateSelectizeInput(session, "factor2",
                             choices = levels_factors,
                             selected = mySelectedFactor2,
                             options = list(placeholder = 'Choose the second factor'),
                             server = T)
      }
      
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
  
  #to use propUrl, need to create all ui from server file.
  output$taxonLevel <- renderUI({
    load_properties()
    if (is.null(properties)) {
      mySelected <- "Species"
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
               input$factor1
               input$factor2}, {

    text <- paste0("input\tselected\n",
                   "input$taxonLevel\t", input$taxonLevel, "\n",
                   "input$category\t", input$category, "\n",
                   "input$factor1\t", input$factor1, "\n",
                   "input$factor2\t", input$factor2
            )

    PUT(propUrl, body = "")
    PUT(propUrl, body = text)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$exchangeBtn, {
    isolate(factor1<-input$factor1)
    isolate(factor2<-input$factor2)
    if(identical(factor1, "Not Factor 2")){
      factor1<-"Not Factor 1"
    }
    if(identical(factor2, "Not Factor 1")){
      factor2<-"Not Factor 2"
    }
    updateSelectizeInput(session, "factor1", selected = factor2, server = F)
    updateSelectizeInput(session, "factor2", selected = factor1, server = F)
  })
  
  observeEvent(input$category, {
    category<-input$category
    if(!identical(category, "")){
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      levels_category<-sort(unique(SAMPLE[[category_column]]))
  
    mySelectedCategory <- properties$selected[properties$input == "input$category"]
    if (is.null(properties)) {
      mySelectedFactor1 <- levels_category[1]
      mySelectedFactor2 <- levels_category[2]
    } else {
      if (category != mySelectedCategory) {
        mySelectedFactor1 <- levels_category[1]
        mySelectedFactor2 <- levels_category[2]
      } else {
        mySelectedFactor1 <- properties$selected[properties$input == "input$factor1"]
        mySelectedFactor2 <- properties$selected[properties$input == "input$factor2"]
      }
    }
    
      if(length(levels_category)>2){
        updateSelectizeInput(session, "factor1",
                             choices = c("Not Factor 2", levels_category),
                             selected = mySelectedFactor1,
                             options = list(placeholder = 'Choose the first factor'),
                             server = F)
        updateSelectizeInput(session, "factor2",
                             choices = c("Not Factor 1", levels_category),
                             selected = mySelectedFactor2,
                             options = list(placeholder = 'Choose the second factor'),
                             server = F)
      }else{
        updateSelectizeInput(session, "factor1",
                             choices = levels_category,
                             selected = mySelectedFactor1,
                             options = list(placeholder = 'Choose the first factor'),
                             server = F)
        updateSelectizeInput(session, "factor2",
                             choices = levels_category,
                             selected = mySelectedFactor2,
                             options = list(placeholder = 'Choose the second factor'),
                             server = F)
      }
    }
  })
  
  mainChart <- function(){}
  output$mainChart <- renderUI({
    if (is.null(input$taxonLevel)) {
      return()
    }

    physeq1<-physeq()
    
    isolate(category<-input$category)
    
    factor1<-input$factor1
    factor2<-input$factor2
    taxon_level<-input$taxonLevel
    if(!identical(category, "") & !identical(factor1, "") & !identical(factor2, "")){
      if(identical(factor1, factor2) | (identical(factor1, "Not Factor 2") & identical(factor2, "Not Factor 1") )){
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
          
          output$plotWrapper<-renderPlotly({
            ggplotly(chart) %>% plotly:::config(displaylogo = FALSE)
          })
          
          result_to_show<-plotlyOutput("plotWrapper")
          
        }else{ 
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
    if(!identical(factor1, factor2) &  !(identical(factor1, "Not Factor 2") & identical(factor2, "Not Factor 1") ) ){
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      
      df_sample_filter <- SAMPLE[,category_column]
      
      if(identical(factor2, "Not Factor 1")){
        df_sample_filter[df_sample_filter[,category_column]!=factor1,category_column]<-"Not Factor 1"
      }else if(identical(factor1, "Not Factor 2")){
        df_sample_filter[df_sample_filter[,category_column]!=factor2,category_column]<-"Not Factor 2"
      }else{
        df_sample_filter<-df_sample_filter[df_sample_filter[,category_column]==factor1 | df_sample_filter[,category_column]==factor2]
      }
      new_physeq_obj<-phyloseq(OTU, TAX, sample_data(df_sample_filter))
      # new_physeq_obj <- subset_samples(new_physeq_obj, !is.na(category_column))
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
	
	rownames(abundance_otu_relative) <- ifelse(duplicated(abundance_taxa[,taxon_level]), abundance_taxa[,taxon_level], paste(rownames(abundance_otu_relative), abundance_taxa[,taxon_level]))

	myOTU <- as.data.frame(t(abundance_otu_relative))
	myOTU$sampleName <- rownames(t(abundance_otu_relative))
	samdata <- data.frame(sample_data(new_physeq_obj))
	samdata$sampleName <- rownames(samdata)
	samdata[, category_column] <- paste("Median abundance", as.character(samdata[, category_column]))
	data <- merge(samdata, myOTU, by = 'sampleName') 
	data <- data %>%
  		group_by(get(category_column)) %>%
		summarise_at(vars(-sampleName), funs(median(as.numeric(.), na.rm = TRUE)))
	data[,2] <- data[,1]
	names <- data[,category_column]
	data <- t(data[3:length(data)])
	colnames(data) <- unlist(names)
	data <- as.data.frame.matrix(data)
	data[[taxon_level]] <- rownames(data)
        cols_to_show<-sigtab[,c("baseMean", "log2FoldChange", "pvalue", taxon_level)]
	cols_to_show <- merge(cols_to_show, data, by=taxon_level)
	       
 
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
