library(shiny)
library(ggplot2)
library(phyloseq)
library(data.table)
source("config.R")
source("../../lib/wdkDataset.R")
library(DESeq2)
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
  
  abundance_otu <- NULL
  abundance_taxa <- NULL
  
  SAMPLE <- NULL
  
  hash_colors <- NULL
  
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
      for(i in 1:length(columns)){
        unique_factors <- as.factor(df_sample.formatted[[hash_sample_names[[columns[i]]]]])
        if(length(levels(unique_factors)) > 1){
          new_columns[k] <- paste0(columns[i], " (",length(levels(unique_factors)), ")")
          hash_count_samples[[new_columns[k]]] <<- columns[i]
          
          if(first_with_two == 0 & length(levels(unique_factors))==2){
            first_with_two<-new_columns[k]
            levels_factors_two<-levels(unique_factors)
          }else if(first_with_three == 0 & length(levels(unique_factors))==3){
            first_with_three<-new_columns[k]
            levels_factors_three<-levels(unique_factors)
          }
          k <- k+1
        }
      }
      columns <<- new_columns
      if(first_with_two != 0){
        updateSelectizeInput(session, "category",
                             choices = c(columns[2:length(columns)]),
                             selected = first_with_two,
                             options = list(placeholder = 'Choose metadata to split the chart'),
                             server = TRUE)
        updateSelectizeInput(session, "factor1",
                             choices = levels_factors_two,
                             selected = levels_factors_two[1],
                             options = list(placeholder = 'Choose metadata to split the chart'),
                             server = TRUE)
        updateSelectizeInput(session, "factor2",
                             choices = levels_factors_two,
                             selected = levels_factors_two[2],
                             options = list(placeholder = 'Choose metadata to split the chart'),
                             server = TRUE)
      }else{
        # if(first_with_three!= 0){
        #   updateSelectizeInput(session, "category",
        #                        choices = c(columns[2:length(columns)]),
        #                        selected = first_with_three,
        #                        options = list(placeholder = 'Choose metadata to split the chart'),
        #                        server = TRUE) 
        #   updateSelectizeInput(session, "factor1",
        #                        choices = levels_factors_three,
        #                        selected = levels_factors_three[1],
        #                        options = list(placeholder = 'Choose metadata to split the chart'),
        #                        server = TRUE) 
        #   updateSelectizeInput(session, "factor2",
        #                        choices = levels_factors_three,
        #                        selected = levels_factors_three[2],
        #                        options = list(placeholder = 'Choose metadata to split the chart'),
        #                        server = TRUE) 
        # }
      }
    } # end if abundance is not null
    
    if(identical(input$taxonLevel, "Phylum")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      column_otu <- 3
      column_tax <- 2
    }else if(identical(input$taxonLevel, "Class")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      column_otu <- 4
      column_tax <- 3
    }else if(identical(input$taxonLevel, "Order")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      column_otu <- 5
      column_tax <- 4
    }else if(identical(input$taxonLevel, "Family")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      column_otu <- 6
      column_tax <- 5
    }else if(identical(input$taxonLevel, "Genus")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      column_otu <- 7
      column_tax <- 6
    }else{
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus+Species~Sample,fun.aggregate = sum,value.var = "AbsoluteAbundance")
      column_otu <- 8
      column_tax <- 7
    }
    
    # output$by_sample_datatable <- renderDataTable(NULL)
    abundance_otu <<- df_abundance.formatted[,column_otu:ncol(df_abundance.formatted)]
    
    OTU = otu_table(abundance_otu, taxa_are_rows = input$taxa_are_rows)
    
    abundance_taxa <<- df_abundance.formatted[,1:column_tax]
    abundance_taxa <<- fix_taxonomy_names(abundance_taxa)
    
    TAX <- tax_table(as.matrix(abundance_taxa))
    
    hash_colors<<-NULL
    merged_phyloseq <- phyloseq(OTU, TAX, SAMPLE)
    merged_phyloseq
  })
  
  observeFunctions <- function(){}
  observeEvent(input$factor1, {
    isolate(
      category<-input$category
    )
    
    if(!identical(category, "")){
      physeq1<-physeq()
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      levels_category<-unique(sample_data(physeq1)[[category_column]])
      
      all_but_selected <- levels_category[!levels_category %in% input$factor1]
      updateSelectizeInput(session, "factor2",
                           choices = levels_category,
                           selected = all_but_selected[1],
                           options = list(placeholder = 'Choose metadata to split the chart'),
                           server = F)
    }
  })
  
  observeEvent(input$factor2, {
    isolate(
      category<-input$category
    )
    
    if(!identical(category, "")){
      physeq1<-physeq()
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      levels_category<-unique(sample_data(physeq1)[[category_column]])
      
      all_but_selected <- levels_category[!levels_category %in% input$factor2]
      updateSelectizeInput(session, "factor1",
                           choices = levels_category,
                           selected = all_but_selected[1],
                           options = list(placeholder = 'Choose metadata to split the chart'),
                           server = F)
    }
  })
  
  observeEvent(input$category, {
    category<-input$category
    if(!identical(category, "")){
      physeq1<-physeq()
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      levels_category<-unique(sample_data(physeq1)[[category_column]])
      if(length(levels_category) == 2){
        if(!identical(levels_category[1], input$factor1)){
          updateSelectizeInput(session, "factor1",
                               choices = levels_category,
                               selected = levels_category[1],
                               options = list(placeholder = 'Choose metadata to split the chart'),
                               server = F)
          updateSelectizeInput(session, "factor2",
                               choices = levels_category[2],
                               selected = levels_category[2],
                               options = list(placeholder = 'Choose metadata to split the chart'),
                               server = F)
        }
      }else{
        updateSelectizeInput(session, "factor1",
                             choices = levels_category,
                             selected = levels_category[1],
                             options = list(placeholder = 'Choose metadata to split the chart'),
                             server = F)
        updateSelectizeInput(session, "factor2",
                             choices = levels_category[2:length(levels_category)],
                             selected = levels_category[2],
                             options = list(placeholder = 'Choose metadata to split the chart'),
                             server = F)
      }
    }
  })
  
  mainChart <- function(){}
  output$mainChart <- renderPlot({
    isolate(
      category<-input$category
    )
    chart<-NULL
    
    factor1<-input$factor1
    factor2<-input$factor2
    
    if(!identical(category, "") & !identical(factor1, "")){
      
      if(identical(factor1, factor2)){
        return(NULL)
      }
      
      physeq1<-physeq()
      category_column <- hash_sample_names[[hash_count_samples[[category]]]]
      
      shinyjs::hide("chartArea")
      shinyjs::show("chartAreaLoading")
      
      df_sample_filter <- sample_data(physeq1)[,category_column]
      
      df_sample_filter<-df_sample_filter[df_sample_filter[,category_column]==factor1 | df_sample_filter[,category_column]==factor2]
      
      
      new_physeq_obj<-phyloseq(otu_table(physeq1), tax_table(physeq1), sample_data(df_sample_filter))
      new_physeq_obj <- prune_samples(sample_sums(new_physeq_obj) > 500, new_physeq_obj)
      
      # print(sample_data(new_physeq_obj)[[category_column]])
      sample_data(new_physeq_obj)[[category_column]] <- factor(sample_data(new_physeq_obj)[[category_column]], levels=c(factor1,factor2))
      
      # levels(sample_data(new_physeq_obj)[,category_column])<-c(factor1,factor2)
      
      diagdds = phyloseq_to_deseq2(new_physeq_obj, as.formula(paste0("~", category_column)))
      
      # levels(colData(diagdds)[,category_column])<-c(factor1,factor2)
      
      # levels_category <- levels(colData(diagdds)[,category_column])
      
      # levels(colData(diagdds)[,category_column])
      
      # print(levels_category)
      # calculate geometric means prior to estimate size factors
      gm_mean = function(x, na.rm=TRUE){
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
      }
      geoMeans = apply(counts(diagdds), 1, gm_mean)
      diagdds = estimateSizeFactors(diagdds, geoMeans = geoMeans)
      diagdds = DESeq(diagdds, fitType="local")
      
      #investigating test result
      res = results(diagdds)
      
      res = res[order(res$padj, na.last=NA), ]
      alpha = 0.01
      sigtab = res[(res$padj < alpha), ]
      # print(sigtab)
      if(nrow(sigtab)>0){
        sigtab = cbind(as(sigtab, "data.frame"), as(tax_table(physeq1)[rownames(sigtab), ], "matrix"))
        
        sigtab<-sigtab[order(sigtab$log2FoldChange),]
        sigtab[,input$taxonLevel]<-factor(sigtab[,input$taxonLevel], levels=sigtab[,input$taxonLevel])
        # sigtab[,input$taxonLevel] <- factor(sigtab[,input$taxonLevel], levels = sigtab[,input$taxonLevel][order(sigtab$log2FoldChange)])
        # print(sigtab)
        
        output$datatableOutput<-renderDataTable(sigtab[,c("baseMean", "log2FoldChange", "pvalue", input$taxonLevel)],
                                                options = list(
                                                  order = list(list(1, 'desc'))
                                                ))
        
        chart<-ggplot(sigtab, aes_string(x="log2FoldChange", y=input$taxonLevel, color="Phylum")) +
          geom_segment(aes_string(x = 0, y = input$taxonLevel, xend = "log2FoldChange", yend = input$taxonLevel), color = "grey50") +
          geom_point(size = 7)

        shinyjs::hide("chartAreaLoading")
        shinyjs::show("chartArea")
      }
    }else{
      physeqobj <- physeq()
    }
    chart
  })
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
})
