# Declaring the packages
library(shiny)
library(ggplot2)
library(phyloseq)
library(data.table)
source("config.R")
source("../../lib/wdkDataset.R")
source("facet_even.R")
source("functions.R")

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
  ggplot_object <- NULL
  ggplot_build_object <- NULL
  
  ggplot_by_otu_object <- NULL
  ggplot_build_by_otu_object <- NULL
  
  ggplot_by_top_otu_object <- NULL
  ggplot_build_by_top_otu_object <- NULL
  
  abundance_otu <- NULL
  abundance_taxa <- NULL
  
  SAMPLE <- NULL
  
  hash_colors <- NULL
  
  # variables to define some plot parameters
  number_of_taxa <- 10
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
      for(i in 1:length(columns)){
        unique_factors <- as.factor(df_sample.formatted[[hash_sample_names[[columns[i]]]]])
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
                           options = list(placeholder = 'Choose metadata to split the chart'),
                           server = TRUE)
      
    }
    
    if(identical(input$taxonLevel, "Phylum")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 3
      column_tax <- 2
    }else if(identical(input$taxonLevel, "Class")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 4
      column_tax <- 3
    }else if(identical(input$taxonLevel, "Order")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 5
      column_tax <- 4
    }else if(identical(input$taxonLevel, "Family")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 6
      column_tax <- 5
    }else if(identical(input$taxonLevel, "Genus")){
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 7
      column_tax <- 6
    }else{
      df_abundance.formatted <- dcast(data = df_abundance,formula = Kingdom+Phylum+Class+Order+Family+Genus+Species~Sample,fun.aggregate = sum,value.var = "RelativeAbundance")
      column_otu <- 8
      column_tax <- 7
    }
    
    output$by_sample_datatable <- renderDataTable(NULL)
    abundance_otu <<- df_abundance.formatted[,column_otu:ncol(df_abundance.formatted)]
    
    highest_otu <- get_n_abundant_overall(number_of_taxa, abundance_otu)

    rows_to_maintain <- rownames(highest_otu)
    if(nrow(abundance_otu) > number_of_taxa){
      filtered_abundance_otu <- abundance_otu[rows_to_maintain,]
      remainder_abundance <- 1-colSums(filtered_abundance_otu)
      filtered_abundance_otu <- rbind(filtered_abundance_otu, remainder_abundance)
    }else{
      filtered_abundance_otu <- abundance_otu
    }
    
    OTU = otu_table(filtered_abundance_otu, taxa_are_rows = input$taxa_are_rows)
    
    abundance_taxa <<- df_abundance.formatted[,1:column_tax]
    abundance_taxa <<- fix_taxonomy_names(abundance_taxa)
    
    if(nrow(abundance_otu) > number_of_taxa){
      filtered_abundance_taxa <- abundance_taxa[rows_to_maintain,]
      filtered_abundance_taxa<-rbind(filtered_abundance_taxa, rep("Other", ncol(filtered_abundance_taxa)))
    }else{
      filtered_abundance_taxa<-abundance_taxa
    }
    TAX <- tax_table(as.matrix(filtered_abundance_taxa))
    
    filterOTUChoices <- sort(abundance_taxa[,input$taxonLevel])
    updateSelectizeInput(session, "filterOTU",
                         choices = c(filterOTUChoices),
                         selected = filterOTUChoices[1],
                         options = list(placeholder = 'Choose a OTU'),
                         server = TRUE)
    hash_colors<<-NULL
    merged_phyloseq <- phyloseq(OTU, TAX, SAMPLE)
    merged_phyloseq
  })
  
  runWilcoxon <- function(){
    physeqobj<-physeq()
    
    filtered_taxa <- abundance_taxa[abundance_taxa[,input$taxonLevel] == input$filterOTU,]
    filtered_otu <- abundance_otu[rownames(filtered_taxa),]
    if(nrow(filtered_otu)>0){
      abundance<-as.data.frame(t(filtered_otu))
      colnames(abundance)<-"Abundance"
      abundance$Sample<-rownames(abundance)
      
      metadata<-sample_data(physeqobj)[,c(hash_sample_names[[hash_count_samples[[input$category]]]])]
      metadata<-data.frame(Sample=rownames(metadata), metadata[,hash_sample_names[[hash_count_samples[[input$category]]]]])
      
      abundance_with_metadata<-merge(metadata, abundance, by="Sample")
      
      result<-wilcox.test(abundance_with_metadata[,"Abundance"]~abundance_with_metadata[,hash_sample_names[[hash_count_samples[[input$category]]]]], conf.int = T, conf.level = 0.95) 
      result
    }
  }
  
  runKruskal <- function(){
    physeqobj<-physeq()
    filtered_taxa <- abundance_taxa[abundance_taxa[,input$taxonLevel] == input$filterOTU,]
    filtered_otu <- abundance_otu[rownames(filtered_taxa),]
    if(nrow(filtered_otu)>0){
      abundance<-as.data.frame(t(filtered_otu))
      colnames(abundance)<-"Abundance"
      abundance$Sample<-rownames(abundance)
      
      metadata<-sample_data(physeqobj)[,c(hash_sample_names[[hash_count_samples[[input$category]]]])]
      metadata<-data.frame(Sample=rownames(metadata), metadata[,hash_sample_names[[hash_count_samples[[input$category]]]]])
      
      abundance_with_metadata<-merge(metadata, abundance, by="Sample")
      
      result<-kruskal.test(abundance_with_metadata[,hash_sample_names[[hash_count_samples[[input$category]]]]] ~ abundance_with_metadata[,"Abundance"])
      result
    }
  }
  
  output$result_tests <-renderUI({
    html_formatted<-""
    if(!identical(input$category, "") &!identical(input$category, "All Samples") ){
      if(identical(input$tabs, "byOTU") & !identical(input$filterOTU, "") ){
        quantity <- gsub("^(.+)\\s\\((\\d+)\\)$", "\\2", input$category, perl=T)
        quantity<-as.numeric(quantity)
        if(quantity==2){
          result<-runWilcoxon()
          html_formatted<-HTML(sprintf("<ul class=\"shell-body\"> <li>Wilcoxon rank sum test: W = %f, p-value = %.8f</li></ul>",
                                       result$statistic, result$p.value))
        }else{
          result<-runKruskal()
          html_formatted<-HTML(sprintf("<ul class=\"shell-body\"> <li>Kruskal-Wallis rank sum test: chi-squared = %f, df = %f, p-value = %.8f</li></ul>",
                                       result$statistic, result$parameter, result$p.value))
        }
      }
    }
    html_formatted
  })
  
  abundanceByOTU <- function(){}
  output$chartByOTU <- renderUI({
    if(identical(input$category, "")){
      output$by_otu_datatable <- renderDataTable(NULL)
      return(NULL)
    }
    
    physeqobj <- physeq()
    otu_picked <- input$filterOTU
    if(!identical(otu_picked, "")){
      quantity_samples <- length(sample_names(physeqobj))
        if(quantity_samples <= maximum_samples_without_resizing){
          plotOutput("singleOtuPlotWrapper", width = "100%", height = "500px", hover = hoverOpts("plot_hoverByOTU", delay = 60, delayType = "throttle" ))
        }else{
          plotOutput("singleOtuPlotWrapper", hover = hoverOpts("plot_hoverByOTU", delay = 60, delayType = "throttle"), width = "100%", height = quantity_samples*minimum_height_after_resizing)
        } 
    }
  })
  
  output$singleOtuPlotWrapper <- renderPlot({
    chart<-NULL
    if(identical(input$category, "")){
      output$by_otu_datatable <- renderDataTable(NULL)
      return(chart)
    }
    
    taxon_level <- input$taxonLevel
    otu_picked <- input$filterOTU
    if(!identical(otu_picked, "")){
      if(is.null(hash_colors)){
        cbPalette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a","#ffff99", "#b15928")
        random_color<-sample(cbPalette)
        cbPalette <- c(random_color[1], "#363636")
      }else{
        if(is.na(match(otu_picked, names(hash_colors)))){
          cbPalette <- c(hash_colors[["Other"]], "#363636")
        }else{
          cbPalette <- c(hash_colors[[otu_picked]], "#363636")
        }
      }
      line_match <- match(otu_picked, abundance_taxa[,taxon_level])
      if(!is.na(line_match)){
        filtered_taxa <- abundance_taxa[abundance_taxa[,taxon_level] == otu_picked,]
        filtered_otu <- abundance_otu[rownames(filtered_taxa),]
        
        if(identical(input$category, "All Samples")){
          shinyjs::hide("result_tests")
          raw_data<-as.data.frame(t(filtered_otu))
          raw_data$Sample<-rownames(raw_data)
          raw_data[,1]<-format(raw_data[,1], scientific = F)
          raw_data<-raw_data[c(2,1)]
          
          colnames(raw_data)<-c("Sample", paste(input$filterOTU, "Relative Abundance"))
          
          # returning the plot
          filtered_otu <- rbind(filtered_otu, 1-filtered_otu)
          filtered_taxa<-rbind(filtered_taxa, "zzzother")
          
          rownames(filtered_taxa)<-filtered_taxa[,input$taxonLevel]
          rownames(filtered_otu)<-filtered_taxa[,input$taxonLevel]
          
          phy <- phyloseq(otu_table(filtered_otu, taxa_are_rows=T), tax_table(as.matrix(filtered_taxa)), SAMPLE)
          output$by_otu_datatable <- renderDataTable(raw_data)
          
          chart<-plot_bar(phy,fill = taxon_level)+
            geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color="black")+
            theme(
              axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
              legend.text = element_text(family = "Palatino", size=14, face="bold", color="black"),
              legend.key.size = unit(x = 1.5, units = "char"),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              axis.text.y=element_blank(),
              axis.ticks.y = element_blank(),
              text = element_text(family = "Palatino", size=13, face="bold", color="black")
            )+
            scale_fill_manual(values=cbPalette, labels=c(otu_picked, "Other"), name="")+
            labs(x="Samples", y=paste(otu_picked,"Relative Abundance"))+
            coord_flip(expand=F)
        }else{
          shinyjs::show("result_tests")
          
          filtered_taxa<-abundance_taxa[abundance_taxa[,input$taxonLevel] == otu_picked,]
          filtered_otu<-abundance_otu[rownames(filtered_taxa),]
          
          rownames(filtered_otu)<-c(otu_picked)
          
          otu_for_plot <- as.data.frame(t(filtered_otu))
          otu_for_plot$SampleName <- rownames(otu_for_plot)
          
          df_sample_selected <- df_sample.formatted[,c("SampleName", hash_sample_names[[hash_count_samples[[input$category]]]])]
          
          data_merged <- merge(df_sample_selected, otu_for_plot, by.x = "SampleName", by.y = "SampleName")
          
          chart<-ggplot(data_merged, aes_string(x=hash_sample_names[[hash_count_samples[[input$category]]]], y=ggname(input$filterOTU)))+geom_boxplot()+
            theme(
              axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              text = element_text(family = "Palatino", size=13, face="bold", color="black"),
              panel.border = element_rect(colour="black", size=1, fill=NA),
              strip.text.x = element_text(family = "Palatino", size=13, face="bold", color="black"),
              strip.background = element_rect(fill="#F3F2F2")
            )+
            labs(x=stringi::stri_trans_totitle(hash_count_samples[[input$category]]), y=paste(input$filterOTU, "Relative Abundance"))
          
          colnames(data_merged)<-c("Sample", stringi::stri_trans_totitle(hash_count_samples[[input$category]]), "Relative Abundance")
          data_merged[,3]<-format(data_merged[,3], scientific = F)
          output$by_otu_datatable <- renderDataTable(data_merged)
        }
        ggplot_by_otu_object <<- chart
        ggplot_build_by_otu_object <<- ggplot_build(chart)
      }else{
        chart<-NULL
      }
    }else{
      chart<-NULL
    }
    chart
  })
  
  abundanceByTop <- function(){}
  output$chartByTopOTU <- renderUI({
    physeqobj <- physeq()
      if(!identical(input$category, "")){
        quantity_samples <- length(sample_names(physeqobj))
        plotOutput("multiOtuPlotWrapper", width = "100%", height = "500px",
                   dblclick = dblclickOpts("plot_dbclickByTopOTU"),
                   hover = hoverOpts("plot_hoverByTopOTU", delay = 60, delayType = "throttle"))
      }
  })
  
  output$multiOtuPlotWrapper <- renderPlot({
    chart<-NULL
    if(identical(input$category, "")){
      return(chart)
    }
    
    if(identical(input$category, "All Samples")){
      output$by_top_otu_datatable <- DT::renderDataTable(NULL)
      
      highest_otu <- get_n_abundant_overall(10, abundance_otu)
      rows_to_maintain <- rownames(highest_otu)
      
      filtered_abundance_otu <- abundance_otu[rows_to_maintain,]
      
      filtered_abundance_taxa <- abundance_taxa[rows_to_maintain,]
      
      abundances <- as.data.frame(t(filtered_abundance_otu))
      colnames(abundances)<-filtered_abundance_taxa[,input$taxonLevel]
      
      abundances$SampleName <- rownames(abundances)
      
      data_melted<-melt(abundances, id.vars=c("SampleName"))
      colnames(data_melted)<-c("SampleName", input$taxonLevel, "RelativeAbundance")
      
      chart<-ggplot(data_melted, aes_string(x=input$taxonLevel, y="RelativeAbundance"))+
        geom_boxplot()+
        theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
        text = element_text(family = "Palatino", size=13, face="bold", color="black")
        )+
        labs(x="OTU", y="Relative Abundance")+
        scale_y_continuous(limits = c(0, 1))+
        scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
      ggplot_by_top_otu_object <<- chart
      ggplot_build_by_top_otu_object <<- ggplot_build(chart)
    }else{
      highest_otu <- get_n_abundant_overall(10, abundance_otu)
      rows_to_maintain <- rownames(highest_otu)
      filtered_abundance_otu <- abundance_otu[rows_to_maintain,]
      filtered_abundance_taxa <- abundance_taxa[rows_to_maintain,]
      
      category_column<-hash_sample_names[[hash_count_samples[[input$category]]]]
      category_original<-hash_count_samples[[input$category]]
      sample_by_category <- subset(df_sample, Property==category_original, select=c("SampleName", "Value"))
      
      colnames(sample_by_category)[2]<-category_column
      
      abundances <- as.data.frame(t(filtered_abundance_otu))
      colnames(abundances)<-filtered_abundance_taxa[,input$taxonLevel]
      abundances$SampleName <- rownames(abundances)
      
      abundance_with_metadata<-join(abundances, sample_by_category, by="SampleName")
      
      data_melted<-melt(abundance_with_metadata, id.vars=c("SampleName", category_column))
      colnames(data_melted)<-c("SampleName", category_column, input$taxonLevel, "RelativeAbundance")
      
      
      unique_category <- unique(sample_by_category[,category_column])
      data_frame_table <- data.frame()
      
      if(length(unique_category) == 2){
        for(i in 1:nrow(filtered_abundance_taxa)){
          otu <- filtered_abundance_taxa[i,input$taxonLevel]
          cat1<-data_melted[data_melted[input$taxonLevel] == otu & data_melted[category_column] == unique_category[1], "RelativeAbundance"]
          cat2<-data_melted[data_melted[input$taxonLevel] == otu & data_melted[category_column] == unique_category[2], "RelativeAbundance"] 
          
          result<-wilcox.test(cat1,  cat2, conf.level = 0.95)
          df<-data.frame("a"=otu, "W"=result$statistic, "P-value"=result$p.value)
          data_frame_table<-rbind(data_frame_table, df)
        }
        colnames(data_frame_table)<-c(input$taxonLevel, "W", "P-Value")
        data_frame_table[,3]<-format(data_frame_table[,3], scientific = F)
        # print(data_frame_table)
        sketch <- tags$table(
          tags$thead(
            tags$tr(
              tags$th(style="text-align:center;", rowspan = 2, input$taxonLevel),
              tags$th(style="text-align:center;",colspan = 2, 'Wilcoxon rank sum test')
            ),
            tags$tr(
              tags$th(style="text-align:center;","W"),
              tags$th(style="text-align:center;","P-Value")
            )
            )
          )
        data_frame_table[,input$taxonLevel] <- paste0("<a class='link_table'onclick='goToOtuTab(\"",data_frame_table[,input$taxonLevel],"\")'>",data_frame_table[,input$taxonLevel],"</a>")
        output$by_top_otu_datatable <- DT::renderDataTable(data_frame_table, escape = F, selection = 'none',
                                                       container = sketch, rownames = FALSE)
      }else{
        for(i in 1:nrow(filtered_abundance_taxa)){
          otu <- filtered_abundance_taxa[i,input$taxonLevel]
          subset_data_melted <- data_melted[data_melted[input$taxonLevel] == otu,]
          
          result<-kruskal.test(subset_data_melted[,category_column] ~ subset_data_melted[,"RelativeAbundance"])
          df<-data.frame("a"=otu, "chi-squared"=unname(result$statistic), "df"=result$parameter, "P-value"=result$p.value)
          data_frame_table<-rbind(data_frame_table, df)
        }
        colnames(data_frame_table)<-c(input$taxonLevel, "chi-squared", "df", "P-Value")
        data_frame_table[,4]<-format(data_frame_table[,4], scientific = F)
        
        sketch <- tags$table(
          tags$thead(
            tags$tr(
              tags$th(style="text-align:center;", rowspan = 2, input$taxonLevel),
              tags$th(style="text-align:center;",colspan = 3, 'Kruskal-Wallis rank sum test')
            ),
            tags$tr(
              tags$th(style="text-align:center;","chi-squared"),
              tags$th(style="text-align:center;","df"),
              tags$th(style="text-align:center;","P-Value")
            )
          )
        )
        
        data_frame_table[,input$taxonLevel] <- paste0("<a class='link_table'onclick='goToOtuTab(\"",data_frame_table[,input$taxonLevel],"\")'>",data_frame_table[,input$taxonLevel],"</a>")
        output$by_top_otu_datatable <- DT::renderDataTable(data_frame_table, escape = F, selection = 'none',
                                                           container = sketch, rownames = FALSE)
      }
      
      chart<-ggplot(data_melted, aes_string(x=input$taxonLevel, y="RelativeAbundance", fill=category_column))+
        geom_boxplot()+
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
          text = element_text(family = "Palatino", size=13, face="bold", color="black")
        )+
        labs(x="OTU", y="Relative Abundance",fill=category_original)+
        scale_y_continuous(limits = c(0, 1))+
        scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
      
      ggplot_by_top_otu_object <<- chart
      ggplot_build_by_top_otu_object <<- ggplot_build(chart)
    }
    chart
  })
  

  abundanceChartF <- function(){}
  output$abundanceChart <- renderUI({
    if(!identical(input$category, "")){
      physeqobj <- physeq()
      quantity_samples <- length(sample_names(physeqobj))
      if(quantity_samples <= maximum_samples_without_resizing){
        plotOutput("plotWrapper",hover = hoverOpts("plot_hover", delay = 100, delayType = "throttle"),
                   click = clickOpts("plot_click"), dblclick = dblclickOpts("plot_dblclick"), width = "100%", height = "500px")
      }else{
        plotOutput("plotWrapper", hover = hoverOpts("plot_hover", delay = 60, delayType = "throttle"),
                   click = clickOpts("plot_click"), width = "100%", height = quantity_samples*minimum_height_after_resizing)
      }
    }else{
    }
  })
  
  output$plotWrapper <- renderPlot({
    physeqobj <- physeq()
    
    if(!identical(input$category, "")){
      cbPalette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a","#ffff99", "#b15928")
      if(identical(input$category, "All Samples")){
        chart <-
          plot_bar(physeqobj, fill=input$taxonLevel)+
          theme(
              axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
              legend.text = element_text(family = "Palatino", size=14, face="bold", color="black"),
              legend.key.size = unit(x = 1.5, units = "char"),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              axis.text.y=element_blank(),
              axis.ticks.y = element_blank(),
              text = element_text(family = "Palatino", size=13, face="bold", color="black")
                )+
          scale_fill_manual(values=cbPalette, name="")+
          labs(x="Samples", y="Phylogenetic Relative Abundance")+
          coord_flip(expand=F)
      }else{
				chart <-
				  plot_bar(physeqobj, fill=input$taxonLevel)+
				  facet_even(as.formula(paste("~ ",hash_sample_names[[hash_count_samples[[input$category]]]])), ncol=1, scales='free_y')+
				  theme(
    				    axis.title = element_text(family = "Palatino", color="black", face="bold", size=16),
    				    legend.text = element_text(family = "Palatino", size=14, face="bold", color="black"),
    				    legend.key.size = unit(x = 1.5, units = "char"),
    				    axis.text.x = element_text(angle = 0, hjust = 0.5),
    				    axis.text.y=element_blank(),
    				    axis.ticks.y = element_blank(),
    				    text = element_text(family = "Palatino", size=13, face="bold", color="black"),
				        panel.spacing = unit(0,"cm"),
				        panel.border = element_rect(colour="black", size=1, fill=NA),
				        strip.text.x = element_text(family = "Palatino", size=13, face="bold", color="black"),
				        strip.background = element_rect(fill="#F3F2F2")
				        )+
				  scale_fill_manual(values=cbPalette, name="")+
				  labs(x="Samples", y="Phylogenetic Relative Abundance")+
				  coord_flip(expand=F)
      }
      
      ggplot_object <<- chart
      ggplot_build_object <<- ggplot_build(chart)
      hash_colors <<- ggplot_build_object$plot$scales$scales[[1]]$palette.cache
      names(hash_colors) <<- ggplot_build_object$plot$scales$scales[[1]]$range$range
    }else{
      chart<-NULL
    }
    chart
  })

  hovers <- function(){}
  
  output$hoverByTopOTU <- renderUI({
    hover <- input$plot_hoverByTopOTU
    topotus<-ggplot_build_by_top_otu_object$layout$panel_ranges[[1]]$x.labels # x axis
    
    if (is.null(hover$x) || round(hover$y) < 0 || round(hover$x) < 1 || round(hover$x) > length(topotus))
      return(NULL)
    
    if(identical(input$category, "All Samples")){
      hover_otu <- topotus[round(hover$x)]
    
      line_data<-ggplot_build_by_top_otu_object$data[[1]][round(hover$x),]
      
      boxplot_hover <- get_single_boxplot_hover(hover, line_data$xmax, line_data$x, line_data$ymin, line_data$lower, line_data$middle, line_data$upper, line_data$ymax, hover_otu)
      
      HTML(boxplot_hover)
    }else{
      hover_otu <- topotus[round(hover$x)]
      group_colors <- ggplot_build_by_top_otu_object$plot$scales$scales[[3]]$palette.cache
      
      group_names <- ggplot_build_by_top_otu_object$plot$scales$scales[[3]]$range$range
      
      names(group_names)<-group_colors
      
      interval_value <- ggplot_build_by_top_otu_object$data[[1]][1,"xmax"]-ggplot_build_by_top_otu_object$data[[1]][1,"xmin"]
      
      line_data<-subset(ggplot_build_by_top_otu_object$data[[1]], hover$x>=xmin & hover$x<=xmax)
      
      if(nrow(line_data)==1){
        category_hover <- group_names[[line_data$fill]]
        text_hover <- paste0(hover_otu, " [ ", category_hover, " ]")
        boxplot_hover <- get_single_boxplot_hover(hover, line_data$xmax, line_data$x, line_data$ymin, line_data$lower, line_data$middle, line_data$upper, line_data$ymax, text_hover)
        
        HTML(boxplot_hover)
      }
    }
  })
  
  output$hoverByOTU <- renderUI({
    hover <- input$plot_hoverByOTU
    
    if(identical(input$category, "All Samples")){
      lvls <- rownames(sample_data(physeq()))
      if (is.null(hover$x) || round(hover$x) <0 || round(hover$y) > length(lvls))
        return(NULL)
      
      hover_sample <- lvls[round(hover$y)]
      
      hover_data<-subset(ggplot_by_otu_object$data, Sample==hover_sample)
      
      otu_abundance<-subset(hover_data, Kingdom!="zzzother")
      other_abundance<-subset(hover_data, Kingdom=="zzzother")
      
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
          "position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
          "left:",
          left_px + 18,
          "px; top:",
          top_px+2,
          "px;"
        )
      
      if(hover$x<=otu_abundance[1,"Abundance"]){
        wellPanel(style = style,
                  tags$b("Sample: "),
                  hover_sample,
                  br(),
                  tags$b(paste(input$taxonLevel, ": ")),
                  otu_abundance[1,input$taxonLevel],
                  br(),
                  tags$b("Abundance: "),
                  otu_abundance[1,"Abundance"]
        )
      }else{
        wellPanel(style = style,
                  tags$b("Sample: "),
                  hover_sample,
                  br(),
                  tags$b(paste(input$taxonLevel, ": ")),
                  "Other",
                  br(),
                  tags$b("Abundance: "),
                  other_abundance[1,"Abundance"]
        )
      }
    }else{
      categories<-ggplot_build_by_otu_object$layout$panel_ranges[[1]]$x.labels # x axis
      if (is.null(hover$x) || round(hover$y) < 0 || round(hover$x) < 1 || round(hover$x) > length(categories))
        return(NULL)
      line_data<-subset(ggplot_build_by_otu_object$data[[1]], hover$x>=xmin & hover$x<=xmax)
      if(nrow(line_data)>0){
        text_hover <- paste0(input$filterOTU, " [ ", categories[round(hover$x)], " ]")

        boxplot_hover <- get_single_boxplot_hover(hover, line_data$xmax, line_data$x, line_data$ymin, line_data$lower, line_data$middle, line_data$upper, line_data$ymax, text_hover)
        # boxplot_hover <- get_single_boxplot_hover(hover, line_data$xmax, line_data$x, line_data$ymin, line_data$lower, line_data$middle, line_data$upper, line_data$ymax)
        HTML(boxplot_hover)
      }else{
        return(NULL)
      }
    }
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    lvls <- rownames(sample_data(physeq()))
    if (is.null(hover$x) || round(hover$x) <0 || round(hover$y)<1 || round(hover$y) > length(lvls))
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
        "position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
        "left:",
        left_px + 2,
        "px; top:",
        top_px + 2,
        "px;"
      )
    
    
    if(identical(input$category, "All Samples")){
    	hover_sample <- lvls[round(hover$y)]
    	
    	hover_data<-subset(ggplot_object$data, Sample==hover_sample & Abundance>0)
    	
    	layer_data_hover<-subset(layer_data(ggplot_object), x==round(hover$y))
    	
    	unique_y<-unique(layer_data_hover$y)
    	
    	unique_y<-unique_y[unique_y>0]
    	
    	abundances_filtered <- get_abundances_from_plot(unique_y)
    	
    	abundances_joined <- join_abundance(abundances_filtered, hover_data)
    	
    	all_sum <- cumsum(abundances_joined$Abundance)
    	
    	index_abundance_hover = get_abundance_index(all_sum, hover$x)
    	
    	if(index_abundance_hover == -1)
    		      return(NULL)
    	wellPanel(style = style,
	              tags$b("Sample: "),
	      				hover_sample,
	      				br(),
	      				tags$b(paste(input$taxonLevel, ": ")),
	      				abundances_joined[index_abundance_hover,input$taxonLevel],
	      				br(),
	      				tags$b("Abundance: "),
	      				abundances_joined[index_abundance_hover,"Abundance"]
	    			  )
    }else{
    	pnl_layout <- ggplot_build_object$layout$panel_layout
    	panel_index <- pnl_layout[ pnl_layout[[hash_sample_names[[hash_count_samples[[input$category]]]]]] == hover$panelvar1 , ]$PANEL
    	
    	if(length(panel_index) > 0){
    	  lvls <- ggplot_build_object$layout$panel_ranges[[panel_index]]$y.labels
    	  hover_sample <- lvls[round(hover$y)]
    	  if(!is.na(hover_sample)){
    	    hover_data<-subset(ggplot_object$data, Sample==hover_sample & Abundance>0)
    	    
    	    layer_data_hover<-subset(layer_data(ggplot_object), x==round(hover$y) & PANEL==panel_index)
    	    
    	    unique_y<-unique(layer_data_hover$y)
    	    unique_y<-unique_y[unique_y>0]
    	    abundances_filtered <- get_abundances_from_plot(unique_y)

    	    abundances_joined <- join_abundance(abundances_filtered, hover_data)
    	    all_sum <- cumsum(abundances_joined$Abundance)
    	    
    	    index_abundance_hover = get_abundance_index(all_sum, hover$x)
    	    
    	    if(index_abundance_hover == -1)
    	      return(NULL)
    	    
    	    wellPanel(style = style,
    	              tags$b("Sample: "),
    	              hover_sample,
    	              br(),
    	              tags$b("Category: "),
    	              hover$panelvar1,
    	              br(),
    	              tags$b(paste(input$taxonLevel, ": ")),
    	              abundances_joined[index_abundance_hover,input$taxonLevel],
    	              br(),
    	              tags$b("Abundance: "),
    	              abundances_joined[index_abundance_hover,"Abundance"]
    	    )
    	  }else{
    	    return(NULL)
    	  }
    	}else{
    	  return(NULL)
    	}
    }
  })
  
  clicks <- function(){}
  
  observeEvent(input$plot_dblclick, {
    click <- input$plot_dblclick
    hover <- click
    lvls <- rownames(sample_data(physeq()))
    if (is.null(click$y))
      return(NULL)
    
    sample<-""
    otu<-""
    
    if(identical(input$category, "All Samples")){
      hover_sample <- lvls[round(hover$y)]
      hover_data<-subset(ggplot_object$data, Sample==hover_sample & Abundance>0)
      
      layer_data_hover<-subset(layer_data(ggplot_object), x==round(hover$y))
      
      unique_y<-unique(layer_data_hover$y)
      
      abundances_filtered <- get_abundances_from_plot(unique_y)
      
      abundances_joined <- join_abundance(abundances_filtered, hover_data)
      all_sum <- cumsum(abundances_joined$Abundance)
      index_abundance_hover = get_abundance_index(all_sum, hover$x)
      
      if(index_abundance_hover == -1)
        return(NULL)
      
      sample<-hover_sample
      otu<-abundances_joined[index_abundance_hover,input$taxonLevel]
    }else{
      pnl_layout <- ggplot_build_object$layout$panel_layout
      panel_index <- pnl_layout[ pnl_layout[[hash_sample_names[[hash_count_samples[[input$category]]]]]] == hover$panelvar1 , ]$PANEL
      
      if(length(panel_index) > 0){
        lvls <- ggplot_build_object$layout$panel_ranges[[panel_index]]$y.labels
        hover_sample <- lvls[round(hover$y)]
        if(!is.na(hover_sample)){
          hover_data<-subset(ggplot_object$data, Sample==hover_sample & Abundance>0)
          layer_data_hover<-subset(layer_data(ggplot_object), x==round(hover$y) & PANEL==panel_index)
          unique_y<-unique(layer_data_hover$y)
          unique_y<-unique_y[unique_y>0]
          abundances_filtered <- get_abundances_from_plot(unique_y)
          
          abundances_joined <- join_abundance(abundances_filtered, hover_data)
          all_sum <- cumsum(abundances_joined$Abundance)
          
          index_abundance_hover = get_abundance_index(all_sum, hover$x)
          
          if(index_abundance_hover == -1)
            return(NULL)
          
          sample<-hover_sample
          otu<-abundances_joined[index_abundance_hover,input$taxonLevel]
          abundances_joined_fill <<- abundances_joined
        }else{
          return(NULL)
        }
      }else{
        return(NULL)
      }
    }
    
    filterOTUChoices <- sort(abundance_taxa[,input$taxonLevel])
    updateSelectizeInput(session, "filterOTU",
                         choices = c(filterOTUChoices),
                         selected = otu,
                         options = list(placeholder = 'Choose a OTU'),
                         server = TRUE)
    updateTabsetPanel(session, "tabs", selected = "byOTU")
    
    return(NULL)
  })
  
  observeEvent(input$plot_click, {
    click <- input$plot_click
    
    if (is.null(click$y))
      return(NULL)
    lvls <- rownames(sample_data(physeq()))
    
    if(identical(input$category, "All Samples")){
    	sample <- lvls[round(click$y)]
    }else{
    	pnl_layout <- ggplot_build_object$layout$panel_layout
    	panel_index <- pnl_layout[ pnl_layout[[hash_sample_names[[hash_count_samples[[input$category]]]]]] == click$panelvar1 , ]$PANEL
    	lvls <- ggplot_build_object$layout$panel_ranges[[panel_index]]$y.labels
    	
    	sample <- lvls[round(click$y)]
    }
    raw_data<-data.frame(rep(sample, nrow(abundance_taxa)), abundance_taxa[,input$taxonLevel],"Abundance"=abundance_otu[,sample])
    
    raw_data<-subset(raw_data, Abundance>0)
    raw_data$Abundance<-format(raw_data$Abundance, scientific = F)
    colnames(raw_data)<-c("Sample",input$taxonLevel, "Relative Abundance")
    raw_data[,input$taxonLevel] <- paste0("<a class='link_table' onclick='goToOtuTab(\"",raw_data[,input$taxonLevel],"\")'>",raw_data[,input$taxonLevel],"</a>")
    
    output$by_sample_datatable <- renderDataTable(raw_data,
                                                  escape = F,
                                            options = list(
                                              order = list(list(2, 'desc'),list(1, 'asc'))
                                              )
                                            )
    # print("pukeeeeee num funciona")
    # js$shinyScrollTo("div_sample_datatable")
  })
  
  observeEvent(input$plot_dbclickByTopOTU, {
    click <- input$plot_dbclickByTopOTU
    topotus<-ggplot_build_by_top_otu_object$layout$panel_ranges[[1]]$x.labels # x axis
    if (is.null(click$y))
      return(NULL)
    
    filterOTUChoices <- sort(abundance_taxa[,input$taxonLevel])
    updateSelectizeInput(session, "filterOTU",
                         choices = c(filterOTUChoices),
                         selected = topotus[round(click$x)],
                         options = list(placeholder = 'Choose a OTU'),
                         server = TRUE)
    updateTabsetPanel(session, "tabs", selected = "byOTU")
    
  })
  
  downloads <- function(){}
  
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
      postscript(file, width=16,height=10.67, family = "Palatino")
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
