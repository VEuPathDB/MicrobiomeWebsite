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
  
  output$chartByOTU <- renderUI({
    physeqobj <- physeq()
    otu_picked <- input$filterOTU
    if(identical(otu_picked, "")){
      
    }else{
      quantity_samples <- length(sample_names(physeqobj))
      if(quantity_samples <= maximum_samples_without_resizing){
        plotOutput("otuPlotWrapper", width = "100%", height = "500px", hover = hoverOpts("plot_hoverByOTU"))
      }else{
        plotOutput("otuPlotWrapper", hover = hoverOpts("plot_hoverByOTU"), width = "100%", height = quantity_samples*minimum_height_after_resizing)
      }
    }
  })
  
  output$btnDownloadPNG <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      png(file, width=1200,height=800,units="px")
      if(identical(input$tabs, "bySample")){
        print(ggplot_object)
      }else{
        print(ggplot_by_otu_object)
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
      }else{
        print(ggplot_by_otu_object)
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
      }else{
        print(ggplot_by_otu_object)
      }
      
      dev.off()
    }
  )
  
  output$btnDownloadCSV <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      if(identical(input$tabs, "bySample")){
        write.csv(ggplot_object$data, file)
      }else{
        write.csv(ggplot_by_otu_object$data, file)
      }
    }
  )
  
  # observeEvent(input$tabs, {
  #   if(identical(input$tabs, "bySample")){
  #     output$by_otu_datatable <- renderDataTable(NULL)
  #     # output$chartByOTU<-renderPlot(NULL)
  #   }
  # })
  
  # observeEvent(input$btnRunAnalysis, {
  #   if(identical(input$tabs, "byOTU")){
  #     if(!identical(input$category, "All Samples")){
  #       quantity <- gsub("^(.+)\\s\\((\\d+)\\)$", "\\2", input$category, perl=T)
  #       quantity<-as.numeric(quantity)
  #       if(quantity==2){
  #         print("run wilcoxon")
  #       }else{
  #         print("run kruskal-wallis")
  #       }
  #     }
  #   }else{
  #     
  #   }
  # }) 
  
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
  
  output$otuPlotWrapper <- renderPlot({
    taxon_level <- input$taxonLevel
    otu_picked <- input$filterOTU
    if(!identical(otu_picked, "")){
      if(is.null(hash_colors)){
        cbPalette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a","#ffff99", "#b15928")
        random_color<-sample(cbPalette)
        cbPalette <- c(random_color[1], "#363636")
        # cbPalette <- c(random_color[1], "white")
      }else{
        if(is.na(match(otu_picked, names(hash_colors)))){
          cbPalette <- c(hash_colors[["Other"]], "#363636")
          # cbPalette <- c(hash_colors[["Other"]], "white")
        }else{
          cbPalette <- c(hash_colors[[otu_picked]], "#363636")
          # cbPalette <- c(hash_colors[[otu_picked]], "white")
        }
      }
      line_match <- match(otu_picked, abundance_taxa[,taxon_level])
      if(!is.na(line_match)){
        # filling the table
        
        # filtered_taxa<-abundance_taxa[abundance_taxa[,taxon_level] == otu_picked,]
        # 
        # filtered_otu<-abundance_otu[rownames(filtered_taxa),]
        # 
        # filtered_otu<-rbind(filtered_otu, 1-filtered_otu)
        # rownames(filtered_otu)<-c(otu_picked, "RemainderOTU")
        # 
        # otu_for_plot <- as.data.frame(t(filtered_otu))
        # otu_for_plot$Sample <- rownames(otu_for_plot)
        # otu_for_plot<-melt(otu_for_plot, id.vars=c("Sample"), variable.name=taxon_level, value.name="Abundance")
        # 
        # otu_for_plot[,taxon_level]<-factor(otu_for_plot[,taxon_level], levels=c("RemainderOTU", otu_picked))
        # 
        # chart<-ggplot(otu_for_plot, aes_string(x="Sample", y="Abundance", fill=taxon_level))+
        #   geom_bar(stat = "identity")+
        #   theme(
        #     axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14),
        #     axis.text.y=element_blank(),
        #     axis.ticks.y = element_blank()
        #   )+
        #   scale_fill_manual(values=cbPalette, labels=c(otu_picked, "Remainder OTUs"), breaks=rev(levels(otu_for_plot[,taxon_level])) )+
        #   labs(x="Samples", y=paste(otu_picked,"Abundance"))+
        #   coord_flip(expand=F)
        
        filtered_taxa <- abundance_taxa[abundance_taxa[,taxon_level] == otu_picked,]
        filtered_otu <- abundance_otu[rownames(filtered_taxa),]

        raw_data<-as.data.frame(t(filtered_otu))
        raw_data$Sample<-rownames(raw_data)
        raw_data[,1]<-format(raw_data[,1], scientific = F)
        raw_data<-raw_data[c(2,1)]

        colnames(raw_data)<-c("Sample", "Relative Abundance")
        output$by_otu_datatable <- renderDataTable(raw_data)

        # returning the plot
        filtered_otu <- rbind(filtered_otu, 1-filtered_otu)
        filtered_taxa<-rbind(filtered_taxa, "zzzother")

        rownames(filtered_taxa)<-filtered_taxa[,input$taxonLevel]
        rownames(filtered_otu)<-filtered_taxa[,input$taxonLevel]
        
        phy <- phyloseq(otu_table(filtered_otu, taxa_are_rows=T), tax_table(as.matrix(filtered_taxa)), SAMPLE)
        if(identical(input$category, "All Samples")){
          shinyjs::hide("result_tests")
          # my_title<-sprintf("Relative abundance of %s %s for each sample", otu_picked, tolower(input$taxonLevel))
          chart<-plot_bar(phy,fill = taxon_level)+
            geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color="black")+
            theme(
              # plot.title = element_text(family = "Trebuchet MS", color="#666666", face = "bold", size=20, hjust = 0.5),
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
            # ggtitle(as.formula(paste("~ underline(\"", my_title, "\")"))) +
            coord_flip(expand=F)
        }else{
          shinyjs::show("result_tests")
          # my_title<-sprintf("Relative abundance of %s %s for each sample under %s", otu_picked, tolower(input$taxonLevel) ,hash_count_samples[[input$category]])
          # if(nchar(my_title)<90){
          #   hj <- 0.5
          #   s<-20
          # }else{
          #   hj <- 0
          #   s<-18
          # }
          chart<-plot_bar(phy, fill = taxon_level)+
            geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color="black")+
            facet_even(as.formula(paste("~ ",hash_sample_names[[hash_count_samples[[input$category]]]])), ncol=1, scales='free_y')+
            theme(
              # plot.title = element_text(family = "Trebuchet MS", color="#666666", face = "bold", size=s, hjust = hj),
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
            scale_fill_manual(values=cbPalette, labels=c(otu_picked, "Other"), name="")+
            labs(x="Samples", y=paste(otu_picked,"Relative Abundance"))+
            # ggtitle(as.formula(paste("~ underline(\"", my_title, "\")"))) +
            coord_flip(expand=F)
          
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
  
  output$abundanceChart <- renderUI({
    if(!identical(input$category, "")){
      physeqobj <- physeq()
      quantity_samples <- length(sample_names(physeqobj))
      if(quantity_samples <= maximum_samples_without_resizing){
        plotOutput("plotWrapper",hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                   click = clickOpts("plot_click"), dblclick = dblclickOpts("plot_dblclick"), width = "100%", height = "500px")
      }else{
        plotOutput("plotWrapper", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                   click = clickOpts("plot_click"), width = "100%", height = quantity_samples*minimum_height_after_resizing)
      }
    }else{
    }
  })
  
  output$temporaryImage <- renderImage({
    
    filename <- normalizePath(paste0(getwd(), '/loading.gif'))
    list(src = filename,contentType = "image/gif", width = "200", heigth="200",
         alt = paste("blabla"))
  }, deleteFile = F)
  
  output$plotWrapper <- renderPlot({
    physeqobj <- physeq()
    
    if(!identical(input$category, "")){
      cbPalette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a","#ffff99", "#b15928")
      if(identical(input$category, "All Samples")){
        # my_title<-sprintf("Relative abundance at %s level for each sample", tolower(input$taxonLevel))
        chart <-
          plot_bar(physeqobj, fill=input$taxonLevel)+
          theme(
                # plot.title = element_text(family = "Trebuchet MS", color="#666666", face = "bold", size=20, hjust = 0.5),
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
          # ggtitle(as.formula(paste("~ underline(\"", my_title, "\")"))) +
          coord_flip(expand=F)
      }else{
        # my_title<-sprintf("Relative abundance at %s level for each sample under %s", tolower(input$taxonLevel), hash_count_samples[[input$category]])
        # if(nchar(my_title)<90){
        #   hj <- 0.5
        # }else{
        #   hj <- 0.1
        # }
				chart <-
				  plot_bar(physeqobj, fill=input$taxonLevel)+
				  facet_even(as.formula(paste("~ ",hash_sample_names[[hash_count_samples[[input$category]]]])), ncol=1, scales='free_y')+
				  theme(
				        # plot.title = element_text(family = "Trebuchet MS", color="#666666", face = "bold", size=20, hjust = hj),
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
				  # ggtitle(as.formula(paste("~ underline(\"", my_title, "\")"))) +
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
  
  # wrapper <- function(x, ...) 
  # {
  #   paste(strwrap(x, ...), collapse = "\n")
  # }
  output$hoverByOTU <- renderUI({
    hover <- input$plot_hoverByOTU
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
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
        "left:",
        left_px + 2,
        "px; top:",
        top_px + 2,
        "px;"
      )
    if(identical(input$category, "All Samples")){
      
      hover_sample <- lvls[round(hover$y)]
      
      hover_data<-subset(ggplot_by_otu_object$data, Sample==hover_sample)
      
      otu_abundance<-subset(hover_data, Kingdom!="zzzother")
      other_abundance<-subset(hover_data, Kingdom=="zzzother")
      
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
      pnl_layout <- ggplot_build_by_otu_object$layout$panel_layout
      
      panel_index <- pnl_layout[ pnl_layout[[hash_sample_names[[hash_count_samples[[input$category]]]]]] == hover$panelvar1 , ]$PANEL
      if(length(panel_index) > 0){
        lvls <- ggplot_build_by_otu_object$layout$panel_ranges[[panel_index]]$y.labels
        hover_sample <- lvls[round(hover$y)]
        hover_data<-subset(ggplot_by_otu_object$data, Sample==hover_sample & Abundance>0)
        otu_abundance<-subset(hover_data, Kingdom!="zzzother")
        other_abundance<-subset(hover_data, Kingdom=="zzzother")
        if(nrow(otu_abundance)>0){
          if(hover$x <= otu_abundance[1,"Abundance"]){
            wellPanel(style = style,
                      tags$b("Sample: "),
                      hover_sample,
                      br(),
                      tags$b("Category: "),
                      hover$panelvar1,
                      br(),
                      tags$b(paste(input$taxonLevel, ": ")),
                      otu_abundance[1,input$taxonLevel],
                      br(),
                      tags$b("Abundance: "),
                      otu_abundance[1, "Abundance"]
            )
          }else{
            wellPanel(style = style,
                      tags$b("Sample: "),
                      hover_sample,
                      br(),
                      tags$b("Category: "),
                      hover$panelvar1,
                      br(),
                      tags$b(paste(input$taxonLevel, ": ")),
                      "Other",
                      br(),
                      tags$b("Abundance: "),
                      other_abundance[1, "Abundance"]
            )
          }
        }else{
          wellPanel(style = style,
                    tags$b("Sample: "),
                    hover_sample,
                    br(),
                    tags$b("Category: "),
                    hover$panelvar1,
                    br(),
                    tags$b(paste(input$taxonLevel, ": ")),
                    "Other",
                    br(),
                    tags$b("Abundance: "),
                    1
          ) 
        }
        
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
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
        "left:",
        left_px + 2,
        "px; top:",
        top_px + 2,
        "px;"
      )
    
    
    if(identical(input$category, "All Samples")){
    	hover_sample <- lvls[round(hover$y)]
    	# hover_data: OTU  Sample  Abundance  SampleName MetaColumns(1..n)  Kingdom  Phylum ...
    	hover_data<-subset(ggplot_object$data, Sample==hover_sample & Abundance>0)
    	
    	layer_data_hover<-subset(layer_data(ggplot_object), x==round(hover$y))
    	# layer_data_hover<-cbind(layer_data_hover, diff=layer_data_hover$ymax-layer_data_hover$ymin)
    	unique_y<-unique(layer_data_hover$y)
    	
    	unique_y<-unique_y[unique_y>0]
    	
    	# layer_data_hover <- subset(layer_data_hover, diff>0, select=c("fill", "diff"))
    	
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
    	    # layer_data_hover<-cbind(layer_data_hover, diff=layer_data_hover$ymax-layer_data_hover$ymin)
    	    unique_y<-unique(layer_data_hover$y)
    	    unique_y<-unique_y[unique_y>0]
    	    abundances_filtered <- get_abundances_from_plot(unique_y)
    	    # layer_data_hover <- subset(layer_data_hover, diff>0, select=c("fill", "diff"))
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
      # layer_data_hover<-cbind(layer_data_hover, diff=layer_data_hover$ymax-layer_data_hover$ymin)
      unique_y<-unique(layer_data_hover$y)
      
      abundances_filtered <- get_abundances_from_plot(unique_y)
      # layer_data_hover <- subset(layer_data_hover, diff>0, select=c("fill", "diff"))
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
          # layer_data_hover <- subset(layer_data_hover, diff>0, select=c("fill", "diff"))
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
  })
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
  shinyjs::show("app-content")
})
