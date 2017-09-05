
mbiome2phyloseq <- function(mstudy, taxon_level){
  
  selected_levels<-get_columns_taxonomy(taxon_level)
  OTU <- mstudy$otu_table$get_sample_as_column_by_otu()
  
  TAX<-OTU[,1:length(selected_levels), with=F]
  OTU <- OTU[,(length(selected_levels)+1):ncol(OTU), with=F]
  
  filtered_categories <- mstudy$get_filtered_categories()
  SAMPLE <- mstudy$get_metadata_as_column(filtered_categories)
  SAMPLE <- sample_data(SAMPLE)
  sample_names(SAMPLE)<-mstudy$get_sample_names()
  OTU = otu_table(OTU, taxa_are_rows = T)
  names(TAX)<-selected_levels
  TAX <- tax_table(as.matrix(TAX))
  
  phyloseq_obj<-phyloseq(OTU, TAX, SAMPLE)
  phyloseq_obj
}

# err <- function(...) cat(sprintf(...), sep='', file=stderr())

get_columns_taxonomy <- function(taxonomy_level=NULL){
  levels_taxa<-c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  if(!is.null(taxonomy_level)){
    levels_taxa<-switch(taxonomy_level,
                            "Phylum"=levels_taxa[1:2],
                            "Class"=levels_taxa[1:3],
                            "Order"=levels_taxa[1:4],
                            "Family"=levels_taxa[1:5],
                            "Genus"=levels_taxa[1:6],
                            "Species"=levels_taxa[1:7]
    )
  }
  levels_taxa
}

fix_taxonomy_data_table<-function(x){
  na_pos <- match("N/A", x)
  if(!is.na(na_pos)){
    new_value <- paste("unclassified", x[[na_pos-1]])
    x[x=="N/A"]<-new_value
  }
  return(x)
}
