
teste<- function(){
  bla<-data.table()
  bla
}

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

# pallete.eupath<-function(){
#   custon_pallete <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a","#ffff99", "#b15928")
#   custon_pallete
# }

fix_taxonomy_data_table<-function(x){
  na_pos <- match("N/A", x)
  if(!is.na(na_pos)){
    new_value <- paste("unclassified", x[[na_pos-1]])
    x[x=="N/A"]<-new_value
  }
  return(x)
}
