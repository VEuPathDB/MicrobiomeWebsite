fix_taxonomy_names <- function(taxonomy_df){
  
  for(i in 1:nrow(taxonomy_df)){
    if(!identical(taxonomy_df[i,1], "N/A")){
      last_defined_taxonomy <- taxonomy_df[i,1]
      for(j in 2:ncol(taxonomy_df[i,])){
        if(identical(taxonomy_df[i,j], "N/A")){
          taxonomy_df[i,j] <- paste("unclassified", last_defined_taxonomy)
        }else{
          last_defined_taxonomy <- taxonomy_df[i,j]
        }
      }
    } 
  }
  taxonomy_df
}