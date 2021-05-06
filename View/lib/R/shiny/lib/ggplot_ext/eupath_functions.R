fix_taxonomy_names <- function(taxonomy_df){
  for(i in 1:nrow(taxonomy_df)){
    if(!identical(taxonomy_df[i,1], "N/A")){
      last_defined_taxonomy <- taxonomy_df[i,1]
      last_defined_taxonomy_rank <- colnames(taxonomy_df)[1]
      for(j in 2:ncol(taxonomy_df[i,])){
        if(identical(taxonomy_df[i,j], "N/A")){
          taxonomy_df[i,j] <- paste0("unclassified, ", last_defined_taxonomy_rank, ": ", last_defined_taxonomy)
        }else{
          last_defined_taxonomy <- taxonomy_df[i,j]
	  last_defined_taxonomy_rank <- colnames(taxonomy_df)[j]
        }
      }
    } 
  }
  
  #patch for unique species
  if ('Species' %in% names(taxonomy_df)) {
    taxonomy_df$Species <- make.unique(taxonomy_df$Species)
  } else if ('Genus' %in% names(taxonomy_df)) {
    taxonomy_df$Genus <- make.unique(taxonomy_df$Genus)
  }

  taxonomy_df
}

group_numerical_dt <- function(dt, col_group, col_count="COUNT", by=NULL, n_keep=10){
  dt_final<-copy(dt)
  dt_final[,initial:=get(col_group)]
  dt_final[,final:=get(col_group)]
  
  maximum<-sum(dt_final[,get(col_count)])+1
  rows_to_delete<-c()
  i <- 1
  n_r<-nrow(dt_final)
  keep_rows<-nrow(dt_final)
  while(keep_rows>n_keep){
    minimum<-min(dt_final[,get(col_count)])
    row_idx<-which(dt_final[,get(col_count)]==minimum)[1]
    
    if(row_idx>1 & row_idx<n_r){
      if(dt_final[row_idx-1,get(col_count)] == maximum){
        left_vector<-dt_final[,get(col_count)][1:(row_idx-1)]
        minimum_left<-which(left_vector!=maximum)

        if(!is.na(minimum_right)){
          minimum_left<-minimum_left[length(minimum_left)]
        }else{
          minimum_left<-1
        }
      }else{
        minimum_left<-row_idx-1
      }

      value_left<-dt_final[,get(col_count)][minimum_left]

      if(dt_final[row_idx+1,get(col_count)] == maximum){
        right_vector<-dt_final[,get(col_count)][(row_idx+1):n_r]
        minimum_right<-which(right_vector!=maximum)[1]
        if(!is.na(minimum_right)){
          minimum_right<-row_idx+minimum_right
        }else{
          minimum_right<-n_r
        }
      }else{
        minimum_right<-row_idx+1
      }

      value_right<-dt_final[,get(col_count)][minimum_right]

      if(value_left<value_right){
        dt_final[row_idx,col_count]<-dt_final[c(minimum_left,row_idx),sum(get(col_count))]
        dt_final[row_idx,]$initial<-dt_final[minimum_left,]$initial
        
        rows_to_delete[i]<-minimum_left
        dt_final[minimum_left,col_count]<-maximum
        # dt_final[minimum_left,"initial"]<-dt_final[row_idx,"initial"]
        # dt_final[minimum_left,"final"]<-dt_final[row_idx,"final"]
      }else{
        dt_final[row_idx,col_count]<-dt_final[c(row_idx,minimum_right),sum(get(col_count))]
        dt_final[row_idx,]$final<-dt_final[minimum_right,]$final
        rows_to_delete[i]<-minimum_right
        dt_final[minimum_right,col_count]<-maximum
        # dt_final[minimum_right,"initial"]<-dt_final[row_idx,"initial"]
        # dt_final[minimum_right,"final"]<-dt_final[row_idx,"final"]
      }
    }else{ # end if(row_idx>1 & row_idx<n_r)
      if(row_idx==1){
        if(dt_final[2,get(col_count)]==maximum){
          right_vector<-dt_final[,get(col_count)][3:n_r]
          minimum_right<-which(right_vector!=maximum)[1]+2
        }else{
          minimum_right<-2
        }
        
        dt_final[minimum_right,col_count]<-dt_final[c(1,minimum_right),sum(get(col_count))]
        dt_final[minimum_right,]$initial<-dt_final[1,]$initial
        
        rows_to_delete[i]<-1
        dt_final[1,col_count]<-maximum 
        # dt_final[1,"initial"]<-dt_final[minimum_right,"initial"]
        # dt_final[1,"final"]<-dt_final[minimum_right,"final"]
        
      }else{
        if(dt_final[n_r-1,get(col_count)]==maximum){
          left_vector<-dt_final[,get(col_count)][1:(n_r-2)]
          minimum_left<-which(left_vector!=maximum)
          minimum_left<-minimum_left[length(minimum_left)]
        }else{
          minimum_left<-n_r-1
        }
        
        dt_final[minimum_left,col_count]<-dt_final[c(minimum_left,n_r),sum(get(col_count))]
        dt_final[minimum_left,]$final<-dt_final[n_r,]$final
        
        dt_final[n_r,col_count]<-maximum
        # dt_final[n_r,"initial"]<-dt_final[minimum_left,"initial"]
        # dt_final[n_r,"final"]<-dt_final[minimum_left,"final"]
        
        
        rows_to_delete[i]<-n_r
      }
      
    }
    i<-i+1
    keep_rows<-keep_rows-1
  }
  # dt_final[-(rows_to_delete),]
  sub_maximum<-subset(dt_final, get(col_count)==maximum)
  sub_others<-subset(dt_final, get(col_count)<maximum)
  for(i in 1:nrow(sub_maximum)){
    corrected_gr<-subset(sub_others, sub_maximum[i,get(col_group)]>=initial & sub_maximum[i,get(col_group)]<=final)
    sub_maximum[i,"initial"]<-corrected_gr[,initial]
    sub_maximum[i,"final"]<-corrected_gr[,final]
    sub_maximum[i,col_count]<-0
  }
  
  # dt_final[,(col_count):=ifelse(get(col_count)==maximum, 0, get(col_count))]
  rbindlist(list(sub_others,sub_maximum))
}

# DESeq2 helper functions - straight from DESeq2
getModelMatrix <- function(object) {
  if (is(design(object), "matrix")) {
    design(object)
  } else if (is(design(object), "formula")) {
    stats::model.matrix.default(design(object), data=as.data.frame(colData(object)))
  }
}
