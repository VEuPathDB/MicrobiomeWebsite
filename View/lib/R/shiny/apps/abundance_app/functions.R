get_abundances_from_plot <- function(array_abundance){
  array_real_abundance <- 0
  array_real_abundance[1] <- array_abundance[1]
  if(length(array_abundance) > 1){
    for (i in 2:length(array_abundance)) {
      diff_abundance <- array_abundance[i]-array_abundance[i-1]
      if(diff_abundance > 0){
        array_real_abundance[i] <- array_abundance[i]-array_abundance[i-1]
      }else{
        break
      }
    }
  }
  array_real_abundance
}
join_abundance <- function(array_abundance, data_from_chart){
  df_to_return <- data.frame()
  for(i in 1:length(array_abundance)){
    abi <- array_abundance[i]
    line_to_remove <- 0
    for(j in 1:nrow(data_from_chart)){
      abj <- data_from_chart[["Abundance"]][j]
      if(isTRUE(all.equal.numeric(abi, abj))){
        df_to_return <- rbind(df_to_return, data_from_chart[j,])
        line_to_remove = j
        break
      }
    }
    if(line_to_remove > 0){
      data_from_chart <- data_from_chart[-c(line_to_remove), ]
    }
  }
  df_to_return
}
get_abundance_index <- function(array_search, abundance_hover){
  index_found = 0
  if(abundance_hover < 0)
    return(-1)
  for (i in 1:length(array_search)) {
    if(i == 1){
      if(abundance_hover <= array_search[1]){
        index_found=1
        break
      }
    }else if(i < length(array_search)){
      if(abundance_hover > array_search[i-1] & abundance_hover <= array_search[i]){
        index_found=i
        break
      }
    }else{
      if(abundance_hover <= array_search[length(array_search)]){
        index_found=length(array_search)
      }else{
        index_found=-1;
      }
    }
  }
  index_found
}

# get_single_boxplot_hover <- function(hover, xmax, x, ymin, lower, middle, upper, ymax, text=NULL){
#   if(hover$x < hover$domain$right-2){
#     left_pct <-
#       (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
#     
#     left_px <-
#       hover$range$left + left_pct * (hover$range$right - hover$range$left)
#     style_line <-
#       paste0(
#         "left: ", left_px+20, "px; top: %fpx"
#       )
#   }else{ # if it's to show the right tooltip
#     left_pct <-
#       (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
# 
#     left_px <-
#       hover$range$left + left_pct * (hover$range$right - hover$range$left)
#     style_line <-
#       paste0(
#         "left: ", left_px-170, "px; top: %fpx"
#       )
#     # right_pct <-
#     #   (hover$domain$right-x) / (hover$domain$right - hover$domain$left)
#     # print(hover$domain$right)
#     # right_px <-
#     #   hover$range$left + right_pct * (hover$range$right - hover$range$left)
#     # style_line <-
#     #   paste0(
#     #     "right: ", right_px-10, "px; top: %fpx"
#     #   )
#   }
#   
#   # left_pct <-
#   #   (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
#   # 
#   # left_px <-
#   #   hover$range$left + left_pct * (hover$range$right - hover$range$left)
#   # style_line <-
#   #   paste0(
#   #     "left: ", left_px-170, "px; top: %fpx"
#   #   )
#   
#   positions_from_top <- c(
#     hover$range$top + (hover$domain$top - ymin) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
#     hover$range$top + (hover$domain$top - lower) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
#     hover$range$top + (hover$domain$top - middle) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
#     hover$range$top + (hover$domain$top - upper) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
#     hover$range$top + (hover$domain$top - ymax) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top)
#   )
#   
#   values_box<-c(ymin, lower, middle, upper, ymax)
#   
#   logical_box<-c(1,1,1,1,1)
#   text_box<-c("Min","25th Percentile","Median","75th Percentile","Max")
#   box<-1
#   for(i in 1:(length(values_box)-1)){
#     if(values_box[i+1]-values_box[i] >= 0.1){
#       box<-box+1
#       logical_box[i+1]<-box
#     }else{
#       logical_box[i+1]<-box
#     }
#   }
# 
#   text_div<-""
#   if(!is.null(text)){
#     if(ymax < 0.2){
#       style_text = sprintf(style_line, 200)
#     }else{
#       style_text = sprintf(style_line, positions_from_top[5]-50)
#     }
#     text_div <- paste0(text_div, "<div class='text_tooltip'>")
#     text_div <- paste0(text_div, sprintf("<div style='%s'>", style_text))
#     text_div <- paste0(text_div, sprintf("&nbsp;<strong>%s</strong>&nbsp;", text))
#     text_div <- paste0(text_div, "</div>")
#     text_div <- paste0(text_div, "</div>")
#   }
#   
#   if(hover$x < hover$domain$right-2){
#     text_div <- paste0(text_div, "<div class='div_tooltip tooltip_right'>")
#   }else{
#     text_div <- paste0(text_div, "<div class='div_tooltip tooltip_left'>")
#   }
#   
#   max_index <- box
#   # print(logical_box)
#   for(i in 1:max_index){
#     indexes <- which(logical_box==i)
#     if(length(indexes) == 1){
#       style_text = sprintf(style_line, positions_from_top[indexes[1]]-14) 
#     }else{
#       style_text = sprintf(style_line, positions_from_top[indexes[length(indexes)]]-11*length(indexes))
#     }
#     text_div <- paste0(text_div, sprintf("<div style='%s'>", style_text))
#     for(j in length(indexes):1){
#       text_div <- paste0(text_div, sprintf("&nbsp;<strong>%s:</strong> %.6f&nbsp;<br>", text_box[indexes[j]], values_box[indexes[j]]))
#     }
#     text_div <- paste0(text_div, "</div>")
#   }
#   
#   text_div <- paste0(text_div, "</div>")
#   
#   text_div
# }
# 
# ggname <- function(x) {
#   if (class(x) != "character") {
#     return(x)
#   }
#   y <- sapply(x, function(s) {
#     if (!grepl("^`", s)) {
#       s <- paste("`", s, sep="", collapse="")
#     }
#     if (!grepl("`$", s)) {
#       s <- paste(s, "`", sep="", collapse="")
#     }
#   }
#   )
#   y 
# }
# 
# 
# format_abundant_taxa <- function(n_quantity, abundance_taxa){
#   if(nrow(abundance_taxa)<=n_quantity){
#     return(abundance_taxa)
#   }
#   
#   abundance_taxa<-rbind(abundance_taxa, rep("Remainder", ncol(abundance_taxa)))
#   abundance_taxa
# }
# 
# fix_taxonomy_names <- function(taxonomy_df){
#   
#   for(i in 1:nrow(taxonomy_df)){
#     if(!identical(taxonomy_df[i,1], "N/A")){
#       last_defined_taxonomy <- taxonomy_df[i,1]
#       for(j in 2:ncol(taxonomy_df[i,])){
#         if(identical(taxonomy_df[i,j], "N/A")){
#           taxonomy_df[i,j] <- paste("unclassified", last_defined_taxonomy)
#         }else{
#           last_defined_taxonomy <- taxonomy_df[i,j]
#         }
#       }
#     } 
#   }
#   taxonomy_df
# }
# 
# get_n_abundant_overall <- function(n_quantity, abundance_data_frame){
#   # if(nrow(abundance_data_frame)<=n_quantity){
#   #   print(abundance_data_frame)
#   #   return(abundance_data_frame)
#   # }
#   
#   df <- data.frame(row.names = c(rownames(abundance_data_frame)))
#   df<-cbind(df, apply(abundance_data_frame, 1, mean))
#   colnames(df)<-c("mean")
#   setorder(df, -mean)
#   df <- head(df, n_quantity)
#   df
# }
# 
# filter_n_abundant <- function(n_quantity, abundance_data_frame){
#   if(nrow(abundance_data_frame)<=n_quantity){
#     return(abundance_data_frame)
#   }
#   
#   df <- data.frame(row.names = c(rownames(abundance_data_frame),nrow(abundance_data_frame)+1))
#   tolerance <- 0.000001
#   for(i in 1:length(abundance_data_frame)){
#     array_column <- abundance_data_frame[[i]]
#     nth_element <- sort(array_column, T)[n_quantity]
#     result_column <- array_column>=nth_element|array_column+tolerance>=nth_element|array_column>=nth_element+tolerance
#     sum_of_remainder <- sum(array_column[!result_column])
#     array_column<-replace(array_column, array_column+tolerance<nth_element, 0)
#     array_column[nrow(df)]<-sum_of_remainder
#     df<-cbind(df, array_column)
#   }
#   colnames(df)<-colnames(abundance_data_frame)
#   df
# }
# 
# 
# 
# 
# # join_abundance <- function(array_abundance, data_from_chart, layer_df){
# #   df_to_return <- data.frame()
# #   for(i in 1:length(array_abundance)){
# #     abi <- array_abundance[i]
# #     fill_obj <- layer_df[i,"fill"]
# #     line_to_remove <- 0
# #     for(j in 1:nrow(data_from_chart)){
# #       abj <- data_from_chart[j,"Abundance"]
# #       if(isTRUE(all.equal(abi,abj))){
# #         d <- data.frame(data_from_chart[j,], fill=fill_obj)
# #         df_to_return <- rbind(df_to_return, d)
# #         line_to_remove = j
# #         break
# #       }
# #     }
# #     if(line_to_remove > 0){
# #       data_from_chart <- data_from_chart[-c(line_to_remove), ]
# #     }
# #   }
# #   df_to_return
# # }
# 
# 
# 
