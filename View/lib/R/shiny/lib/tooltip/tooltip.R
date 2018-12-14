
generic_boxplot <- function(hover, ggbuild_obj, width,
                            top_offset=0, right_offset=0, left_offset=0){
  
  # for boxplot the direction should use the position of the boxplot
  # and not the hover, but this will be fixed later
  direction <- get_direction(hover, ggbuild_obj)
  
  x_col<-ggbuild_obj$plot$mapping$x
  y_col<-ggbuild_obj$plot$mapping$y
  
  panel_layout <- ggbuild_obj$layout$panel_layout
  position_panel<-NULL
  if(ncol(panel_layout)==6){
    col_facet<-colnames(panel_layout)[4]
    position_panel <- subset(panel_layout, get(col_facet)==hover$panelvar1)
  }else if(ncol(panel_layout)==7){
    col_facet1<-colnames(panel_layout)[4]
    col_facet2<-colnames(panel_layout)[5]
    position_panel <- subset(panel_layout, get(col_facet1)==hover$panelvar2 & get(col_facet2)==hover$panelvar1)
  }else if(ncol(panel_layout)==8){
    col_facet1<-colnames(panel_layout)[4]
    col_facet2<-colnames(panel_layout)[5]
    col_facet3<-colnames(panel_layout)[6]
    
    position_panel <- subset(panel_layout, get(col_facet1)==hover$panelvar2 &
                             get(col_facet2)==hover$panelvar3 & 
                             get(col_facet3)==hover$panelvar1
    )
  }
  if(is.null(position_panel)){
    boxplot_info<-ggbuild_obj$data[[1]][1,]
  }else{
    boxplot_info<-ggbuild_obj$data[[1]][position_panel$PANEL,] 
  }
  
  
  ymin<-boxplot_info$ymin
  lower<-boxplot_info$lower
  middle<-boxplot_info$middle
  upper<-boxplot_info$upper
  ymax<-boxplot_info$ymax
  
  if(identical(direction, "right")){
    left_pct <-
      (boxplot_info$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    
    left_px <-
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    style_line <-
      paste0(
        "left: ", left_px+left_offset, "px; top: %fpx"
      )
  }else{
    right_pct <-
      (boxplot_info$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    
    right_px <-
      (width-hover$range$right)+ right_pct * (hover$range$right - hover$range$left)
    style_line <-
      paste0(
        "right: ", right_px+right_offset, "px; top: %fpx"
      )
  }
  
  positions_from_top <- c(
    hover$range$top + (hover$domain$top - ymin) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - lower) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - middle) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - upper) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - ymax) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top)
  )
  
  logical_box<-c(1,1,1,1,1)
  values_box<-c(ymin, lower, middle, upper, ymax)
  text_box<-c("Min","25th Percentile","Median","75th Percentile","Max")
  box<-1
  
  for(i in 1:(length(positions_from_top)-1)){
    if(positions_from_top[i]-positions_from_top[i+1] >= 19){
      box<-box+1
      logical_box[i+1]<-box
    }else{
      logical_box[i+1]<-box
    }
  }
  
  max_index <- box
  text_div <- sprintf("<div class='div_boxplot boxplot_%s'>", direction)
  for(i in 1:max_index){
    indexes <- which(logical_box==i)
    if(length(indexes) == 1){
      style_text = sprintf(style_line, positions_from_top[indexes[1]]-14) 
    }else{
      style_text = sprintf(style_line, positions_from_top[indexes[length(indexes)]]-11*length(indexes))
    }
    text_div <- paste0(text_div, sprintf("<div style='%s'>", style_text))
    for(j in length(indexes):1){
      text_div <- paste0(text_div, sprintf("&nbsp;<strong>%s:</strong> %.6f&nbsp;<br>", text_box[indexes[j]], values_box[indexes[j]]))
    }
    text_div <- paste0(text_div, "</div>")
  }
  
  text_div <- paste0(text_div, "</div>")
  
  return(HTML(text_div))
}


# boxplot needed to be refactored
get_single_boxplot_hover <- function(hover, xmax, x, ymin, lower, middle, upper, ymax, width, text=NULL){
  if(hover$x < hover$domain$right-2){
    left_pct <-
      (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    
    left_px <-
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    style_line <-
      paste0(
        "left: ", left_px+20, "px; top: %fpx"
      )
  }else{ # if it's to show the right tooltip
    right_pct <-
      (hover$domain$right-x) / (hover$domain$right - hover$domain$left)
    
    right_px <-
      (width-hover$range$right)+right_pct * (hover$range$right - hover$range$left)
    style_line <-
      paste0(
        "right: ", right_px+20, "px; top: %fpx"
      )
  }
  
  positions_from_top <- c(
    hover$range$top + (hover$domain$top - ymin) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - lower) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - middle) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - upper) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top),
    hover$range$top + (hover$domain$top - ymax) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top)
  )
  
  values_box<-c(ymin, lower, middle, upper, ymax)
  
  logical_box<-c(1,1,1,1,1)
  text_box<-c("Min","25th Percentile","Median","75th Percentile","Max")
  box<-1
  for(i in 1:(length(values_box)-1)){
    if(values_box[i+1]-values_box[i] >= 0.1){
      box<-box+1
      logical_box[i+1]<-box
    }else{
      logical_box[i+1]<-box
    }
  }
  
  text_div<-""
  if(!is.null(text)){
    if(ymax < 0.2){
      style_text = sprintf(style_line, 200)
    }else{
      style_text = sprintf(style_line, positions_from_top[5]-50)
    }
    text_div <- paste0(text_div, "<div class='text_boxplot'>")
    text_div <- paste0(text_div, sprintf("<div style='%s'>", style_text))
    text_div <- paste0(text_div, sprintf("&nbsp;<strong>%s</strong>&nbsp;", text))
    text_div <- paste0(text_div, "</div>")
    text_div <- paste0(text_div, "</div>")
  }
  
  if(hover$x < hover$domain$right-2){
    text_div <- paste0(text_div, "<div class='div_boxplot boxplot_right'>")
  }else{
    text_div <- paste0(text_div, "<div class='div_boxplot boxplot_left'>")
  }
  
  max_index <- box
  
  for(i in 1:max_index){
    indexes <- which(logical_box==i)
    if(length(indexes) == 1){
      style_text = sprintf(style_line, positions_from_top[indexes[1]]-14) 
    }else{
      style_text = sprintf(style_line, positions_from_top[indexes[length(indexes)]]-11*length(indexes))
    }
    text_div <- paste0(text_div, sprintf("<div style='%s'>", style_text))
    for(j in length(indexes):1){
      text_div <- paste0(text_div, sprintf("&nbsp;<strong>%s:</strong> %.6f&nbsp;<br>", text_box[indexes[j]], values_box[indexes[j]]))
    }
    text_div <- paste0(text_div, "</div>")
  }
  
  text_div <- paste0(text_div, "</div>")
  
  text_div
}

get_direction <- function(hover, ggbuild_obj){
  max_col <- max(ggbuild_obj$layout$panel_layout$COL)
  
  direction <- "right"
  
  if(max_col == 1){
    middle <- (hover$domain$right+hover$domain$left)/2
    if(hover$x>middle){
      direction<-"left"
    }
  }else{
    panel_layout <- ggbuild_obj$layout$panel_layout
    # six means that we have one variable to facet
    # refactor this after the august release
    if(ncol(panel_layout)==6){
      col_facet<-colnames(panel_layout)[4]
      panel_layout <- subset(panel_layout, get(col_facet)==hover$panelvar1)
    }else if(ncol(panel_layout)==7){
      col_facet1<-colnames(panel_layout)[4]
      col_facet2<-colnames(panel_layout)[5]
      panel_layout <- subset(panel_layout, get(col_facet1)==hover$panelvar2 & get(col_facet2)==hover$panelvar1)
    }else if(ncol(panel_layout)==8){
      col_facet1<-colnames(panel_layout)[4]
      col_facet2<-colnames(panel_layout)[5]
      col_facet3<-colnames(panel_layout)[6]
      
      panel_layout <- subset(panel_layout, get(col_facet1)==hover$panelvar2 &
                               get(col_facet2)==hover$panelvar3 & 
                               get(col_facet3)==hover$panelvar1
                             )
    }
    
    
    if(panel_layout$COL[1]==max_col){
      # middle <- (hover$domain$right-hover$domain$left)/2
      # if(hover$x>middle){
      direction<-"left"
      # }
    }
  }
  direction
}

generic_point <- function(hover, ggbuild_obj, ggdata_obj, width = 0,
                          top_offset=0, right_offset=0, left_offset=0,
                          show_only = NULL, rename_cols = NULL){
  
  max_col <- max(ggbuild_obj$layout$panel_layout$COL)
  direction <- get_direction(hover, ggbuild_obj)
  
  near_points <- nearPoints(ggdata_obj, hover)
  
  if(nrow(near_points)==1){
    
    # so far the number of the rows have been returned as the rowname of the point
    # ggbuild$plot$data # now i need to discover what's is x and y
    map_x<-as.character(ggbuild_obj$plot$mapping$x)
    map_y<-as.character(ggbuild_obj$plot$mapping$y)
    
    value_y <- near_points[[map_y]]
    value_x <- near_points[[map_x]]
    
    if(ggbuild_obj$layout$panel_scales$y[[1]]$is_discrete()){
      all_y<-ggbuild_obj$layout$panel_ranges[[1]]$y.labels
      point_y<-match(value_y, all_y)
    }else{
      point_y<-value_y
    }
    
    if(ggbuild_obj$layout$panel_scales$x[[1]]$is_discrete()){
      all_x<-ggbuild_obj$layout$panel_ranges[[1]]$x.labels
      point_x<-match(value_x, all_x)
    }else{
      point_x<-value_x
    }
    
    top_pct <-
      (hover$domain$top - point_y) / (hover$domain$top - hover$domain$bottom)
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    
    div_formatted<-NULL
    
    if(identical(direction, "right")){
      left_pct <-
        (point_x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left)
      
      
      style <-
        paste0(
          "padding: 5px;",
          "left:",
          left_px + left_offset,
          "px; top:",
          top_px+top_offset,
          "px;"
        )
    }else{
      right_pct <-
        (hover$domain$right-point_x) / (hover$domain$right - hover$domain$left)
      
      if(width){
        right_px <-
          (width-hover$range$right) + right_pct * (hover$range$right - hover$range$left)
      }else{
        right_px <- right_pct * (hover$range$right - hover$range$left)
      }
      
      style <-
        paste0(
          "padding: 5px;",
          "right:",
          right_px+right_offset,
          "px; top:",
          top_px+top_offset,
          "px;"
        )
    }
    
    if(!is.null(show_only)){
      if("data.table" %chin% class(near_points)){
        df<-near_points[,show_only, with=F]
      }else{
        df<-near_points[,show_only]
      }
    }else{
      df<-near_points
    }
    
    if(!is.null(rename_cols)){
      colnames(df)<-rename_cols
    }
    
    
    text_div <- sprintf("<div style='%s' class='div_tooltip tooltip_%s'>",style, direction)
    
    table_text <- "<table style='padding:0;margin:0;' class='table table-striped table-condensed'><tbody>"
    
    value<-apply(df, 1, function(x){
      table_line <- "<tr>"
      table_line<-paste0(table_line,"<td><strong>", names(x), "</strong></td>")
      table_line<-paste0(table_line,"<td>", x, "</td>")
      table_line<-paste0(table_line,"</tr>")
      table_line
    })
    value<-paste(value, collapse ="")
    
    table_text <- paste0(table_text, value, "</tbody></table>")
    
    text_div <- paste0(text_div, table_text,"</div>")
    return(HTML(text_div))
  }
  return(NULL)
}