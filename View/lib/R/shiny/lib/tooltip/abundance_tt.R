barplot_points <- function(ggplot_object, hover, width, stacked = F, right_offset=0,
                           left_offset=0,top_offset=0){
  g<-ggplot_build(ggplot_object)
  x_is_disc <- g$layout$panel_scales$x[[1]]$is_discrete()
  y_is_disc <- g$layout$panel_scales$y[[1]]$is_discrete()
  
  if("CoordFlip" %in% class(ggplot_object$coordinates)){
    x_<-ifelse(x_is_disc, round(hover$y), hover$y)
    y_<-ifelse(y_is_disc, round(hover$x), hover$x)
  }else{
    x_<-ifelse(x_is_disc, round(hover$x), hover$x)
    y_<-ifelse(y_is_disc, round(hover$y), hover$y)
  }
  
  if(stacked){
    # if hover$panelvar1 is null means that there is no facet
    if(!is.null(hover$panelvar1)){
      panel_layout<-g$layout$panel_layout # panel_layout returns a data.frame
                                          # with all panels currently being shown
      # the forth column is the data used to facet the plot
      facet_column <- colnames(panel_layout)[4]
      # hover$panelvar1 returns the name of the facet where the mouse are hovering
      panel_hover <- subset(panel_layout, get(facet_column)==hover$panelvar1)
      panel_index <- panel_hover$PANEL
    }else{
      panel_index<-1
    }
    # it's necessary to know the panel index since when the plot it's facet, each
    # panel has its own axis (there is one y=1 for each panel)
    
    # we filter the data using the axis and panel information
    filtered_data <- subset(g$data[[1]], x==x_ & (y_>ymin & y_<=ymax) & PANEL==panel_index)
    
    # we map the values used to fill by their colors
    ranges<-g$plot$scales$scales[[1]]$range$range # get values in plot
    # names(ranges)<-g$plot$scales$scales[[1]]$palette(length(ranges)) # without using a defined pallete
    names(ranges)<-g$plot$scales$scales[[1]]$palette.cache[1:length(ranges)]
    
    value_filled<-ranges[[filtered_data[["fill"]]]]
    
    stacked_value<-filtered_data[["ymax"]]-filtered_data[["ymin"]]
    
    y_value<-g$layout$panel_ranges[[panel_index]]$y.labels[x_]
    
    df<-data.frame("Sample"=y_value, "Taxon"=value_filled, "Abundance"=stacked_value)
    colnames(df)<-c("Sample", g$plot$labels$fill, "Abundance")
    
    return(tooltip_simple(hover, df, width, hover$x, hover$y, right_offset, left_offset, top_offset))
  }
  return(NULL)
}

#' Function to 
#' @param data The datatable with one object per line
#' @param hover The hover object returned by shiny
#' @export
horizontal_points <- function(data, hover){
  rows_values<-data[,get(hover$mapping$y)]
  y<-round(hover$y)
  x<-round(hover$x)
  column_values <- levels(factor(data[,get(hover$mapping$x)]))
  if(is.factor(rows_values)){
    rows <- levels(data[,get(hover$mapping$y)])
  }else{
    rows <- levels(factor(data[,get(hover$mapping$y)]))
  }
  line<-subset(data, get(hover$mapping$y)==rows[y])
  line[,selected_:=TRUE]
  if(nrow(line)==1){
    column_index <- match(line[,get(hover$mapping$x)], column_values)
    line[,point_x:=column_index]
    line
  }else{
    line<-subset(line, get(hover$mapping$x)==column_values[x])
    column_index <- match(line[,get(hover$mapping$x)], column_values)
    line[,point_x:=column_index]
    line
  }
}

#' Generate a tooltip
#' @param hover The hover object returned by shiny
#' @param df The data frame with information to show
#' @param point_x The x axis value to show the tooltip
#' @param point_y The y axis value to show the tooltip
#' @param right_offset Offset value to show the tooltip from the right side of the plot
#' @param left_offset Offset value to show the tooltip from the left side
#' @param top_offset Offset value to show the tooltip from the top
#' @return Returns a HTML object
#' @keywords tooltip
#' @export
tooltip_simple <- function(hover, df, width, point_x, point_y, right_offset=0,
                  left_offset=0,top_offset=0){
  middle <- (hover$domain$right-hover$domain$left)/2
  
  if(point_x < middle){
    direction <- "right"
  }else{
    direction <- "left"
  }
  
  top_pct <-
    (hover$domain$top - point_y) / (hover$domain$top - hover$domain$bottom)
  top_px <-
    hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
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
        top_px + top_offset,
        "px;"
      )
  }else{
    right_pct <-
      (hover$domain$right-point_x) / (hover$domain$right - hover$domain$left)

    right_px <-
      (width-hover$range$right)+right_pct * (hover$range$right - hover$range$left)
    
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
  
  text_div <- sprintf("<div style='%s' class='div_tooltip'>",style)
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
  
  HTML(text_div)
}

#' Generate a tooltip for  input files for MicrobiomeDB
#' @param hover The hover object returned by shiny
#' @param df The data frame with information to show
#' @param point_x The x axis value to show the tooltip
#' @param point_y The y axis value to show the tooltip
#' @param right_offset Offset value to show the tooltip from the right side of the plot
#' @param left_offset Offset value to show the tooltip from the left side
#' @param top_offset Offset value to show the tooltip from the top
#' @return Returns a HTML object
#' @keywords tooltip
#' @export
point_tooltip <- function(hover, df, point_x, point_y, right_offset=0,
                          left_offset=0,top_offset=0 ){
  middle <- (hover$domain$right-hover$domain$left)/2

  if(point_x < middle){
    direction <- "right"
  }else{
    direction <- "left"
  }

  top_pct <-
    (hover$domain$top - point_y) / (hover$domain$top - hover$domain$bottom)
  top_px <-
    hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

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

    right_px <-
      right_pct * (hover$range$right - hover$range$left)

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

  HTML(text_div)
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

generic_point <- function(hover, ggbuild_obj, ggdata_obj, width = 0,
                          top_offset=0, right_offset=0, left_offset=0,
                          show_only = NULL, rename_cols = NULL){
  
  max_col <- max(ggbuild_obj$layout$panel_layout$COL)
  direction <- "right"
  
  if(max_col == 1){
    middle <- (hover$domain$right+hover$domain$left)/2
    if(hover$x>middle){
      direction<-"left"
    }
  }else{
    panel_layout <- ggbuild_obj$layout$panel_layout
    col_facet<-colnames(panel_layout)[4]
    panel_layout <- subset(panel_layout, get(col_facet)==hover$panelvar1)

    if(panel_layout$COL[1]==max_col){
      # middle <- (hover$domain$right-hover$domain$left)/2
      # if(hover$x>middle){
        direction<-"left"
      # }
    }
  }
  
  near_points <- nearPoints(ggdata_obj, hover)
  
  if(nrow(near_points)==1){
    # so far the number of the rows have been returned as the rowname of the point
    # ggbuild$plot$data # now i need to discover what's is x and y
    map_x<-as.character(ggbuild_obj$plot$mapping$x)
    map_y<-as.character(ggbuild_obj$plot$mapping$y)
    
    value_y <- near_points[[map_y]]
    value_x <- near_points[[map_x]]
    
    all_y<-ggbuild_obj$layout$panel_ranges[[1]]$y.labels
    
    point_y<-match(value_y, all_y)
    point_x<-value_x
    
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
          width-hover$range$right+right_pct * (hover$range$right - hover$range$left)
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
      df<-near_points[,show_only]
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





