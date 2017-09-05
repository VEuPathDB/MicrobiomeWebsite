horizontal_points <- function(data, hover){
  x<-round(hover$x)
  y<-round(hover$y)
  
  rows_values<-data[,get(hover$mapping$y)]
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


generate_point_hover <- function(hover, text){
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
      "position:absolute; z-index:10000; background-color: rgba(245, 245, 245, 0.85); "
      ,
      "left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )
  
  wellPanel(style = style,
            HTML(text)
  )
}

generate_boxplot_hover <- function(hover, text){
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
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); "
      ,
      "left:",
      left_px + 2,
      "px; top:",
      top_px + 2,
      "px;"
    )
  
  wellPanel(style = style,
            HTML(text)
  )
}

get_single_boxplot_hover <- function(hover, xmax, x, ymin, lower, middle, upper, ymax, text=NULL){
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
    left_pct <-
      (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    
    left_px <-
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    style_line <-
      paste0(
        "left: ", left_px-170, "px; top: %fpx"
      )
    # right_pct <-
    #   (hover$domain$right-x) / (hover$domain$right - hover$domain$left)
    # print(hover$domain$right)
    # right_px <-
    #   hover$range$left + right_pct * (hover$range$right - hover$range$left)
    # style_line <-
    #   paste0(
    #     "right: ", right_px-10, "px; top: %fpx"
    #   )
  }
  
  # left_pct <-
  #   (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  # 
  # left_px <-
  #   hover$range$left + left_pct * (hover$range$right - hover$range$left)
  # style_line <-
  #   paste0(
  #     "left: ", left_px-170, "px; top: %fpx"
  #   )
  
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
    text_div <- paste0(text_div, "<div class='text_tooltip'>")
    text_div <- paste0(text_div, sprintf("<div style='%s'>", style_text))
    text_div <- paste0(text_div, sprintf("&nbsp;<strong>%s</strong>&nbsp;", text))
    text_div <- paste0(text_div, "</div>")
    text_div <- paste0(text_div, "</div>")
  }
  
  if(hover$x < hover$domain$right-2){
    text_div <- paste0(text_div, "<div class='div_tooltip tooltip_right'>")
  }else{
    text_div <- paste0(text_div, "<div class='div_tooltip tooltip_left'>")
  }
  
  max_index <- box
  # print(logical_box)
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

get_simple_hover <- function(hover, text, tooltip_direction="right", offset_right=0, offset_left=0){
  middle <- (hover$domain$right-hover$domain$left)/2
  x<-hover$x
  
  text_div<-""
  if(identical(tooltip_direction, "right")){ # left tooltip
    left_pct <-
      (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    
    top_pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <-
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <-
      paste0(
        "position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 0.85);"
        ,
        "left:",
        left_px + 17+offset_left,
        "px; top:",
        top_px + 2,
        "px;"
      )
    text_div <- paste0(text_div, sprintf("<div class='div_tooltip tooltip_left'>"))
    
  }else{ # right tooltip
    # left_pct <-
    #   (x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    left_pct <-
      ( hover$domain$right-x) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <-
       (hover$range$right - hover$range$left)*(left_pct)
    
    # left_px <-
    #      ((hover$range$right - hover$range$left) / hover$range$left)+(left_pct*hover$range$left)
    
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <-
      paste0(
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85);"
        ,
        "right:",
        left_px + 22 + offset_right,
        "px; top:",
        top_px + 2,
        "px;"
      )
    
    text_div <- paste0(text_div, "<div class='div_tooltip tooltip_right'>")
  }
  
  text_div <- paste0(text_div, text)
  text_div <- paste0(text_div, "</div>")
  
  wellPanel(style = style,
            HTML(text_div)
  )
}

point_hover <- function(hover, df, point_x, point_y, right_offset=0, left_offset=0,top_offset=0){
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
  # print(table_text)
  # table_text<-df$rho
  text_div <- paste0(text_div, table_text,"</div>")
  
  HTML(text_div)
}