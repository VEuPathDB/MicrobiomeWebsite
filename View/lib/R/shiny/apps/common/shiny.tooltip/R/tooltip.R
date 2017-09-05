#' Function to read input files for MicrobiomeDB and returns a MicrobiomeData
#'   object.
#' @param data The datatable with one object per line
#' @param hover The hover object returned by shiny
#' @export
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

#' Generate a tooltip for  input files for MicrobiomeDB and returns a MicrobiomeData
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
  # print(table_text)
  # table_text<-df$rho
  text_div <- paste0(text_div, table_text,"</div>")

  HTML(text_div)
}
