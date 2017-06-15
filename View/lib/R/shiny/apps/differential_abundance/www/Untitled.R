library(ggplot2)
library(proto)
library(plyr)

facet_edges <- function(facets, nrow = NULL, ncol = NULL, scales = 'fixed',
                            shrink = TRUE, labeller = 'label_value', as.table = TRUE,
                            switch = NULL, drop = TRUE, dir = 'h',
                            strip.position = 'top') {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  if (!is.null(switch)) {
    .Deprecated("strip.position", old = "switch")
    strip.position <- if (switch == "x") "bottom" else "left"
  }
  strip.position <- match.arg(strip.position, c("top", "bottom", "left", "right"))
  ?match.arg
  ggproto(NULL, FacetEdges,
          shrink = shrink,
          params = list(facets = plyr::as.quoted(facets), free=free,
                        as.table = as.table, strip.position = strip.position,
                        drop = drop, ncol = ncol, nrow = nrow,
                        labeller = labeller, scales = scales,strip.position=strip.position,
                        dir = dir)
  )
}

FacetEdges <- ggproto('FacetEdges', FacetWrap,
                      draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                                             data, theme, params) {
                      # Place panels according to settings
                      # panel_table <- gtable::gtable_matrix(layout, panels)
                        # print("Panel")
                        # print(str(ranges))
                        # print("Params")
                        # print(params)
                        
                        vtiles <- sapply(lapply(ranges, "[[", "y.major"), length)
                        
                        ncol <- max(layout$COL)
                        nrow <- max(layout$ROW)
                        n <- nrow(layout)
                        panel_order <- order(layout$ROW, layout$COL)
                        layout <- layout[panel_order, ]
                        panels <- panels[panel_order]
                        panel_pos <- convertInd(layout$ROW, layout$COL, nrow)
                        
                        axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)
                        
                        labels_df <- layout[names(params$facets)]
                        attr(labels_df, "facet") <- "wrap"
                        strips <- render_strips(
                          structure(labels_df, type = "rows"),
                          structure(labels_df, type = "cols"),
                          params$labeller, theme)
                        
                        # If user hasn't set aspect ratio, and we have fixed scales, then
                        # ask the coordinate system if it wants to specify one
                        aspect_ratio <- theme$aspect.ratio
                        if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
                          aspect_ratio <- coord$aspect(ranges[[1]])
                        }
                        
                        if (is.null(aspect_ratio)) {
                          aspect_ratio <- 1
                          respect <- FALSE
                        } else {
                          respect <- TRUE
                        }
                        
                        empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
                        panel_table <- empty_table
                        panel_table[panel_pos] <- panels
                        # empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
                        #unit(rep(aspect_ratio, nrow), "null")
                        panel_table <- gtable::gtable_matrix("layout", panel_table,
                                                     widths = unit(rep(1, ncol), "null"),
                                                     heights = unit(vtiles, "null"), respect = respect, clip = "on", z = matrix(1, ncol = ncol, nrow = nrow))
                        panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

                        
                        # Add axes
                        axis_mat_x_top <- empty_table
                        axis_mat_x_top[panel_pos] <- axes$x$top[layout$SCALE_X]
                        axis_mat_x_bottom <- empty_table
                        axis_mat_x_bottom[panel_pos] <- axes$x$bottom[layout$SCALE_X]
                        axis_mat_y_left <- empty_table
                        axis_mat_y_left[panel_pos] <- axes$y$left[layout$SCALE_Y]
                        axis_mat_y_right <- empty_table
                        axis_mat_y_right[panel_pos] <- axes$y$right[layout$SCALE_Y]
                        if (!params$free$x) {
                          axis_mat_x_top[-1,]<- list(zeroGrob())
                          axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
                        }
                        if (!params$free$y) {
                          axis_mat_y_left[, -1] <- list(zeroGrob())
                          axis_mat_y_right[, -ncol] <- list(zeroGrob())
                        }
                        axis_height_top <- unit(apply(axis_mat_x_top, 1, max_height), "cm")
                        axis_height_bottom <- unit(apply(axis_mat_x_bottom, 1, max_height), "cm")
                        axis_width_left <- unit(apply(axis_mat_y_left, 2, max_width), "cm")
                        axis_width_right <- unit(apply(axis_mat_y_right, 2, max_width), "cm")
                        
                        panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
                        panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
                        panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
                        panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)
                        
                        # strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
                        strip_padding <- unit(theme$strip.switch.pad.wrap, "cm")
                        strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
                        strip_mat <- empty_table
                        strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
                        if (params$strip.position %in% c("top", "bottom")) {
                          # inside <- (theme$strip.placement.x | theme$strip.placement | "inside") == "inside"
                          inside <- T
                          if (params$strip.position == "top") {
                            placement <- if (inside) -1 else -2
                            strip_pad <- axis_height_top
                          } else {
                            placement <- if (inside) 0 else 1
                            strip_pad <- axis_height_bottom
                          }
                          strip_height <- unit(apply(strip_mat, 1, max_height), "cm")
                          panel_table <- weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, "on")
                          if (!inside) {
                            strip_pad[unclass(strip_pad) != 0] <- strip_padding
                            panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
                          }
                        }
                        panel_table
                        
  }
)

convertInd <- function(row, col, nrow) {
  (col - 1) * nrow + row
}

weave_tables_col <- function(table, table2, col_shift, col_width, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_col))) {
    col_ind <- panel_col[i] + col_shift
    table <- gtable::gtable_add_cols(table, col_width[i], pos = col_ind)
    if (!missing(table2)) {
      table <- gtable::gtable_add_grob(table, table2[, i], t = panel_row, l = col_ind + 1, clip = clip, name = paste0(name, "-", seq_along(panel_row), "-", i), z = z)
    }
  }
  table
}
weave_tables_row <- function(table, table2, row_shift, row_height, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_row))) {
    row_ind <- panel_row[i] + row_shift
    table <- gtable::gtable_add_rows(table, row_height[i], pos = row_ind)
    if (!missing(table2)) {
      table <- gtable::gtable_add_grob(table, table2[i, ], t = row_ind + 1, l = panel_col, clip = clip, name = paste0(name, "-", seq_along(panel_col), "-", i), z = z)
    }
  }
  table
}

