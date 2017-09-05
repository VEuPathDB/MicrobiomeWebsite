ggname <- function(x) {
  if (class(x) != "character") {
    return(x)
  }
  y <- sapply(x, function(s) {
    if (!grepl("^`", s)) {
      s <- paste("`", s, sep="", collapse="")
    }
    if (!grepl("`$", s)) {
      s <- paste(s, "`", sep="", collapse="")
    }
  }
  )
  y 
}

theme_eupath_default <- function(base_size=14, base_family="Helvetica", ...){
  theme(
    plot.background = element_rect(colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_line(colour="#f0f0f0"),
    panel.grid.minor = element_blank(),
    text = element_text(size=base_size, family=base_family, color = "black"),
    axis.title = element_text(face = "bold", size = rel(1.1)),
    strip.text = element_text(face="bold", size=rel(1.1)),
    strip.background = element_rect(fill="#F3F2F2"), ...)
}



# adapted from https://rpubs.com/Koundy/71792
theme_eupath <- function(
  base_size=14, base_family="Helvetica",
  legend.position="bottom", legend.direction = "horizontal", ...) {
  
  theme(
        text = element_text(size=base_size, family=base_family),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(colour = "white"),
        axis.title = element_text(face = "bold", size = rel(1.1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(size=rel(0.9), face="bold"),
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = NA, fill="white"),
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend.title = element_text(face="italic"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold"), ...)
}

#Inserts newlines into strings every N interval
new_lines_adder = function(x, interval) {
  #add spaces after /
  x = stringr::str_replace_all(x, "/", "/ ")
  #split at spaces
  x.split = strsplit(x, " ")[[1]]
  # get length of snippets, add one for space
  lens <- nchar(x.split) + 1
  # now the trick: split the text into lines with
  # length of at most interval + 1 (including the spaces)
  lines <- cumsum(lens) %/% (interval + 1)
  # construct the lines
  x.lines <- tapply(x.split, lines, function(line)
    paste0(paste(line, collapse=" "), "\n"), simplify = TRUE)
  # put everything into a single string
  result <- paste(x.lines, collapse="")
  #remove spaces we added after /
  result = stringr::str_replace_all(result, "/ ", "/")
  return(result)
}

#wrapper for the above, meant for users
add_newlines = function(x, total.length = 85) {
  # make sure, x is a character array   
  x = as.character(x)
  #determine number of groups
  groups = length(x)
  # apply splitter to each
  t = sapply(x, FUN = new_lines_adder, interval = round(total.length/groups), USE.NAMES=FALSE)
  return(t)
}

wrap_column <- function(col_taxa){
  if(length(col_taxa)>1){
    mapply(wrap_taxa, col_taxa)
  }else{
    wrap_taxa(col_taxa)
  }
  
}
wrap_taxa <- function(taxa){
  words<-unlist(strsplit(taxa, " "))
  if(length(words)>1){
    rest <- paste(words[2:length(words)], collapse =" ")
    paste0(words[1], "\n", rest)
  }else{
    taxa
  }
}


# 
# scale_fill_Publication <- function(...){
#   library(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }
# 
# scale_colour_Publication <- function(...){
#   library(scales)
#   discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }