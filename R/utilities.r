svg2tempfile <- function(svg) {
  f <- tempfile(fileext = '.svg') 
  cat(svg, file = f) 
  return(f)
}

#' @import grDevices

colorb<-function(Expression,low = "blue", high="red"){    
  scaleExpr<-(Expression-min(Expression)) / diff(range(Expression)) 
  scaleExpr<-round(scaleExpr,2) * 1000 + 1 #1-(n+1)
  colorB2R <- colorRampPalette(colors = c(low, high))
  colorB2R(1001)[sort(scaleExpr)]
}


svg_halos <- function(svg, pos, gene) {

  svg[pos-1] <- paste(
    sub("fill:black; stroke:none;",
        "\" class=\"halo",
        svg[pos-1]),
    sub("/>",
        paste(">",gene,"</text>",sep=""),
        svg[pos-1]
    ))
  return(svg)
}

svg_halos2 <- function(svg, positions, gene) {
  for (pos in positions) {
    svg <- svg_halos(svg, pos, gene)
  }
  return(svg)
}

replace_bg <- function(svg, position, color) {
  j <- rev(grep("<g", svg[1:position]))[1]

  replace <- sub("fill:.+;.+",paste("fill:",color,
                                    "; text-rendering:geometricPrecision; stroke:white;\"",sep = ""),
                 svg[j])

  svg[j] <- replace
  return(svg)
}

replace_bg2 <- function(svg, positions, color) {
  for (position in positions) {
    svg <- replace_bg(svg, position, color)
  }
  return(svg)
}
