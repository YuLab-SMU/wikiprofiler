svg2tempfile <- function(svg) {
  f <- tempfile(fileext = '.svg') 
  cat(svg, file = f) 
  return(f)
}

#' @import grDevices

colorb<-function(Expression,low = "blue", high = "red"){
  Expression_low <- Expression[which(Expression <= 0)]
  Expression_high <- Expression[which(Expression > 0)]
  scaleExpr_low <- (Expression_low-min(Expression_low)) / diff(range(Expression_low)) 
  scaleExpr_low <- round(scaleExpr_low,2) * 1000 + 1 #1-1001
  scaleExpr_high <- (Expression_high-min(Expression_high)) / diff(range(Expression_high)) 
  scaleExpr_high <- round(scaleExpr_high,2) * 1000 + 1 #1-1001
  colorB2R_low <- colorRampPalette(colors = c(low, 'white'))
  colorB2R_high <- colorRampPalette(colors = c('white', high))
  c(colorB2R_low(1001)[sort(scaleExpr_low)], colorB2R_high(1001)[sort(scaleExpr_high)])
}

legend_generator <- function(value, low = "blue", high = "red"){
  temp <- pretty(value, 4)
  seq1 <- ceiling(seq(from = 1, to = 1001, length.out = length(temp[which(temp <= 0)])))
  seq2 <- ceiling(seq(from = 1, to = 1001, length.out = length(temp[which(temp >= 0)])))
  c(colorRampPalette(colors = c(low, 'white'))(1001)[seq1], colorRampPalette(color = c('white', high))(1001)[seq2[-1]])
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
