svg2tempfile <- function(svg) {
  f <- tempfile(fileext = ".svg")
  cat(svg, file = f)
  return(f)
}

#' @import grDevices

colorb <- function(Expression, low = "blue", high = "red") {
  zero_scale_line <- find_zero_scale(Expression)
  textele <- pretty(Expression, 4)
  textele_low <- textele[which(textele <= zero_scale_line)]
  textele_high <- textele[which(textele > zero_scale_line)]
  
  Expression_low <- Expression[which(Expression <= zero_scale_line)]
  Expression_high <- Expression[which(Expression > zero_scale_line)]
  scaleExpr_low <- (Expression_low - min(textele_low)) / (zero_scale_line - min(textele_low))
  scaleExpr_high <- (Expression_high - zero_scale_line) / (max(textele_high) - zero_scale_line)
  scaleExpr_low <- round(scaleExpr_low, 2) * 1000 + 1 # 1-1001
  scaleExpr_high <- round(scaleExpr_high, 2) * 1000 + 1 # 1-1001
  colorB2R_low <- colorRampPalette(colors = c(low, "white"))
  colorB2R_high <- colorRampPalette(colors = c("white", high))
  c(colorB2R_low(1001)[sort(scaleExpr_low)], colorB2R_high(1001)[sort(scaleExpr_high)])
}

find_zero_scale <- function(value){
  zero_scale_line <- 0
  if(all(pretty(value, 4) > 0) || all(pretty(value, 4) < 0)){
    zero_scale_line <- pretty(value, 4)[round(length(pretty(value, 4)) / 2)]
  }
  return(zero_scale_line)
}

legend_generator <- function(value, low = "blue", high = "red") {
  temp <- pretty(value, 4)
  seq1 <- ceiling(seq(from = 1, to = 1001, length.out = length(temp[which(temp <= 0)])))
  seq2 <- ceiling(seq(from = 1, to = 1001, length.out = length(temp[which(temp >= 0)])))
  c(colorRampPalette(colors = c(low, "white"))(1001)[seq1], colorRampPalette(colors = c("white", high))(1001)[seq2[-1]])
}

svg_halos <- function(svg, pos, gene) {
  svg[pos - 1] <- paste(
    sub(
      "fill:black; stroke:none;",
      "\" class=\"halo",
      svg[pos - 1]
    ),
    sub(
      "/>",
      paste(">", gene, "</text>", sep = ""),
      svg[pos - 1]
    )
  )
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
  
  replace <- sub(
    "fill:.+;.+", paste("fill:", color,
                        "; text-rendering:geometricPrecision; stroke:white;\"",
                        sep = ""
    ),
    svg[j]
  )
  
  svg[j] <- replace
  return(svg)
}

replace_bg2 <- function(svg, positions, color) {
  if (is.null(positions[1]) || is.na(positions[1]) || length(positions[1]) == 0) {
    return(svg)
  }
  
  for (position in positions) {
    svg <- replace_bg(svg, position, color)
  }
  
  return(svg)
}
