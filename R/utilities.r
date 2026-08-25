svg2tempfile <- function(svg) {
  f <- tempfile(fileext = ".svg")
    cat(svg, sep = "\n", file = f)
  return(f)
}

#' @import grDevices

strip_svg_value <- function(x) {
    suppressWarnings(as.numeric(sub("[^0-9.]+$", "", x)))
}

svg_attr_value <- function(line, attr) {
    m <- regexec(sprintf('%s="([^"]+)"', attr), line)
    hit <- regmatches(line, m)[[1]]
    if (length(hit) < 2) {
        return(NA_character_)
    }
    hit[2]
}

svg_dimensions <- function(svg) {
    svg_line <- svg[grep("<svg", svg, fixed = TRUE)][1]
    width <- svg_attr_value(svg_line, "width")
    height <- svg_attr_value(svg_line, "height")

    if (is.na(width) || is.na(height)) {
        viewbox <- svg_attr_value(svg_line, "viewBox")
        if (!is.na(viewbox)) {
            parts <- strsplit(viewbox, "\\s+")[[1]]
            if (length(parts) >= 4) {
                if (is.na(width)) {
                    width <- parts[3]
                }
                if (is.na(height)) {
                    height <- parts[4]
                }
            }
        }
    }

    c(width = strip_svg_value(width), height = strip_svg_value(height))
}

svg_user_dimensions <- function(svg) {
    svg_line <- svg[grep("<svg", svg, fixed = TRUE)][1]
    viewbox <- svg_attr_value(svg_line, "viewBox")

    if (!is.na(viewbox)) {
        parts <- strsplit(viewbox, "\\s+")[[1]]
        if (length(parts) >= 4) {
            return(c(
                width = suppressWarnings(as.numeric(parts[3])),
                height = suppressWarnings(as.numeric(parts[4]))
            ))
        }
    }

    svg_dimensions(svg)
}

extract_svg_label <- function(line) {
    if (grepl("<tspan", line, fixed = TRUE) && grepl("</tspan>", line, fixed = TRUE)) {
        return(sub("</tspan>.*", "", sub(".*<tspan[^>]*>", "", line)))
    }

    if (grepl("<text", line, fixed = TRUE) && grepl("</text>", line, fixed = TRUE)) {
        return(sub("</text>.*", "", sub(".*<text[^>]*>", "", line)))
    }

    NA_character_
}

find_gene_positions <- function(svg, gene) {
    idx <- grep("<tspan", svg, fixed = TRUE)
    if (length(idx) > 0) {
        labels <- vapply(svg[idx], extract_svg_label, character(1))
        hits <- idx[!is.na(labels) & labels == gene]
        if (length(hits) > 0) {
            return(hits)
        }
    }

    idx <- grep("<text", svg, fixed = TRUE)
    if (length(idx) > 0) {
        labels <- vapply(svg[idx], extract_svg_label, character(1))
        return(idx[!is.na(labels) & labels == gene])
    }

    integer(0)
}

find_node_rect <- function(svg, position) {
    from <- max(1, position - 10)
    idx <- grep("<rect", svg[from:position], fixed = TRUE)
    if (length(idx) == 0) {
        return(NA_integer_)
    }
    from + idx[length(idx)] - 1
}

replace_fill <- function(line, color) {
    if (grepl('fill="', line, fixed = TRUE)) {
        return(sub('fill="[^"]+"', sprintf('fill="%s"', color), line))
    }

    if (grepl("fill:", line, fixed = TRUE)) {
        return(sub("fill:[^;\"']+", paste0("fill:", color), line))
    }

    line
}

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
    if (length(pos) == 0 || all(is.na(pos))) {
      return(svg)
    }

    for (i in pos) {
      if (grepl('class="halo"', svg[i], fixed = TRUE)) {
        next
      }

      if (grepl("<tspan", svg[i], fixed = TRUE) && grepl("</tspan>", svg[i], fixed = TRUE)) {
        svg[i] <- sub(
          "(<tspan)([^>]*>)(.*)(</tspan>)",
          "\\1 class=\"halo\"\\2\\3\\4\\1\\2\\3\\4",
          svg[i]
        )
        next
      }

      if (grepl("<text", svg[i], fixed = TRUE) && grepl("</text>", svg[i], fixed = TRUE)) {
        svg[i] <- sub(
          "(<text)([^>]*>)(.*)(</text>)",
          "\\1 class=\"halo\"\\2\\3\\4\\1\\2\\3\\4",
          svg[i]
        )
      }
    }

  return(svg)
}


svg_halos2 <- function(svg, positions, gene) {
  for (pos in positions) {
    svg <- svg_halos(svg, pos, gene)
  }
  return(svg)
}

replace_bg <- function(svg, position, color) {
    j <- find_node_rect(svg, position)
    if (is.na(j)) {
      return(svg)
    }

    svg[j] <- replace_fill(svg[j], color)
  return(svg)
}

replace_bg2 <- function(svg, positions, color) {
  if (is.null(positions) || all(is.na(positions)) || length(positions) == 0) {
    return(svg)
  }
  
  for (position in positions) {
    if (is.na(position)) next
    svg <- replace_bg(svg, position, color)
  }
  
  return(svg)
}

inject_halo_style <- function(svg, bg.col, bg.r) {
    if (any(grepl(".halo{", svg, fixed = TRUE))) {
      return(svg)
    }

    style <- sprintf(
      "<style>.halo{fill:%s;stroke:%s;stroke-width:%s;stroke-linejoin:round;paint-order:stroke fill;vector-effect:non-scaling-stroke;}</style>",
      bg.col,
      bg.col,
      bg.r
    )
    end <- grep("</svg", svg, fixed = TRUE)[1]
    if (is.na(end)) {
      return(c(svg, style))
    }

    append(svg, style, after = end - 1)
}
