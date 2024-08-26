#' @title Input specific wikipathways ID to get an output in class of wpplot.
#' @description Use wikipathways ID to open a local svg file. Then extract related information from svg file and build a wpplot class variance.
#' @param ID ID is wikipathways' ID.
#' @return A 'wpplot' object
#' @export
#' @examples 
#' if (yulab.utils::has_internet())
#'    wpplot('WP179') 
wpplot <- function(ID) {
  url0 <- 'https://www.wikipathways.org/wikipathways-assets/pathways'
  url <- sprintf("%s/%s/%s.svg", url0, ID, ID)

  svg <- yulab.utils::yread(url)
  if (!any(grepl('<svg', svg[1:10]))) {
    stop("fail to read online wiki pathway file")
  }
  structure(list(
    ID = ID,
    svg = svg,
    geneExpr = NULL
  ), class = "wpplot")
}

#' @title Fill the background of gene with color according to amount of gene expression.
#' @description Generate a color array.Fill the gene then generate the legend.
#' @param p p is
#' @param value value is the amount of expression.
#' @param low The color of lowest gene.
#' @param high The color of highest gene.
#' @param legend Whether you need legend.
#' @param legend_x horizontal position of the legend
#' @param legend_y vertical position of the legend
#' @return A 'wpplot' object
# @import org.Hs.eg.db
# @import BiocGenerics
#' @export
wp_bgfill <- function(p, value, high="red", low="blue", legend = TRUE, legend_x = 0.001, legend_y = 0.94) {
  if(legend_x < 0 || legend_x > 1 || legend_y < 0 || legend_y > 1){
    message('Parameters legend_x and legend_y must be numbers between 0 to 1!')
  }
  SYMBOLS <- sub('\\s+', '', sub('>', '', sub('</text', '', p$svg[grep('</text', p$svg)])))
  SYMBOLS <- SYMBOLS[is.na(suppressWarnings(as.numeric(SYMBOLS)))]
  
  if(!any(names(value) %in% SYMBOLS)){
    message("Please make sure the input gene ID type is 'SYMBOL'")
    return(p)
  }
  value <- value[names(value) %in% SYMBOLS]
  
  mini <- min(value) %/% 10 * 10
  maxi <- ceiling(max(value)/10) * 10
  colornum <- (maxi-mini) / 10
  
  colorbar <- colorb(value, low, high)
  color <- colorbar[order(value)]  
  legendcolor <- legend_generator(value, low, high)
  
  genes <- names(value)
  
  for (i in seq_along(genes)) {
    pos <- grep(genes[i], p$svg)
    p$svg <- replace_bg2(p$svg, pos, color[i])
  }
  
  svg_width <- as.numeric(strsplit(strsplit(p$svg[4], 'width=\"')[[1]][2], '\" height=\"')[[1]][1])
  svg_height <- as.numeric(strsplit(strsplit(strsplit(p$svg[4], 'width=\"')[[1]][2], '\" height=\"')[[1]][2], '\"')[[1]][1])
  
  incrementX <- svg_width * legend_x
  incrementY <- svg_height * (1 - legend_y)
  if(incrementX > svg_width - 48)
    incrementX <- svg_width - 48
  
  if(incrementY > svg_height - 122){
    incrementY <- svg_height - 122
  }else if(incrementY < 3)
    incrementY <- 3
  
  textele <- rev(pretty(value, 4))  
  legendX <- 0 + incrementX
  legendY <- 0 + incrementY
  textX <- 40 + incrementX
  textY <- seq(from = 5,to = 120,length.out = length(textele)) + incrementY
  scalelineX <- 27 + incrementX
  scalelineY <- seq(from = 2,to = 118,length.out = length(textele)) +incrementY
  
  if(legend){
    zero_scale_line <- find_zero_scale(value)
    proportion <- seq(from = 2,to = 118,length.out = length(textele)) / 120
    proportion <- proportion[length(which(pretty(value, 4) >= zero_scale_line))]
    if(max(pretty(value, 4)) == 0){
      proportion <- '0%'
    }
    if(min(pretty(value, 4)) == 0){
      proportion <- '100%'
    }
    temp<-grep("</svg",p$svg)
    p$svg[temp]<-sub("</svg",paste("<defs><linearGradient id=\"grad1\" x1=\"0%\" y1=\"0%\" x2=\"0%\" y2=\"100%\"><stop offset=\"0%\" style=\"stop-color:",high,";stop-opacity:1\"></stop><stop offset=\"",proportion,"\" style=\"stop-color:","white",";stop-opacity:1\"></stop><stop offset=\"100%\" style=\"stop-color:",low,";stop-opacity:1\"></stop></linearGradient></defs><rect x=\"",legendX,"\" y=\"",legendY,"\" width =\"30\" height=\"120\" style=\"fill:url(#grad1 );stroke-width:0;stroke:black\"></rect></svg",sep = ""),p$svg[temp])
    
    for (i in 1:length(pretty(value, 4))){
      temp<-grep("</svg",p$svg)
      p$svg[temp]<-sub("</svg",paste("<text x=\"",textX,"\" y=\"",textY[i],"\" style=\"font-size:10; fill:black; stroke:none\">",textele[i],"</text></svg",sep = ""),p$svg[temp])
    }
    for (i in 1:length(pretty(value, 4))){
      temp<-grep("</svg",p$svg)
      p$svg[temp]<-sub("</svg",paste("<rect width=\"3\" height=\"1\" x=\"",scalelineX,"\" y=\"",scalelineY[i],"\" style=\"fill:white; stroke:none\"></rect></svg",sep = ""),p$svg[temp])
    }
  }
  p$geneExpr <- value
  return(p)
}


#' @title Add halo above gene name to get a clear view.
#' @description Add use svghalo2 function to add halo.
#' @param p An wpplot class variance.
#' @param bg.r The width of halo.
#' @param bg.col The color of halo.
#' @return A 'wpplot' object
#' @export
wp_shadowtext <- function(p, bg.r = 2, bg.col = "white") {
  if (is.null(p$geneExpr)) return(p)
  
  genes <- names(p$geneExpr)
  
  for (i in seq_along(genes)) {
    pos <- grep(genes[i], p$svg)
    p$svg <- svg_halos(p$ svg, pos, genes[i])
  }
  
  i <- grep("><!--Generated by the Batik Graphics2D SVG Generator-->", p$svg)
  
  p$svg[i] <- paste0(
    "><!--Generated by the Batik Graphics2D SVG Generator-->",
    "<style>.halo{fill:", bg.col,
    ";stroke:", bg.col, "; stroke-width:", bg.r,
    ";stroke-linejoin:round; vector-effect:non-scaling-stroke;",
    "}</style><defs id=\"genericDefs\""
  )
  
  return(p)
}


#' @title Save the 'wpplot' object to a file.
#' @param p A 'wpplot' object
#' @param file the file to save the object
#' @param width Width of the figure
#' @param height Height of the figure
#' @param ... additional parameter passed to 'ggsave'
#' @return output the file and the input 'wpplot' object (invisible)
#' @import rsvg
#' @importFrom ggplot2 ggsave
#' @export
wpsave <- function(p, file, width=NULL, height=NULL, ...) {
  # fileext <- sub(".*(\\..+)", "\\1", file)
  # f <- svg2tempfile(p$svg)
  # if (fileext == '.svg') {
  #   rsvg::rsvg_svg(f, file = file, width = width, height = height)
  # } else if (fileext == '.pdf') {
  #   rsvg::rsvg_pdf(f, file = file, width = width, height = height)
  # } else if (fileext == '.png') {
  #   rsvg::rsvg_png(f, file = file, width = width, height = height)
  # } else {
  #   stop("file type not supported")
  # }
  
  g <- ggplotify::as.ggplot(p)
  
  ggplot2::ggsave(plot = g,
    filename = file,
    width = width,
    height = height, 
    ...)

  invisible(p)
}


##' @importFrom ggplot2 ggsave
##' @export
ggplot2::ggsave
