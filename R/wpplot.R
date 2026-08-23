#' @title Input specific wikipathways ID to get an output in class of wpplot.
#' @description Use wikipathways ID to open a local svg file. Then extract related information from svg file and build a wpplot class variance.
#' @param ID ID is wikipathways' ID.
#' @return A 'wpplot' object
#' @export
#' @examples 
#' \dontrun{
#'    wpplot('WP179') 
#' }
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
    geneExpr = NULL,
    mapping_table = NULL,
    comparison = NULL
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
  mapping_table <- attr(value, "mapping_table", exact = TRUE)
  value <- normalize_wp_vector(value)
  positions <- lapply(names(value), function(gene) find_gene_positions(p$svg, gene))
  matched <- lengths(positions) > 0

  if(!any(matched)){
    message("Please make sure the input gene ID type is 'SYMBOL'")
    return(p)
  }
  value <- value[matched]
  positions <- positions[matched]
  if (!is.null(mapping_table)) {
    mapping_table <- mapping_table[mapping_table$symbol %in% names(value), , drop = FALSE]
  }
  
  colorbar <- colorb(value, low, high)
  color <- colorbar[order(value)]  
  
  genes <- names(value)
  
  for (i in seq_along(genes)) {
    p$svg <- replace_bg2(p$svg, positions[[i]], color[i])
  }
  
  if(legend){
    p$svg <- append_wp_legend(
      svg = p$svg,
      value = value,
      high = high,
      low = low,
      legend_x = legend_x,
      legend_y = legend_y
    )
  }
  p$geneExpr <- value
  p$mapping_table <- mapping_table
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
      pos <- find_gene_positions(p$svg, genes[i])
      p$svg <- svg_halos(p$svg, pos, genes[i])
  }
  
    p$svg <- inject_halo_style(p$svg, bg.col = bg.col, bg.r = bg.r)
  
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
