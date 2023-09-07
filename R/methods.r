
##' @method print wpplot
##' @import utils
##' @export
print.wpplot <- function(x, ...) {
  #if (Sys.getenv("TERM_PROGRAM") == "vscode") {
  #  p <- ggplotify::as.ggplot(x)
  #  print(p)
  #} else {
  #    browseURL(svg2tempfile(x$svg))
  #}

  print(ggplotify::as.ggplot(x))

}


##' @importFrom ggplotify as.grob
##' @method as.grob wpplot
##' @importFrom grid rasterGrob
##' @export
as.grob.wpplot <- function(plot, ...) {
  f <- svg2tempfile(plot$svg)
  p <- rsvg::rsvg_nativeraster(f)
  rasterGrob(p)
}
