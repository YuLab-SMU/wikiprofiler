##' @method print wpplot
##' @export
print.wpplot <- function(x, ...) {
    browseURL(svg2tempfile(x$svg))
}


##' @importFrom ggplotify as.grob
##' @method as.grob wpplot
##' @importFrom grid rasterGrob
as.grob.wpplot <- function(plot, ...) {
    f <- svg2tempfile(plot$svg)
    p <- rsvg::rsvg_nativeraster(f) 
    rasterGrob(p)
}
