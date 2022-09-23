##' parse wikipathway gmt file to a gson object
##'
##'
##' @title read.wp
##' @rdname read-wp
##' @param file wikipathway gmt file 
##' downloaded from 'https://wikipathways-data.wmcloud.org/current/gmt/'
##' @importFrom gson read.gmt.wp
##' @export
##' @return a 'gson' object
##' @author Guangchuang Yu
read.wp <- function(file) {
    gson::read.gmt.wp(file, output = "gson")
}

