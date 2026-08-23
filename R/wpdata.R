#' @title Map user values to pathway symbols
#' @description Normalize a named numeric vector or a data.frame into a
#' symbol-keyed numeric vector that can be used by `wp_bgfill()`.
#' @param data A named numeric vector or a data.frame.
#' @param value_col Value column name when `data` is a data.frame.
#' @param symbol_col Symbol column name when `data` already contains gene symbols.
#' @param id_col Identifier column name when `data` needs ID mapping.
#' @param mapping Optional ID-to-symbol mapping. It can be a named character
#' vector or a two-column data.frame.
#' @param mapping_from Source ID column in `mapping` when `mapping` is a data.frame.
#' @param mapping_to Target symbol column in `mapping` when `mapping` is a data.frame.
#' @param aggregator Aggregation rule used when multiple rows map to the same symbol.
#' @param na.rm Whether to remove `NA` values before aggregation.
#' @return A named numeric vector keyed by gene symbol.
#' @export
wp_map <- function(data,
                   value_col = NULL,
                   symbol_col = NULL,
                   id_col = NULL,
                   mapping = NULL,
                   mapping_from = NULL,
                   mapping_to = NULL,
                   aggregator = c("mean", "median", "max_abs", "first", "sum"),
                   na.rm = TRUE) {
  aggregator <- match.arg(aggregator)
  prepared <- prepare_wp_mapping_input(
    data = data,
    value_col = value_col,
    symbol_col = symbol_col,
    id_col = id_col,
    mapping = mapping,
    mapping_from = mapping_from,
    mapping_to = mapping_to
  )

  if (!nrow(prepared)) {
    return(stats::setNames(numeric(), character()))
  }

  aggregated <- aggregate_wp_mapping(prepared, aggregator = aggregator, na.rm = na.rm)
  values <- stats::setNames(aggregated$value, aggregated$symbol)

  prepared$aggregated_value <- aggregated$value[match(prepared$symbol, aggregated$symbol)]
  attr(values, "mapping_table") <- prepared
  attr(values, "aggregator") <- aggregator
  values
}

#' @title Visualize pathway differences between two conditions
#' @description Compute pathway-level comparison values from two symbol-keyed
#' numeric vectors and render them through `wp_bgfill()`.
#' @param p A `wpplot` object.
#' @param value Case-condition values.
#' @param control Control-condition values.
#' @param mode Comparison mode: direct difference or log2 ratio.
#' @param pseudocount Added before ratio calculation when `mode = "log2_ratio"`.
#' @param low Low-end color passed to `wp_bgfill()`.
#' @param high High-end color passed to `wp_bgfill()`.
#' @param legend Whether to draw the legend.
#' @param legend_x Horizontal position of the legend.
#' @param legend_y Vertical position of the legend.
#' @return A `wpplot` object.
#' @export
wp_comparefill <- function(p,
                           value,
                           control,
                           mode = c("difference", "log2_ratio"),
                           pseudocount = 1,
                           high = "red",
                           low = "blue",
                           legend = TRUE,
                           legend_x = 0.001,
                           legend_y = 0.94) {
  mode <- match.arg(mode)
  value <- normalize_wp_vector(value, arg = "value")
  control <- normalize_wp_vector(control, arg = "control")

  genes <- intersect(names(value), names(control))
  if (!length(genes)) {
    message("No overlapping symbols were found between 'value' and 'control'.")
    return(p)
  }

  case_values <- value[genes]
  control_values <- control[genes]
  comparison_values <- switch(
    mode,
    difference = case_values - control_values,
    log2_ratio = log2((case_values + pseudocount) / (control_values + pseudocount))
  )

  comparison_values <- stats::setNames(as.numeric(comparison_values), genes)
  comparison_table <- data.frame(
    symbol = genes,
    case = as.numeric(case_values),
    control = as.numeric(control_values),
    comparison = as.numeric(comparison_values),
    stringsAsFactors = FALSE
  )
  attr(comparison_values, "mapping_table") <- comparison_table

  p <- wp_bgfill(
    p = p,
    value = comparison_values,
    high = high,
    low = low,
    legend = legend,
    legend_x = legend_x,
    legend_y = legend_y
  )

  attr(comparison_table, "mode") <- mode
  attr(comparison_table, "pseudocount") <- pseudocount
  p$comparison <- comparison_table
  p
}

#' @title Render one or more WikiPathways entries
#' @description Render pathway IDs directly from a character vector, a result
#' table, or an enrichment-like S4 object with a `result` slot.
#' @param pathway Pathway IDs, a data.frame, or an enrichment-like S4 object.
#' @param value Optional symbol-keyed values for single-condition rendering.
#' @param control Optional symbol-keyed control values for comparison rendering.
#' @param n Maximum number of pathway IDs to render.
#' @param id_col Column name that stores pathway IDs.
#' @param dir Optional output directory. When provided, PNG files are written there.
#' @param width Width passed to `wpsave()`.
#' @param height Height passed to `wpsave()`.
#' @param shadowtext Whether to add halo text after fill rendering.
#' @param bg.r Halo width passed to `wp_shadowtext()`.
#' @param bg.col Halo color passed to `wp_shadowtext()`.
#' @param mode Comparison mode passed to `wp_comparefill()`.
#' @param pseudocount Pseudocount passed to `wp_comparefill()`.
#' @param high High-end color passed to fill functions.
#' @param low Low-end color passed to fill functions.
#' @param legend Whether to draw the legend.
#' @param legend_x Horizontal position of the legend.
#' @param legend_y Vertical position of the legend.
#' @return A named list of `wpplot` objects.
#' @export
wp_render <- function(pathway,
                      value = NULL,
                      control = NULL,
                      n = NULL,
                      id_col = "ID",
                      dir = NULL,
                      width = NULL,
                      height = NULL,
                      shadowtext = FALSE,
                      bg.r = 2,
                      bg.col = "white",
                      mode = c("difference", "log2_ratio"),
                      pseudocount = 1,
                      high = "red",
                      low = "blue",
                      legend = TRUE,
                      legend_x = 0.001,
                      legend_y = 0.94) {
  mode <- match.arg(mode)
  ids <- extract_wp_ids(pathway, id_col = id_col, n = n)
  plots <- vector("list", length(ids))
  names(plots) <- ids

  if (!is.null(dir) && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  for (i in seq_along(ids)) {
    p <- wpplot(ids[i])
    if (!is.null(control)) {
      p <- wp_comparefill(
        p = p,
        value = value,
        control = control,
        mode = mode,
        pseudocount = pseudocount,
        high = high,
        low = low,
        legend = legend,
        legend_x = legend_x,
        legend_y = legend_y
      )
    } else if (!is.null(value)) {
      p <- wp_bgfill(
        p = p,
        value = value,
        high = high,
        low = low,
        legend = legend,
        legend_x = legend_x,
        legend_y = legend_y
      )
    }

    if (shadowtext && !is.null(p$geneExpr)) {
      p <- wp_shadowtext(p, bg.r = bg.r, bg.col = bg.col)
    }

    if (!is.null(dir)) {
      wpsave(
        p = p,
        file = file.path(dir, paste0(ids[i], ".png")),
        width = width,
        height = height
      )
    }

    plots[[i]] <- p
  }

  plots
}

prepare_wp_mapping_input <- function(data,
                                     value_col = NULL,
                                     symbol_col = NULL,
                                     id_col = NULL,
                                     mapping = NULL,
                                     mapping_from = NULL,
                                     mapping_to = NULL) {
  if (is.data.frame(data)) {
    if (is.null(value_col) || !value_col %in% names(data)) {
      stop("'value_col' must refer to an existing column in 'data'.")
    }
    values <- suppressWarnings(as.numeric(data[[value_col]]))
    if (!is.null(symbol_col)) {
      if (!symbol_col %in% names(data)) {
        stop("'symbol_col' must refer to an existing column in 'data'.")
      }
      symbols <- as.character(data[[symbol_col]])
      input_ids <- if (!is.null(id_col) && id_col %in% names(data)) {
        as.character(data[[id_col]])
      } else {
        symbols
      }
    } else {
      if (is.null(id_col) || !id_col %in% names(data)) {
        stop("Provide 'symbol_col' or a valid 'id_col' together with 'mapping'.")
      }
      map_table <- normalize_wp_mapping(mapping, mapping_from, mapping_to)
      input_ids <- as.character(data[[id_col]])
      symbols <- map_table$symbol[match(input_ids, map_table$input_id)]
    }
  } else if (is.numeric(data)) {
    if (is.null(names(data))) {
      stop("Named numeric vectors are required when 'data' is not a data.frame.")
    }
    values <- as.numeric(data)
    input_ids <- names(data)
    if (is.null(mapping)) {
      symbols <- input_ids
    } else {
      map_table <- normalize_wp_mapping(mapping, mapping_from, mapping_to)
      symbols <- map_table$symbol[match(input_ids, map_table$input_id)]
    }
  } else {
    stop("'data' must be a named numeric vector or a data.frame.")
  }

  table <- data.frame(
    input_id = as.character(input_ids),
    symbol = as.character(symbols),
    value = as.numeric(values),
    stringsAsFactors = FALSE
  )

  keep <- !is.na(table$value) &
    !is.na(table$symbol) &
    nzchar(table$symbol)
  table[keep, , drop = FALSE]
}

normalize_wp_mapping <- function(mapping, mapping_from = NULL, mapping_to = NULL) {
  if (is.null(mapping)) {
    stop("'mapping' is required when IDs need to be converted to symbols.")
  }

  if (is.data.frame(mapping)) {
    if (ncol(mapping) < 2) {
      stop("'mapping' data.frame must contain at least two columns.")
    }
    if (is.null(mapping_from)) {
      mapping_from <- names(mapping)[1]
    }
    if (is.null(mapping_to)) {
      mapping_to <- names(mapping)[2]
    }
    if (!all(c(mapping_from, mapping_to) %in% names(mapping))) {
      stop("'mapping_from' and 'mapping_to' must exist in 'mapping'.")
    }
    map_table <- data.frame(
      input_id = as.character(mapping[[mapping_from]]),
      symbol = as.character(mapping[[mapping_to]]),
      stringsAsFactors = FALSE
    )
  } else if (is.vector(mapping) && !is.null(names(mapping))) {
    map_table <- data.frame(
      input_id = names(mapping),
      symbol = as.character(unname(mapping)),
      stringsAsFactors = FALSE
    )
  } else {
    stop("'mapping' must be a named vector or a data.frame.")
  }

  keep <- !is.na(map_table$input_id) &
    nzchar(map_table$input_id) &
    !is.na(map_table$symbol) &
    nzchar(map_table$symbol)
  map_table <- map_table[keep, , drop = FALSE]
  map_table[!duplicated(map_table$input_id), , drop = FALSE]
}

aggregate_wp_mapping <- function(mapping_table, aggregator = "mean", na.rm = TRUE) {
  split_values <- split(mapping_table$value, mapping_table$symbol)
  aggregated <- vapply(
    split_values,
    aggregate_wp_values,
    numeric(1),
    aggregator = aggregator,
    na.rm = na.rm
  )

  data.frame(
    symbol = names(aggregated),
    value = as.numeric(aggregated),
    stringsAsFactors = FALSE
  )
}

aggregate_wp_values <- function(x, aggregator = "mean", na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (!length(x)) {
    return(NA_real_)
  }

  switch(
    aggregator,
    mean = mean(x),
    median = stats::median(x),
    max_abs = x[which.max(abs(x))[1]],
    first = x[1],
    sum = sum(x),
    stop("Unsupported aggregator: ", aggregator)
  )
}

normalize_wp_vector <- function(x, arg = "value") {
  if (!is.numeric(x) || is.null(names(x))) {
    stop("'", arg, "' must be a named numeric vector.")
  }

  x <- stats::setNames(as.numeric(x), as.character(names(x)))
  keep <- !is.na(x) & !is.na(names(x)) & nzchar(names(x))
  x[keep]
}

extract_wp_ids <- function(pathway, id_col = "ID", n = NULL) {
  has_result_slot <- FALSE
  slot_names <- tryCatch(
    methods::slotNames(pathway),
    error = function(e) character()
  )
  if (length(slot_names)) {
    has_result_slot <- "result" %in% slot_names
  }

  if (is.character(pathway)) {
    ids <- pathway
  } else if (is.data.frame(pathway)) {
    if (!id_col %in% names(pathway)) {
      stop("'", id_col, "' column was not found in 'pathway'.")
    }
    ids <- pathway[[id_col]]
  } else if (has_result_slot) {
    result <- methods::slot(pathway, "result")
    if (!is.data.frame(result) || !id_col %in% names(result)) {
      stop("The S4 object's 'result' slot must contain an '", id_col, "' column.")
    }
    ids <- result[[id_col]]
  } else {
    stop("'pathway' must be a character vector, data.frame, or enrichment-like S4 object.")
  }

  ids <- unique(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!is.null(n)) {
    ids <- head(ids, n)
  }
  ids
}

append_svg_elements <- function(svg, elements) {
  end <- grep("</svg", svg, fixed = TRUE)[1]
  if (is.na(end)) {
    return(c(svg, elements))
  }

  append(svg, elements, after = end - 1)
}

append_wp_legend <- function(svg,
                             value,
                             high = "red",
                             low = "blue",
                             legend_x = 0.001,
                             legend_y = 0.94) {
  dims <- svg_dimensions(svg)
  svg_width <- dims[["width"]]
  svg_height <- dims[["height"]]

  incrementX <- svg_width * legend_x
  incrementY <- svg_height * (1 - legend_y)
  if (incrementX > svg_width - 48) {
    incrementX <- svg_width - 48
  }

  if (incrementY > svg_height - 122) {
    incrementY <- svg_height - 122
  } else if (incrementY < 3) {
    incrementY <- 3
  }

  textele <- rev(pretty(value, 4))
  textX <- 40 + incrementX
  textY <- seq(from = 5, to = 120, length.out = length(textele)) + incrementY
  scalelineX <- 27 + incrementX
  scalelineY <- seq(from = 2, to = 118, length.out = length(textele)) + incrementY

  zero_scale_line <- find_zero_scale(value)
  proportion <- seq(from = 2, to = 118, length.out = length(textele)) / 120
  proportion <- proportion[length(which(pretty(value, 4) >= zero_scale_line))]
  if (max(pretty(value, 4)) == 0) {
    proportion <- "0%"
  }
  if (min(pretty(value, 4)) == 0) {
    proportion <- "100%"
  }

  elements <- c(
    paste(
      "<defs><linearGradient id=\"grad1\" x1=\"0%\" y1=\"0%\" x2=\"0%\" y2=\"100%\">",
      "<stop offset=\"0%\" style=\"stop-color:", high, ";stop-opacity:1\"></stop>",
      "<stop offset=\"", proportion, "\" style=\"stop-color:white;stop-opacity:1\"></stop>",
      "<stop offset=\"100%\" style=\"stop-color:", low, ";stop-opacity:1\"></stop>",
      "</linearGradient></defs>",
      "<rect x=\"", incrementX, "\" y=\"", incrementY,
      "\" width=\"30\" height=\"120\" style=\"fill:url(#grad1 );stroke-width:0;stroke:black\"></rect>",
      sep = ""
    ),
    paste(
      "<text x=\"", textX,
      "\" y=\"", textY,
      "\" style=\"font-size:10; fill:black; stroke:none\">",
      textele,
      "</text>",
      sep = ""
    ),
    paste(
      "<rect width=\"3\" height=\"1\" x=\"", scalelineX,
      "\" y=\"", scalelineY,
      "\" style=\"fill:white; stroke:none\"></rect>",
      sep = ""
    )
  )

  append_svg_elements(svg, elements)
}
