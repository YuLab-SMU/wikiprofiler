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
#' @param name_col Optional column name used for output file naming when
#' `pathway` is a data.frame or an enrichment-like result table.
#' @param dir Optional output directory. When provided, PNG files are written there.
#' @param file_ext Output file extension(s) passed to `wpsave()`.
#' @param filename_template Output filename template. Supported placeholders:
#' `{index}`, `{id}`, and `{name}`.
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
                      name_col = NULL,
                      dir = NULL,
                      file_ext = "png",
                      filename_template = "{id}",
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
  targets <- extract_wp_targets(
    pathway = pathway,
    id_col = id_col,
    name_col = name_col,
    n = n
  )
  file_ext <- unique(as.character(file_ext))
  plots <- vector("list", nrow(targets))
  names(plots) <- targets$id

  if (!is.null(dir) && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  for (i in seq_len(nrow(targets))) {
    p <- wpplot(targets$id[i])
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
      output_stub <- build_wp_output_stub(
        index = targets$index[i],
        id = targets$id[i],
        name = targets$name[i],
        filename_template = filename_template
      )
      for (ext in file_ext) {
        wpsave(
          p = p,
          file = file.path(dir, paste0(output_stub, ".", ext)),
          width = width,
          height = height
        )
      }
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
  extract_wp_targets(pathway = pathway, id_col = id_col, n = n)$id
}

extract_wp_targets <- function(pathway, id_col = "ID", name_col = NULL, n = NULL) {
  has_result_slot <- FALSE
  slot_names <- tryCatch(
    methods::slotNames(pathway),
    error = function(e) character()
  )
  if (length(slot_names)) {
    has_result_slot <- "result" %in% slot_names
  }

  if (is.character(pathway)) {
    table <- data.frame(
      id = as.character(pathway),
      name = as.character(pathway),
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(pathway)) {
    if (!id_col %in% names(pathway)) {
      stop("'", id_col, "' column was not found in 'pathway'.")
    }
    table <- data.frame(
      id = as.character(pathway[[id_col]]),
      name = extract_wp_target_names(pathway, name_col = name_col, fallback = pathway[[id_col]]),
      stringsAsFactors = FALSE
    )
  } else if (has_result_slot) {
    result <- methods::slot(pathway, "result")
    if (!is.data.frame(result) || !id_col %in% names(result)) {
      stop("The S4 object's 'result' slot must contain an '", id_col, "' column.")
    }
    table <- data.frame(
      id = as.character(result[[id_col]]),
      name = extract_wp_target_names(result, name_col = name_col, fallback = result[[id_col]]),
      stringsAsFactors = FALSE
    )
  } else {
    stop("'pathway' must be a character vector, data.frame, or enrichment-like S4 object.")
  }

  keep <- !is.na(table$id) & nzchar(table$id)
  table <- table[keep, , drop = FALSE]
  table <- table[!duplicated(table$id), , drop = FALSE]
  if (!is.null(n)) {
    table <- head(table, n)
  }
  table$index <- seq_len(nrow(table))
  table
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
  breaks <- resolve_wp_legend_breaks(value)
  layout <- resolve_wp_legend_layout(
    svg = svg,
    labels = breaks$labels,
    legend_x = legend_x,
    legend_y = legend_y
  )
  elements <- c(
    build_wp_legend_gradient(
      x = layout$legend_x,
      y = layout$legend_y,
      proportion = breaks$proportion,
      high = high,
      low = low
    ),
    build_wp_legend_labels(
      x = layout$text_x,
      y = breaks$text_y + layout$legend_y,
      labels = breaks$labels
    ),
    build_wp_legend_ticks(
      x = layout$scaleline_x,
      y = breaks$scaleline_y + layout$legend_y
    )
  )

  append_svg_elements(svg, elements)
}

extract_wp_target_names <- function(data, name_col = NULL, fallback) {
  if (!is.null(name_col)) {
    if (!name_col %in% names(data)) {
      stop("'", name_col, "' column was not found in pathway data.")
    }
    labels <- data[[name_col]]
  } else {
    labels <- fallback
  }

  labels <- as.character(labels)
  labels[is.na(labels) | !nzchar(labels)] <- as.character(fallback)[is.na(labels) | !nzchar(labels)]
  labels
}

build_wp_output_stub <- function(index, id, name, filename_template = "{id}") {
  output_name <- filename_template
  output_name <- gsub("\\{index\\}", as.character(index), output_name)
  output_name <- gsub("\\{id\\}", as.character(id), output_name)
  output_name <- gsub("\\{name\\}", as.character(name), output_name)
  sanitize_wp_filename(output_name)
}

sanitize_wp_filename <- function(x) {
  x <- gsub("[\\\\/:*?\"<>|]+", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("_+", "_", x)
  x <- sub("^_+", "", x)
  x <- sub("_+$", "", x)
  if (!nzchar(x)) {
    return("wpplot")
  }
  x
}

resolve_wp_legend_layout <- function(svg,
                                     labels,
                                     legend_x = 0.001,
                                     legend_y = 0.94) {
  dims <- svg_user_dimensions(svg)
  svg_width <- dims[["width"]]
  svg_height <- dims[["height"]]
  label_width <- estimate_wp_label_width(labels)
  legend_width <- 30 + 10 + 3 + 10 + label_width

  incrementX <- svg_width * legend_x
  incrementY <- svg_height * (1 - legend_y)
  if (incrementX > svg_width - legend_width) {
    incrementX <- svg_width - legend_width
  }
  if (incrementY > svg_height - 122) {
    incrementY <- svg_height - 122
  } else if (incrementY < 3) {
    incrementY <- 3
  }

  list(
    legend_x = incrementX,
    legend_y = incrementY,
    text_x = 40 + incrementX,
    scaleline_x = 27 + incrementX
  )
}

estimate_wp_label_width <- function(labels) {
  if (!length(labels)) {
    return(48)
  }

  max(48, max(nchar(as.character(labels)), na.rm = TRUE) * 7)
}

resolve_wp_legend_breaks <- function(value) {
  labels <- rev(pretty(value, 4))
  text_y <- seq(from = 5, to = 120, length.out = length(labels))
  scaleline_y <- seq(from = 2, to = 118, length.out = length(labels))

  zero_scale_line <- find_zero_scale(value)
  proportion <- seq(from = 2, to = 118, length.out = length(labels)) / 120
  proportion <- proportion[length(which(pretty(value, 4) >= zero_scale_line))]
  if (max(pretty(value, 4)) == 0) {
    proportion <- "0%"
  }
  if (min(pretty(value, 4)) == 0) {
    proportion <- "100%"
  }

  list(
    labels = labels,
    text_y = text_y,
    scaleline_y = scaleline_y,
    proportion = proportion
  )
}

build_wp_legend_gradient <- function(x, y, proportion, high = "red", low = "blue") {
  paste(
    "<defs><linearGradient id=\"grad1\" x1=\"0%\" y1=\"0%\" x2=\"0%\" y2=\"100%\">",
    "<stop offset=\"0%\" style=\"stop-color:", high, ";stop-opacity:1\"></stop>",
    "<stop offset=\"", proportion, "\" style=\"stop-color:white;stop-opacity:1\"></stop>",
    "<stop offset=\"100%\" style=\"stop-color:", low, ";stop-opacity:1\"></stop>",
    "</linearGradient></defs>",
    "<rect x=\"", x, "\" y=\"", y,
    "\" width=\"30\" height=\"120\" style=\"fill:url(#grad1 );stroke-width:0;stroke:black\"></rect>",
    sep = ""
  )
}

build_wp_legend_labels <- function(x, y, labels) {
  paste(
    "<text x=\"", x,
    "\" y=\"", y,
    "\" style=\"font-size:10; fill:black; stroke:none\">",
    labels,
    "</text>",
    sep = ""
  )
}

build_wp_legend_ticks <- function(x, y) {
  paste(
    "<rect width=\"3\" height=\"1\" x=\"", x,
    "\" y=\"", y,
    "\" style=\"fill:white; stroke:none\"></rect>",
    sep = ""
  )
}
