# wikiprofiler 0.1.7

+ update `wp_bgfill()` and `wp_shadowtext()` to support current WikiPathways SVG format (2026-08-23, Sun)
+ use exact gene-label matching to avoid partial matches such as `ANAPC1`/`ANAPC10`
+ add `wp_map()` for ID-to-symbol mapping and node-level aggregation before rendering
+ add `wp_comparefill()` to visualize case-control differences or log2 ratios on pathways
+ add `wp_render()` for batch pathway rendering from IDs, data frames, or enrichment-like results

# wikiprofiler 0.1.5

+ update wikipathway URL and wpplot examples (2024-08-26, Mon)

# wikiprofiler 0.1.3

+ re-export `ggplot2::ggsave()` and it supports 'wpplot' object (2023-09-08, Fri)
+ `grid.draw()` method for 'wpplot' object (2023-09-08, Fri)
+ use `ggplotify::as.ggplot()` in `print()` and `wpsave()` (2023-09-07, Thu)

# wikiprofiler 0.1.2

+ on CRAN (2023-09-06, Wed)
+ `wpplot()`, `wp_bgfill()`, `wp_shadowtext()`, `wpsave()` and `read.wp()`
+ initial version since 2022-06-01
