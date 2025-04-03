#' @importFrom data.table data.table as.data.table dcast fread fwrite rbindlist uniqueN :=
#' @importFrom dplyr distinct filter group_by mutate pull select summarise n_distinct recode
#'                    rename arrange left_join
#' @importFrom ggplot2 aes geom_bar ggsave ggplot labs scale_fill_manual scale_x_continuous 
#'                    scale_y_continuous scale_y_log10 theme theme_minimal scale_fill_brewer
#'                    element_text element_rect element_line element_blank unit geom_line 
#'                    geom_point scale_color_brewer
#' @importFrom grid unit
#' @importFrom Matrix sparseMatrix summary t
#' @importFrom RSQLite dbClearResult dbConnect dbDisconnect dbExecute dbFetch
#'                     dbGetQuery dbHasCompleted dbListTables dbSendQuery
#'                     dbWriteTable SQLite dbExecute dbWriteTable
#' @importFrom scales alpha comma hue_pal
#' @importFrom stats na.omit reorder runif setNames var
#' @importFrom stringr str_squish str_replace str_remove str_detect
#' @importFrom utils capture.output head object.size tail read.csv
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom gridExtra grid.arrange
#' @importFrom ggtext element_textbox_simple
#' @importFrom knitr kable
#' @importFrom cowplot get_legend plot_grid
#' @importFrom RColorBrewer brewer.pal

NULL

#' Pipe
#'
#' Pipe an object forward into a function or call expression.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return Result of rhs applied to lhs, see details in magrittr package.
#' @export
NULL

#' Assignment pipe
#'
#' Pipe an object forward into a function or call expression and update the
#' `lhs` object with the resulting value.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %<>%
#' @name %<>%
#' @rdname compound
#' @param lhs An object which serves both as the initial value and as target.
#' @param rhs a function call using the magrittr semantics.
#' @return None, used to update the value of lhs.
#' @export
NULL

#' Exposition pipe
#'
#' Expose the names in `lhs` to the `rhs` expression.
#' Magrittr imported function, see details and examples in the magrittr package.
#'
#' @importFrom magrittr %$%
#' @name %$%
#' @rdname exposition
#' @param lhs A list, environment, or a data.frame.
#' @param rhs An expression where the names in lhs is available.
#' @return Result of rhs applied to one or several names of lhs.
#' @export
NULL



