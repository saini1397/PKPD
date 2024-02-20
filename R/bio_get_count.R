#' R function to get counts in a given group .
#'
#' @description
#' R function to get counts in a given group .
#'
#' @import shiny
#' @rdname bio_get_count

bio_get_count <- function(df, group_var, var) {
  group_var <- enquo(group_var) # need to quote

  df %>%
    group_by(!!group_var) %>%
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    ungroup()
}
