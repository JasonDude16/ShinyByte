# helper for displaying the variable along with its data type
get_var_name_class <- function(df) {
  if (length(df) > 0) {
    var_class <- unlist(purrr::map(df, class))
    vars <- names(var_class)
    labels <- paste0(names(var_class), " {", var_class, "}")
    names(vars) <- labels
    return(vars) 
  }
}

non_num_cols <- function(df) {
  get_var_name_class(purrr::keep(df, ~ is.factor(.x) | is.character(.x)))
}

non_char_cols <- function(df) {
  get_var_name_class(purrr::keep(df, ~ !is.character(.x)))
}

num_cols <- function(df) {
  get_var_name_class(purrr::keep(df, ~ is.numeric(.x)))
}

fctr_cols <- function(df) {
  get_var_name_class(purrr::keep(df, ~ is.factor(.x)))
}