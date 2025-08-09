feat_rm_zv <- function(df) {
  
  is.zv <- function(x) { length(unique(x)) == 1L } 

  df |> 
    dplyr::select(-tidyselect::where(is.zv))
}

if (FALSE) {
  set.seed(123)
  df <- tibble::tibble(
    a = 1, 
    b = 1:5, 
    c = sample(c(NA, 1), 5, replace = TRUE), 
    d = rep(NA, 5)
  )
  df |> feat_rm_zv() 
}


