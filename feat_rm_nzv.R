feat_rm_nzv <- function(
    x,
    max_frequency_ratio = 19,
    max_uniqueness_ratio = 10
) {
  
  is.nzv <- function(
    x, 
    max_frequency_ratio, 
    max_uniqueness_ratio
  ) {
    
    tb <- table(x)
    
    first_mode <- max(tb)
    second_mode <- max(tb[tb != max(tb)])
    frequency_ratio <- first_mode / second_mode
    excessive_frequency_ratio <- frequency_ratio > max_frequency_ratio
    
    n_unique <- length(tb)
    n_total <- sum(tb)
    uniqueness_ratio <- n_total / n_unique
    excessive_uniqueness_ratio <- uniqueness_ratio > max_uniqueness_ratio
    
    excessive_frequency_ratio && excessive_uniqueness_ratio
  }
  
  x |> 
    dplyr::select(-tidyselect::where(function(x) { 
      is.nzv(
        x = x, 
        max_frequency_ratio = max_frequency_ratio, 
        max_uniqueness_ratio = max_uniqueness_ratio
      ) 
    } ))
  
}

if (FALSE) {
  set.seed(123)
  df <- tibble::tibble(
    a = round(rnorm(100 * 5), 2), # FALSE TRUE
    b = c(rep(1, 95 * 5), rep(2, 5 * 5)),  # FALSE FALSE
    c = c(rep(1, 96 * 5), rep(2, 4 * 5)) # TRUE TRUE
  )
  df |> feat_rm_nzv() 
}


