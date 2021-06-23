winsorize <- function(x, product) {
  case_when(
    x > mean(x, na.rm = TRUE) + product * sd(x, na.rm = TRUE) ~
      mean(x, na.rm = TRUE) + product * sd(x, na.rm = TRUE),
    
    x < mean(x, na.rm = TRUE) - product * sd(x, na.rm = TRUE) ~
      mean(x, na.rm = TRUE) - product * sd(x, na.rm = TRUE),
    
    TRUE ~ x 
  )
}
