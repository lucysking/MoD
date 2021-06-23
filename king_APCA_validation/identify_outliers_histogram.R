identify_outliers_hist = function(data, x) {
  x = enquo(x)
  
  data %>% 
    ggplot(aes(!!x)) +
    geom_histogram() +
    geom_vline(
      aes(
        xintercept = mean(
          !!x,
          na.rm = TRUE
        ) +
          (sd(
            !!x,
            na.rm = TRUE
          ) * 3)
      ),
      color = "red"
    ) +
    geom_vline(
      aes(
        xintercept = mean(
          !!x,
          na.rm = TRUE
        ) -
          (sd(
            !!x,
            na.rm = TRUE
          ) * 3)
      ),
      color = "red"
    )
}