sas_contents <- function(df){
  do.call(rbind, lapply(df, function(x)
    data.frame(
      Type = class(x)[1],
      Min = if(is.numeric(x)) min(x, na.rm=TRUE) else NA,
      Mean = if(is.numeric(x)) mean(x, na.rm = TRUE) else NA,
      Median = if(is.numeric(x)) median(x, na.rm = TRUE) else NA,
      Max = if(is.numeric(x)) max(x, na.rm = TRUE) else NA,
      Missing = sum(is.na(x))
    )))
}

