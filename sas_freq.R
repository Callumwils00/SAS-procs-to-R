if(!require(tidyverse)){library(tidyverse, quietly = TRUE)}
if(!require(dplyr)){library(dplyr, quietly = TRUE)}
if(!require(rlang)){library(rlang, quietLy = TRUE)}
if(!require(gt)){library(gt, quietly = TRUE)}


sas_freq <- function(data , by_vars, table = FALSE, as_list = "AS_TABLES"){
  if(is.data.frame(data) == FALSE){
    stop("Data must be a dataframe")
  }else if(table %ni% c(TRUE, FALSE)){
    stop("table argument must be TRUE or FALSE")
  }else if(as_list %ni% c("AS_TABLES", "AS_TAB", "AS_LIST")){
    stop("Invalid as_list option")
  }
  
  byvars <- rlang::parse_exprs(by_vars)
  
  proc <- function(var){
    df <- data %>% dplyr::arrange(!!!var) %>%
      group_by(!!!var) %>% 
      dplyr::mutate(num = n()) %>% ungroup() %>%
      mutate(perc = num/sum(num),
             cumulative_perc = cumsum(num)/sum(num)) %>%
      distinct(!!!var, .keep_all = TRUE) %>%
      dplyr::select(!!!var, num, perc, cumulative_perc)
    if(table == TRUE){
      df <- df %>% gt
    }
    return(df)
  }
  
  if(as_list == "AS_TABLES"){
    if(table == FALSE){
      out <- map(byvars, proc)}
    else{
      out <- map(map(byvars, proc), gt)
    }
  } else if(as_list == "AS_TAB"){
    temp <- proc(byvars) %>% select(!!!byvars, perc)
    out <- temp %>% pivot_wider(names_from = (!!!eval(byvars[1])), values_from = perc,
                                values_fill = 0)
  } else{
    out <- proc(byvars)
  }
  return(out)
}
