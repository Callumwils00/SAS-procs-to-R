
sas_freq <- function(data ,..., by_vars = NA, table = FALSE, as_list = "AS_TABLES"){
  if(is.data.frame(data) == FALSE){
    stop("Data must be a dataframe")
  }else if(!table %in% c(TRUE, FALSE)){
    stop("table argument must be TRUE or FALSE")
  }else if(!as_list %in% c("AS_TABLES", "AS_TAB", "AS_LIST")){
    stop("Invalid as_list option")
  }
  
  proc <- function(...){
    df <- data %>% dplyr::arrange(...) %>%
      group_by(...) %>% 
      dplyr::mutate(num = n()) %>% ungroup() %>%
      distinct(..., .keep_all = TRUE) %>%
      mutate(perc = num/sum(num),
             cumulative_perc = cumsum(num)/sum(num)) %>%
      dplyr::select(..., num, perc, cumulative_perc)
    if(table == TRUE){
      df <- df %>% gt
    }
    return(df)
  }
  
  if(as_list == "AS_TABLES"){
    if(table == FALSE){
      if(is.na(by_vars) == FALSE){
        byvars <- rlang::parse_exprs(by_vars)
        out <- map(byvars, proc)
      } else{
        out <- proc(...)
      }
    }
    }
    else{
      if(is.na(by_vars) == FALSE){
        out <- map(map(byvars, proc),gt)
      } else{
        out <- gt(proc(...))
      }
    }
    #else if(as_list == "AS_TAB"){
      #temp <- proc(byvars) %>% select(!!!byvars, perc)
      #out <- temp %>% pivot_wider(names_from = (!!!eval(byvars[1])), values_from = perc,
      #        values_fill = 0)
    #} else{
      #out <- proc(byvars)
    #}
    return(out)
  }

sas_freq(mtcars, by_vars = "cyl;carb")
