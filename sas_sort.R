##sas_sort function:

# This Function sorts a dataframe in a similar way to how a sas dataset
# is sorted using a proc sort, returning first. and last. columns. 
# The first argument to the function is the dataframe, followed by
# the sorting "BY" variables. The by variables are enclosed in quotation marks and 
# seperated by a semicolon. eg. "var1 ; var2" or "desc(var1) ; var2".
#
# A nodots argument specifies whether the firstdot and lastdot columns should be added
# to the dataframe.
#
# The showdots argument specifies whether the firstdot and lastdot columns should be added to the 
# table.
#
# The nodupout argument subsets the dataframe so that duplicates of the
# dataframe on the "BY" variables are removed.
#
# The title argument allows the user to add a title.


if(!require(tidyverse)){library(tidyverse, quietly = TRUE)}
if(!require(rlang)){library(rlang, quietly = TRUE)}

sas_sort <- function(data, by_vars, nodupout = FALSE){
  
  arrange_expr <- rlang::parse_exprs(by_vars)
  if(!nodupout %in% c(TRUE, FALSE)){
    stop("nodupout must be either TRUE or FALSE")
  } else if(nodupout == TRUE){
    data <- data %>% distinct(!!!arrange_expr, .keep_all = TRUE)
  } else{
    invisible()
  }
  
  proc <- function(var, data){
    ds <- data %>% arrange(!!!arrange_expr) %>% group_by(!!!var, .add = TRUE) %>%
      group_modify(~{
        .x %>% mutate("first.{{var}}" := ifelse(row_number() == 1, 1, 0),
                      "last.{{var}}" := ifelse(row_number() == n(), 1, 0))})
  }
  
  for(i in arrange_expr){
    data <- proc(i, data)
  }
  data <- data %>% arrange(!!!arrange_expr)
  return(data)
}






