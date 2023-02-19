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
if(!require(gt)){library(gt, quietly = TRUE)}
if(!require(janitor)){library(janitor, quietly =TRUE )}

`%ni%` <- Negate(`%in%`)

sas_sort <- function(data, by_vars, showdots = FALSE, nodupout = FALSE, title = "", obs){
  
  if(showdots %ni% c(FALSE,TRUE)){
    stop("showdots must be TRUE or FALSE")
  } else if(nodupout %ni% c(FALSE,TRUE)){
    stop("nodupout must be TRUE or FALSE")
  } else{
  }
  
  arrange_expr <- rlang::parse_exprs(by_vars)
  if(nodupout == TRUE){
    data <- data %>% distinct(!!!arrange_expr, .keep_all = TRUE)
  }else{
  }
  ds <- data %>% 
    dplyr::arrange(!!!arrange_expr[1]) %>% group_by(!!!arrange_expr[1]) %>%
    group_modify(~ {
    .x %>% mutate("firstdot" := ifelse(row_number() == 1, 1, 0),
                  "lastdot" := ifelse(row_number() == n(), 1, 0))}) %>%
    ungroup() %>% group_by(!!!arrange_expr[2]) %>% group_modify(~ {
      .x %>% mutate("firstdot_2" := ifelse(row_number() == 1, 1, 0),
                    "lastdot_2" := ifelse(row_number() == n(), 1, 0))}) %>%
      select(names(data), firstdot, lastdot, firstdot_2, lastdot_2)
  
  if(showdots == FALSE){
  gt_table <- ds %>% slice(1:obs) %>% gt() %>% cols_hide(c(firstdot, lastdot)) %>%
    tab_header(title = md(title))
  }
  else{
    gt_table <- ds %>% slice(1:obs) %>% gt() %>% tab_header(title = md(title))
  }
  out <- list(ds, gt_table)
  return(out)
}






