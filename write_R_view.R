write_R_view <- function(df,filepath){
  #return(substitute(df))
  list_of_cols <- setNames(split(as.matrix(df), col(df)), names(df))
  #list_of_cols
  #view <- as.list(c(names(list_of_cols)))
  view <- paste('data.frame(',
                paste(paste0(names(list_of_cols), ' = ',
                             as.list(paste0('c(', sapply(list_of_cols,function(x)paste0("'", x, "'", collapse = ",")),
                                                         #function(x) ifelse(is.numeric(x), paste0(x, collapse=', '), paste0(paste0('"', list_of_cols, '"'),collapse=', '))), ')')),
                             #collapse = ', '),
                ')'))), collapse = ","), ")")
  #eval(substitute(string), df)
  
  save(view, file = filepath)
}

