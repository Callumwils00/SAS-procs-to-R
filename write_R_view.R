rename_object <- function(name_old, name_new){
  assign(name_new, name_old, pos = 1)
  return(eval(parse(text = substitute(name_new))))
}

write_R_view <- function(df, new_name, view_type){

  if(view_type == "R"){
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
  
   
    
  }else if(view_type == "SAS"){
    #column_names <- names(df)
    type_of_column <- paste0(sapply(df, class), collapse = " ")
    type_of_column <- paste0(gsub("character | character|character", "character $20. ", type_of_column), ";")
    
    data_for_SAS <- as.matrix(df)
    
    SAS_Data <- ""
    
    for(i in 1:nrow(data_for_SAS)){
      SAS_Data <- paste0(SAS_Data, paste0(paste0(data_for_SAS[i,], collapse = " "), "/n "))
    }
    
    view <- paste0(
      "data ",new_name, ";
          INPUT ", type_of_column,
          "datalines;",
          SAS_Data,
              ";
       run;")
    #return(SAS_Code)
    
  }
  
  rename_object <- function(name_old, name_new){
    assign(name_new, name_old, pos = 1)
    return(eval(parse(text = substitute(name_new))))
  }
  
  rename_object(view, new_name)
  
  return(eval(parse_expr(new_name)))
}
write_R_view(iris, "iris_new", "SAS")
cat(iris_new, file = "C:\\Users\\wilsonc\\Documents\\SAS_procs_to_R\\test.txt")





iris_new
iris_new
iris <- eval(parse(text=iris_new))
iris
