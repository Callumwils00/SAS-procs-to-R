rename_object <- function(name_old, name_new){
  assign(name_new, name_old, pos = 1)
  return(eval(parse(text = substitute(name_new))))
}

write_R_view <- function(df, new_name, view_type){

  if(view_type == "R"){
    
    warning("Coercing ", paste(names(df)[sapply(df, class) %in% c("numeric", "integer", "factor")], collapse = ",")," to characters")
    
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
    if(grepl("[.]", paste0(names(df), collapse = ", "))){
      warning("Dataset names coercion occurred, special characters replaced with underscores")
    }
    names_of_column <- sub("[.]", "_", names(df))
    type_of_column <- unlist(strsplit(type_of_column, " "))
    
    input_types <- data.frame("names_of_column" = as.character(names_of_column), "column_types" = as.character(c(type_of_column)))
    
    input_types <- input_types %>% mutate(names_of_column = ifelse(trimws(column_types, which = "both") %in% c("character", "factor"), paste0(names_of_column, " $20."),
                                                    names_of_column))
    
    #return(input_types)
    
    data_for_SAS <- as.matrix(df)
    
    SAS_Data <- ""
    
    for(i in 1:nrow(data_for_SAS)){
      SAS_Data <- paste0(SAS_Data, paste0(paste0(data_for_SAS[i,], collapse = " "), "\n "))
    }
    
    view <- paste0(
      "data ",new_name, ";
          INPUT ", paste0(input_types$names_of_column, collapse = " "), ";",
          "datalines; \n",
           SAS_Data,
              "
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

write_R_view(airquality, "airquality_new", "R")

save(airquality_new ,file="outfile.RData")
