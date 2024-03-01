read_view <- function(path){
  if(grepl("\\.RDATA$|\\.RDS$", toupper(path))){
    view <- get(load(path))
  }else{
    
    ## Code to run SAS code on command line and temporarily store the sas7bdat to read in
  }
  data_set <- eval(parse(text = view))
  data_set
}