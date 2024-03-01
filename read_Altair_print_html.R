read_Altair_print_html <- function(x){
  
  df_n <- function(x){
    x <- read_html(x) %>% html_elements("table th") %>% html_text()
    x
  }
  
  names_x <- df_n(x)
  
  df_d <- function(x){
    y <- read_html(x) %>% html_elements("table td") %>% html_text()#.,xpath ="//table//td")
    y
  }
  
  y <- df_d(x)
  y <- matrix(c(y)[2:length(c(y))], nrow = length(c(names_x)))
  transposed_data <- as.data.frame(t(y))
  names(transposed_data) <- c(names_x)
  transposed_data
}


