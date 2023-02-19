if(!require(tidyr)){library(tidyr, quietly = TRUE)}
if(!require(tidyr)){library(dplyr, quietly = TRUE)}
if(!require(stringr)){library(stringr, quietly = TRUE)}
if(!require(gt)){library(gt, quietly = TRUE)}

sas_mean <- function(data, vars = everything()){
  df <- data %>% select({{vars}}) %>% summarise_if(is.numeric, list(mean = mean, min = min, max = max,
                                                                    sd = sd), na.rm = TRUE)
  
  mean_ <- df %>% select(ends_with("mean")) %>% pivot_longer(cols = everything(),
                                                           names_to = "var",
                                                           values_to = "mean_") %>% 
    mutate(across("var", str_replace, "_mean", ""))

  min_ <- df %>% select(ends_with("min"))%>% pivot_longer(cols = everything(),
                                                        names_to = "var",
                                                        values_to = "min_") %>% 
    mutate(across("var", str_replace, "_min", ""))

  max_ <- df %>% select(ends_with("max"))%>% pivot_longer(cols = everything(),
                                                        names_to = "var",
                                                        values_to = "max_") %>% 
    mutate(across("var", str_replace, "_max", ""))
  sd_ <- df %>% select(ends_with("sd")) %>% pivot_longer(cols = everything(),
                                                         names_to = "var",
                                                         values_to = "sd_") %>%
    mutate(across("var", str_replace, "_sd", ""))

  new_df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(mean_,min_,max_, sd_), accumulate=FALSE)
  return(new_df)
}
