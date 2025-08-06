prepare_data <- function(DF) {
  
  DF_temp = 
    DF %>% 
    mutate(precio = as.integer(gsub(",", "", stringr::str_extract(x2, "^[0-9]*,[0-9]*|^[0-9]*[0-9]*"))),
           garage = stringr::str_extract(x2, "Garage \\w*"),
           # bedrooms = stringr::str_extract(x3, "^\\d* bed"),
           bedrooms = stringr::str_extract(stringr::str_extract(DF$x3, "^\\d* bed"), "^\\d*"),
           metros = as.integer(gsub(" m", "", stringr::str_extract(x3, "\\d* m"))),
           exterior = grepl("exterior", x3),
           lift = grepl("with lift", x3, fixed = TRUE),
           floor = as.numeric(stringr::str_extract(gsub("Ground", "0th", stringr::str_extract(DF$x3, "[0-9a-zA-Z]{1,10} floor")), "^\\d*")),
           type = gsub("(.*) in .*", "\\1", x1),
           where = gsub("(.*) in (.*)", "\\2", x1),
           
           exterior = 
             case_when(
               type %in% c("Detached house", "Rural manor", "Rustic house", "Semi-detached house", "Terraced house") ~ TRUE,
               TRUE ~ exterior),
           garage = 
             case_when(
               grepl("optional", garage) ~ "opcional",
               grepl("included", garage) ~ "incluido",
               TRUE ~ "unknown"),
           bedrooms = (
             case_when(
               is.na(bedrooms) ~ "0",
               TRUE ~ bedrooms)
             ),
           # floor = 
           #   case_when(
           #     is.na(floor) ~ -1,
           #     TRUE ~ floor
           #   )
           ) |> #%>% select(x1, x3, floor) %>% View
    select(-x4)
 
  return(DF_temp)
   
}
