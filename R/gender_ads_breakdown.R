gender_ads_breakdown <- function(id_vector, name_vector, platform){
  
  suppressMessages(require(dplyr))
  
  print("Fetching gender data")
  gender_tibble <- tibble(
    gender_name = c("Male", "Female"),
    number = c(1,2)
  )
  
  
  gender_vector <- sapply(1:2,function(i)
    fbad_reachestimate(targeting_spec = list(
      genders = gender_tibble$number[i],
      geo_locations = list(countries = 'US'),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = as.character(id_vector),
          name = as.character(name_vector)
        ))
      )
    ))$users
  )
  
  gender_df <- tibble(gender = c("Male", "Female"),
                      total = as.numeric(as.character(gender_vector))) %>%
    magrittr::set_colnames(c("Gender", "Count"))
  
  print("Gender data fetched")
  return(gender_df)
}
