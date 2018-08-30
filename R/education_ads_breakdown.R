education_ads_breakdown <- function(id_vector,name_vector, platform){

  
  suppressMessages(library(dplyr))
  print('Fetching education data')
  
  edu_df <- tribble(
    ~education_status, ~code,
    "In High School",1,
    "In College", 2,
    "College Graduate", 3,
    "High School Graduate", 4,
    "Some College", 5,
    "Associate Degree", 6,
    "In Graduate School", 7,
    "Some Graduate School", 8,
    "Master's Degree", 9,
    "Professional Degree", 10,
    "Doctorate", 11,
    "Unspecified", 12,
    "Some High School",13
  )
  
  
  education_vector <- sapply(1:nrow(edu_df), 
  function(i)fbad_reachestimate(targeting_spec = list(
    geo_locations = list(countries = 'US'),
    education_statuses = edu_df$code[i],
    publisher_platforms = platform,
    flexible_spec = list(
      list(interests = data.frame(
        id = id_vector,
        name = name_vector
      ))
    )
  ))$users
  )
  
  education_frame <- tibble(
    Education = edu_df$education_status,
    Count = as.numeric(as.character(education_vector))
  ) %>% arrange(desc(Count))
  
  
  print('Education data fetched')
  return(education_frame)
}