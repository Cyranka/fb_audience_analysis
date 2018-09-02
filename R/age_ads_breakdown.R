#' FB audience age breakdown
#'
#' Function that returns a data frame with the total audience by predefined brackets age for a selected interest.
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' age_ads_breakdown("6003123299417", "Computer science", "facebook")



age_ads_breakdown <- function(id_vector, name_vector, platform){
  
  suppressMessages(require(dplyr))
  
  print("Fetching age data")
  
  age <- dplyr::tribble(
    ~Category, ~age_1, ~age_2,
    "13-17",13,17,
    "18-24",18,24,
    "25-29",25,29,
    "30-34",30,34,
    "35-39",35,39,
    "40-44",40,44,
    "45-49",45,49,
    "50-54",50,54,
    "55-59",55,59,
    "Over 60",60,65
  )
  
  age_vector <- sapply(1:nrow(age),function(i)fbad_reachestimate(targeting_spec = list(
    age_min = unbox(age$age_1[i]),
    age_max = unbox(age$age_2[i]),
    geo_locations = list(countries = 'US'),
    publisher_platforms = platform,
    flexible_spec = list(
      list(interests = data.frame(
        id = id_vector,
        name = name_vector
      ))
    )
  ))$users
  )
  
  age_data_frame <- dplyr::tibble(Category = age$Category, Count = age_vector) %>%
    mutate(age_vector = as.numeric(as.character(age_vector))) %>%
    magrittr::set_colnames(c("Age Category","Count"))
  
  
  print("Age data fetched")
  
  return(age_data_frame)
}
