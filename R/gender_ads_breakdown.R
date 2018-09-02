#' FB audience gender breakdown
#'
#' Function that returns a data frame with the total audience by gender for a selected interest.
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' gender_ads_breakdown("6003123299417", "Computer science", "facebook")


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
