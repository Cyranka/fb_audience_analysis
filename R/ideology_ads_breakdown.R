#' FB audience ideology breakdown
#'
#' Function that returns a data frame with the total audience by ideology for a selected interest.
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' ideology_ads_breakdown("6003123299417", "Computer science", "facebook")


ideology_ads_breakdown <- function(id_vector, name_vector, platform){
  
  suppressMessages(require(dplyr))
  print("Fetching ideology data")
  
  pol_df <- tribble(
    ~Name, ~id,
    "Very Liberal","6015759997983",
    "Liberal","6015760027783",
    "Moderate", "6015760036783",
    "Conservative", "6015760532183",
    "Very Conservative", "6015762142783"
  )
  
  political_vector <- sapply(1:nrow(pol_df),function(i)
    fbad_reachestimate(targeting_spec = list(
    geo_locations = list(countries = 'US'),
    publisher_platforms = platform,
    flexible_spec = list(
      list(interests = data.frame(
        id = id_vector,
        name = name_vector
      )),
      list(politics = data.frame(
        id = pol_df$id[i],
        name = pol_df$Name[i]
      ))      
    ))
  )$users
  )
  
  pol_return <- tibble(
    Politics = pol_df$Name,
    Count = political_vector
  ) %>% mutate(Count = as.numeric(as.character(Count)))
  
  print("Ideology data fetched")
  return(pol_return)
}
