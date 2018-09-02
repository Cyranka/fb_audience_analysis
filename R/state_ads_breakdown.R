#' FB audience state breakdown
#'
#' Function that returns a data frame with the total audience by state (excludes Washington, DC) for a selected interest.
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' state_ads_breakdown("6003123299417", "Computer science", "facebook")

state_ads_breakdown <- function(id_vector, name_vector, platform){

  suppressMessages(library(dplyr))
  print("Fetching state data")
  
  states <- tribble(
    ~key, ~name,
    "3849","Connecticut",
    "3862","Maine",
    "3864","Massachusetts",
    "3872","New Hampshire",
    "3882","Rhode Island",
    "3888","Vermont",
    "3850","Delaware",
    "3863","Maryland",
    "3873","New Jersey",
    "3875","New York",
    "3881","Pennsylvania",
    "3843","Alabama",
    "3846","Arkansas",
    "3852","Florida",
    "3853","Georgia",
    "3860","Kentucky",
    "3861","Louisiana",
    "3867","Mississippi",
    "3868","Missouri",
    "3876","North Carolina",
    "3883","South Carolina",
    "3885","Tennessee",
    "3889","Virginia",
    "3891","West Virginia",
    "3845","Arizona",
    "3874","New Mexico",
    "3879","Oklahoma",
    "3886","Texas",
    "3856","Illinois",
    "3857","Indiana",
    "3858","Iowa",
    "3859","Kansas",
    "3865","Michigan",
    "3866","Minnesota",
    "3870","Nebraska",
    "3877","North Dakota",
    "3878","Ohio",
    "3884","South Dakota",
    "3892","Wisconsin",
    "3844","Alaska",
    "3847","California",
    "3848","Colorado",
    "3854","Hawaii",
    "3855","Idaho",
    "3869","Montana",
    "3871","Nevada",
    "3880","Oregon",
    "3887","Utah",
    "3890","Washington",
    "3893","Wyoming"
  ) %>% mutate(name = as.character(name))
  
  state_counts <- sapply(1:nrow(states), function(i)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(regions =data.frame(key = as.character(states$key[i]))),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = as.character(id_vector),
          name = as.character(name_vector)
        ))
      )
    ))$users
  )
  
  
  state_data_frame <- tibble(
    State = states$name,
    Count = as.numeric(as.character(state_counts))
  )
  
  
  print("State data fetched")
  return(state_data_frame)
}