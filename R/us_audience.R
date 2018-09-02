#' FB audience: Total US Audience
#'
#' Function that returns a number with the total audience for an interest in the United States
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' us_audience("6003123299417", "Computer science", "facebook")

us_audience <- function(id_vector, name_vector, platform){
  
  audience <- fbad_reachestimate(targeting_spec = list(
    geo_locations = list(countries = 'US'),
    publisher_platforms = platform,
    flexible_spec = list(
      list(interests = data.frame(
        id = as.character(id_vector),
        name = as.character(name_vector)
      ))
    )
  ))$users
  
  
  return(as.numeric(audience))
}

