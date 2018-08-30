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

