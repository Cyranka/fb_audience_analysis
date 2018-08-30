affinity_ads_breakdown <- function(id_vector, name_vector, platform){
  
  suppressMessages(require(dplyr))
  print("Fetching multicultural affinity data")
  
  race_df <- tibble(
    race_ids = c("6021722613183","6018745176183","6003133212372"),
    names = c("Asian-Americans","African-Americans","Hispanics")
  )
  
  ##Minorities
  race_counts <- sapply(1:nrow(race_df), function(i)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(countries = 'US'),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = id_vector,
          name = name_vector
        )),
        list(behaviors = data.frame(
          id = race_df$race_ids[i],
          name = race_df$names[i]
        ))
      )
    ))$users
  )
  
  ##Whites
  total_audience <- fbad_reachestimate(targeting_spec = list(
    geo_locations = list(countries = 'US'),
    publisher_platforms = platform,
    flexible_spec = list(
      list(interests = data.frame(
        id = as.character(id_vector),
        name = as.character(name_vector)
      ))
    )
  ))$users
  
  race_counts[4] <- (as.numeric(total_audience) - sum(as.numeric(race_counts)))
  
  
  affinity_df <- tibble(
    `Ethnic/Cultural Group` = c("Asian-Americans","African-Americans","Hispanics", "Whites"),
    Count = as.numeric(race_counts)
  ) %>% mutate(percent = Count/sum(Count))

  affinity_df <- affinity_df[c(4,2,3,1),]
  
  print("Multicultural affinity data fetched")
  
  return(affinity_df)
}