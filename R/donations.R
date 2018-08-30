donations <- function(id_vector,name_vector, platform){
  
  suppressMessages(library(dplyr))
  
  print("Fetching donations data")
  donor <- tribble(
    ~donor_key, ~name,
    "6006448678099", "Charity",
    "23842646930340111", "Arts",
    "6010054501299", "Children's Causes",
    "6010054500899", "Cancer",
    "6006448678899", "Veterans"
  )
 
  donor_counts <- sapply(1:nrow(donor), function(i)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(countries = 'US'),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = as.character(id_vector),
          name = name_vector
        )),
        list(behaviors = data.frame(
          id = donor$donor_key[i],
          name = donor$name[i]
        ))
      )
    ))$users
  )

  total_audience <- us_audience(id_vector, name_vector, platform)
  
  donation <- tibble(
    `Donation Type` = donor$name,
    Count = as.numeric(donor_counts),
    proportion = round(as.numeric(donor_counts)/total_audience,3)
  )
  
  print("Donations data fetched")
  return(donation)
}