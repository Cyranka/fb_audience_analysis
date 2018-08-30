music_genres <- function(id_vector, name_vector, platform){
  
  suppressMessages(library(dplyr))
  print('Fetching music data')
  
  music <-tribble( 
    ~id,~name, 
    "6003341579196", "Pop",
    "6003225556345", "Hip Hop",
    "6002951587955", "Classical",
    "6003582732907", "Rock",
    "6003493980595", "Country",
    "6003902397066", "Electronic",
    "6003195554098", "R&B",
    "6003146442552", "Jazz",
    "6003257757682", "Blues",
    "6003107699532", "Soul"
    )
  
  network <- tribble(
    ~Music, ~pct_of_network,
    "Pop", 0.395,
    "Hip Hop",0.345,
    "Classical", 0.077,
    "Rock",0.427,
    "Country",0.218,
    "Electronic",0.273,
    "R&B",0.286,
    "Jazz",0.127,
    "Blues",0.241,
    "Soul",0.218
  )
  
  music_vector <- sapply(1:nrow(music),function(i)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(countries = 'US'),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = id_vector,
          name = name_vector
        )),
        list(interests = data.frame(
          id = music$id[i],
          name = music$name[i]
        ))
      )
    ))$users
  )
  
  percent_audience <- us_audience(id_vector, name_vector, platform)
  
  final_frame <- tibble(
    Music = music$name,
    Count = as.numeric(music_vector),
    pct_total = round(music_vector/percent_audience,3)
  ) %>% inner_join(network) %>%
    mutate(difference = pct_total - pct_of_network) %>% arrange(desc(Count))
  
  print('Music data fetched')
  return(final_frame)
  
}
