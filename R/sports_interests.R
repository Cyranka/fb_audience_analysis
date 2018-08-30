major_sports <- function(id_vector, name_vector, platform){

  sports <-tribble( 
    ~id,~name,   
    "6003107902433","Soccer",    
    "6003376089674","Football",  
    "6003369240775","Basketball",
    "6003388549418","Ice hockey",
    "6003087413192","Baseball",  
    "6003510075864","Golf",      
    "6003397425735","Tennis",
    "6003110460645","Mixed martial arts",
    "6003481546864", "Track and field",
    "6003166397215", "Swimming")
  
  network <- tribble(
    ~Sports, ~pct_of_network,
    "Soccer", 0.381,
    "Football",0.336,
    "Basketball", 0.318,
    "Ice hockey", 0.135,
    "Baseball",0.314,
    "Golf", 0.135,
    "Tennis", 0.063,
    "Mixed martial arts", 0.112,
    "Track and field", 0.170,
    "Swimming", 0.072
  )
  
  sports_vector <- sapply(1:nrow(sports),function(i)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(countries = 'US'),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = id_vector,
          name = name_vector
        )),
        list(interests = data.frame(
          id = sports$id[i],
          name = sports$name[i]
        ))
      )
    ))$users
  )
  
  percent_audience <- us_audience(id_vector, name_vector, platform)
  
  final_frame <- tibble(
    Sports = sports$name,
    Count = as.numeric(sports_vector),
    pct_total = round(sports_vector/percent_audience,3)
  ) %>% inner_join(network) %>%
    mutate(difference = pct_total - pct_of_network) %>% arrange(desc(Count))
  
  return(final_frame)
  
}

