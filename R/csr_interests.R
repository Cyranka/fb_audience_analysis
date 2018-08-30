csr_interests <- function(id_vector, name_vector, platform){
  
  suppressMessages(library(dplyr))
  print('Fetching CSR data')
  
  csr <-tribble( 
    ~id,~name, 
    "1232909720088602","Black lives matter",
    "6003899280466" ,"Women's rights",
    "6002898215362" ,"Bernie Sanders",
    "6003970975896" ,"Environmentalism",
    "6003204938561" ,"Elizabeth Warren"
  )
  

  csr_vector <- sapply(1:nrow(csr),function(i)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(countries = 'US'),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = id_vector,
          name = name_vector
        )),
        list(interests = data.frame(
          id = csr$id[i],
          name = csr$name[i]
        ))
      )
    ))$users
  )
  
  percent_audience <- us_audience(id_vector, name_vector, platform)
  
  final_frame <- tibble(
    interest = csr$name,
    Count = as.numeric(csr_vector),
    pct_total = round(csr_vector/percent_audience,3)
  ) %>% arrange(desc(Count))
  
  print('CSR data fetched')
  return(final_frame)
  
}
