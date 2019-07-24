#' FB audience CSR breakdown
#'
#' Function that returns a data frame with the total overlap between a vector of interests and predefined CSR interests.
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' csr_interests("6003123299417", "Computer science", "facebook")


csr_interests <- function(id_vector, name_vector, platform){
  
  suppressMessages(library(dplyr))
  print('Fetching CSR data')
  
  csr <-tribble( 
    ~name,~id, 
    "Environmentalism", "6003970975896",
    "Sustainability", "6003172273055",
    "Charity", "6003422719241",
    "Politics" ,"6003654559478",
    "Community issues", "6003049202156",
    "Religion", "6003395353671",
    "Veterans","6003280740901",
    "Volunteering", "6003137105590"
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
