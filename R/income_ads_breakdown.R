#' FB audience income breakdown
#'
#' Function that returns a data frame with the total audience by income for a selected interest.
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' income_ads_breakdown("6003123299417", "Computer science", "facebook")


income_ads_breakdown <- function(id_vector,name_vector, platform){
  
  suppressMessages(require(dplyr))
  
  print("Fetching income data")
  incomes <- dplyr::tribble(
    ~name,~key,
    "Top 25%-50% of ZIP codes","6107813554583",
    "Top 10%-25% of ZIP codes","6107813553183",
    "Top 10% of ZIP codes","6107813551783",
    "Top 5% of ZIP codes","6107813079183",
  )
  
  income_vector <- sapply(1:nrow(incomes),function(i)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(countries = 'US'),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = id_vector,
          name = name_vector
        )),
        list(income = data.frame(
          id = incomes$key[i],
          name = incomes$name[i]
        ))
      )
    ))$users
  )
  
  ##Calculate people outside
  remaining <- us_audience(id_vector, name_vector, platform) - sum(as.numeric(as.character(income_vector)))
  remaining <- tibble(
    bracket = "Below 50% of ZIP codes",
    number = remaining
  )
  
  ##
  
  income_data_frame <- tibble(bracket = incomes$name, number = income_vector) %>%
    mutate(number = as.numeric(as.character(number))) %>%
    bind_rows(remaining) %>%
    magrittr::set_colnames(c("Bracket", "Count"))
  
  print("Income data fetched")
  
  return(income_data_frame)
}