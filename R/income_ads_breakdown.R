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
    "0. $30,000 - $39,999","23842569734890111",
    "1. $40,000 - $49,999","23842569735140111",
    "2. $50,000 - $74,999","23842569735250111",
    "3. $75,000 - $99,999","23842569735510111",
    "4. $100,000 - $124,999","23842569735560111",
    "5. $125,000 - $149,999","23842569735600111",
    "6. $150,000 - $249,999", "23842569735640111",
    "7. $250,000 - $349,999", "23842569735690111",
    "8. $350,000 - $499,999", "23842569735700111",
    "9. Over $500,000", "23842569735910111"
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
  
  income_data_frame <- tibble(bracket = incomes$name, number = income_vector) %>%
    mutate(number = as.numeric(as.character(number))) %>%
    magrittr::set_colnames(c("Bracket", "Count"))

  print("Income data fetched")
  
  return(income_data_frame)
}