#' FB audience generate .xlsx report
#'
#' Function that provides a wrapper for most the audience functions and writes an .xlsx report with the data
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @param excel_file: An excel file name, adding ".xlsx" extension is not necessary
#' @keywords FB ads audience
#' @export
#' @examples
#' retrieve_report("6003123299417", "Computer science", "facebook", "computer_science")



retrieve_report <- function(id_vector, name_vector, my_platform, excel_file){
  options(stringsAsFactors = FALSE)
  options(scipen = 999)
  
  
  a_1 <- income_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_2 <- age_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_3 <- state_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_4 <- affinity_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_5 <- gender_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_6 <- ideology_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_7 <- education_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_8 <- major_sports(id_vector,name_vector, my_platform)
  Sys.sleep(60)
  a_9 <- music_genres(id_vector, name_vector, my_platform)
  Sys.sleep(60)
  a_10 <- dma_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(60)
#  a_11 <- donations(id_vector, name_vector,my_platform)
  a_11 <- csr_interests(id_vector,name_vector,my_platform)
  
  
  
  report_list <- list(a_1,a_2,a_3,a_4,
                  a_5,a_6,a_7,a_8,
                  a_9,a_10,a_11)
  
  names(report_list) <- c("income", "age", "state", "ethnicity",
                          "gender", "ideology","education", "sports",
                          "music", "media_markets", "csr_interests")
  
  print("Writing report")
  writexl::write_xlsx(report_list, stringr::str_glue(excel_file, ".xlsx"))
  
  
  return(report_list)
}