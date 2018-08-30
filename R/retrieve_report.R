retrieve_report <- function(id_vector, name_vector, my_platform, excel_file){
  options(stringsAsFactors = FALSE)
  options(scipen = 999)
  
  
  a_1 <- income_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_2 <- age_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_3 <- state_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_4 <- affinity_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_5 <- gender_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_6 <- ideology_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_7 <- education_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_8 <- major_sports(id_vector,name_vector, my_platform)
  Sys.sleep(10)
  a_9 <- music_genres(id_vector, name_vector, my_platform)
  Sys.sleep(10)
  a_10 <- dma_ads_breakdown(id_vector, name_vector, my_platform)
  Sys.sleep(15)
  a_11 <- donations(id_vector, name_vector,my_platform)
  Sys.sleep(10)
  a_12 <- csr_interests(id_vector,name_vector,my_platform)
  
  
  
  report_list <- list(a_1,a_2,a_3,a_4,
                  a_5,a_6,a_7,a_8,
                  a_9,a_10,a_11, a_12)
  
  names(report_list) <- c("income", "age", "state", "ethnicity",
                          "gender", "ideology","education", "sports",
                          "music", "media_markets", "donations", "csr_interests")
  
  print("Writing report")
  writexl::write_xlsx(report_list, stringr::str_glue(excel_file, ".xlsx"))
  
  
  return(report_list)
}