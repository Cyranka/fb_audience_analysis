remove(list =ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
library(janitor)
sheets_to_loop <- c("geo_pivot2","age_pivot2",
                    "income_pivot2", "gender_fixed",
                    "ideology_fixed","music", "sports",
                    "csr_interests", "ethnicity",
                    "education_pivot2","age_raw")

function_for_line_graph <- function(working_directory){
  setwd(working_directory)
  list_of_names <- grep("\\.xlsx", list.files(), value = TRUE)
  list_of_names <- subset(list_of_names, list_of_names != "multiple_comparison.xlsx")
  fix_names <- stringr::str_to_title(gsub("_", " ",gsub("\\.xlsx", "", list_of_names)))
  
  education_colnames <- function(x){
  	colnames(x) <- c("Education", "Count", "proportional")
  	return(x)
  }

  gender_colnames <- function(x){
    colnames(x) <- c("Gender", "Count", "proportional")
    return(x)
  }
  
  music_sports_cols <- function(x){
    y <- x[,c(1:3)]
    colnames(y) <- c("category", "Count", "proportional")
    return(y)
  }
  
  csr_interests_cols <- function(x){
    colnames(x) <- c("category", "Count", "proportional")
    return(x)
  }
  
  ethnicity_cols <- function(x){
    colnames(x) <- c("category", "Count", "proportional")
    return(x)
  }
  
  select_sheet <- function(define_sheet){
    first_list <- purrr::map(1:length(list_of_names), function(i)readxl::read_excel(list_of_names[i],sheet =define_sheet))
    if(define_sheet == "gender_fixed"){
      first_list <- purrr::map(first_list, gender_colnames)
    }else{
      first_list <- first_list
    }
    
    if(any(define_sheet == "music"|define_sheet == "sports")){
      first_list <- purrr::map(first_list, music_sports_cols)
    }else{
      first_list <- first_list
    }
    
    if(define_sheet == "csr_interests"){
      first_list <- purrr::map(first_list, csr_interests_cols)
    }else{
      first_list <- first_list
    }
    
    if(define_sheet == "ethnicity"){
      first_list <- purrr::map(first_list, ethnicity_cols)
    }else{
      first_list <- first_list
    }

    if(define_sheet == "education_pivot2"){
      first_list <- purrr::map(first_list, education_colnames)
    }else{
      first_list <- first_list
    }
    
    
    for(i in 1:length(fix_names)){
      first_list[[i]]$name <- fix_names[i]
      colnames(first_list[[i]])[1] <- "category"
      
    }
    remove(i)
    x <- bind_rows(first_list) %>% select(name, category, proportional) %>%
      spread(category, proportional, convert = FALSE)
    colnames(x)[1] <- "category"
    
    return(x)
  }
  
  select_sheet("csr_interests")
  
  ##Get total counts
  get_total_counts <- function(work_directory){
    
    list_of_names <- grep("\\.xlsx", list.files(), value = TRUE)
    list_of_names <- subset(list_of_names, list_of_names != "multiple_comparison.xlsx")
    fix_names <- stringr::str_to_title(gsub("_", " ",gsub("\\.xlsx", "", list_of_names)))
    
    
    first_list <- purrr::map(1:length(list_of_names),
                             function(i)readxl::read_excel(list_of_names[i],sheet ="age") %>%
                               mutate(category = fix_names[[i]]) %>%
                               select(category,Count) %>% janitor::clean_names())
    return_frame <- bind_rows(first_list) %>%
      group_by(category) %>% summarise(count = sum(count, na.rm = TRUE)) %>%
      arrange(desc(count))
    
    return(return_frame)
  }
  
  ##Do if for all sheets
  create_workbook <- function(list_of_files){
      list_with_data_frames <- purrr::map(1:length(sheets_to_loop),
                                          function(i)select_sheet(sheets_to_loop[i]))
    
      list_with_data_frames[[1]] <- list_with_data_frames[[1]][,c(1,4,3,2,6,5,7)]
      list_with_data_frames[[2]] <- list_with_data_frames[[2]][,c(1,6,2,3,4,5)]
      list_with_data_frames[[3]] <- list_with_data_frames[[3]][,c(1,2,5,4,3,6)]
      list_with_data_frames[[5]] <- list_with_data_frames[[5]][,c(1,6,3,4,2,5)]
      list_with_data_frames[[9]] <- list_with_data_frames[[9]][,c(1,5,2,4,3)]
      list_with_data_frames[[10]] <- list_with_data_frames[[10]][,c(1,4,3,2)]
      list_with_data_frames[[12]] <- get_total_counts(getwd())
      
    
      names(list_with_data_frames) <- c(sheets_to_loop, "total_counts")
      writexl::write_xlsx(list_with_data_frames, "multiple_comparison.xlsx")

    return(list_with_data_frames)
  }
  create_workbook(list_of_names)
} 

#cat("Enter Working Directory: ")
#my_work_dir <- readline("Enter Working Directory: ")

#function_for_line_graph(my_work_dir)