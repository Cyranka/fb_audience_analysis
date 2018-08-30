excel_preparer <- function(excel_file){
  
  print("Starting data processing")
  suppressMessages(require(tidyverse))
  
  ##Load initial list
  sheets_to_read <- readxl::excel_sheets(excel_file)
  initial_list <- lapply(1:length(sheets_to_read), function(i)readxl::read_excel(excel_file, sheet = sheets_to_read[[i]]))
  
  ##Create income pivot
  income_pivot <- function(excel_file){
    income_categories <- c("Under 50K","50K - 75K", "75K - 100K",
                           "100K - 150K", "100K - 150K","Over 150K","Over 150K","Over 150K","Over 150K")
    
    y <- readxl::read_excel(excel_file, sheet = "income") %>% 
                      filter(Bracket !="0. $30,000 - $39,999") %>%
      mutate(income_categories = income_categories) %>% 
      group_by(income_categories) %>%
      summarise(Count = sum(Count)) %>% 
      mutate(income_categories = factor(income_categories,
             levels = c("Under 50K", "50K - 75K","75K - 100K","100K - 150K","Over 150K"))) %>% ungroup() %>%
      arrange(income_categories) %>% mutate(proportional = round(Count/sum(Count),3))
    return(y)
  }
  
  
  ##Create age pivot
  age_pivot <- function(excel_file){
    age_categories <- c("Under 30","Under 30", "Under 30","30-39","30-39",
                        "40-49","40-49","50-59","50-59","Over 60")
    
    y <- readxl::read_excel(excel_file, sheet = "age") %>% 
      mutate(age_categories = age_categories) %>% 
      group_by(age_categories) %>%
      summarise(Count = sum(Count)) %>% 
      mutate(age_categories = factor(age_categories,
            levels = c("Under 30", "30-39", "40-49", "50-59", "Over 60"))) %>% ungroup() %>%
      arrange(age_categories) %>% mutate(proportional = round(Count/sum(Count),3))
    
    return(y)
  }
  
  ##Create education pivot
  education_pivot <- function(excel_file){
    
    education_levels <- tribble(
      ~Education, ~level,
      "In High School","Less than college",
      "In College", "Less than college",
      "College Graduate", "College",
      "High School Graduate", "Less than college",
      "Some College", "Less than college",
      "Associate Degree", "Less than college",
      "In Graduate School", "College",
      "Some Graduate School", "College",
      "Master's Degree", "Advanced degree",
      "Professional Degree", "Advanced degree",
      "Doctorate", "Advanced degree",
      "Unspecified", "Unspecified",
      "Some High School","Less than college"
    )
    
    x <- readxl::read_excel(excel_file, sheet = "education") %>%
      inner_join(education_levels) %>% group_by(level) %>%
      summarise(Count = sum(Count)) %>% filter(!level == "Unspecified") %>%
      mutate(proportional = round(Count/sum(Count),3)) %>%
      mutate(level = factor(level, levels = c("Less than college", "College", "Advanced degree"))) %>%
      dplyr::rename(categories = level) %>% arrange(categories)
    return(x)
  }
  
  ##Create geo pivot
  geo_pivot <- function(excel_file){
    geo <- tibble::tribble(
      ~Region,                 ~State, ~Abbreviation,
      "Northeast",        "Connecticut","CT",
      "Northeast",              "Maine","ME",
      "Northeast",      "Massachusetts","MA",
      "Northeast",      "New Hampshire","NH",
      "Northeast",       "Rhode Island","RI",
      "Northeast",            "Vermont","VT",
      "Northeast",           "Delaware","DE",
      "Northeast",           "Maryland","MD",
      "Northeast",         "New Jersey","NJ",
      "Northeast",           "New York","NY",
      "Northeast",       "Pennsylvania","PA",
      "South",            "Alabama",    "AL",
      "South",           "Arkansas",    "AR",
      "South",            "Florida",    "FL",
      "South",            "Georgia",    "GA",
      "South",           "Kentucky",    "KY",
      "South",          "Louisiana",    "LA",
      "South",        "Mississippi",    "MS",
      "Midwest",         "Missouri",    "MO",
      "South",     "North Carolina",    "NC",
      "South",     "South Carolina",    "SC",
      "South",          "Tennessee",    "TN",
      "South",           "Virginia",    "VA",
      "South",      "West Virginia",    "WV",
      "Southwest",            "Arizona", "AZ",
      "Southwest",  "New Mexico",        "NM",
      "Southwest",    "Oklahoma",          "OK",
      "Southwest",       "Texas",          "TX",
      "Midwest",    "Illinois",          "IL",
      "Midwest",     "Indiana",          "IN",
      "Midwest",        "Iowa",          "IA",
      "Midwest",        "Kansas",          "KS",
      "Midwest",      "Michigan",          "MI",
      "Midwest",     "Minnesota",          "MN",
      "Midwest",             "Nebraska",    "NE",
      "Midwest",         "North Dakota",    "ND",
      "Midwest",                 "Ohio",    "OH",
      "Midwest",         "South Dakota",    "SD",
      "Midwest",            "Wisconsin",    "WI",
      "Western Overseas", "Alaska",   "AK",
      "West", "California","CA",
      "West", "Colorado","CO",
      "Western Overseas", "Hawaii","HI",
      "West","Idaho","ID",
      "West","Montana","MT",
      "West","Nevada","NV",
      "West","Oregon","OR",
      "West", "Utah","UT",
      "West", "Washington", "WA",
      "West", "Wyoming", "WY",
      "Northeast", "District of Columbia","DC"
    )
    
    x <- readxl::read_excel(excel_file, sheet = "state") %>%
      inner_join(geo)
    y <- readxl::read_excel(excel_file, sheet = "state") %>%
      inner_join(geo) %>% group_by(Region) %>% summarise(Count = sum(Count)) %>%
      ungroup() %>% mutate(proportional = round(Count/sum(Count),3))
    
    return(list(x,y))
    
  }
  
  ##Create gender pivot
  gender_pivot <- function(excel_file){
    x <- readxl::read_excel(excel_file, sheet = "gender") %>%
      mutate(Count = as.numeric(Count)) %>%
      mutate(percent = round(Count/sum(Count),3))
    return(x)
  }
  
  ##Create ideology pivot
  ideology <- function(excel_file){
    x <- readxl::read_excel(excel_file, sheet = "ideology") %>%
      mutate(Count = as.numeric(Count)) %>%
      mutate(proportional = round(Count/sum(Count),3))
    return(x)
  }
  
  pivot_list <- list(income_pivot(excel_file),
                     geo_pivot(excel_file)[[1]],
                     geo_pivot(excel_file)[[2]],
                     education_pivot(excel_file),
                     age_pivot(excel_file),
                     gender_pivot(excel_file),
                     fix_ideology(excel_file))
  
  final_list <- append(initial_list,pivot_list)
  names(final_list) <- c(sheets_to_read,
                         "income_pivot2",
                         "states_with_regions",
                         "geo_pivot2",
                         "education_pivot2",
                         "age_pivot2",
                         "gender_fixed",
                         "ideology_fixed")
  writexl::write_xlsx(final_list,excel_file)
  return(cat("Data processed"))
}