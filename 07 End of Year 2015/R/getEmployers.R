function(User.ID_list, Employer_list){
  df <- as_data_frame(list(User.ID=User.ID_list, Employer=Employer_list))
  
  ## Parse the employers
  df$Employer <- sapply(df$Employer, 
                        source("~/Github/Austin Pets Alive/07 End of Year 2015/R/parseEmployers.R")$value)
  
  
  ## Parse the chamber data
  df$Chamber <- sapply(df$Employer, 
                       source("~/Github/Austin Pets Alive/07 End of Year 2015/R/parseChamber.R")$value)
  # Add full name of employer
  df <- gs_title("Age & Employeer") %>% 
    gs_read_csv("Austin_Chamber") %>%
    select(Chamber=Key, Chamber.Employer=Search) %>%
    left_join(df, .)
  # If found a match from chamber data, replace it
  df$Employer <- ifelse(is.na(df$Chamber), df$Employer, df$Chamber.Employer)
  df$Chamber.Employer <- NULL
  
  ### Adds a column indicating the type of employer
  df$Employer.Type <- sapply(df$Employer, 
                             source("~/Github/Austin Pets Alive/07 End of Year 2015/R/employerType.R")$value)
  return(df)
}