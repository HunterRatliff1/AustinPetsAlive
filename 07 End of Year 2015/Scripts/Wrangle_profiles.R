# ---- Get Volunteer Profiles ----
Profiles <- gs_title("Summary 2015") %>% 
  gs_read_csv("Profiles") %>%
  select(-FirstName, -LastName, -Username) %>%
  
  ## Rename column names to be more human readable
  rename(   
    User.ID     = DatabaseUserId,    # ID variables
    Hours       = HoursWorked,       # Hours volunteered
    
    # Qualification variables
    isAdmin     = AdministratorStatus,
    isActive    = VolunteerStatus,
    Staff       = Q...APA..Staff,
    
    # Birthday and employer
    Birthday1   = Birthday,
    Birthday2   = CF...General.Information...Date.of.Birth,
    Employer    = CF...General.Information...Employer,
    
    # Time variables
    Orientation = Q...General...Orientation,
    RTR_Date    = CF...Training.Questions...RuffTail.Runner.Training.Date,
    Joined      = DateJoined, 
    Login       = LastLoginDate) %>%
  
  ## Format some varables
  mutate( 
    # Format the dates as such, dropping the hours and minutes
    Birthday1 = floor_date(mdy_hms(Birthday1), unit = "day"),
    Birthday2 = floor_date(mdy_hms(Birthday2), unit = "day"),
    Login     = floor_date(mdy_hms(Login),     unit = "day"),
    Joined    = floor_date(mdy_hms(Joined),    unit = "day"),
    RTR_Date  = floor_date(mdy_hms(RTR_Date),  unit = "day"),
    
    # Round the hours to 0 digits
    Hours     = round(Hours),
    Started   = ifelse(Hours>0, 1, 0),
    Has.Hours = ifelse(Hours>0, "Has Hours", "Hasn't Started"))




# ---- Find their Birthdays ----
## Function that selects birthday 
# (since the two birthday options are inconsistantly filled out)  
Profiles$Birthday <- mapply(
  FUN = source("Scripts/Profile_birthdays.R")$value,
  as.character(Profiles$Birthday1), 
  as.character(Profiles$Birthday2), 
  SIMPLIFY = T)

# reparse the birthday variable as a date
Profiles$Birthday <- ymd(Profiles$Birthday)




# ---- Clean up the data.frame ----
## Make factors
Profiles$isAdmin  <- as.factor(Profiles$isAdmin)
Profiles$isActive <- as.factor(Profiles$isActive)


Profiles <- Profiles %>% 
  # Select our refined columns
  select(Birthday, User.ID, Joined, Login, isAdmin, isActive, Staff, Hours, Employer, Started) %>%
  
  # Filter out staff, then drop that variable
  filter(is.na(Staff)) %>% select(-Staff) %>%
  
  # Filter Birthdays to make things reasonable
  mutate(Age = as.numeric(Birthday %--% today(), unit="year"))




# ---- Define the age groups ----
## Remove erronious ages & round to nearest year
Profiles$Age[Profiles$Age<12] <- NA
Profiles$Age[Profiles$Age>85] <- NA
Profiles$Age <- round(Profiles$Age)

Profiles$Age.Group <- sapply(Profiles$Age, source("Scripts/Profile_AgeFactors.R")$value)
# Make age groups ordered factor
Profiles$Age.Group <- factor(Profiles$Age.Group, 
                             levels=c("Minor", "College", "Adult-Younger", 
                                      "Adult", "Adult-Older", "Senior"), ordered=T)