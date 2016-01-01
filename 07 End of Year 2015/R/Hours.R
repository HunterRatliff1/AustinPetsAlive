gs_title("Summary 2015") %>% 
  gs_read_csv("Hours") %>%
  
  # Rename from the names provided by Vol2
  rename(
    Date.Vol = DateVolunteered,
    Duration = HoursWorked,
    Category = ActivityCategoryName,
    Activity = ActivityName,
    User.ID  = DatabaseUserId
  ) %>%
  
  # Make a few adjustments
  mutate(
    Date.Vol = floor_date(mdy_hms(Date.Vol), unit="day"),
    Duration = round(Duration, 2)) %>%
  
  # Filter out annoying categories
  filter(
    !grepl("New Volunteers", Category),
    !grepl("Volunteer Social", Category),
    !grepl("Existing Volunteers", Category)) %>%
  
  # Add extra qualifiers from sheet
  left_join((gs_title("Summary 2015") %>% gs_read_csv("Hours_Type"))) %>%
  select(User.ID, Location, Animal=animalType, Type, SubType, 
         Duration, Date.Vol, Activity, isTraining) %>%
  
  # Remove Staff Hours
  anti_join(
    (gs_title("Summary 2015") %>% gs_read_csv("Profiles") %>%
       select(User.ID=DatabaseUserId, isStaff=Q...APA..Staff) %>%
       filter(isStaff))) %>%
  
  filter(year(Date.Vol)==2015)
