# Profiles
read.profiles <- function(file_name="Profiles_2015-11-21", type="Vol", ext="csv") {
  require(dplyr)
  require(stringi)
  
  path.profiles <- "~/Github/Austin Pets Alive/Data/Profiles/"
  path_file <- paste0(path.profiles, file_name, ".", ext)
  file.csv  <- read.csv(path_file, na.strings="")
  
  message(paste("File source:", path_file))
  
  ## if Custom Field 
  if(type=="CF") {
    # # # # # # # # # # # # # # # # # # # # # # # # # 
    # If we only care about the Custom Fields table #
    # # # # # # # # # # # # # # # # # # # # # # # # #
    CF <- file.csv %>% select(DatabaseUserId, starts_with("CF"))
    names(CF) <- gsub("CF\\.\\.\\.", "", names(CF))
    
    # Simplify names
    names(CF) <- gsub("General.Information",                                      "Gen.Info", names(CF))
    names(CF) <- gsub("Do.you.volunteer.with.other.animal.welfare.organizations", "Past.Orgs", names(CF))
    names(CF) <- gsub("What.prior.experience.do.you.have.with.animals",           "Anml.Expr", names(CF))
    names(CF) <- gsub("Emergency.Contact.Information",                            "Emrg.Info", names(CF))
    names(CF) <- gsub("Criminal.History",                                         "Crim.Hist", names(CF))
    names(CF) <- gsub("Community.Service.Restitution.for.Criminal.Activity",      "CSR.Info", names(CF))
    names(CF) <- gsub("Training.Questions",                                       "Training", names(CF))
    names(CF) <- gsub("Cat.Program.Opportunities",                                "Opps.Cat", names(CF))
    names(CF) <- gsub("Dog.Program.Opportunities",                                "Opps.Dog", names(CF))
    names(CF) <- gsub("General.Opportunities",                                    "Opps.Gen", names(CF))
    names(CF) <- gsub("Volunteer.Recognition",                                    "Top.Vol", names(CF))
    names(CF) <- gsub("Hourly.Tracking.Follow.Up",                                "Tracking", names(CF))
    
    # Message listing column names
    message(paste(" -", 
                  unique(stri_split_regex(names(CF), pattern="\\.\\.\\.", n = 2, simplify=T)[, 1])))
    return(CF)
  }
  
  ## if Qualification
  if(type=="Q") {
    # # # # # # # # # # # # # # # # # # # # # # # # # 
    # If we only care about the Qualification table #
    # # # # # # # # # # # # # # # # # # # # # # # # #
    Q <- file.csv %>% select(DatabaseUserId, starts_with("Q"))
    names(Q) <- gsub("Q\\.\\.\\.",                 "",          names(Q))
    names(Q) <- gsub("Generic.Team.Leader",        "tmld",      names(Q))
    names(Q) <- gsub("General",                    "Gen",       names(Q))
    names(Q) <- gsub("Animal.Qualifications",      "Anml.Qual", names(Q))
    names(Q) <- gsub("APA..Staff",                 "is.Staff",  names(Q))
    names(Q) <- gsub("Sent.Welcome.Email",         "Wlcm.Sent", names(Q))
    names(Q) <- gsub("Sent.RTR.JumpStart.Email",   "RTR.Sent",  names(Q))
    names(Q) <- gsub("Opt.Out.Comm",               "No.Comm",   names(Q))
    names(Q) <- gsub("Sent.CSR.Completion.Letter", "Anml",      names(Q))
    names(Q) <- gsub("AAC.Volunteer",              "AAC.Vol",   names(Q))
    names(Q) <- gsub("\\.\\.\\.",                  "_",         names(Q))
    message(paste(" -", 
                  unique(stri_split_regex(names(Q), pattern="_", n = 2, simplify=T)[, 1])))
    message(paste(names(Q), " "))
    return(Q)
  }
  
  if(type=="Vol") {
    Vol <- file.csv %>% select(-starts_with("CF"), -starts_with("Q"))
    names(Vol) <- gsub("FirstName",                       "Name.F",       names(Vol))
    names(Vol) <- gsub("LastName",                        "Name.L",       names(Vol))
    names(Vol) <- gsub("LastLoginDate",                   "LastLogin",    names(Vol))
    names(Vol) <- gsub("MiddleName",                      "Name.M",       names(Vol))
    names(Vol) <- gsub("PostalCode",                      "Zipcode",      names(Vol))
    names(Vol) <- gsub("Province",                        "State",        names(Vol))
    names(Vol) <- gsub("EmailAddress",                    "Email",        names(Vol))
    names(Vol) <- gsub("Profile",                         "Profile.",     names(Vol))
    names(Vol) <- gsub("HoursWorked",                     "Hours",        names(Vol))
    names(Vol) <- gsub("CellPhone",                       "Cell",         names(Vol))
    names(Vol) <- gsub("TwitterUsername",                 "Twitter",      names(Vol))
    names(Vol) <- gsub("LinkedInProfile.Url",             "LinkedIn",     names(Vol))
    names(Vol) <- gsub("AdministratorStatus",             "Admin",        names(Vol))
    names(Vol) <- gsub("VolunteerStatus",                 "Status",       names(Vol))
    names(Vol) <- gsub("VolunteerArchivedStatusReason",   "Status.Type",  names(Vol))
    names(Vol) <- gsub("DateOfLastVolunteerStatusChange", "Status.Date",  names(Vol))
    names(Vol) <- gsub("Address1",                        "Address",      names(Vol))
    names(Vol) <- gsub("DateJoined",                      "Date.Joined",  names(Vol))
    names(Vol) <- gsub("Profile.Created",                 "Date.Created", names(Vol))
    names(Vol) <- gsub("Profile.Updated",                 "Date.Updated", names(Vol))
    
    Vol$LegalFirstName                <- NULL
    Vol$Suffix                        <- NULL
    Vol$Title                         <- NULL
    Vol$Address2                      <- NULL
    Vol$Country                       <- NULL
    Vol$SecondaryEmail                <- NULL
    Vol$MobileEmail                   <- NULL
    Vol$HomePhone                     <- NULL
    Vol$WorkPhone                     <- NULL
    Vol$WorkPhoneExt                  <- NULL
    Vol$PhonePreference               <- NULL
    Vol$Language                      <- NULL
    Vol$SecurityRole                  <- NULL
    Vol$VolunteerInactiveStatusReason <- NULL
    Vol$LoggedHoursGoalStartDate      <- NULL
    Vol$LoggedHoursGoalEndDate        <- NULL
    Vol$VolunteerNotes                <- NULL
    Vol$SendScheduleReminderEmails    <- NULL
    Vol$DonorRecordUrl                <- NULL
    Vol$IsContactPerson               <- NULL
    Vol$IsBillingContact              <- NULL
    Vol$DateOfLastStatusChange        <- NULL
    Vol$GeneralTimelogPermission      <- NULL
    Vol$TimeclockTimelogPermission    <- NULL
    Vol$LoggedHoursGoalNumberOfHours  <- NULL
    Vol$LegalName.F  <- NULL
    Vol$Name.M       <- NULL
      
    
    message(paste("Dimensions:", nrow(Vol), "x", ncol(Vol)))
    message(paste(names(Vol), " "))
    return(Vol)  
  }
  
}

# read.profiles() %>% dim()
# read.profiles(type = "Q") %>% dim()
# read.profiles(type = "CF") %>% dim()
