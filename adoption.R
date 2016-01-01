

Adoptions <- gs_title("AnimalOutcome2015") %>% gs_read_csv() %>%
  select(-ARN, -Microchip.Number, -Species, -Spayed.Neutered, 
         -Pre.Altered, -Primary.Colour, -Secondary.Colour, -Operation.Type,
         -By, -Altered, -Site.Name, -Jurisdiction.Out, -Operation.Sub.Type,
         -Outcome.YYYYMMDD) %>%
  rename(Name=Animal.Name, Breed=Primary.Breed, Breed2=Secondary.Breed,
         DOB=Date.Of.Birth, Sex=Gender, Outcome.Date=Outcome.Date.Time, 
         Age.Month=Outcome.Age.As.Month, Health.Status=Asilomar.Status)

# Make Factors
Adoptions$Sex            <- as.factor(Adoptions$Sex)
Adoptions$Danger         <- as.factor(Adoptions$Danger)
Adoptions$Danger.Reason  <- as.factor(Adoptions$Danger.Reason)
Adoptions$Age.Group      <- as.factor(Adoptions$Age.Group)
Adoptions$Outcome.Reason <- as.factor(Adoptions$Outcome.Reason)
Adoptions$Health.Status  <- as.factor(Adoptions$Health.Status)

# Parse dates
Adoptions$DOB <- mdy(Adoptions$DOB)
Adoptions$Outcome.Date <- mdy_hm(Adoptions$Outcome.Date)



glimpse(Adoptions)


  














glimpse(Vol_Hours)
