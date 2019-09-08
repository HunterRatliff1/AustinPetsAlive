# ---- Parse the list of Employeers ----
Profiles$Employer.Parse <- sapply(Profiles$Employer, source("Scripts/Employer_parse.R")$value)



# ---- Match Employees to the Chamber data ----
# Read in the 'Chamber' data
Chamber <- gs_title("Age & Employeer") %>% 
  gs_read_csv("Austin_Chamber")

# Wapper function
xMatch <- function(p, x)    grepl(pattern=p, x=x, ignore.case = T)


Chambs <- Profiles %>% 
  select(User.ID, Hours, Employer.Parse) %>%
  mutate(
    AISD  = xMatch("Austin ISD", Employer.Parse),
    CoA   = xMatch("Austin", Employer.Parse) & xMatch("City", Employer.Parse),
    Dell  = xMatch("Dell$", Employer.Parse),
    Feds  = xMatch("Federal", Employer.Parse) | xMatch("Veterans Affairs", Employer.Parse) | 
      xMatch("Revenue Service", Employer.Parse) | xMatch("^IRS", Employer.Parse) |
      xMatch("^US ", Employer.Parse) | xMatch("gov", Employer.Parse) & 
      !xMatch("Credit", Employer.Parse),
    IBM   = xMatch("IBM", Employer.Parse),
    Seton = xMatch("Seton", Employer.Parse),
    StDvd = xMatch("StDavids", Employer.Parse),
    SoTX  = xMatch("State", Employer.Parse) & xMatch("Texas", Employer.Parse) & 
      !xMatch("Univ", Employer.Parse),
    UTexas = xMatch("UTexas", Employer.Parse) & !xMatch("Student", Employer.Parse),
    Accn = xMatch("Accenture", Employer.Parse),
    Appl = xMatch("Apple", Employer.Parse),
    AppM = xMatch("Applied Materials", Employer.Parse),
    ATnT = xMatch("AT&T", Employer.Parse),
    AAC  = xMatch("^ACC$", Employer.Parse) | xMatch("Austin", Employer.Parse) & 
      xMatch("College", Employer.Parse) & xMatch("Community", Employer.Parse),
    Flxtr = xMatch("Flextronics", Employer.Parse),
    Frscl = xMatch("Freescale", Employer.Parse),
    HISD  = xMatch("HISD", Employer.Parse) | 
      xMatch("Hays", Employer.Parse) & xMatch("ISD", Employer.Parse),
    KllrW = xMatch("Keller Williams", Employer.Parse),
    LISD  = xMatch("LISD", Employer.Parse) | 
      xMatch("Leander", Employer.Parse) & xMatch("ISD", Employer.Parse),
    NtnlI = xMatch("National Instruments", Employer.Parse),
    PfISD = xMatch("PISD", Employer.Parse) | 
      xMatch("Pflugerville", Employer.Parse) & xMatch("ISD", Employer.Parse),
    RRISD = xMatch("RRISD", Employer.Parse) | 
      xMatch("Round Rock", Employer.Parse) & xMatch("ISD", Employer.Parse),
    Smsng = xMatch("Samsung", Employer.Parse),
    TxsSt = xMatch("State", Employer.Parse) & xMatch("Texas", Employer.Parse) & 
      xMatch("Univ", Employer.Parse),
    TrvsC = xMatch("Travis", Employer.Parse) | xMatch("County", Employer.Parse),
    IRS   = xMatch("^IRS", Employer.Parse) | xMatch("Revenue Service", Employer.Parse),
    WhlFd = xMatch("Whole Foods", Employer.Parse),
    MMM   = xMatch("3M", Employer.Parse),
    AMD   = xMatch("^AMD$", Employer.Parse) | xMatch("Advanced Micro", Employer.Parse),
    AstnE = xMatch("Austin Energy", Employer.Parse),
    AstRC = xMatch("Austin Regional Clinic", Employer.Parse),
    BSnW  = xMatch("Baylor Scott", Employer.Parse),
    ChrlS = xMatch("Charles Schwab", Employer.Parse),
    GM    = xMatch("^GM$", Employer.Parse) | xMatch("General Motors", Employer.Parse),
    Gdwll = xMatch("Goodwill", Employer.Parse),
    HP    = xMatch("^HP$", Employer.Parse) | xMatch("Hewlett", Employer.Parse),
    HmAwy = xMatch("HomeAway", Employer.Parse),
    Hospr = xMatch("Hospira", Employer.Parse),
    Intel = xMatch("Intel", Employer.Parse),
    LCRA  = xMatch("Colorado River Authority", Employer.Parse) | xMatch("LCRA", Employer.Parse), 
    Oracl = xMatch("Oracle", Employer.Parse),
    Prgrs = xMatch("Progressive", Employer.Parse),
    Sears = xMatch("Sears", Employer.Parse),
    StFrm = xMatch("State Farm", Employer.Parse),
    TWC   = xMatch("Time Warner", Employer.Parse) | xMatch("TWC", Employer.Parse),
    URS   = xMatch("URS", Employer.Parse),
    VA    = xMatch("Veterans Affairs", Employer.Parse) | xMatch("^VA", Employer.Parse),
    WllsF = xMatch("Wells Fargo", Employer.Parse),
    ActvB = xMatch("Activision", Employer.Parse),
    AmrCC = xMatch("American Campus", Employer.Parse),
    ACS   = xMatch("American Cancer ", Employer.Parse),
    BAESy = xMatch("BAE", Employer.Parse),
    BnkoA = xMatch("Bank", Employer.Parse) & xMatch("America", Employer.Parse),
    Bzrvc = xMatch("Bazaarvoice", Employer.Parse),
    CFAN  = xMatch("CFAN", Employer.Parse),
    CrrsL = xMatch("Cirrus Logic", Employer.Parse),
    Cisco = xMatch("Cisco", Employer.Parse),
    ClnPL = xMatch("Clinical Pathology Lab", Employer.Parse),
    Cncnt = xMatch("Concentrix", Employer.Parse),
    eBay  = xMatch("eBay", Employer.Parse),
    ERCoT = xMatch("Electric.+Council", Employer.Parse),
    ElctA = xMatch("Electronic Arts", Employer.Parse),
    EmrPM = xMatch("Emerson", Employer.Parse),
    Facbk = xMatch("Facebook", Employer.Parse),
    FrmIG = xMatch("Farmers Insurance", Employer.Parse),
    Googl = xMatch("Google", Employer.Parse),
    GrndC = xMatch("^Grande$", Employer.Parse) | xMatch("Grande Communications", Employer.Parse),
    HrtH  = xMatch("Harte.Hank", Employer.Parse),
    HEB   = xMatch("^HEB", Employer.Parse) | xMatch("H-E-B", Employer.Parse),
    HmDpt = xMatch("Home Depot", Employer.Parse),
    ImgMc = xMatch("Image Microsystems", Employer.Parse),
    JMCnC = xMatch("JP", Employer.Parse) & xMatch("Morgan", Employer.Parse) | 
      xMatch("Chase", Employer.Parse),
    ThLqC = xMatch("Liquidation", Employer.Parse),
    Maxms = xMatch("Maximus", Employer.Parse),
    Persn = xMatch("Pearson", Employer.Parse),
    PhrPD = xMatch("Pharma.+Dev", Employer.Parse),
    Q2    = xMatch("Q2", Employer.Parse),
    Rcksp = xMatch("Rackspace", Employer.Parse),
    SlcnL = xMatch("Silicon Lab", Employer.Parse),
    SlrWn = xMatch("SolarWinds", Employer.Parse),
    Spnsn = xMatch("Spansion", Employer.Parse),
    StEdw = xMatch("Edwards", Employer.Parse),
    Visa  = xMatch("Visa", Employer.Parse),
    VMWar = xMatch("VMWare", Employer.Parse),
    Volsn = xMatch("Volusion", Employer.Parse),
    WynFS = xMatch("Wayne", Employer.Parse),
    Xerox = xMatch("Xerox", Employer.Parse),
    Yodle = xMatch("Yodle", Employer.Parse)
  )




Chamb.sum <- Chambs %>% select(-Employer.Parse) %>% 
  melt(id.vars = c("User.ID", "Hours"), variable.name = "Employer") %>%
  filter(value) %>% 
  mutate(Start = ifelse(Hours>0, 1, 0)) %>%
  group_by(Employer) %>%
  summarise(Vols = n(), Hours = sum(Hours), Start = sum(Start)) %>%
  left_join(select(Chamber, Size, Key, Search), by=c("Employer"="Key")) %>%
  rename(Key=Employer, Employer=Search)
# Make ordered factor
Chamb.sum$Size <- factor(Chamb.sum$Size, levels=c("Under 1k", "1-2k", "2-6k", "6k+"), ordered=T)




## Replace matched employeers from Chamber data in the 
## profiles frame
Profiles <- Chambs %>% select(-Employer.Parse) %>% 
  melt(id.vars = c("User.ID", "Hours"), variable.name = "Employer") %>%
  filter(value) %>% 
  select(User.ID, Key=Employer) %>%
  left_join(select(Chamber, Key, Employer=Search)) %>%
  right_join(select(Profiles, -Employer, -isAdmin)) %>%
  tbl_df()




## Consolidate into a clean frame
Profiles <- Profiles %>%
  mutate(Emps = ifelse(is.na(Employer), Employer.Parse, Employer)) %>%
  select(-Employer, -Key, -Employer.Parse, -Birthday) %>%
  rename(Employer=Emps)

# Replace "NA" charachter with "None", since NA means no data
Profiles$Employer[Profiles$Employer=="NA"] <- "No Employer"
Profiles$Employer[Profiles$Employer=="Self"] <- "Self-Employed"

## Remove Chamber frames
rm(Chambs, Chamber, Chamb.sum)



# ---- Employer Type ----
### This block adds a column indicating the type of employer
Profiles$Employer.Type <- sapply(Profiles$Employer, function(x){
  ## Wrapper functions to clean code up
  xMatch <- function(p, x)    grepl(pattern=p,               x=x, ignore.case = T)
  xSub   <- function(p, r, x) gsub(pattern=p, replacement=r, x=x, ignore.case = T)
  
  if(xMatch("Self", x))       return("Self")
  
  if(xMatch("Retired", x))    return("No Employer")
  if(xMatch("^NA$", x))       return("No Employer")
  if(xMatch("Unemployed", x)) return("No Employer")
  if(xMatch("^Home$", x))     return("No Employer")
  if(xMatch("Disabled", x))   return("No Employer")
  if(xMatch("None", x))       return("No Employer")
  
  
  if(xMatch("Student", x))    return("Education")
  if(grepl("ISD", x))         return("Education")
  if(xMatch("University", x)) return("Education")
  if(xMatch("College", x))    return("Education")
  if(xMatch("School", x))     return("Education")
  if(xMatch("Texas State", x))return("Education")
  if(xMatch("St Edwards", x)) return("Education")
  if(xMatch("UTexas", x))     return("Education")
  if(xMatch("Teacher", x))    return("Education")
  
  
  if(xMatch("Seton", x))         return("Healthcare")
  if(xMatch("DellChildrens", x)) return("Healthcare")
  if(xMatch("StDavids", x))      return("Healthcare")
  if(xMatch("Hospital", x))      return("Healthcare")
  if(xMatch("Medical", x))       return("Healthcare")
  if(xMatch("Clinic", x))        return("Healthcare")
  if(xMatch("Baylor Scott", x))  return("Healthcare")
  
  
  if(xMatch("Department", x) & xMatch("Texas", x))         return("Gov")
  if(xMatch("State", x) & xMatch("Texas", x))              return("Gov")
  if(xMatch("Department", x) & xMatch("Austin", x))        return("Gov")
  if(xMatch("City", x) & xMatch("Austin", x))              return("Gov")
  if(xMatch("Travis", x) & xMatch("County", x))            return("Gov")
  if(xMatch("Federal", x))                                 return("Gov")
  if(xMatch("IRS", x))                                     return("Gov")
  if(xMatch("Veterans Affairs", x))                        return("Gov")
  if(xMatch("Human Services", x) & xMatch("Texas", x))     return("Gov")
  if(xMatch("Human Services", x) & xMatch("Austin", x))    return("Gov")
  if(xMatch("Human Services", x) & xMatch("US", x))        return("Gov")
  if(xMatch("Internal Revenue", x))                        return("Gov")
  if(xMatch("Human Services", x))                          return("Gov")
  if(xMatch("Government", x))                              return("Gov")
  
  if(is.na(x)) return(NA)
  return("Other")
})

# Make factor
Profiles$Employer.Type <- as.factor(Profiles$Employer.Type)
