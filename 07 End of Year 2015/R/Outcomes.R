Outcomes <- gs_title("Summary 2015") %>% 
  gs_read_csv("Outcomes") %>%
  select(Animal.ID, 
         Species, 
         Breed     = Primary.Breed,
         DOB       = Date.Of.Birth,
         Sex       = Gender, 
         Colour    = Primary.Colour,
         Type      = Operation.Type,
         SubType   = Operation.Sub.Type,
         Date      = Outcome.Date.Time,
         Age.Month = Outcome.Age.As.Month,
         Status    = Asilomar.Status) %>%
  mutate(
    DOB  = mdy(DOB),
    Date = mdy_hm(Date)) %>%
  filter(Type=="Adoption") %>%
  select(-Type, -DOB)

## Age Group
Outcomes$Age.Group <- sapply(Outcomes$Age.Month, function(x){
  if(x>0  & x<=3)  return("<3 m.o.")
  if(x>3  & x<=6)  return("3-6 m.o.")
  if(x>6  & x<=12) return("6-12 m.o.")
  if(x>12 & x<=24) return("1-2 y.o.")
  if(x>24 & x<=60) return("2-6 y.o.")
  if(x>60 & x<=120) return("6-10 y.o.")
  if(x>120)         return(">10 y.o.")
})
Outcomes$Age.Group <- factor(Outcomes$Age.Group, 
                             levels=c("<3 m.o.", "3-6 m.o.", "6-12 m.o.", 
                                      "1-2 y.o.", "2-6 y.o.", 
                                      "6-10 y.o.", ">10 y.o."), ordered=T)

## Sex
Outcomes$Sex[Outcomes$Sex=="U"] <- NA
Outcomes$Sex     <- as.factor(Outcomes$Sex)

## Breed
Outcomes$Breed.Sub <- reshape2::colsplit(Outcomes$Breed, ", ", c("Breed", "Breed.Sub"))$Breed.Sub
Outcomes$Breed     <- reshape2::colsplit(Outcomes$Breed, ", ", c("Breed", "Breed.Sub"))$Breed
Outcomes$Breed.Sub[Outcomes$Breed.Sub==""] <- NA

## Parse colours
parseColours <- function(df, cutoff=0.9) {
  ### This function accepts a data frame with a column named 'Colour'
  ### and a numeric cutoff. If the cutoff is greater than 1, it returns
  ### that number of colours (weighted by frequency). If the cutoff is 
  ### less than 1, it returns whatever number of colours are required to
  ### account for that percentage of the total data frame
  
  # Make a count data frame
  df.c <- df %>% 
    count(Colour, sort=T) %>% 
    mutate(percent = cumsum(n)/sum(n))
  
  # If given a cutoff for the number of colours
  if(cutoff>=1) {
    df.c <- top_n(df.c, n = cutoff, wt = n)
    message(paste0("Selecting the top ", cutoff, " colours (", 
                   percent(max(df.c$percent)), " of total)"))
  }
  # If given a cutoff for the percentage of colours
  if(cutoff<1) {
    df.c <- filter(df.c, percent<=cutoff)
    message(paste0("Selecting the top ", percent(cutoff), " colours (", 
                   nrow(df.c), " colours)"))
  }
  
  return(bind_rows(
    semi_join(df, df.c),        # Colours to keep
    mutate(anti_join(df, df.c), # Colours to make 'Other'
           Colour = "Other")))
}
Outcomes <- parseColours(Outcomes, cutoff = 0.9)
Outcomes$Colour  <- as.factor(Outcomes$Colour)

## Status
Outcomes$Status <- sapply(Outcomes$Status, function(x) {
  if(xMatch("Untreatable", x)) return("Untreatable")
  if(xMatch("Treatable", x))   return("Treatable")
  if(xMatch("Healthy", x))     return("Healthy")
  return(NA)
})
Outcomes$Status  <- as.factor(Outcomes$Status)

## SubType
Outcomes$SubType2 <- sapply(Outcomes$SubType, function(x) {
  x <- gsub("Dogs-", "", x)
  x <- gsub("Dogs- ", "", x)
  x <- gsub("Cats-", "", x)
  x <- gsub("Cats- ", "", x)
  if(xMatch("TLAC", x))     return("TLAC")
  if(xMatch("Ringworm", x)) return("TLAC")
  return(x)
})
Outcomes$SubType <- sapply(Outcomes$SubType2, function(x) {
  if(xMatch("TLAC", x))      return("TLAC")
  if(xMatch("PetSmart", x))  return("PetSmart")
  if(xMatch("PetCo", x))     return("PetCo")
  if(xMatch("Tomlinson", x)) return("Tomlinson")
  return(x)
})

## Split by species
Dogs <- filter(Outcomes, Species=="Dog") %>% select(-Species)
Cats <- filter(Outcomes, Species=="Cat") %>% select(-Species)

## Parse breeds
parseBreeds <- function(df, cutoff=15) {
  ### This function accepts a data frame with a column named 'Breed'
  ### and a numeric cutoff. If the cutoff is greater than 1, it returns
  ### that number of breeds (weighted by frequency). If the cutoff is 
  ### less than 1, it returns whatever number of breeds are required to
  ### account for that percentage of the total data frame
  
  # Make a count data frame
  df.c <- df %>% 
    count(Breed, sort=T) %>% 
    mutate(percent = cumsum(n)/sum(n))
  
  # If given a cutoff for the number of breeds
  if(cutoff>=1) {
    df.c <- top_n(df.c, n = cutoff, wt = n)
    message(paste0("Selecting the top ", cutoff, " breeds (", 
                   percent(max(df.c$percent)), " of total)"))
  }
  # If given a cutoff for the percentage of breeds
  if(cutoff<1) {
    df.c <- filter(df.c, percent<=cutoff)
    message(paste0("Selecting the top ", percent(cutoff), " breeds (", 
                   nrow(df.c), " breeds)"))
  }
  
  return(bind_rows(
    semi_join(df, df.c),        # Breeds to keep
    mutate(anti_join(df, df.c), # Breeds to make 'Other'
           Breed = "Other")))
}

Dogs <- parseBreeds(Dogs, cutoff = 15)
Cats <- parseBreeds(Cats, cutoff = 5)


## Make Factors (for each)
# Dogs
Dogs$Breed     <- as.factor(Dogs$Breed)
Dogs$Breed.Sub <- as.factor(Dogs$Breed.Sub)
Dogs$SubType   <- as.factor(Dogs$SubType)
# Cats
Cats$Breed     <- as.factor(Cats$Breed)
Cats$Breed.Sub <- as.factor(Cats$Breed.Sub)
Cats$SubType   <- as.factor(Cats$SubType)
