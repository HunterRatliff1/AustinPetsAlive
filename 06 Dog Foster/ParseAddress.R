# FileName: ParseAddress


ToLookup <- unique(select(Foster_details, Person.ID, Address)) # Save a list of unique addresses to look up
ToLookup$Row.ID <- c(1:nrow(ToLookup)) # Add row numbers as a column


## Process Zipcodes
# Regular expression for zipcode


extractify <- function(Address_List, regex){
  
  # Grab the expression
  Exp <- regmatches(Address_List, regexec(regex, Address_List))
  
  # If no match found, replace with NA
  Exp <- sapply(Exp, function(x) ifelse(length(x)==0, NA, x))
                                                 
  return(Exp)
}  

Address_List <- ToLookup$Address

## Pulls the five digit zip code from the end of the entry
Zip   <- extractify(Address_List = Address_List, regex = "\\d\\d\\d\\d\\d$")
Address_List <- trimws(gsub("\\d\\d\\d\\d\\d$", "", Address_List)) # Remove zipcode and trim whitespace

## Pulls the state from the end of the entry
State <- extractify(Address_List = Address_List, regex = "\\S+$")
Address_List <- trimws(gsub("\\S+$", "", Address_List)) # Remove state and trim whitespace
  
## Process City
City <-  extractify(Address_List = Address_List, regex = "[A-Z]{2,}\\s?[A-Z]+$")
Address_List <- trimws(gsub("[A-Z]{2,}\\s?[A-Z]+$", "", Address_List)) # Remove city and trim whitespace


###  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  ###
###                     Street Address cleanup                      ###
###  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  ###
### This is a little crash course in regular expressions, so I tried
### to document it as best as I could. R does regex a little weird,
### so you might want to check out the guides I used:
# >>   http://www.pcre.org/original/pcre.txt
# >>   http://www.regular-expressions.info/rlanguage.html
# >>   http://regexr.com/
# 
# For each opperation, I have a function adds a column called
# Test, which represnets which entries are going to be affected
# by the opperation. This is just FYI, and was to help me debug
# the expressions as I figured it out

## Make the 'ave' abbreviation of avenue the full name
## Note: Notice that I included a whitespace in both the regex
##       and replacement to prevent matching inside the words
Address_List <- gsub("(Ave\\s)", "Avenue ", Address_List)

## Match the capture group Avenue + space + zero or more digits.
## Replace with Avenue + underscore. This will preserve Avenue A
## as Avenue_A, while emilinating trailing numbers after addresses
## like Main Avenue 304. In essence, we are converting the street 
## names that are Avenue + (a letter) to a protectable name, to 
## prepare for further parsing of trailing numbers/letters
Address_List <- gsub("(Avenue\\s\\d*)|(avenue\\s\\d*)", 
                        "Avenue_", Address_List)

## Do a similar type of protection for FM (farmers market) roads
Address_List    <- gsub("(FM\\s)|(fm\\s)", "FM_", Address_List)

## And with these
Address_List <- gsub("Ranch Road ", "Ranch_Road_", Address_List)
Address_List <- gsub("Hwy ", "Hwy_", Address_List)
Address_List <- gsub("HS ", "HS_", Address_List)
Address_List <- gsub("IH ", "IH-", Address_List)


## If the address has Unit + (letter/number), remove it. 
## Note: The match must be Unit + (0-4 charachters) + end of line.
##       This is to prevent any street name with Unit within it from
##       being removed (ex: United)
Address_List <- gsub("Unit.{0,4}$", "", Address_List) %>% trimws()

# Remove any numbers at the end of the address that are preceded by a
# whitespace. Most of these are apartment numbers that just throw off 
# the geocoding API
### Function to mark these as a T/F column 
Address_List <- gsub(" \\d+$", "", Address_List) %>% trimws()

# Remove all single alphabetic charachters at the end of the 
# entry (ex: apartment A)
Address_List <- gsub(" \\w$", "", Address_List) %>% trimws()

# Remove all digits at the end of the entry
# Note: This is only digits preceded by a whitespace and doesn't
# include mixed alpha-numeric or entries with an underscore
Address_List <- gsub(" \\d+$", "", Address_List) %>% trimws()

# Handle the remaining odd balls (C313, W102, 332maria, etc)
Address_List <- gsub("( [A-Z]\\d+$)|(332maria)", "", Address_List)

# Replace underscores with spaces (not blanks)
# This 'unhides' the groups we protected before
Address_List <- gsub("_", " ", Address_List)

# Random bit, but noticed this while scrolling
Address_List <- gsub("Doc Holliday Tri",
                        "Doc Holliday Trail", Address_List)

# Make all empty strings NA
ToLookup[ToLookup==""] <- NA

# Make query paramater
Q <- paste(Address_List, City, State, Zip)

# Save as a file
root_path <- "/Users/main/Github/Austin Pets Alive/06 Dog Foster"

if(getwd()!=root_path) setwd(as.character(root_path))


saveRDS(ToLookup, "data/ToLookup2.rds")