function(x){
  ## Wrapper functions to clean code up
  xMatch <- function(p, x)    grepl(pattern=p,               x=x, ignore.case = T)
  xSub   <- function(p, r, x) gsub(pattern=p, replacement=r, x=x, ignore.case = T)
  
  
  x <- trimws(x)
  x <- xSub("St\\.",  "St",           x)
  x <- xSub("'s",     "s",            x)
  x <- xSub("\\.com", "\\[dot\\]com", x)
  x <- xSub("www\\.", "",             x)
  x <- xSub("\\.",    "",             x)
  
  x <- xSub(", inc$",  "",  x)
  x <- xSub(" inc$",   "",  x)
  x <- xSub(", llc$",  "",  x)
  x <- xSub(" llc$",   "",  x)
  x <- xSub(", pllc$", "",  x)
  x <- xSub(" pllc$",  "",  x)
  x <- xSub(", llp$",  "",  x)
  x <- xSub(" llp$",   "",  x)
  
  x <- xSub("N/A",  "NA", x)
  x <- xSub("None", "NA", x)
  
  # x <- xSub("AT&T", "AT&T", x)
  x <- xSub("Dept", "Department", x)
  x <- xSub("tx", "Texas", x)
  
  
  
  x <- xSub("University of Texas",         "UTexas",     x)
  x <- xSub("UTexas at Austin",            "UTexas",     x)
  x <- xSub("UT Austin",                   "UTexas",     x)
  x <- xSub("The UTexas",                  "UTexas",     x)
  x <- xSub("UT-Austin",                   "UTexas",     x)
  x <- xSub("UTexas Austin",               "UTexas",     x)
  x <- xSub("Independent School District", "ISD",        x)
  x <- xSub("School District",             "ISD",        x)
  x <- xSub("AISD",                        "Austin ISD", x)
  
  
  
  if(xMatch("^na$", x)) return("NA")
  if(xMatch("^no$", x)) return("NA")
  if(xMatch("^-$", x))  return("NA")
  
  if(xMatch("Self Employed", x))  return("Self")
  if(xMatch("Self-employed", x))  return("Self")
  if(xMatch("self$", x))          return("Self")
  
  if(xMatch("Unemployed$", x))    return("Unemployed")
  if(xMatch("^not employed$", x)) return("Unemployed")
  if(xMatch("^not working$", x))  return("Unemployed")
  
  if(xMatch("Stay", x) & xMatch("home", x))  return("Home")
  if(xMatch("Stay", x) & xMatch("work", x))  return("Home")
  if(xMatch("home", x) & xMatch("parnet", x))return("Home")
  if(xMatch("home", x) & xMatch("mom", x))   return("Home")
  if(xMatch("home", x) & xMatch("dad", x))   return("Home")
  if(xMatch("^home$", x))                    return("Home")
  if(xMatch("^mom$", x))                     return("Home")
  if(xMatch("housewife", x))                 return("Home")
  if(xMatch("home", x) & xMatch("maker", x)) return("Home Maker")
  
  if(xMatch("Austin pets alive", x))         return("APA")
  if(xMatch("^APA", x))                      return("APA")
  
  if(xMatch("student", x) & grepl("UT", x))  return("UTexas_Student")
  if(xMatch("student", x) & 
     xMatch("UTexas", x))                    return("UTexas_Student")
  if(xMatch("Longhorn Pets Alive", x))       return("UTexas_Student")
  if(grepl("LPA", x))                        return("UTexas_Student")
  if(xMatch("student", x))                   return("Student")
  if(xMatch("^UT$", x))                      return("UTexas")
  
  if(xMatch("Disabled", x))                  return("Disabled")
  if(xMatch("retired", x))                   return("Retired")
  
  if(xMatch("David", x))                      return("StDavids")
  if(xMatch("Seton", x))                      return("Seton")
  if(xMatch("Dell", x) & xMatch("Child", x))  return("DellChildrens")
  if(xMatch("Dell", x))                       return("Dell")
  
  return(x)
}