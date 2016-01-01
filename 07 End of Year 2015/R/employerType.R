# employerType <- 
function(x){
  ## Wrapper functions to clean code up
  xMatch <- function(p, x)    grepl(pattern=p,               x=x, ignore.case = T)
  xSub   <- function(p, r, x) gsub(pattern=p, replacement=r, x=x, ignore.case = T)
  if(is.na(x)) return(NA)
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
  
  return("Other")
}