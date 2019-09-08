function(x) {
  if(is.na(x))     return(NA)
  if(x>=12 & x<18) return("Minor")
  if(x>=18 & x<23) return("College")
  if(x>=23 & x<30) return("Adult-Younger")
  if(x>=30 & x<50) return("Adult")
  if(x>=50 & x<65) return("Adult-Older")
  if(x>=65) return("Senior")
  return("Other")
}