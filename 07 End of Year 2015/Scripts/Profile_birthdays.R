function(A, B) {
  if( is.na(A) &  is.na(B)) return(NA)
  if(!is.na(A) &  is.na(B)) return(A)
  if( is.na(A) & !is.na(B)) return(B)
  if(!is.na(A) & !is.na(B)) {
    if(A==B) return(A)
    A.yr <- as.numeric(ymd(A) %--% today(), unit="year")
    B.yr <- as.numeric(ymd(B) %--% today(), unit="year")
    if(A.yr >= 12  &  A.yr <= 90) return(A)
    if(B.yr >= 12  &  B.yr <= 90) return(B)
    else(return(NA))
  }
}