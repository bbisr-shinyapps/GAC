boxColor<- function (low, high, col_low, col_mid, col_high)
{
  color = col_mid
  if ((low < 1) & (high < 1)) { color = col_low}
  else if((low > 1) & (high >1)) {color = col_high}
  return(as.character(color))
}