## Change the Format of Numbers on Axis with ggplot 


fancy_scientific <- function(l) {
  ## function taken from stackoverflow.com post
  ## http://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot/24241954
  # turn in to character string in scientific notation
  x <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  #y <- gsub("^(.*)e", "'\\1'e", x)
  # turn the 'e+' into plotmath format
  z <- gsub("^.*e", "10^", x)
  # return this as an expression
  parse(text=z)
}
