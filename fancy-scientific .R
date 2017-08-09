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


## add the fdc to the plot and set how the axes will appear
p1 <- ggplot(data = df.data.flow) +
  geom_line(aes(x = flow.exceed, y = value),
            color = "blue", size = 1.5) +
  scale_y_log10(limits = range(tmp.breaks), breaks = tmp.breaks,
                minor_breaks = c(sapply(tmp.breaks, function(x) seq(0, x, x/10))),
                labels = fancy_scientific) +
  scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
  labs(
    x = "Flow Exceedance (%)",
    y = "Average Daily Flow (cfs)"
  )