## Function to Calculate Flow Exceedance 


flow.exceed <- function(v.flow) {
  tmp.rank <- rank(v.flow, ties.method = "average")
  tmp.exceed <- tmp.rank / length(v.flow)
  tmp.exceed <- 100 * (1 - tmp.exceed)
  return(tmp.exceed)
}


## calculate the flow exceedance for the flow data
df.data.flow$flow.exceed <- flow.exceed(df.data.flow$value)