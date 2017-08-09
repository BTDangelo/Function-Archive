## Function to Calculate Flow Exceedance 


flow.exceed <- function(v.flow) {
  tmp.rank <- rank(v.flow, ties.method = "average")
  tmp.exceed <- tmp.rank / length(v.flow)
  tmp.exceed <- 100 * (1 - tmp.exceed)
  return(tmp.exceed)
}
