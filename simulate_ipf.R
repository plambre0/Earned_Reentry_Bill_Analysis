################################################################################
# by Pasha Lambre, 2026
# paololambre0@gmail.com
################################################################################

################################################################################
# Simulate Population
################################################################################

simulate_ipf <- function(x_ipf){
  # Integerize calibrated weights ################################################
  x_ipf[, N.sim := floor(calibWeight)]
  
  N <- round(sum(x_ipf$calibWeight))
  remainder <- N - sum(x_ipf$N.sim)
  
  # Allocate remaining individuals to largest fractional weights #################
  x_ipf[
    order(-(calibWeight - N.sim))[seq_len(remainder)],
    N.sim := N.sim + 1L
  ]
  
  # Expand weighted cells into individual records ################################
  synthetic_population <- x_ipf[rep(seq_len(.N), N.sim)]
  
  # Assign individual identifiers ################################################
  synthetic_population[, Person.ID := .I]
  
  # Remove calibration variables #################################################
  synthetic_population[, c("calibWeight", "N.sim") := NULL]
  
  # Randomize record ordering ####################################################
  synthetic_population <- synthetic_population[sample(.N)]
}
