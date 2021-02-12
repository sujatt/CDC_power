# create transition matrix
Ptransition <- function(
  dropout_rate = 0.01,
  v1e = 0.50, v2e = 0.50, v3e = 0.50, # vaccine efficiency
  v1r = 0.01, v2r = 0.01, v3r = 0.01, # vaccination rates per week
  covid_rate = 10 # new cases per day per 100K
) {
  covid_per_day <- covid_rate*7/100000
  
  col_row_names = c('unvaccinated, not infected', 'unvaccinated, infected', 'dropped out',
    'vaccine 1, week 1, not infected', 'vaccine 1, week 1, infected',
    'vaccine 1, week 2, not infected', 'vaccine 1, week 2, infected',
    'vaccine 1, week 3, not infected', 'vaccine 1, week 3, infected',
    'vaccine 1, week 4, not infected', 'vaccine 1, week 4, infected',
    'vaccinated 1, not infected', 'vaccinated 1, infected',
    'vaccine 2, week 1, not infected', 'vaccine 2, week 1, infected',
    'vaccine 2, week 2, not infected', 'vaccine 2, week 2, infected',
    'vaccine 2, week 3, not infected', 'vaccine 2, week 3, infected',
    'vaccine 2, week 4, not infected', 'vaccine 2, week 4, infected',
    'vaccinated 2, not infected', 'vaccinated 2, infected',
    'vaccine 3, week 1, not infected', 'vaccine 3, week 1, infected',
    'vaccine 3, week 2, not infected', 'vaccine 3, week 2, infected',
    'vaccine 3, week 3, not infected', 'vaccine 3, week 3, infected',
    'vaccine 3, week 4, not infected', 'vaccine 3, week 4, infected',
    'vaccinated 3, not infected', 'vaccinated 3, infected')
  P <- matrix(0, nrow=length(col_row_names), ncol=length(col_row_names))
  colnames(P) <- col_row_names
  rownames(P) <- col_row_names
  p0 <- P[1,]
  
  # row 1: transition out of unvaccinated state
  p <- p0
  names(p) <- col_row_names
  p['unvaccinated, infected'] <- covid_per_day
  p['dropped out'] <- dropout_rate
  p['vaccine 2, week 1, not infected'] <- v1r*(1-covid_per_day)
  p['vaccine 2, week 1, infected'] <- v1r*covid_per_day # no protection in the first week
  p['vaccine 2, week 1, not infected'] <- v2r*(1-covid_per_day)
  p['vaccine 2, week 1, infected'] <- v2r*covid_per_day # no protection in the first week
  p['vaccine 3, week 1, not infected'] <- v3r*(1-covid_per_day)
  p['vaccine 3, week 1, infected'] <- v3r*covid_per_day # no protection in the first week
  sum_p <- sum(p)
  p['unvaccinated, not infected'] <- 1 - sum_p
  P[1,] <- p
  
  # row 2, etc: infected is an absorption state
  P['unvaccinated, infected','unvaccinated, infected'] <- 1
  P['vaccine 1, week 1, infected', 'vaccine 1, week 1, infected'] <- 1
  P['vaccine 1, week 2, infected', 'vaccine 1, week 2, infected'] <- 1
  P['vaccine 1, week 3, infected', 'vaccine 1, week 3, infected'] <- 1
  P['vaccine 1, week 4, infected', 'vaccine 1, week 4, infected'] <- 1
  P['vaccine 2, week 1, infected', 'vaccine 2, week 1, infected'] <- 1
  P['vaccine 2, week 2, infected', 'vaccine 2, week 2, infected'] <- 1
  P['vaccine 2, week 3, infected', 'vaccine 2, week 3, infected'] <- 1
  P['vaccine 2, week 4, infected', 'vaccine 2, week 4, infected'] <- 1
  P['vaccine 3, week 1, infected', 'vaccine 3, week 1, infected'] <- 1
  P['vaccine 3, week 2, infected', 'vaccine 3, week 2, infected'] <- 1
  P['vaccine 3, week 3, infected', 'vaccine 3, week 3, infected'] <- 1
  P['vaccine 3, week 4, infected', 'vaccine 3, week 4, infected'] <- 1
  P['vaccinated 1, infected','vaccinated 1, infected'] <- 1
  P['vaccinated 2, infected','vaccinated 2, infected'] <- 1
  P['vaccinated 3, infected','vaccinated 3, infected'] <- 1
  
  # row 3: dropped out is an absorption state
  P['dropped out', 'dropped out'] <- 1
  
  # vaccine 1 progression
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 1, week 2, infected'] <- (1-v1e*1/4)*covid_per_day # partial protection
  p['vaccine 1, week 2, not infected'] <- 1 - p['vaccine 1, week 2, infected'] - p['dropped out']
  P['vaccine 1, week 1, not infected',] <- p
    
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 1, week 3, infected'] <- (1-v1e*2/4)*covid_per_day # partial protection
  p['vaccine 1, week 3, not infected'] <- 1 - p['vaccine 1, week 3, infected'] - p['dropped out']
  P['vaccine 1, week 2, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 1, week 4, infected'] <- (1-v1e*3/4)*covid_per_day # partial protection
  p['vaccine 1, week 4, not infected'] <- 1 - p['vaccine 1, week 4, infected'] - p['dropped out']
  P['vaccine 1, week 3, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccinated 1, infected'] <- (1-v1e)*covid_per_day # full protection
  p['vaccinated 1, not infected'] <- 1 - p['vaccinated 1, infected'] - p['dropped out']
  P['vaccine 1, week 4, not infected',] <- p
  P['vaccinated 1, not infected',] <- p
  
  # vaccine 2 progression
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 2, week 2, infected'] <- (1-v1e*1/4)*covid_per_day # partial protection
  p['vaccine 2, week 2, not infected'] <- 1 - p['vaccine 2, week 2, infected'] - p['dropped out']
  P['vaccine 2, week 1, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 2, week 3, infected'] <- (1-v1e*2/4)*covid_per_day # partial protection
  p['vaccine 2, week 3, not infected'] <- 1 - p['vaccine 2, week 3, infected'] - p['dropped out']
  P['vaccine 2, week 2, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 2, week 4, infected'] <- (1-v1e*3/4)*covid_per_day # partial protection
  p['vaccine 2, week 4, not infected'] <- 1 - p['vaccine 2, week 4, infected'] - p['dropped out']
  P['vaccine 2, week 3, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccinated 2, infected'] <- (1-v1e)*covid_per_day # full protection
  p['vaccinated 2, not infected'] <- 1 - p['vaccinated 2, infected'] - p['dropped out']
  P['vaccine 2, week 4, not infected',] <- p
  P['vaccinated 2, not infected',] <- p
  
  # vaccine 3 progression
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 3, week 2, infected'] <- (1-v1e*1/4)*covid_per_day # partial protection
  p['vaccine 3, week 2, not infected'] <- 1 - p['vaccine 3, week 2, infected'] - p['dropped out']
  P['vaccine 3, week 1, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 3, week 3, infected'] <- (1-v1e*2/4)*covid_per_day # partial protection
  p['vaccine 3, week 3, not infected'] <- 1 - p['vaccine 3, week 3, infected'] - p['dropped out']
  P['vaccine 3, week 2, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 3, week 4, infected'] <- (1-v1e*3/4)*covid_per_day # partial protection
  p['vaccine 3, week 4, not infected'] <- 1 - p['vaccine 3, week 4, infected'] - p['dropped out']
  P['vaccine 3, week 3, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccinated 3, infected'] <- (1-v1e)*covid_per_day # full protection
  p['vaccinated 3, not infected'] <- 1 - p['vaccinated 3, infected'] - p['dropped out']
  P['vaccine 3, week 4, not infected',] <- p
  P['vaccinated 3, not infected',] <- p
  
  # quality check
  stopifnot( sum(abs(rowSums(P)) - rep(1, length(col_row_names))) < 1e-8 )

  # return
  return(P)
}
P_transition <- Ptransition()

# update 1 person to next week


# create one person's trajectory
person_status <- rep(0,52)

# initial state
initial_prob <- function(
  vaccinated_start = 0.3,
  vm1 = 0.4, vm2 = 0.4, vm3 = 0.2,
  P_transition # is required
) {
  col_row_names <- colnames(P_transition)
  
  p <- rep(0, length(col_row_names))
  names(p) <- col_row_names

  p['vaccinated 1, not infected'] <- vaccinated_start * vm1
  p['vaccinated 2, not infected'] <- vaccinated_start * vm2
  p['vaccinated 3, not infected'] <- vaccinated_start * vm3
  p['unvaccinated, not infected'] <- 1 - vaccinated_start
  
  return(p)
}

p_init <- initial_prob(P_transition = P_transition)

person_status[1] <- sample( 1:length(p_init), size=1, prob=p_init)
for( w in 2:52) {
  person_status[w] <- sample( 1:length(p_init), size=1, prob=P_transition[person_status[w-1],])
}

# summarize one person's outcome: weeks no vaccination, weeks Vacc A, B, C; infected
person_results <- function(person_status, col_row_names) {
  weeks_unvaccinated <- sum(person_status==which(col_row_names ==  "unvaccinated, not infected"))
  weeks_vaccinated1  <- sum(person_status==which(col_row_names ==  "vaccinated 1, not infected"))
  weeks_vaccinated2  <- sum(person_status==which(col_row_names ==  "vaccinated 2, not infected"))
  weeks_vaccinated3  <- sum(person_status==which(col_row_names ==  "vaccinated 3, not infected"))
  infected_unvaccinated <- person_status[52] == which(col_row_names ==  "unvaccinated, infected")
  infected_partial <- person_status[52] %in% c(
    which(col_row_names ==  "vaccine 1, week 1, infected"), which(col_row_names ==  "vaccine 1, week 2, infected"),
    which(col_row_names ==  "vaccine 1, week 3, infected"), which(col_row_names ==  "vaccine 1, week 4, infected"),
    which(col_row_names ==  "vaccine 2, week 1, infected"), which(col_row_names ==  "vaccine 2, week 2, infected"),
    which(col_row_names ==  "vaccine 2, week 3, infected"), which(col_row_names ==  "vaccine 2, week 4, infected"),
    which(col_row_names ==  "vaccine 3, week 1, infected"), which(col_row_names ==  "vaccine 3, week 2, infected"),
    which(col_row_names ==  "vaccine 3, week 3, infected"), which(col_row_names ==  "vaccine 3, week 4, infected")
  )
  infected_vaccinated1 <- person_status[52] == which(col_row_names ==  "vaccinated 1, infected")
  infected_vaccinated2 <- person_status[52] == which(col_row_names ==  "vaccinated 2, infected")
  infected_vaccinated3 <- person_status[52] == which(col_row_names ==  "vaccinated 3, infected")
  dropped_out <- person_status[52] == which(col_row_names ==  "dropped out")
  person_results <- c(weeks_unvaccinated, weeks_vaccinated1, weeks_vaccinated2, weeks_vaccinated3,
                      infected_unvaccinated, infected_vaccinated1, infected_vaccinated2,
                      infected_vaccinated3, dropped_out)
  names(person_results) <- c("weeks_unvaccinated", "weeks_vaccinated1", "weeks_vaccinated2", "weeks_vaccinated3",
                             "infected_unvaccinated", "infected_vaccinated1", "infected_vaccinated2",
                             "infected_vaccinated3", "dropped_out")
  return(person_results)
}

person_results(person_status, col_row_names = colnames(P_transition))

# simulate one site
simulate_site <- function(
  seed = 1234,
  vaccinated_start = 0.3,
  vaccinated_end = 0.7,
  vm1 = 0.4, vm2 = 0.4, vm3 = 0.2,
  dropout_rate = 0.01,
  v1e = 0.50, v2e = 0.50, v3e = 0.50, # vaccine efficiency
  covid_rate = 10, # new cases per day per 100K
  n = 1000 # number of simulated persons
) {
  set.seed(seed)
  
  # create the site-specific transition matrix
  P_transition <- Ptransition(dropout_rate = dropout_rate,
                              v1e = v1e, v2e = v2e, v3e = v3e,
                              v1r = (vaccinated_end-vaccinated_start)/52*vm1, 
                              v2r = (vaccinated_end-vaccinated_start)/52*vm2, 
                              v3r = (vaccinated_end-vaccinated_start)/52*vm3,
                              covid_rate = covid_rate
                              )
  
  # pre-populate the person-week matrices
  trajectories <- matrix(0, nrow = n, ncol = 52)
  
  # initial state
  p_init <- initial_prob(P_transition = P_transition, vaccinated_start = vaccinated_start,
               vm1 = vm1, vm2 = vm2, vm3 = vm3)
  
  for(i in 1:n) {
    # initialize the state
    trajectories[i,1] <- sample( 1:length(p_init), size=1, prob=p_init)
    # update
    for( w in 2:52) {
      trajectories[i,w] <- sample( 1:length(p_init), size=1, prob=P_transition[trajectories[i,w-1],])
    }
  }
  
  # summaries
  summary <- person_results( trajectories[1,], col_row_names = colnames(P_transition))
  for(i in 2:n) {
    summary <- summary + person_results( trajectories[i,], col_row_names = colnames(P_transition))
  }
  
  return(list(trajectories=trajectories, summary=summary, p_init = p_init, P_transition = P_transition,
              start = trajectories[,1], end = trajectories[,52]))
}

sim <- simulate_site(n=1000, covid_rate = 100, seed=2)
sim$summary
sim$start
sim$end
