#' Create transition matrix for the Markov chain of person trajectories.
#' 
#' The transition matrix has have probabilities of transition from a current state
#' (the row) to the new state (the column), and is intended for the right multiplication
#' in the linear algebra of Markov chains. 
#' The probabilities sum up to 1 across the row.
#' It is best to address the entries of the matrix by their labels.
#' 
#' @param dropout_rate Probability of a dropout in a given week (default 0.01).
#' @param covid_rate   Rate of new cases, per day per 100K
#' @param v1e          Effectiveness of vaccine 1 (default 0.50).
#' @param v2e          Effectiveness of vaccine 2 (default 0.50).
#' @param v3e          Effectiveness of vaccine 3 (default 0.50). 
#'                     The two vaccine situation can be modeled by setting this to zero.
#' @param v1r          Probability to get vaccine 1 next week (default 0.01).
#' @param v2r          Probability to get vaccine 1 next week (default 0.01).
#' @param v3r          Probability to get vaccine 1 next week (default 0.01).
#'                     The two vaccine situation can be modeled by setting this to zero.
#'                    
#' @return             33x33 Markov chain transition matrix with informatively labeled rows/columns.
#' 
#' @examples 
#' 
#' Ptransition()
#' Ptransition(dropout_rate = 0, v1e = 0.9, v2e = 0.5, v3e = 0.3, 
#'             v1r = (vaccinated_end-vaccinated_start)/52*v1m,
#'             v2r = (vaccinated_end-vaccinated_start)/52*v1m,
#'             v3r = (vaccinated_end-vaccinated_start)/52*v1m,
#'             covid_rate = 50)
#' 
#' @export
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
  p['vaccine 2, week 2, infected'] <- (1-v2e*1/4)*covid_per_day # partial protection
  p['vaccine 2, week 2, not infected'] <- 1 - p['vaccine 2, week 2, infected'] - p['dropped out']
  P['vaccine 2, week 1, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 2, week 3, infected'] <- (1-v2e*2/4)*covid_per_day # partial protection
  p['vaccine 2, week 3, not infected'] <- 1 - p['vaccine 2, week 3, infected'] - p['dropped out']
  P['vaccine 2, week 2, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 2, week 4, infected'] <- (1-v2e*3/4)*covid_per_day # partial protection
  p['vaccine 2, week 4, not infected'] <- 1 - p['vaccine 2, week 4, infected'] - p['dropped out']
  P['vaccine 2, week 3, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccinated 2, infected'] <- (1-v2e)*covid_per_day # full protection
  p['vaccinated 2, not infected'] <- 1 - p['vaccinated 2, infected'] - p['dropped out']
  P['vaccine 2, week 4, not infected',] <- p
  P['vaccinated 2, not infected',] <- p
  
  # vaccine 3 progression
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 3, week 2, infected'] <- (1-v3e*1/4)*covid_per_day # partial protection
  p['vaccine 3, week 2, not infected'] <- 1 - p['vaccine 3, week 2, infected'] - p['dropped out']
  P['vaccine 3, week 1, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 3, week 3, infected'] <- (1-v3e*2/4)*covid_per_day # partial protection
  p['vaccine 3, week 3, not infected'] <- 1 - p['vaccine 3, week 3, infected'] - p['dropped out']
  P['vaccine 3, week 2, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccine 3, week 4, infected'] <- (1-v3e*3/4)*covid_per_day # partial protection
  p['vaccine 3, week 4, not infected'] <- 1 - p['vaccine 3, week 4, infected'] - p['dropped out']
  P['vaccine 3, week 3, not infected',] <- p
  
  p <- p0
  names(p) <- col_row_names
  p['dropped out'] <- dropout_rate
  p['vaccinated 3, infected'] <- (1-v3e)*covid_per_day # full protection
  p['vaccinated 3, not infected'] <- 1 - p['vaccinated 3, infected'] - p['dropped out']
  P['vaccine 3, week 4, not infected',] <- p
  P['vaccinated 3, not infected',] <- p
  
  # quality check
  stopifnot( sum(abs(rowSums(P)) - rep(1, length(col_row_names))) < 1e-8 )

  # return
  return(P)
}

#' Compute the vector of initial probabilities for the Markov chain trajectory
#' 
#' @param vaccinated_start   Proportion vaccinated at the start of the study.
#' @param vm1                Market share of vaccine 1 (default 0.4).
#' @param vm2                Market share of vaccine 1 (default 0.4).
#' @param vm3                Market share of vaccine 1 (default 0.2).
#'                     The two vaccine situation can be modeled by setting this to zero.
#' @param P_transition       Transition matrix; only needed to inherit the names.      
#' 
#' @examples
#' 
#' person_status <- rep(0,52)
#' p_init <- initial_prob(P_transition = P_transition, vaccinated_start = 0.3,
#'                         vm1 = 0.5, vm2 = 0.3, vm3 = 0.2)
#' person_status[1] <- sample( 1:length(p_init), size=1, prob=p_init)
#' 
#' @export
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

#' Summarize one person's outcome
#' 
#' This function takes a person's 52-week trajectory and provides a summary of person-weeks and statuses.
#' 
#' @param person_status   A 52-vector of statuses in the study
#' @param col_row_names   A 33-vector of labels
#' 
#' @return A vector with entries:
#' 
#'         weeks_unvaccinated, weeks_vaccinated1, weeks_vaccinated2, weeks_vaccinated3: 
#'               number from 0 to 52 in this status
#'         infected_unvaccinated, infected_vaccinated1, infected_vaccinated2,
#'         infected_vaccinated3, infected_partial, dropped_out:
#'               0/1 logical variable
#'               
#' @examples person_results(person_status, col_row_names = colnames(P_transition))
#'               
#' @export
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
                      infected_vaccinated3, infected_partial, dropped_out)
  names(person_results) <- c("weeks_unvaccinated", "weeks_vaccinated1", "weeks_vaccinated2", "weeks_vaccinated3",
                             "infected_unvaccinated", "infected_vaccinated1", "infected_vaccinated2",
                             "infected_vaccinated3", "infected_partial", "dropped_out")
  return(person_results)
}

#' Simulate site settings
#' 
#' @param n_sites   Number of sites, required
#' @param attr_lo   The lower range of attrition rates
#' @param attr_hi   The upper range of attrition rates
#' @param covid_lo  The lower range of attrition rates
#' @param covid_hi  The upper range of attrition rates
#' @param seed      Random seed for reproducibility
#' 
#' @return          Data frame n_sites x 3 with "Site", "Attrition Rate", "Covid Rate" variables
#' 
#' @export
simulate_site_settings <- function(n_sites, attr_lo, attr_hi, covid_lo, covid_hi, seed=NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  df1 <- as.data.frame(matrix(0,nrow=n_sites,3)) # Dummy dataframes to contain the results 
  colnames(df1) <- c("Site","Attrition Rate","Covid Rate")
  df1$Site <- paste("Site",1:n_sites)
  df1$`Attrition Rate` <- runif(n_sites)*(attr_hi-attr_lo) + attr_lo
  df1$`Covid Rate`     <- runif(n_sites)*(covid_hi-covid_lo) + covid_lo
  
  return(df1)
}

#' Simulate vaccination and infection trajectories of one site
#' 
#' Based on the input parameters, creates a transition matrix internally,
#' and runs the 52-week trajectories for the specified number of participants.
#' 
#' @param vaccinated_start    Proportion vaccinated at the beginning of the study, default 0.3
#' @param vaccinated_end      Proportion vaccinated at the beginning of the study, default 0.7
#' @param vm1                 Market share of vaccine 1 (default 0.4).
#' @param vm2                 Market share of vaccine 1 (default 0.4).
#' @param vm3                 Market share of vaccine 1 (default 0.2).
#'                            The two vaccine situation can be modeled by setting this to zero.
#' @param v1e                 Effectiveness of vaccine 1 (default 0.50).
#' @param v2e                 Effectiveness of vaccine 2 (default 0.50).
#' @param v3e                 Effectiveness of vaccine 3 (default 0.50). 
#'                            The two vaccine situation can be modeled by setting this to zero.
#' @param dropout_rate        Probability of a dropout in a given week (default 0.01).
#' @param covid_rate          Rate of new cases, per day per 100K (default 10)
#' @param n                   Number of participants per site (default 1000)
#' @param seed                Random seed for reproducibility
#' @param verbose             Produce diagnostic output
#' 
#' @return                    list with entries
#'                            $trajectories: the whole (n x 52) matrix of simulated trajectories
#'                            $summary: the aggregate of person_results()
#'                            $p_init: the vector of the initial state probabilities
#'                            $P_transition: the transition matrix
#'                            $start: the starting state (i.e., the first column of the trajectories)
#'                            $end: the ending state (i.e., the last column of trajectories)
#'                            $persons: person level summaries
#'                            
#' @examples
#' 
#'                            simulate_site_trajectories(n=1000, covid_rate = 100, seed=2, verbose = TRUE)                            
#'                            
#' @seealso                   [Ptransition()] for the Markov chain transition matrix,
#'                            [person_results()] for the aggregation of individual trajectories,
#'                            [p_init()] for the initial state probabilities                             
#' @export                            
simulate_site_trajectories <- function(
  seed = NULL,
  vaccinated_start = 0.3,
  vaccinated_end = 0.7,
  vm1 = 0.4, vm2 = 0.4, vm3 = 0.2,
  dropout_rate = 0.01,
  v1e = 0.50, v2e = 0.50, v3e = 0.50, # vaccine efficiency
  covid_rate = 10, # new cases per day per 100K
  n = 1000, # number of simulated persons
  verbose = FALSE
) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (verbose) {
    print("Parameters in simulate_site_trajectories:")
    print( c(vaccinated_start = vaccinated_start, vaccinated_end = vaccinated_end, 
             vm1 = vm1, vm2 = vm2, vm3 = vm3, v1e = v1e, v2e = v2e, v3e = v3e, 
             covid_rate = covid_rate, dropout_rate = dropout_rate) )
  }
  
  # create the site-specific transition matrix
  P_transition <- Ptransition(dropout_rate = dropout_rate,
                              v1e = v1e, v2e = v2e, v3e = v3e,
                              v1r = (vaccinated_end-vaccinated_start)/52*vm1, 
                              v2r = (vaccinated_end-vaccinated_start)/52*vm2, 
                              v3r = (vaccinated_end-vaccinated_start)/52*vm3,
                              covid_rate = covid_rate
                              )
  
  if (verbose) {
    print("Some checks on the transition matrix:")
    print(P_transition['unvaccinated, not infected','unvaccinated, infected'])
    print(P_transition['vaccinated 1, not infected','vaccinated 1, infected'])
    print(P_transition['vaccinated 2, not infected','vaccinated 2, infected'])
    print(P_transition['vaccinated 3, not infected','vaccinated 3, infected'])
  }
  
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
  person1 <- person_results( trajectories[1,], col_row_names = colnames(P_transition))
  persons <- matrix(0,nrow=n, ncol=length(person1))
  
  for(i in 1:n) {
    persons[i,] <- person_results( trajectories[i,], col_row_names = colnames(P_transition))
  }
  colnames(persons) <- names(person1)
  summary <- colSums(persons)
  summary <- c(summary, 'n'=n)
    
  return(list(trajectories=trajectories, persons=persons, summary=summary, p_init = p_init, P_transition = P_transition,
              start = trajectories[,1], end = trajectories[,52]))
}

#' Simulate vaccination and infection trajectories of one site
#' 
#' Based on the input parameters, creates a transition matrix internally,
#' and runs the 52-week trajectories for the specified number of participants.
#' 
#' @param vaccinated_start    Proportion vaccinated at the beginning of the study, default 0.3
#' @param vaccinated_end      Proportion vaccinated at the beginning of the study, default 0.7
#' @param vm1                 Market share of vaccine 1 (default 0.4).
#' @param vm2                 Market share of vaccine 1 (default 0.4).
#' @param vm3                 Market share of vaccine 1 (default 0.2).
#'                            The two vaccine situation can be modeled by setting this to zero.
#' @param v1e                 Effectiveness of vaccine 1 (default 0.50).
#' @param v2e                 Effectiveness of vaccine 2 (default 0.50).
#' @param v3e                 Effectiveness of vaccine 3 (default 0.50). 
#'                            The two vaccine situation can be modeled by setting this to zero.
#' @param n                   Number of participants per site (default 1000)
#' @param seed                Random seed for reproducibility
#' @param sites               n_sites x 3 data frame with site parameters
#' @param verbose             Produce more diagnostic output
#' 
#' @return                    $site_summary: (n_sites x 11) data frame of site summaries
#'                            $site_perseons: ( (n_sites*n) x 12 ) data frame of person-level results
#'                            
#' @examples
#'             df1 <- simulate_site_settings(n_sites = 8, attr_lo = 0.2, attr_hi = 0.5, covid_lo = 10, covid_hi = 50)
#'             sim_study <- simulate_study(sites=df1)
#'                            
#' @seealso                   [Ptransition()] for the Markov chain transition matrix,
#'                            [person_results()] for the aggregation of individual trajectories,
#'                            [p_init()] for the initial state probabilities                             
#' @export                            

simulate_study <- function(
  verbose = FALSE,
  seed = NULL,
  vaccinated_start = 0.3,
  vaccinated_end = 0.7,
  vm1 = 0.4, vm2 = 0.4, vm3 = 0.2,
  v1e = 0.50, v2e = 0.50, v3e = 0.50, # vaccine efficiency
  sites,   # data frame of site parameters
  n = 1000 # number of simulated persons
) {

  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (verbose) {
    print("Parameters in simulate_study():")
    print( c(vaccinated_start = vaccinated_start, vaccinated_end = vaccinated_end, 
             vm1 = vm1, vm2 = vm2, vm3 = vm3, v1e = v1e, v2e = v2e, v3e = v3e, n_sites=nrow(sites), n=n) )
  }
  
  n_sites <- nrow(sites)
  site_summary <- as.data.frame( matrix(0,nrow=0,ncol=11) )
  # sites_persons <- as.data.frame( matrix(0, nrow=0, ncol=0))
  
  for(k in 1:n_sites) {
    this_site <- simulate_site_trajectories(
      verbose = verbose,
      vaccinated_start = vaccinated_start, vaccinated_end = vaccinated_end,
      vm1 = vm1, vm2 = vm2, vm3 = vm3, v1e = v1e, v2e = v2e, v3e = v3e,
      n = n, covid_rate = sites[k,'Covid Rate'],
      dropout_rate = -log(1-sites[k,'Attrition Rate']/100)/52
    )
    site_summary[k,] <- this_site$summary
    persons_summary <- as.data.frame(this_site$persons)
    persons_summary$site <- k
    if (k==1) sites_persons <- persons_summary
    else sites_persons <- bind_rows( sites_persons, persons_summary )
  }
  names(site_summary) <- names(this_site$summary)
    
  return(list(site_summary=site_summary, sites_persons=sites_persons) )
}  

#' Crude vaccine efficiency statistics
#' 
#' @param site_summary  site summary returned by simulate_study()
#' 
#' @seealso    [simulate_study()] for the parent function
#' 
#' @return     vector with entries for vaccine efficiency named 've1', 've2',  've3'
#' 
#' @export
crude_veff <- function(site_summary) {

  pooled_sim_study <- colSums(site_summary)
  
  ir0 <- pooled_sim_study['infected_unvaccinated']/pooled_sim_study['weeks_unvaccinated']
  ir1 <- pooled_sim_study['infected_vaccinated1']/pooled_sim_study['weeks_vaccinated1']
  ir2 <- pooled_sim_study['infected_vaccinated2']/pooled_sim_study['weeks_vaccinated2']
  ir3 <- pooled_sim_study['infected_vaccinated3']/pooled_sim_study['weeks_vaccinated3']
  
  ve1 <- 1 - ir1/ir0
  ve2 <- 1 - ir2/ir0
  ve3 <- 1 - ir3/ir0
  
  return(c('ve1'=unname(ve1), 've2'=unname(ve2), 've3'=unname(ve3) ) )
}

#' Vaccine efficiency statistics, corrected for clustering in sites
#' 
#' @param sites_persons person level summaries returned by simulate_study()
#' 
#' @seealso    [simulate_study()] for the parent function, [svycontrast()]
#' 
#' @return     svyconstrast object: estimates and standard errors.
#'             The estimates can be extracted by names or with coef(); the standard errors, with SE().
#' 
#' @export
cl_veff <- function(sites_persons) {
  
  require(survey)
  svy_study <- svydesign(ids=~site, data=sites_persons, weights = ~1)
  totals <- svytotal(~
             weeks_unvaccinated + weeks_vaccinated1 + weeks_vaccinated2 + weeks_vaccinated3 +
             infected_unvaccinated + infected_vaccinated1 + infected_vaccinated2 + infected_vaccinated3, 
           design=svy_study)
  svycontrast(totals, list(
    ve1 = quote(1-(infected_vaccinated1/weeks_vaccinated1)/(infected_unvaccinated/weeks_unvaccinated)),
    ve2 = quote(1-(infected_vaccinated2/weeks_vaccinated2)/(infected_unvaccinated/weeks_unvaccinated)),
    ve3 = quote(1-(infected_vaccinated3/weeks_vaccinated3)/(infected_unvaccinated/weeks_unvaccinated))
  )
)

  
}

#' Simulate the distribution of vaccine effectivenss
#' 
#' @param vaccinated_start    Proportion vaccinated at the beginning of the study, default 0.3
#' @param vaccinated_end      Proportion vaccinated at the beginning of the study, default 0.7
#' @param vm1                 Market share of vaccine 1 (default 0.4).
#' @param vm2                 Market share of vaccine 1 (default 0.4).
#' @param vm3                 Market share of vaccine 1 (default 0.2).
#'                            The two vaccine situation can be modeled by setting this to zero.
#' @param v1e                 Effectiveness of vaccine 1 (default 0.50).
#' @param v2e                 Effectiveness of vaccine 2 (default 0.50).
#' @param v3e                 Effectiveness of vaccine 3 (default 0.50). 
#'                            The two vaccine situation can be modeled by setting this to zero.
#' @param n                   Number of participants per site (default 1000)
#' @param seed                Random seed for reproducibility
#' @param sites               n_sites x 3 data frame with site parameters
#' @param verbose             Produce more diagnostic output
#' @param reps                Number of replications (default 100)
#' 
#' @return                    (reps x 5) data frame of estimated vaccine effectiveness
#' @export
simulate_multiple_studies <- function(
    verbose = FALSE,
    seed = NULL,
    vaccinated_start = 0.3,
    vaccinated_end = 0.7,
    vm1 = 0.4, vm2 = 0.4, vm3 = 0.2,
    v1e = 0.50, v2e = 0.50, v3e = 0.50, # vaccine efficiency
    sites = NULL,   # data frame of site parameters
    n = 1000, # number of simulated persons per site
    reps = 100 # number of replications
) {

  if (is.null(sites)) {
    stop("sites argument is requried")
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  veff_dist <- data.frame( r=1:reps, v1e = NA, v2e = NA, v3e=NA, se_v1e=NA, se_v2e = NA, se_v3e=NA, timing = NA)
  
  for(i in 1:reps) {
    timing <- system.time(this_study <- simulate_study( 
        verbose = FALSE,
        sites = sites, n = n,
        vaccinated_start = vaccinated_start,
        vaccinated_end   = vaccinated_end,
        vm1 = vm1, v1e = v1e, 
        vm2 = vm2, v2e = v2e, 
        vm3 = vm3, v3e = v3e ) )
    this_veff <- cl_veff(this_study$sites_persons)
    veff_dist[i,2:4] <- coef(this_veff)
    veff_dist[i,5:7] <- SE(this_veff)
    veff_dist[i,'timing'] <- timing['elapsed']
    cat('.')
    if (i %% 20 == 0) cat(' ', i, '\n')
  }
  
  return(veff_dist)  
}
