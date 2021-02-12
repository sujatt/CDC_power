# simulate the whole study once

df1 <- simulate_site_settings(n_sites = 8, attr_lo = 20, attr_hi = 50, covid_lo = 10, covid_hi = 50)

system.time( sim_study <- simulate_study(sites=df1, v1e = 0.75, v2e = 0.50, v3e = 0.24, verbose = TRUE) )

(pooled_sim_study <- colSums(sim_study))

ir0 <- pooled_sim_study['infected_unvaccinated']/pooled_sim_study['weeks_unvaccinated']
ir1 <- pooled_sim_study['infected_vaccinated1']/pooled_sim_study['weeks_vaccinated1']
ir2 <- pooled_sim_study['infected_vaccinated2']/pooled_sim_study['weeks_vaccinated2']
ir3 <- pooled_sim_study['infected_vaccinated3']/pooled_sim_study['weeks_vaccinated3']

ve1 <- 1 - ir1/ir0
ve2 <- 1 - ir2/ir0
ve3 <- 1 - ir3/ir0

c('ve1'=unname(ve1), 've2'=unname(ve2), 've3'=unname(ve3) )

# distribution simulation
veff_dist <- data.frame( r=1:100, v1e = NA, v2e = NA, v3e=NA, timing = NA)

for(i in 1:100) {
  timing <- system.time(this_study <- simulate_study( verbose = FALSE,
                                sites = df1, n = 500,
                                vaccinated_start = 0.30,
                                vaccinated_end   = 0.70,
                                vm1 = 0.4, v1e = 0.7, 
                                vm2 = 0.4, v2e = 0.5, 
                                vm3 = 0.2, v3e = 0.2 ) )
  this_veff <- crude_veff(this_study)
  veff_dist[i,2:4] <- this_veff
  veff_dist[i,'timing'] <- timing['elapsed']
  cat('.')
  if (i %% 20 == 0) cat(' ', i, '\n')
}

# same as
veff_dist2 <- simulate_multiple_studies(
  n = 500, reps = 100, seed = 4123,
  verbose = FALSE,
  sites = df1, 
  vaccinated_start = 0.30,
  vaccinated_end   = 0.70,
  vm1 = 0.4, v1e = 0.7, 
  vm2 = 0.4, v2e = 0.5, 
  vm3 = 0.2, v3e = 0.2   
)

ggplot(veff_dist2) + 
  geom_density(aes(x=v1e, linetype='Alpha', colour='Alpha'), size = 1) + 
  geom_density(aes(x=v2e, linetype='Beta', colour='Beta'), size = 1) +
  geom_density(aes(x=v3e, linetype='Gamma', colour='Gamma'), size = 1) + 
  scale_linetype_manual("Vaccines", values=c('dashed', 'dotted', 'twodash')) +
  scale_colour_manual("Vaccines", values=c('#7566A0', '#789D4A', '#E87722')) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab('Vaccine effectiveness') +
  theme_light() +
  theme(legend.position = "bottom", legend.box = "horizontal") # legend at the bottom
  