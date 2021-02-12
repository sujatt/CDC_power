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

