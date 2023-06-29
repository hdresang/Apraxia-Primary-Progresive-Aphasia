beta_coef_prior = prior(student_t(3, 0, 2.5), class = b)
BayesIntxn <- brm(Score ~ 1 + Component*Subtype*Task + Severity + (1|Subject) + (1|Item), 
                  data = ppaGnoAP, 
                  family = bernoulli,       
                  warmup = 1500, 
                  iter = 6000, 
                  chains = 4, 
                  cores = 4, 
                  seed = 42,
                  prior = beta_coef_prior,
                  #backend = 'cmdstan',
                  init = "random", 
                  #adapt_delta = 0.99, 
                  #file = here("model output", "event_kdt"),
                  file_refit = "on_change")

pp_check(BayesIntxn, ndraws = 600) 
summary(BayesIntxn, prob = .9)
conditional_effects(BayesIntxn)
prior_summary(BayesIntxn)
mcmc_plot(BayesIntxn, type = 'trace') 

hypothesis(BayesIntxn, class = "b", "Taskmeaningless >0") 
hypothesis(BayesIntxn, class = "b", "SubtypeSemantic >0") 
hypothesis(BayesIntxn, class = "b", "SubtypeNonfluent >0") 
hypothesis(BayesIntxn, class = "b", "ComponentKIN >0") 
hypothesis(BayesIntxn, class = "b", "ComponentKIN:SubtypeSemantic:Taskmeaningless >0") 
hypothesis(BayesIntxn, class = "b", scope=c("standard", "ranef", "coef")) 

emmeans(BayesIntxn, pairwise~Component*Subtype*Task, adjust="tukey")