# ==================================================================================================== #
# Sensitivity analysis of the model using variance based techniques for >>> 8 parameters <<<  #
# description: Perform a sensitivity analysis for Simulation Sim2.1
#             (single pesticide event with symmetric migration)
#              - use a variance based Method described in Satelli 2010
#                called soboljansen() in the package "sensitivity"
#              - vary 8 parameters, including the structural ones (length_eggs & l_adult) 
# Author: Bernhard Kuehn 
# last change: 20.02.2017
# revised by RBS on 5.3.2017
# ==================================================================================================== #

rm(list=ls(all=TRUE))

# # set library
# .libPaths("/home/rstudio/R/x86_64-pc-linux-gnu-library/3.0")
# 
# # install.packages
# install.packages("sensitivity")
# install.packages("boot")
# install.packages("doParallel")
# install.packages("foreach")


# load libraries
library(sensitivity)
library(boot)
library(doParallel)
library(foreach)


#### short Model description #####                            

# Simulation 2.1: Migr_SingEx
# Pesticide event: single, on day 120
# WITH migration from an upstream patch
# Affected parameters: prey population size Ni, emergence rate, growth rate in
# the larval phase. Reduction by 0, 25, 50, 75%, all 
# combinations possible.
# Emigration rate: late instar larvae 0.2, adult 0.3
# mortality rate of migration: 0.3 for late instar larvae and 0.3 for adults

# # set up cluster for parellel computing
#  parallelCluster <- parallel::makeCluster(parallel::detectCores())
# print(parallelCluster)
# ## socket cluster with 4 nodes on host 'localhost'

# --------------------------------------------------------------------------------- #
### Parameters ###

params <- data.frame(# no theta logistic growth
                    eps_linst = 0.2, # migration rate late instar larvae
                    eps_adult = 0.3, # migration rate adult
                    d_linst = 0.7, # 1-mortality larvae
                    d_adult = 0.7, # 1-mortality adult
                    s = 2, # migration shape parameter
                    day_pest = 120, # day of pesticide application
                    l_egg = 40, # duration of egg growth in winter
                    l_adult = 40) # duration of adult period
                    
# ----------------------------------------------------------------------------------- #

# write the model in a different form (as function), so it is possible to use the function soboljansen() on it

simulate.model.sim2.1 = function(X){
  stopifnot(require(simecol))

  
  inits = data.frame(ini_Ni = 3000, # initial value patch A
                     ini_Nj = 3000, # initial value patch B 
                     sim_years = 10 ) #number of simulated years
  
  # constant parameter:
  K = 3000 # carrying capacity like in Speirs 2000
  rd_linst = -log(1/2)/149 # exponential decay of larvae (150days)
  
  # Recovery threshold
  N_recov <- K * 0.9
  
  # years
  sim_years <- inits$sim_years # simulation years
  
  #define years
  i = seq(0,360*sim_years,by = 360)+1 # each start day of the year (e.g. 1, 361 ...)
  
  
  # Reduction of parameters due to pesticide event 
  f_mort <- c(1, 0.75, 0.5, 0.25) # reduces population size of prey (Ni)
  f_emerg <- c(1, 0.75, 0.5, 0.25) # reduces adult rate
  f_growth <- c(1, 0.75, 0.5, 0.25) # reduces growth rate (r2) in the adult/egg phase
  
  
  N_E_G = as.matrix(expand.grid(f_mort,f_emerg,f_growth))
  
  # for sensitivity analysis:
  # calculate model for a matrix input
  
  # ........................................ #
  # for parallelisation: nested foreach- loops
  # ....................................... # 
  
rec_times.all = foreach(iter = 1:nrow(X), .packages='simecol',.combine = 'rbind') %:%   

  
# simulate one single exposure scenario with one predefined parameterset
foreach(z = 1:nrow(N_E_G),.combine = 'c') %dopar% {
  
 # define growth-rate of eggs based on days eggs need to grow
  r_earlinst <- -(log((1/0.9 -1)*(0.5*K)/(K-(0.5*K)))/(X[[iter,7]]))
  
  
  #Matrix for population sizes at the beginning of the year
  N_year <- matrix(NA, nrow=sim_years+1, ncol=2, dimnames = list(c(1:(sim_years+1)),c("Ni", "Nj")))
  N_year[1,] <- c(inits$ini_Ni,inits$ini_Nj)
  
  #Matrix for the daily population sizes, i.a. for plotting
  N_daily <- matrix(NA, nrow=sim_years*360, ncol=3)
  N_daily[1,] <- c(as.integer(1), inits$ini_Ni,inits$ini_Nj)
  N_daily[,1] <- 1:(sim_years*360)
  
  
  
  # ---------------------------------------------------- #
  # Single exposure in the first year of the simulation:  
  # ---------------------------------------------------- #
  
  # Day 1 - x exponential decrease of aquatic larvae due to predation
  linst1_x <- new("odeModel", 
                  main = function (time, init, parms) {
                    with(as.list(c(init, parms)), {
                      # Patch A
                      dNi <-   -r1 * Ni + 
                        r2 * Ni * (1 - (Ni / K)) -  
                        eps * (Ni / K) ^ s * Ni + 
                        d * eps * (Nj / K) ^ s * Nj
                      # Patch B
                      dNj <-   -r1 * Nj +
                        r2 * Nj * (1 - (Nj / K)) - 
                        eps * (Nj / K) ^ s * Nj + 
                        d * eps * (Ni / K) ^ s * Ni
                      list(c(dNi,dNj))
                    })
                  },
                  parms  = c(r1 = rd_linst, K = K,d = X[[iter,3]], eps = X[[iter,1]], 
                             r2 = 0, s = X[[iter,5]]),
                  times  = c(from = 1, to = X[[iter,6]], by = 1),
                  init   = c(Ni = N_year[1,1],Nj = N_year[1,2]),
                  solver = "lsoda"
  )
  
  # Call ODE model  
  res_linst1_x <- sim(linst1_x)
  
  # Store results as matrix
  res_linst1_x_mat <-as.matrix(out(res_linst1_x))
  
  
  # Day x - 150  Pesticide exposure & exponential decrease of aquatic larvae due to predation
  linstx_150 <- new("odeModel", 
                    main = function (time, init, parms) {
                      with(as.list(c(init, parms)), {
                        # Patch A
                        dNi <-   -r1 * Ni + 
                          r2 * Ni * (1 - (Ni / K)) -  
                          eps * (Ni / K) ^ s * Ni + 
                          d * eps * (Nj / K) ^ s * Nj
                        # Patch B
                        dNj <-   -r1 * Nj +
                          r2 * Nj * (1 - (Nj / K)) - 
                          eps * (Nj / K) ^ s * Nj + 
                          d * eps * (Ni / K) ^ s * Ni
                        list(c(dNi,dNj))
                      })
                    },
                    parms  = c(r1 = rd_linst, K = K,d = X[[iter,3]], eps = X[[iter,1]], 
                               r2 = 0, s = X[[iter,5]]),
                    times  = c(from = X[[iter,6]], to = 150, by = 1),
                    # Pesticide exposure (f_mort), only Patch A effected
                    init   = c(Ni = N_E_G[[z,1]] *as.vector(res_linst1_x_mat[nrow(res_linst1_x_mat),2]),
                               Nj = as.vector(res_linst1_x_mat[nrow(res_linst1_x_mat),3])),
                    solver = "lsoda"
  )
  
  # Call ODE model  
  res_linstx_150 <- sim(linstx_150)
  
  # Store results as matrix
  res_linstx_150_mat <- as.matrix(out(res_linstx_150))
  
  # Day 151-190 Adults emerge
  adult151_190 <- new("odeModel", 
                      main = function (time, init, parms) {
                        with(as.list(c(init, parms)), {
                          # Patch A
                          dNi <-   -r1 * Ni + 
                            r2 * Ni * (1 - (Ni / K)) -  
                            eps * (Ni / K) ^ s * Ni + 
                            d * eps * (Nj / K) ^ s * Nj
                          # Patch B
                          dNj <-   -r1 * Nj +
                            r2 * Nj * (1 - (Nj / K)) - 
                            eps * (Nj / K) ^ s * Nj + 
                            d * eps * (Ni / K) ^ s * Ni
                          list(c(dNi,dNj))
                        })
                      },
                      parms  = c(r1 = 0, K = K,d = X[[iter,4]], eps = X[[iter,2]], 
                                 r2 = 0, s = X[[iter,5]]),
                      times  = c(from = 151, to = 150+X[[iter,8]], by = 1),
                      # adult reduced by f_emerg, only Patch A effected
                      init   = c(Ni =  N_E_G[[z,2]] * as.vector(res_linstx_150_mat[nrow(res_linstx_150_mat),2]),
                                 Nj =  as.vector(res_linstx_150_mat[nrow(res_linstx_150_mat),3])),
                      solver = "lsoda"
  )
  # Call ODE model  
  res_adult151_190 <- sim(adult151_190)
  
  # Store results as matrix
  res_adult151_190_mat <-as.matrix(res_adult151_190@out)
  
  # Day 191-360 Egg laying, growing of the population till carrying capacity is reached 
  earlinst191_360 <- new("odeModel", 
                     main = function (time, init, parms) {
                       with(as.list(c(init, parms)), {
                         # Patch A
                         # Pesticide event - reduction in r2: G * r2
                         dNi <-   -r1 * Ni + 
                           r2 * Ni * (1 - (Ni / K)) -  
                           eps * (Ni / K) ^ s * Ni + 
                           d * eps * (Nj / K) ^ s * Nj
                         # Patch B (no pesticide)
                         dNj <-   -r1 * Nj +
                           r2 * Nj * (1 - (Nj / K)) - 
                           eps * (Nj / K) ^ s * Nj + 
                           d * eps * (Ni / K) ^ s * Ni
                         list(c(dNi,dNj))
                       })
                     },
                     parms  = c(r1 = 0, K = K,d = X[[iter,3]], eps = 0, 
                                r2 = N_E_G[[z,3]]* r_earlinst, s = X[[iter,5]]),
                     times  = c(from = 150+X[[iter,8]], to = 360, by = 1),
                     init   = c(Ni = as.vector(res_adult151_190_mat[nrow(res_adult151_190_mat),2]),
                                Nj = as.vector(res_adult151_190_mat[nrow(res_adult151_190_mat),3])),
                     solver = "lsoda"
  )
  # Call ODE model  
  res_earlinst191_360 <- sim(earlinst191_360)
  
  # Store results as matrix
  res_earlinst191_360_mat <- as.matrix(res_earlinst191_360@out)
  
  # store results of the whole year in N_daily-matrix for plotting 
  
  N_daily[i[1]:(i-1)[2],2:3] <- rbind(res_linst1_x_mat[,-1],
                                      res_linstx_150_mat[-1,-1],
                                      res_adult151_190_mat[,-1],
                                      res_earlinst191_360_mat[-1,-1])
  
  
  #Storing population size in N for following year
  N_year[2,1] <- res_earlinst191_360_mat[nrow(res_earlinst191_360_mat),2]
  N_year[2,2] <- res_earlinst191_360_mat[nrow(res_earlinst191_360_mat),3]
  
  # --------------------------------------------------- #
  # Rest of the simulation period: no further exposure
  # --------------------------------------------------- #
  
  for(j in 2:sim_years) {
    
    # Day 1-150 exponential decrease of aquatic larvae due to predation
    linst1_150 <- new("odeModel", 
                      main = function (time, init, parms) {
                        with(as.list(c(init, parms)), {
                          # Patch A
                          dNi <-   -r1 * Ni + 
                            r2 * Ni * (1 - (Ni / K)) -  
                            eps * (Ni / K) ^ s * Ni + 
                            d * eps * (Nj / K) ^ s * Nj
                          # Patch B
                          dNj <-   -r1 * Nj +
                            r2 * Nj * (1 - (Nj / K)) - 
                            eps * (Nj / K) ^ s * Nj + 
                            d * eps * (Ni / K) ^ s * Ni
                          list(c(dNi,dNj))
                        })
                      },
                      parms  = c(r1 = rd_linst, K = K,d = X[[iter,3]], eps = X[[iter,1]], 
                                 r2 = 0, s = X[[iter,5]]),
                      times  = c(from = 1, to = 150, by = 1),
                      init   = c(Ni = N_year[j,1],Nj = N_year[j,2]),
                      solver = "lsoda"
    )
    
    # Call ODE model  
    res_linst1_150 <- sim(linst1_150)
    
    # Store results as matrix
    res_linst1_150_mat <-as.matrix(out(res_linst1_150))
    
    # Day 151-190 Adults emerge
    adult151_190 <- new("odeModel", 
                        main = function (time, init, parms) {
                          with(as.list(c(init, parms)), {
                            # Patch A
                            dNi <-   -r1 * Ni + 
                              r2 * Ni * (1 - (Ni / K)) -  
                              eps * (Ni / K) ^ s * Ni + 
                              d * eps * (Nj / K) ^ s * Nj
                            # Patch B
                            dNj <-   -r1 * Nj +
                              r2 * Nj * (1 - (Nj / K)) - 
                              eps * (Nj / K) ^ s * Nj + 
                              d * eps * (Ni / K) ^ s * Ni
                            list(c(dNi,dNj))
                          })
                        },
                        parms  = c(r1 = 0, K = K,d = X[[iter,4]], eps = X[[iter,2]], 
                                   r2 = 0, s = X[[iter,5]]),
                        times  = c(from = 151, to = 150+X[[iter,8]], by = 1),
                        init   = c(Ni = as.vector(res_linst1_150_mat[nrow(res_linst1_150_mat),2]),
                                   Nj = as.vector(res_linst1_150_mat[nrow(res_linst1_150_mat),3])),
                        solver = "lsoda"
    )
    # Call ODE model  
    res_adult151_190 <- sim(adult151_190)
    
    # Store results as matrix
    res_adult151_190_mat <-as.matrix(res_adult151_190@out)
    
    # Day 191-360 Egg laying, growing of the population till carrying capacity is reached 
    earlinst191_360 <- new("odeModel", 
                       main = function (time, init, parms) {
                         with(as.list(c(init, parms)), {
                           # Patch A
                           dNi <-   -r1 * Ni + 
                             r2 * Ni * (1 - (Ni / K)) -  
                             eps * (Ni / K) ^ s * Ni + 
                             d * eps * (Nj / K) ^ s * Nj
                           # Patch B
                           dNj <-   -r1 * Nj +
                             r2 * Nj * (1 - (Nj / K)) - 
                             eps * (Nj / K) ^ s * Nj + 
                             d * eps * (Ni / K) ^ s * Ni
                           list(c(dNi,dNj))
                         })
                       },
                       parms  = c(r1 = 0, K = K,d = X[[iter,3]], eps = 0, 
                                  r2 = r_earlinst, s = X[[iter,5]]),
                       times  = c(from = 150+X[[iter,8]], to = 360, by = 1),
                       init   = c(Ni = as.vector(res_adult151_190_mat[nrow(res_adult151_190_mat),2]),
                                  Nj = as.vector(res_adult151_190_mat[nrow(res_adult151_190_mat),3])),
                       solver = "lsoda"
    )
    # Call ODE model  
    res_earlinst191_360 <- sim(earlinst191_360)
    
    # Store results as matrix
    res_earlinst191_360_mat <-as.matrix(res_earlinst191_360@out)
    
    # store results of the whole year in N_daily-matrix for plotting 
    
    N_daily[i[j]:(i-1)[j+1],2:3] <- rbind(res_linst1_150_mat[,-1],
                                          res_adult151_190_mat[,-1],
                                          res_earlinst191_360_mat[-1,-1])
    
    
    #Storing population size in N for following year
    N_year[j+1,1] <- res_earlinst191_360_mat[nrow(res_earlinst191_360_mat),2]
    N_year[j+1,2] <- res_earlinst191_360_mat[nrow(res_earlinst191_360_mat),3]
        
  } # end of time loop
  
  ### Calculate recovery times
  # first value below threshold of K * 0.9
  v1 <- which(N_daily[,2] < N_recov)[1]
  # time until the threshold is reached again = recovery time
  rec_time <- which(N_daily[(v1+1):nrow(N_daily),2] >= N_recov)[1]
  
  rec_time

}
return(rec_times.all)

}

# # check how long single model evaluation takes
# ptm = proc.time()
# simulate.model.sim2.1(rbind(params,params+params*0.1))
# proc.time() - ptm
# 
#  # Shutdown cluster neatly
#  if(!is.null(parallelCluster)) {
#    parallel::stopCluster(parallelCluster)
#    parallelCluster <- c()
#  }

# ====================================== #
# actual sensitivity analysis:
# variance based technique from sobol & jansen
# ====================================== #

# --------------------------------------- #
# prepare input for sensitivity analysis
# --------------------------------------- #

n <- 1000 # number of bootstrap samples

# which parameters are getting varied?
# theta, eps_linst,eps_adult,d_linst,d_adult,s,day_pest,l_egg,l_adult
# +-50%
# parameters constant: K, rd_linst. , theta
# extra conditions for: day_pest upper limit = 150,
#                       d_linst ~ [0.5,1],
#                       d_adult ~ [0.5,1],

# set upper limit
upper_limit = params + params*0.5
# upper_limit$theta = 1 # no theta
upper_limit$day_pest = 150
upper_limit$d_linst = 1
upper_limit$d_adult = 1

# set lower limit
lower_limit = params - params*0.5
lower_limit$day_pest = 90
# lower_limit$theta = 1 # no theta 
lower_limit$d_linst = 0.5
lower_limit$d_adult = 0.5

# set initial values from an uniform distribution 
X1 = X2 = matrix(ncol = length(params), nrow = n)
for(i in 1:length(params)) {
 X1[,i] = runif(n,min = lower_limit[[i]],max = upper_limit[[i]])
 X2[,i] <- runif(n,min = lower_limit[[i]],max = upper_limit[[i]])
}
# round values, where discrete input is required: (discrete timesteps in the model)
X1[,6] = floor(X1[,6])
X2[,6] = floor(X2[,6])
X1[,7] = floor(X1[,7])
X2[,7] = floor(X2[,7])
X1[,8] = floor(X1[,8])
X2[,8] = floor(X2[,8])
# define colnames for input data, otherwise sobol-function throws an error
colnames(X1) = colnames(X2) = colnames(params)

# --------------------------------------- # 
# perform the sensitivity analysis
# and record the time the analysis takes
# --------------------------------------- #

# set up cluster for parellel computing
num_cores = detectCores()-2
cl<-makeCluster(num_cores)
registerDoParallel(cl)

ptm = proc.time()
sens.out.S2.1.allParams = soboljansen(model = simulate.model.sim2.1,X1,X2,nboot = 1000)
proc.time() - ptm

# save output
save(sens.out.S2.1.allParams,file = "../Sens.out.S2.1.allParams.RData")

# Shutdown cluster neatly
if(!is.null(cl)) {
  stopCluster(cl)
  stopImplicitCluster()
  cl <- c()
}


