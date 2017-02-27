# ----------------------------------------------- #
# Simulation 2.1 with additional factor level 0.5:
# ----------------------------------------------- # 
# Pesticide event: single exposure, on day 120
# with symmetric migration from an upstream patch

library(simecol)

# needs ajdustment to your path:
setwd("~/Desktop/Modelling_Twopatch/Latest_scripts/results")

# --------------------------------------------------------------------------------- #
### Parameters ###

# Carrying capacity
K <- 3000 # following Speirs 2000

# Recovery threshold
N_recov <- K * 0.9

# Initial population sizes
N_i = N_j <- K 

# parameter for theta-logistic growth
theta = 1
# we only model logistic growth, but setting the parameter to different values
# allows to implement theta logistic growth, but see discussion in:
# Clark F., Brook B.W., Delean S., Reşit Akçakaya H. & Bradshaw C.J.A. (2010) 
# The theta-logistic is unreliable for modelling most census data. 
# Methods in Ecology and Evolution 1, 253–262.

# growth rates 
r_linst <- -log(1/2)/149 # Mortality rate for water phase
l_earlinst <- 40
r_earlinst <- -(log((1/0.9^theta-1)*(0.5*K)^theta/(K^theta-(0.5*K)^theta))/(l_earlinst*theta))

# Immigration parameters
eps_linst = 0.2  # mobility of linsts (emigration fraction)
eps_adult = 0.3 # higher mobility of adults  
d_linst <- 0.7 # 1-d = migration mortality, i.e. 0.3 for linsts and 0.2 for adults
d_adult <- 0.7
s = 2 # 

### Pesticide Parameters ###
day_pesticide = 120 # day where the pesticide enters the water column

# Reduction of parameters due to pesticide event 
f_mort <- c(1, 0.75,0.5, 0.25) # reduces population size of prey (Ni)
f_emerg <- c(1, 0.75,0.5, 0.25) # reduces emergence rate
f_growth <- c(1, 0.75,0.5, 0.25) # reduces growth rate (r_earlinst)
# ----------------------------------------------------------------------------------- #

# years
sim_years <- 10 # simulation years
i = seq(0,360*sim_years,by = 360)+1 # each start day of the year (e.g. 1, 361 ...)

# Matrix for population sizes at the beginning of the year
N_year <- matrix(NA, nrow=sim_years+1, ncol=2, dimnames = list(c(1:(sim_years+1)),c("Ni", "Nj")))
N_year[1,] <- c(N_i,N_j)

# Matrix for the daily population sizes, i.a. for plotting
N_daily <- matrix(NA, nrow=sim_years*360, ncol=3)
N_daily[1,] <- c(as.integer(1), N_i,N_j)
N_daily[,1] <- 1:(sim_years*360)

### Simulate first year with pesticide event and no migration  

d <- NULL

for (N in f_mort){
  for (E in f_emerg){
    for (G in f_growth){
      
      # ---------------------------------------------------- #
      # Single exposure in the first year of the simulation:  
      # ---------------------------------------------------- #
      
      # Day 1 to day_pesticide exponential decrease of aquatic late instars (see paper)
      linst1_x <- new("odeModel", 
                      main = function (time, init, parms) {
                        with(as.list(c(init, parms)), {
                          # Patch I (polluted)
                          dNi <-   -r1 * Ni + 
                            r2 * Ni * (1 - (Ni / K) ^ theta) -  
                            eps * (Ni / K) ^ s * Ni + 
                            d * eps * (Nj / K) ^ s * Nj
                          # Patch J (upstream)
                          dNj <-   -r1 * Nj +
                            r2 * Nj * (1 - (Nj / K) ^ theta) - 
                            eps * (Nj / K) ^ s * Nj + 
                            d * eps * (Ni / K) ^ s * Ni
                          list(c(dNi,dNj))
                        })
                      },
                      parms  = c(r1 = r_linst, K = K,d = d_linst, eps = eps_linst, 
                                 r2 = 0, s = s, theta = theta),
                      times  = c(from = 1, to = day_pesticide, by = 1),
                      init   = c(Ni = N_year[1,1],Nj = N_year[1,2]),
                      solver = "lsoda"
      )
      
      # Call ODE model  
      res_linst1_x <- sim(linst1_x)
      
      # Store results as matrix
      res_linst1_x_mat <-as.matrix(out(res_linst1_x))
      
      
      # Day day_pesticide to 150  Pesticide exposure & decrease of aquatic late instars
      linstx_150 <- new("odeModel", 
                        main = function (time, init, parms) {
                          with(as.list(c(init, parms)), {
                            # Patch I (polluted)
                            dNi <-   -r1 * Ni + 
                              r2 * Ni * (1 - (Ni / K) ^ theta) -  
                              eps * (Ni / K) ^ s * Ni + 
                              d * eps * (Nj / K) ^ s * Nj
                            # Patch J (upstream)
                            dNj <-   -r1 * Nj +
                              r2 * Nj * (1 - (Nj / K) ^ theta) - 
                              eps * (Nj / K) ^ s * Nj + 
                              d * eps * (Ni / K) ^ s * Ni
                            list(c(dNi,dNj))
                          })
                        },
                        parms  = c(r1 = r_linst, K = K,d = d_linst, eps = eps_linst, 
                                   r2 = 0, s = s, theta = theta),
                        times  = c(from = day_pesticide, to = 150, by = 1),
                        # Pesticide exposure (f_mort), only Patch I (polluted) effected
                        init   = c(Ni = N*as.vector(res_linst1_x_mat[nrow(res_linst1_x_mat),2]),
                                   Nj = as.vector(res_linst1_x_mat[nrow(res_linst1_x_mat),3])),
                        solver = "lsoda"
      )
      
      # Call ODE model  
      res_linstx_150 <- sim(linstx_150)
      
      # Store results as matrix
      res_linstx_150_mat <-as.matrix(out(res_linstx_150))
      
      # Day 151-190 Adults emerge
      adult151_190 <- new("odeModel", 
                          main = function (time, init, parms) {
                            with(as.list(c(init, parms)), {
                              # Patch I (polluted)
                              dNi <-   -r1 * Ni + 
                                r2 * Ni * (1 - (Ni / K) ^ theta) -  
                                eps * (Ni / K) ^ s * Ni + 
                                d * eps * (Nj / K) ^ s * Nj
                              # Patch J (upstream)
                              dNj <-   -r1 * Nj +
                                r2 * Nj * (1 - (Nj / K) ^ theta) - 
                                eps * (Nj / K) ^ s * Nj + 
                                d * eps * (Ni / K) ^ s * Ni
                              list(c(dNi,dNj))
                            })
                          },
                          parms  = c(r1 = 0, K = K,d = d_adult, eps = eps_adult, 
                                     r2 = 0, s = s, theta= theta),
                          times  = c(from = 151, to = 190, by = 1),
                          
                          # emergence reduced by f_emerg, only Patch I (polluted) effected
                          init   = c(Ni = E * as.vector(res_linstx_150_mat[nrow(res_linstx_150_mat),2]),
                                     Nj = as.vector(res_linstx_150_mat[nrow(res_linstx_150_mat),3])),
                          solver = "lsoda"
      )
      # Call ODE model  
      res_adult151_190 <-  sim(adult151_190)      
      # Store results as matrix
      res_adult151_190_mat <-as.matrix(res_adult151_190@out)
      
      # Day 191-360 Oviposition and growing of the population
      earlinst191_360 <- new("odeModel", 
                         main = function (time, init, parms) {
                           with(as.list(c(init, parms)), {
                             # Patch I (polluted)
                             # Pesticide event - reduction in r2: G * r2
                             dNi <-   -r1 * Ni + 
                               G * r2 * Ni * (1 - (Ni / K) ^ theta) -  
                               eps * (Ni / K) ^ s * Ni + 
                               d * eps * (Nj / K) ^ s * Nj
                             # Patch J (upstream) (no pesticide)
                             dNj <-   -r1 * Nj +
                               r2 * Nj * (1 - (Nj / K) ^ theta) - 
                               eps * (Nj / K) ^ s * Nj + 
                               d * eps * (Ni / K) ^ s * Ni
                             list(c(dNi,dNj))
                           })
                         },
                         parms  = c(r1 = 0, K = K,d = d_linst, eps = 0, 
                                    r2 = r_earlinst, s = s, theta = theta),
                         times  = c(from = 190, to = 360, by = 1),
                         init   = c(Ni = as.vector(res_adult151_190_mat[nrow(res_adult151_190_mat),2]),
                                    Nj = as.vector(res_adult151_190_mat[nrow(res_adult151_190_mat),3])),
                         solver = "lsoda"
      )
      # Call ODE model  
       res_earlinst191_360 <- sim(earlinst191_360)
      
      # Store results as matrix
      res_earlinst191_360_mat <-as.matrix(res_earlinst191_360@out)
      
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
        
       # Day 1 to 150 exponential decrease of aquatic late instars (see paper)
        linst1_150 <- new("odeModel", 
                          main = function (time, init, parms) {
                            with(as.list(c(init, parms)), {
                              # Patch I (polluted)
                              dNi <-   -r1 * Ni + 
                                r2 * Ni * (1 - (Ni / K) ^ theta) -  
                                eps * (Ni / K) ^ s * Ni + 
                                d * eps * (Nj / K) ^ s * Nj
                              # Patch J (upstream)
                              dNj <-   -r1 * Nj +
                                r2 * Nj * (1 - (Nj / K) ^ theta) - 
                                eps * (Nj / K) ^ s * Nj + 
                                d * eps * (Ni / K) ^ s * Ni
                              list(c(dNi,dNj))
                            })
                          },
                          parms  = c(r1 = r_linst, K = K,d = d_linst, eps = eps_linst, 
                                     r2 = 0, s = s, theta = theta),
                          times  = c(from = 1, to = 150, by = 1),
                          init   = c(Ni = N_year[j,1],Nj = N_year[j,2]),
                          solver = "lsoda"
        )
        
        # Call ODE model  
        res_linst1_150       <- sim(linst1_150)
        
        # Store results as matrix
        res_linst1_150_mat <-as.matrix(out(res_linst1_150))
        
        # Day 151-190 Adults emerge
        adult151_190 <- new("odeModel", 
                            main = function (time, init, parms) {
                              with(as.list(c(init, parms)), {
                                # Patch I (polluted)
                                dNi <-   -r1 * Ni + 
                                  r2 * Ni * (1 - (Ni / K) ^ theta) -  
                                  eps * (Ni / K) ^ s * Ni + 
                                  d * eps * (Nj / K) ^ s * Nj
                                # Patch J (upstream)
                                dNj <-   -r1 * Nj +
                                  r2 * Nj * (1 - (Nj / K) ^ theta) - 
                                  eps * (Nj / K) ^ s * Nj + 
                                  d * eps * (Ni / K) ^ s * Ni
                                list(c(dNi,dNj))
                              })
                            },
                            parms  = c(r1 = 0, K = K,d = d_adult, eps = eps_adult, 
                                       r2 = 0, s = s, theta= theta),
                            times  = c(from = 151, to = 190, by = 1),
                            init   = c(Ni = as.vector(res_linst1_150_mat[nrow(res_linst1_150_mat),2]),
                                       Nj = as.vector(res_linst1_150_mat[nrow(res_linst1_150_mat),3])),
                            solver = "lsoda"
        )
        # Call ODE model  
        res_adult151_190 <-  sim(adult151_190)
        
        # Store results as matrix
        res_adult151_190_mat <-as.matrix(res_adult151_190@out)
        
        # Day 191-360 Egg laying, growing of the population till carrying capacity is reached 
        earlinst191_360 <- new("odeModel", 
                           main = function (time, init, parms) {
                             with(as.list(c(init, parms)), {
                               # Patch I (polluted)
                               dNi <-   -r1 * Ni + 
                                 r2 * Ni * (1 - (Ni / K) ^ theta) -  
                                 eps * (Ni / K) ^ s * Ni + 
                                 d * eps * (Nj / K) ^ s * Nj
                               # Patch J (upstream)
                               dNj <-   -r1 * Nj +
                                 r2 * Nj * (1 - (Nj / K) ^ theta) - 
                                 eps * (Nj / K) ^ s * Nj + 
                                 d * eps * (Ni / K) ^ s * Ni
                               list(c(dNi,dNj))
                             })
                           },
                           parms  = c(r1 = 0, K = K,d = d_linst, eps = 0, 
                                      r2 = r_earlinst, s = s, theta = theta),
                           times  = c(from = 190, to = 360, by = 1),
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

 # store output
       d <- rbind(d, cbind(as.data.frame(N_daily), N, E, G)) 
    }
  }
} 
 
names(d) <- c("Day", "N_i", "N_j", "f_mort", "f_emerg", "f_growth")
 
d$treat <- as.factor(paste(d$f_mort,d$f_emerg, d$f_growth, sep="_"))
      
# compute minimum of Nj
min_nj <- do.call("rbind", (as.list(by(d[,3], factor(d$treat), min))))
 
### compute minimum for Ni
v_min <- do.call("rbind", (as.list(by(d[,2], factor(d$treat), min))))  
            
### Calculate recovery times
# first value below threshold of K * 0.9
v1 <- do.call("rbind", (as.list(by(d[,2], factor(d$treat), function(x) which(x < N_recov)[1]))))
# time until the threshold is reached again = recovery time

rec_time <- data.frame(matrix(data = NA, ncol=2, nrow=length(v1)))
names(rec_time) <- c("Rec_time", "treat")
temp_rec <- NULL

for(I in 1:length(levels(d$treat))) {  
	 # extract data
	 temp_rec <- d[d$treat == levels(d$treat)[I],2]
	 rec_time[I,1] <- which(temp_rec[v1[I]:length(temp_rec)] >= N_recov)[1]	
	 rec_time[I,2] <- levels(d$treat)[I]
	}     

# merge results
v2 <- data.frame(v1, row.names=NULL)
v2$treat <- rownames(v1)
min_nj2 <- data.frame(min_nj, row.names=NULL)
min_nj2$treat <- rownames(min_nj)
v3 <- data.frame(v_min, row.names=NULL)
v3$treat <- rownames(v_min)
diff_temp_a <- merge(rec_time, v2)
diff_temp_b <- merge(diff_temp_a, min_nj2)
diff_temp <- merge(diff_temp_b, v3)
  
# for comparison of different scenarios with different baseline recovery times
# calculation of difference to baseline scenario for recovery and minimum: 
diff_temp$diff = abs(diff_temp[diff_temp$treat=="1_1_1", "Rec_time"] - diff_temp[ , "Rec_time"])  
diff_temp$diff_min = round(abs(diff_temp[diff_temp$treat=="1_1_1", "min_nj"] - diff_temp[ , "min_nj"])) 

# add individual factors
fac1 <- unique(d[ ,4:6])
fac1$treat <- as.factor(paste(fac1$f_mort,fac1$f_emerg, fac1$f_growth, sep="_"))
final <- merge(diff_temp, fac1)
 
# Plot Ni (Patch I (polluted)) and Nj
for(I in 1:length(levels(d$treat)))
     {
     	 png(filename = file.path(getwd(), paste0("figures/", "Sim2_SingleEvent_symmMigr", levels(d$treat)[I], ".png"))) 
      plot(d[d$treat == levels(d$treat)[I],1], d[d$treat == levels(d$treat)[I],2], xlab = "Time [d]", ylab = "Population size", 
           type = "l", lty = 2, lwd = 2, ylim = c(0,7000))
      lines(d[d$treat == levels(d$treat)[I],1], d[d$treat == levels(d$treat)[I],3], type = "l", lty = 1, lwd = 2)     
      legend(20,6980, c("Ni"),lty = 2, lwd=2)
      legend(20,5900, c("Nj"), lty=1, lwd=2)
      text(c(2200,2200,3200,3200,3200), c(6980,5900,6980,6480,5900), labels=c(
        eps_linst,eps_adult, unique(d[d$treat == levels(d$treat)[I],"f_mort"]), unique(d[d$treat == levels(d$treat)[I],"f_emerg"]), unique(d[d$treat == levels(d$treat)[I],"f_growth"])), cex = 0.9)
      text(c(1700,1700,2800,2800,2800), c(6980,5900,6980,6480,5900), c("eps_linst =",
                                                                       "eps_adult   =","f_mort =","f_emerg =","f_growth ="), cex = 0.9)
   dev.off()                                                                    
}

# Export daily population size
write.csv(final, file= paste0(getwd(),"/Sim2_symmMigr_SingEx_rec_time_new.csv"), row.names=F)
 
# Save recovery times
write.csv(d, file = paste0(getwd(),"/Z_Sim2_symmMigr_SingEx_daily_Ns.csv"))