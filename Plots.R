##############################
# Plots for the publication  #
##############################
setwd("~/Desktop/Modelling_Twopatch/Latest_scripts/")

# read raw data for illustration of dynamics
nomigr_single <- read.table("results/Z_Sim1_NoMigr_SingEx_daily_Ns.csv",header =T, sep = ",")
migr_single <- read.table("results/Z_Sim2_symmMigr_SingEx_daily_Ns.csv",header =T, sep = ",")
nomigr_multi <- read.table("results/Z_Sim3_NoMigr_MultEx_daily_Ns.csv",header =T, sep = ",")
migr_multi <- read.table("results/Z_Sim4_symmMigr_MultiEx_daily_Ns.csv",header =T, sep = ",")

############################
# use subset for Figure 1
#############################
nms <- nomigr_single[nomigr_single$treat =="0.25_0.25_0.25", -1]
ms <- migr_single[migr_single$treat =="0.25_0.25_0.25", -1]
nmm <- nomigr_multi[nomigr_multi$treat =="0.25_0.25_0.25", -1]
mm <- migr_multi[migr_multi$treat =="0.25_0.25_0.25", -1]

names(nms)[1:3] = names(ms)[1:3] = names(nmm)[1:3] = names(mm)[1:3] = c("Time","Ni","Nj")

# create Figure 1
par(cex=1.5, las=1)
plot(nms$Time, nms$Ni, type = "l", lty = 1,lwd =2, ylim = c(0,3200), xlab = "time [d]", ylab = "N")
lines(ms$Time, ms$Ni, lty = 2,lwd = 2, ylim = c(0,3200), col = "grey20")
lines(nmm$Time, nmm$Ni,lty = 3,lwd = 2, ylim = c(0,3200), col = "grey50")
lines(mm$Time, mm$Ni, lty = 4,lwd = 2, ylim = c(0,3200), col = "grey70")
legend(50, 3300,
       c("Nomig_s","Mig_s","Nomig_m","Mig_m"),
       lty = c(1,2,3,4),
       lwd = c(2,2,2,2),
       cex = 0.8,
       col = c("black","grey20","grey50","grey70","red"))

############################
# Figure 2
############################
library('ggplot2')
nomigr_single_rec <- read.table("results/Sim1_NoMigr_SingEx_rec_time_new.csv",header =T, sep = ",")
migr_single_rec <- read.table("results/Sim2_symmMigr_SingEx_rec_time_new.csv",header =T, sep = ",")
asmigr_single_rec <- read.table("results/Sim2_asymmMigr_SingEx_rec_time_new.csv",header =T, sep = ",")
nomigr_multi_rec <- read.table("results/Sim3_NoMigr_MultEx_rec_time_new.csv",header =T, sep = ",")
migr_multi_rec <- read.table("results/Sim4_symmMigr_MultiEx_rec_time_new.csv",header =T, sep = ",")
asmigr_multi_rec <- read.table("results/Sim4_asymmMigr_MultiEx_rec_time_new.csv",header =T, sep = ",")

nomigr_single_rec$mig <- "no migration" 
migr_single_rec$mig <- "symm. migration" 
asmigr_single_rec$mig <- "asymm. migration" 
nomigr_multi_rec$mig <- "no migration"
migr_multi_rec$mig <-  "symm. migration"
asmigr_multi_rec$mig <- "asymm. migration" 

nomigr_single_rec$exp <- "single exposure" 
migr_single_rec$exp <- "single exposure"
asmigr_single_rec$exp <- "single exposure" 
nomigr_multi_rec$exp <- "multiple exposure"
migr_multi_rec$exp <-  "multiple exposure"
asmigr_multi_rec$exp <- "multiple exposure"

d <- list(nomigr_single_rec, migr_single_rec, asmigr_single_rec, nomigr_multi_rec, migr_multi_rec, asmigr_multi_rec)
library(dplyr)
dat_f <- do.call(bind_rows,d)
# check that everything is correct
nrow(dat_f) == 64*6

# Convert to % reduction
dat_f$mor_red <- 1 - dat_f$f_mort
dat_f$em_red <- 1 - dat_f$f_emerg
dat_f$gro_red <- 1 - dat_f$f_growth

dat_f$mig <- factor(dat_f$mig, levels=c("no migration", "symm. migration", "asymm. migration"))
dat_f$exp <- factor(dat_f$exp, levels=c("single exposure","multiple exposure"))
dat_f$mor_red <- factor(dat_f$mor_red, levels=c(0.75,0.5,0.25,0))
dat_f$em_red <- factor(dat_f$em_red, labels =c("0%","25%", "50%", "75%"))
dat_f$gro_red <- factor(dat_f$gro_red, labels =c("0%","25%", "50%", "75%"))

# prepare plots
eff.lab <- c(
  '0' = "no lethal effect",
  '0.25' = paste(eff[2], "lethal effect"),
  '0.5' = paste(eff[3], "lethal effect"),
  '0.75' = paste(eff[4], "lethal effect"))

names(dat_f)[c(3,4)]  <- c("first_below0.9K", "min_ni")

ggplot(dat_f, aes(x = em_red, y = gro_red, fill = diff)) + 
  geom_raster() + 
  facet_grid(mor_red ~ exp + mig, labeller = labeller(.rows=as_labeller(eff.lab))) +
  labs(x = "Reduction of emergence", y = "Reduction in growth", fill = "Recovery time")+
  #scale_fill_gradientn(colors = c("darkblue","gold","darkred")) + 
  scale_fill_gradientn(colors = rev(c(rgb(215,48,39, maxColorValue = 256),
                                      rgb(252,141,89, maxColorValue = 256),
                                      rgb(254,224,144, maxColorValue = 256),
                                      rgb(224,243,248, maxColorValue = 256),
                                      rgb(145,191,219, maxColorValue = 256),
                                      rgb(69,117,180, maxColorValue = 256)))) + 
  #scale_fill_distiller( type = "div" , palette = "RdYlBu" )+ #RdYlBu
  theme_minimal(base_size=15) 
  
# uncomment to save  
# ggsave("Figure_2.png",device="png",height=12,width=16)

############################
# Figure S1 Migration effects
############################

# plot with difference in migration
# a) no migr vs. migr (singEx) 
# b) no migr vs. migr (multEx)
# c) no migr vs. assym migr (singEx) 
# d) no migr vs. assym migr (multEx)
# e) assym migr vs. sym migr (singEx) 
# f) assym migr vs. sym migr (multEx)


library(plyr)
migr.diff <- ddply(dat_f, c("mor_red","em_red","gro_red"), summarize,
                  "Sym_mig vs. no_mig single" = Rec_time[mig == "symm. migration" & exp == "single exposure"] - Rec_time[mig == "no migration" & exp == "single exposure"],
                         "Sym_mig vs. no_mig multi" = Rec_time[mig == "symm. migration" & exp == "multiple exposure"] - Rec_time[mig == "no migration" & exp == "multiple exposure"],
                    "Asym_mig vs. no_mig single" = Rec_time[mig == "asymm. migration" & exp == "single exposure"] - Rec_time[mig == "no migration" & exp == "single exposure"],
                   "Asym_mig vs. no_mig multi" = Rec_time[mig == "asymm. migration" & exp == "multiple exposure"] - Rec_time[mig == "no migration" & exp == "multiple exposure"],
                    "Asym_mig vs. sym_mig single"  = Rec_time[mig == "asymm. migration" & exp == "single exposure"] - Rec_time[mig == "symm. migration" & exp == "single exposure"],
                     "Asym_mig vs. sym_mig multi"  = Rec_time[mig == "asymm. migration" & exp == "multiple exposure"] - Rec_time[mig == "symm. migration" & exp == "multiple exposure"])

# labelling for NAs
# NAs only occur in the multiple exposure scenario
Na.handle <- data.frame(migr.diff[,1:3])
Na.handle$mul_sym = ifelse(is.na(dat_f$Rec_time[dat_f$mig == "symm. migration" & dat_f$exp == "multiple exposure"]),1,0)
Na.handle$mul_nm = ifelse(is.na(dat_f$Rec_time[dat_f$mig == "no migration" & dat_f$exp == "multiple exposure"]),1,0)
Na.handle$mul_asym = ifelse(is.na(dat_f$Rec_time[dat_f$mig == "asymm. migration" & dat_f$exp == "multiple exposure"]),1,0)
	
library(reshape2)
migr.diff.long = melt(migr.diff, id = 1:3)
range(migr.diff.long$value, na.rm = T)

migr.diff.long.mod <- merge(migr.diff.long, Na.handle, by = c("mor_red","em_red","gro_red"), sort = F) 
# crucial for for getting the gray values in the correct fields (otherwise everything will mess up!!!)
migr.diff.long.mod.sort <- migr.diff.long.mod[order(migr.diff.long.mod$mor_red, migr.diff.long.mod$variable), ]


na.value = ifelse(is.na(migr.diff.long.mod.sort$value) &
                   migr.diff.long.mod.sort$no.migr.MultEx == 1 & 
                   migr.diff.long.mod.sort$migr.MultEx == 1 &
                   migr.diff.long.mod.sort$variable == "migr.diff.MultEx", "grey40",
                  ifelse(is.na(migr.diff.long.mod.sort$value) &
                           migr.diff.long.mod.sort$no.migr.MultEx == 1 & 
                           migr.diff.long.mod.sort$migr.assym.MultEx == 1 &
                           migr.diff.long.mod.sort$variable == "migr.diff.assym.MultEx", "grey40",
                         ifelse(is.na(migr.diff.long.mod.sort$value) &
                                  migr.diff.long.mod.sort$no.migr.MultEx == 1 & 
                                  migr.diff.long.mod.sort$migr.assym.MultEx == 0 &
                                  migr.diff.long.mod.sort$variable == "migr.diff.assym.MultEx", "grey75",
                                ifelse(is.na(migr.diff.long.mod.sort$value) &
                                        migr.diff.long.mod.sort$no.migr.MultEx == 1 & 
                                        migr.diff.long.mod.sort$migr.MultEx == 0 &
                                        migr.diff.long.mod.sort$variable == "migr.diff.MultEx", "grey75","grey60"))))[which(is.na(migr.diff.long.mod.sort$value))]

# ggplot difference symetric migration
ggplot(migr.diff.long, aes(x = em_red, y = gro_red, fill = value)) + 
  geom_raster() + 
  facet_grid(mor_red ~ variable, labeller = labeller(.rows=as_labeller(eff.lab))) +
  labs(x = "Reduction of emergence", y = "Reduction in growth", fill = "Difference in \n recovery time")+
  #scale_fill_gradientn(colors = c("darkblue","gold","darkred")) + 
  scale_fill_gradientn(colors = rev(c(rgb(215,48,39,maxColorValue = 256),
                                      rgb(252,141,89,maxColorValue = 256),
                                      rgb(254,224,144,maxColorValue = 256),
                                      rgb(224,243,248,maxColorValue = 256),
                                      rgb(145,191,219,maxColorValue = 256),
                                      rgb(69,117,180,maxColorValue = 256)
                                      ))) + 
  #scale_fill_distiller( type = "div" , palette = "RdYlBu" )+ #RdYlBu
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels = eff) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels = eff) 
# Beschriftung hab ich jetzt noch nicht angepasst.
# ------------------------------------------------------------------------------------------------------------------ #

max.rec <- 450
d$Diff_fill <- d$Diff

ggplot(d, aes(x = N_red, y = Diff_fill, col = Ered_f, shape = Gred_f)) + 
  geom_point() + 
  facet_grid(expo~ mig) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels = eff)

####

par(las=1,bty="l",mar=c(4,6,1,1))
colfun <- colorRampPalette(c("darkblue", "lightblue"))
d[is.na(d$Diff_fill),"Diff_fill"] <- max.rec
plot(1:nrow(d),d$Diff_fill,pch = c(1,2,17,19)[d$Nred_f], col = colfun(4)[d$Ered_f],ylim=c(0,450),xaxt="n",ylab = "recovery time", xlab="",yaxt="n")
axis(2,at=seq(0,400,100))
axis(2,at=450,"no recovery")
#legend(x=200,y=400,col=colfun(4),pch=20,legend=paste("reduction of emergence", c(0.75,0.5,0.25,0)*100,"%"))

str_mig <-c("migration","no migration")
str_expo <- c("single exposure","multiple exposure")
farb <-  c("darkred","darkorange","cornflowerblue","darkblue")
pdf("Recovery_Scatter.pdf",height=8,width=10)
par(mfcol=c(2,2),mar=c(4,6,2,1),las=1,bty="l")
for(i in 1:2){
  for(j in 1:2){
    ind <- which(d$mig == str_mig[i] & d$expo == str_expo[j])
    plot(y = d[ind,"Diff_fill"], x = 1:length(ind),
         col =farb[d[ind,"Gred_f"]], 
         pch = c(17,19,2,1)[d[ind,"Ered_f"]], ylim = c(0,500),
         ylab = "recovery", xlab = "lethal effect", xaxt= "n", yaxt = "n",
         main = paste0(str_expo[j],", ", str_mig[i]))
    axis(2,at=seq(0,400,100))
    axis(2,at=450,"no recovery")
    axis(1,at=tapply(1:length(ind),d[ind,"N_red"],median),eff)
    if(i == 1 & j == 1){
      legend(x=38,y=500,col=rev(farb),pch=19,legend = eff, title ="hatching effect",ncol=2,bty="n",x.intersp = 0.75, y.intersp = 0.75)
      legend(x=0,y=500,col=1,pch=rev(c(17,19,2,1)),legend = eff, title = "emergence effect",ncol=2,bty="n",x.intersp = 0.75, y.intersp = 0.75)
    }
  }
}
dev.off()
