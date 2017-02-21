##############################
# Plots for the publication  #
##############################
setwd("~/Desktop/Modelling_Twopatch/Latest_scripts/")

# read raw data for illustration of dynamics
nomigr_single <- read.table("results/Z_Sim1_NoMigr_SingEx_daily_Ns.csv",header =T, sep = ",")
migr_single <- read.table("results/Z_Sim2_symmMigr_SingEx_daily_Ns.csv",header =T, sep = ",")
migras_single <- read.table("results/Z_Sim2_asymmMigr_SingEx_daily_Ns.csv",header =T, sep = ",")
nomigr_multi <- read.table("results/Z_Sim3_NoMigr_MultEx_daily_Ns.csv",header =T, sep = ",")
migr_multi <- read.table("results/Z_Sim4_symmMigr_MultiEx_daily_Ns.csv",header =T, sep = ",")
migras_multi <- read.table("results/Z_Sim4_asymmMigr_MultiEx_daily_Ns.csv",header =T, sep = ",")

# use subset for Figure 1
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

# Figure 2
library('ggplot2')
nomigr_single$mig <- "no migration" 
migr_single$mig <- "symm. migration" 
migras_single$mig <- "asymm. migration" 
nomigr_multi$mig <- "no migration"
migr_multi$mig <-  "symm. migration"
migras_multi$mig <- "asymm. migration" 

nomigr_single$exp <- "single exposure" 
migr_single$exp <- "single exposure"
migras_single$exp <- "single exposure" 
nomigr_multi$exp <- "multiple exposure"
migr_multi$exp <-  "multiple exposure"
migras_multi$exp <- "multiple exposure"

d <- list(nomigr_single, migr_single, migras_single, nomigr_multi, migr_multi, migras_multi)
dat_e <- do.call(rbind,d)
dat_f <- dat_e[ ,-1]
# check that everything is correct
nrow(dat_f) == 64*3600*6

# Convert to % reduction
dat_f$mor_red <- 1 - dat_f$f_mort
dat_f$em_red <- 1 - dat_f$f_emerg
dat_f$gro_red <- 1 - dat_f$f_growth

dat_f$mig <- factor(dat_f$mig)
dat_f$exp <- factor(dat_f$exp)
dat_f$mor_red <- factor(dat_f$mor_red)
dat_f$em_red <- factor(dat_f$em_red)
dat_f$gro_red <- factor(dat_f$gro_red)

factor(dat_f$mig, levels="asymmetric")
d$expo_f = factor(d$expo, levels=c("single exposure","multiple exposure"))
d$Nred_f = factor(d$N_red, levels=c(0.75,0.5,0.25,0))
d$Ered_f = factor(d$E_red, levels=c(0.75,0.5,0.25,0))
d$Gred_f = factor(d$G_red, levels=c(0.75,0.5,0.25,0))

eff <- c("no effect", "25%", "50%", "75%")
eff.lab <- c(
  '0' = "no lethal effect",
  '0.25' = paste(eff[2], "lethal effect"),
  '0.5' = paste(eff[3], "lethal effect"),
  '0.75' = paste(eff[4], "lethal effect"))

ggplot(dat_f, aes(x = em_red, y = gro_red, fill = Diff)) + 
  geom_raster() + 
  facet_grid(mor_red ~ exp + mig , labeller = labeller(.rows=as_labeller(eff.lab))) +
  labs(x = "Reduction of emergence", y = "Reduction in growth", fill = "Recovery time")+
  #scale_fill_gradientn(colors = c("darkblue","gold","darkred")) + 
  scale_fill_gradientn(colors = rev(c(rgb(215,48,39,maxColorValue = 256),
                                      rgb(252,141,89,maxColorValue = 256),
                                      rgb(254,224,144,maxColorValue = 256),
                                      rgb(224,243,248,maxColorValue = 256),
                                      rgb(145,191,219,maxColorValue = 256),
                                      rgb(69,117,180,maxColorValue = 256)))) + 
  #scale_fill_distiller( type = "div" , palette = "RdYlBu" )+ #RdYlBu
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels = eff) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels = eff) 

## RBS: Könntest Du hier die labels an ticks sowie Achsenbeschriftungen größer machen?

ggsave("Recovery_Image.pdf",device="pdf",height=9,width=10)


# ---------------------------------------------------------------------------------------------------------------------------- #
# added on 15.02. BK: 
# plot with difference in migration
# a) no migr vs. migr (singEx)  =  migr.diff.SingEx
# b) no migr vs. migr (multEx) = migr.diff.MultEx
# c) no migr vs. assym migr (singEx) = migr.diff.assym.SingEx
# d) no migr vs. assym migr (multEx) = migr.diff.assym.MultEx

library(plyr)
# symetric migration: single exposure and multiple exposure (a and b)
migr.diff = ddply(d,c("N_red","E_red","G_red"),summarize,
                  migr.diff.SingEx = Recovery.time..d.[mig == "migration" & expo == "single exposure"] - Recovery.time..d.[mig == "no migration" & expo == "single exposure"],
                         migr.diff.MultEx = Recovery.time..d.[mig == "migration" & expo == "multiple exposure"] - Recovery.time..d.[mig == "no migration" & expo == "multiple exposure"])


# read in assymetric migration 
b = list()
for(i in 1:length(fil_sel2)) {
  b[[i]] = read.csv(fnames[fil_sel2][i], header=T, row.names = 1)
  b[[i]]$N_red = 1 - b[[i]]$N_red
  b[[i]]$E_red = 1 - b[[i]]$E_red 
  b[[i]]$G_red = 1 - b[[i]]$G_red 
}
names(b) = c("Migr_assym_SingEx","Migr_assym_MultEx")

# calculate difference between assymetric migration and base scenario (noMigr, SingEx/MultEx)
migr.diff$migr.diff.assym.SingEx = b$Migr_assym_SingEx$Recovery.time..d. - d$Recovery.time..d.[d$mig == "no migration" & d$expo == "single exposure"] 
migr.diff$migr.diff.assym.MultEx = b$Migr_assym_MultEx$Recovery.time..d. - d$Recovery.time..d.[d$mig == "no migration" & d$expo == "multiple exposure"] 


library(reshape2)
migr.diff.long = melt(migr.diff,id = 1:3)
range(migr.diff.long$value,na.rm = T)

# ggplot difference symetric migration
ggplot(migr.diff.long, aes(x = E_red, y = G_red, fill = value)) + 
  geom_raster() + 
  facet_grid(as.factor(N_red) ~ variable, labeller = labeller(.rows=as_labeller(eff.lab))) +
  labs(x = "Reduction of emergence", y = "Reduction in growth", fill = "recovery")+
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
