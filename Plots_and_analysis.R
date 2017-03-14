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
pdf("Figure_1.pdf",height=7,width=12)
par(cex=1.5, las=1,bty="l", mar=c(4,4,1,1))
plot(nms$Time, nms$Ni, type = "l", lty = 1,lwd =2, ylim = c(0,3250), xlab = "time [d]", ylab = "N")
lines(ms$Time, ms$Ni, lty = 2,lwd = 2, ylim = c(0,3200), col = "grey20")
lines(nmm$Time, nmm$Ni,lty = 3,lwd = 2, ylim = c(0,3200), col = "grey50")
lines(mm$Time, mm$Ni, lty = 4,lwd = 2, ylim = c(0,3200), col = "grey70")
legend(#50, 3300,
       #c("Nomig_s","Mig_s","Nomig_m","Mig_m"),
      "top",
       c("NoMig-SingEx",expression("Mig"["sym"]*"-SingEx"),"NoMig-MultEx",expression("Mig"["sym"]*"-MultEx")),
       lty = c(1,2,3,4),
       lwd = c(2,2,2,2),
       cex = 0.8,
       col = c("black","grey20","grey50","grey70","red"),
       ncol = 4)
dev.off()
       
############################
# TOC Art
#############################       
 
nms_base <- nomigr_single[nomigr_single$treat =="1_1_1", -1]
names(nms_base)[1:3] = c("Time","Ni","Nj")
pdf("TOC_art.pdf", width = 12, height = 7)
par(cex=2, mar=c(2.5,3,1,1),bty="l", lwd=2)
plot(nms$Time, nms$Ni, type = "l", lty = 1,lwd =3, ylim = c(0,3200), xlim = c(0,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
lines(nms_base$Time, nms_base$Ni, lty = 2,lwd = 3, ylim = c(0,3200), col = "grey20")
axis(1, labels=F, lwd = 2)
axis(2, labels=F, lwd = 2)
mtext("Time", 1, cex=3, line = 1)
mtext("Population size", 2, cex=3, line = 1, las =0)
legend("bottomright", legend= c("With pesticide", "Without pesticide"), lty = c(1,2), lwd = c(2,2), bty= "n")

# identify 0.9 K for baseline series
base_rw1 <- which(nms_base$Ni < 0.9*nms_base$Ni[1] )[1]
base_rw2 <- which(nms_base$Ni > 0.9*nms_base$Ni[1] )[base_rw1+1]
pest_rw3 <- which(nms$Ni > 0.9*nms_base$Ni[1] )[base_rw1+1]

arrows(x0 = base_rw2, y0 = nms_base$Ni[base_rw2], x1 = pest_rw3, y1 = nms$Ni[pest_rw3], lwd = 2.5, col = "red", code = 3)
text(x = base_rw2+ (pest_rw3-base_rw2)/2, y = 3150,  "Recovery time", col = "red" , cex = 1.2)
dev.off() 
      

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

eff.lab <- c(
  '0' = "no acute mortality",
  '0.25' = "25% acute mortality",
  '0.5' = "50% acute mortality",
  '0.75' = "75% acute mortality")

names(dat_f)[c(3,4)]  <- c("first_below0.9K", "min_ni")

ggplot(dat_f, aes(x = em_red, y = gro_red, fill = diff)) + 
  geom_raster() + 
  facet_grid(mor_red ~ exp + mig, labeller = labeller(.rows=as_labeller(eff.lab))) +
  labs(x = "Reduction of emergence", y = "Reduction in growth", fill = "Recovery time")+
  #scale_fill_gradientn(colors = c("darkblue","gold","darkred")) + 
  geom_text(aes(label = diff)) +
  scale_fill_gradientn(colors = rev(c(rgb(215,48,39, maxColorValue = 256),
                                      rgb(252,141,89, maxColorValue = 256),
                                      rgb(254,224,144, maxColorValue = 256),
                                      rgb(224,243,248, maxColorValue = 256),
                                      rgb(145,191,219, maxColorValue = 256),
                                      rgb(69,117,180, maxColorValue = 256)))) + 
  #scale_fill_distiller( type = "div" , palette = "RdYlBu" )+ #RdYlBu
  theme_minimal(base_size=15) 

# theme(axis.text = element_text(size=rel(1)), 
#        axis.title = element_text(size=rel(1.1)),
#        legend.text = element_text(size=rel(1)),
#        strip.text = element_text(size=rel(1.1))) +
  
# uncomment to save  
ggsave("Figure_2.pdf",device="pdf",height=10,width=16)

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
                  "Sym_mig vs. no_mig single" = diff[mig == "symm. migration" & exp == "single exposure"] - diff[mig == "no migration" & exp == "single exposure"],
                         "Sym_mig vs. no_mig multi" = diff[mig == "symm. migration" & exp == "multiple exposure"] - diff[mig == "no migration" & exp == "multiple exposure"],
                    "Asym_mig vs. no_mig single" = diff[mig == "asymm. migration" & exp == "single exposure"] - diff[mig == "no migration" & exp == "single exposure"],
                   "Asym_mig vs. no_mig multi" = diff[mig == "asymm. migration" & exp == "multiple exposure"] - diff[mig == "no migration" & exp == "multiple exposure"],
                    "Asym_mig vs. sym_mig single"  = diff[mig == "asymm. migration" & exp == "single exposure"] - diff[mig == "symm. migration" & exp == "single exposure"],
                     "Asym_mig vs. sym_mig multi"  = diff[mig == "asymm. migration" & exp == "multiple exposure"] - diff[mig == "symm. migration" & exp == "multiple exposure"])

library(reshape2)
migr.diff.long <- melt(migr.diff, id = 1:3)
range(migr.diff.long$value, na.rm = T)

# labelling for NAs
# NAs only occur in the multiple exposure scenario
Na.handle <- data.frame(dat_f[dat_f$mig == "symm. migration" & dat_f$exp == "multiple exposure",c("mor_red","em_red","gro_red")])
# sorting is the same, we extract only for one of the cases
Na.handle$mul_sym = ifelse(is.na(dat_f$diff[dat_f$mig == "symm. migration" & dat_f$exp == "multiple exposure"]),1,0)
Na.handle$mul_nm = ifelse(is.na(dat_f$diff[dat_f$mig == "no migration" & dat_f$exp == "multiple exposure"]),1,0)
Na.handle$mul_asym = ifelse(is.na(dat_f$diff[dat_f$mig == "asymm. migration" & dat_f$exp == "multiple exposure"]),1,0)
	
migr.diff.long.mod <- merge(migr.diff.long, Na.handle, by = c("mor_red","em_red","gro_red"), sort = F) 
# crucial for for getting the gray values in the correct fields (otherwise everything will mess up!!!)
migr.diff.long.mod.sort <- migr.diff.long.mod[order(migr.diff.long.mod$mor_red, migr.diff.long.mod$variable), ]

# check for cases 
check_data_mig <- migr.diff.long.mod.sort[is.na(migr.diff.long.mod.sort$value ), ]

# multi symmetric migration is always NA
# set cases where only symmetric migration == NA to grey 40
check_data_mig$na.value <- NULL
check_data_mig$na.value[check_data_mig$mul_nm == 0 &
               			check_data_mig$variable == "Sym_mig vs. no_mig multi" ] <- "grey75"
check_data_mig$na.value[check_data_mig$mul_asym == 0 &
               			check_data_mig$variable == "Asym_mig vs. sym_mig multi" ] <- "grey75"
# now identify cases for which only no migration or asymmetric is NA
check_data_mig$na.value[check_data_mig$mul_nm == 0 &
               			check_data_mig$variable == "Asym_mig vs. no_mig multi" ] <- "grey50"
check_data_mig$na.value[check_data_mig$mul_asym == 0 &
               			check_data_mig$variable == "Asym_mig vs. no_mig multi" ] <- "grey25"
check_data_mig$na.value[is.na(check_data_mig$na.value)] <- "black"

# whole plot for difference in migration
p1 <- ggplot(migr.diff.long, aes(x = em_red, y = gro_red)) + 
  geom_raster(data = subset(migr.diff.long.mod.sort, !is.na(value)), aes(fill = value)) + 
  geom_raster(data = subset(migr.diff.long.mod.sort, is.na(value)),fill = check_data_mig$na.value) +
  facet_grid(mor_red ~ variable, labeller = labeller(.rows=as_labeller(eff.lab))) +
  labs(x = "Reduction of emergence", y = "Reduction in growth", fill = "Difference in \n recovery time")+
  geom_text(aes(label = value)) +
  scale_fill_gradientn(colors = rev(c(rgb(215,48,39,maxColorValue = 256),
                                      rgb(252,141,89,maxColorValue = 256),
                                      rgb(254,224,144,maxColorValue = 256),
                                      rgb(224,243,248,maxColorValue = 256),
                                      rgb(145,191,219,maxColorValue = 256),
                                      rgb(69,117,180,maxColorValue = 256)
  ))) +
  theme_minimal(base_size=15) 

# Plot only for NAs
p2 <-   ggplot(migr.diff.long, aes(x = em_red, y = gro_red)) + 
  facet_grid(mor_red ~ variable, labeller = labeller(.rows=as_labeller(eff.lab))) +
  geom_raster(data = subset(migr.diff.long.mod.sort, is.na(value)),aes(fill = check_data_mig$na.value)) +
  scale_fill_manual(name = "No recovery", values = c('black','gray25','gray50','gray75'),
                    labels = c("both","only no_mig","only asym_mig","only sym_mig")) +
  theme_minimal(base_size=15) 

# save legends extra 

# recovery legend 
tmp1 <- ggplot_gtable(ggplot_build(p1))
leg1 <- which(sapply(tmp1$grobs, function(x) x$name) == "guide-box")
legend1 <- tmp1$grobs[[leg1]]

legend1_fin <- legend1
legend1_fin$width <- legend1$width*5
legend1_fin$height <- legend1$height*5

# NA.legend
tmp2 <- ggplot_gtable(ggplot_build(p2))
leg2 <- which(sapply(tmp2$grobs, function(x) x$name) == "guide-box")
legend2 <- tmp2$grobs[[leg2]]

library("grid")
pdf("Figure_S1.pdf", width = 18, height = 10) 
# plot 
grid.newpage()
pushViewport( viewport( layout = grid.layout( 1 , 2 , widths = unit( c(0.9,0.1) , "npc" ) ) ) ) # define layout
print( p1 + theme(legend.position="none") , vp = viewport( layout.pos.row = 1 , layout.pos.col = 1 ) ) # plot p1 without legend
upViewport(0)
# draw legends extra 
vp2 <- viewport( width = unit(1,"npc") , x = 0.94 , y = 0.6) # specifies postition of legend 1
vp3 <- viewport( width = unit(1,"npc") , x = 0.94 , y = 0.42) # specifies postition of legend 2
pushViewport(vp2)
grid.draw( legend1_fin )
popViewport()
pushViewport(vp3)
grid.draw( legend2 )
popViewport()
dev.off()

############################
# Figure 3 Effects on Nj
############################

# check range of effects on Nj
range(dat_f$diff_min, na.rm =T)

# compute % reduction in Nj compared to related control
dat_f$diff_min_perc <- round((1-dat_f$min_nj/(dat_f$min_nj + dat_f$diff_min))*100)

# plotting makes only sense for migration scenarios
dat_f_mig <- dat_f[dat_f$mig != "no migration", ]

ggplot(dat_f_mig, aes(x = em_red, y = gro_red, fill = diff_min_perc)) + 
  geom_raster() + 
  facet_grid(mor_red ~ exp + mig, labeller = labeller(.rows=as_labeller(eff.lab))) +
  labs(x = "Reduction of emergence", y = "Reduction in growth", fill = "% difference")+
  scale_fill_gradientn(colors = c("white","black"))  + 
  #scale_fill_distiller( type = "div" , palette = "RdYlBu" )+ #RdYlBu
  theme_minimal(base_size=15) 

# uncomment to save
ggsave("Figure_3.pdf",device="pdf",height=12,width=16)

######################################################
# Extinction for scenario without migration
#####################################################

# check range of effects on Ni
range(dat_f[dat_f$mig=="no migration", ]$min_ni, na.rm =T)

# identify scenarios with extinction
dat_f[dat_f$mig=="no migration" & dat_f$min_ni < 30, ]

# check influence of the parameters for extinction risk
library(car)
model_lm <- lm(min_ni ~ mor_red + exp + em_red + gro_red, data = dat_f[dat_f$mig=="no migration", ])
summary(model_lm)
Anova(lm(model_lm), type=2)
par(mfrow=c(2,2))
plot(model_lm)
dev.off()
# residuals show pattern, but less relevant as purpose is not statistical inference
library(hier.part)
hier.part(dat_f[dat_f$mig=="no migration", ]$min_ni, dat_f[dat_f$mig=="no migration",c(10, 13:15) ])
# same contribution of reduction due to mortality and emergence
#
######################################################
# Figure 4 & Figure S2:4 sensitivity analysis
#####################################################

load('results/Sens.out.S2.1.allParams.RData')
load('results/Sens.out.S2.1.noStructuralChanges.RData')

# we need to load scenario names
# if you run the sensitivity analysis yourself, you can use those of the input file
headers <- read.table("results/Headers.csv",header =T, sep = ",")
factor.levels <- paste(headers[ ,1],headers[ ,2],headers[ ,3] , sep = "-")

#################################
# Calculate ranks for variables #
#################################

######################
# for 6 parameters   #
######################

# First order
nostruct_long <- melt(sens.out.S2.1.noStructuralChanges$S[,1,])
nostruct_long$ranking <- unlist(by(-nostruct_long$value, nostruct_long$Var2, rank))

# compute average rank
rank_av1 <- aggregate(nostruct_long$ranking, list(nostruct_long$Var1), mean)
names(rank_av1)[2] <- "Avg. rank first order, no structural" 

# Total effect
nostruct_long2 <- melt(sens.out.S2.1.noStructuralChanges$T[,1,])
nostruct_long2$ranking <- unlist(by(-nostruct_long2$value, nostruct_long2$Var2, rank))

# compute average rank
rank_av2 <- aggregate(nostruct_long2$ranking, list(nostruct_long2$Var1), mean)
names(rank_av2)[2] <- "Avg. rank total order, no structural" 
								
######################
# for all parameters 
######################

# First order
struct_long <- melt(sens.out.S2.1.allParams$S[,1,])
struct_long$ranking <- unlist(by(-struct_long$value, struct_long$Var2, rank))

# compute average rank
rank_av3 <- aggregate(struct_long$ranking, list(struct_long$Var1), mean)
names(rank_av3)[2] <- "Avg. rank first order, structural" 

# Total effect
struct_long2 <- melt(sens.out.S2.1.allParams$T[,1,])
struct_long2$ranking <- unlist(by(-struct_long2$value, struct_long2$Var2, rank))

# compute average rank
rank_av4 <- aggregate(struct_long2$ranking, list(struct_long2$Var1), mean)
names(rank_av4)[2] <- "Avg. rank total order, structural" 

# combine all into table
do.call("merge", list(rank_av1,rank_av2, rank_av3, rank_av4))

final_rank_table <- join_all(list(rank_av1,rank_av2, rank_av3, rank_av4), type="full")

# order table by structural index
fin_rank_exp <- final_rank_table[order(final_rank_table[ ,4]), ]
write.csv(fin_rank_exp, "results/fin_rank_exp.csv", row.names=F)

# ---------------------------------------------------------------------- #
# Plot indices for sensitivity analysis without structural model changes
# ---------------------------------------------------------------------- #

library(sensitivity)
library(plotrix)
#library(colorRamps)

outfile <- "Figure_S2"
pdf(file = file.path(getwd(), paste0(outfile, ".pdf")),
    width = 25, height = 20)

par(mfrow = c(8,8), mar = c(2,1.1,1,2),oma = c(8,2,0,0))
for (i in 1:length(factor.levels)) {
  # plot first order sensitivity indices with CI:
  plotCI(x = c(1:5,6.2),y = sens.out.S2.1.noStructuralChanges$S[,1,i],
         col = gray.colors(6,start = 0.2,end = 0.8)[order(order(sens.out.S2.1.noStructuralChanges$S[,1,i],decreasing = T))],
         ylim = c(-0.1,1.1),xlim = c(1,6.7),pch = 16,
         ylab = "sensitivity indices",xlab = "parameters",
         li=as.vector(sens.out.S2.1.noStructuralChanges$S[,"min. c.i.",i]), 
         ui=as.vector(sens.out.S2.1.noStructuralChanges$S[,"max. c.i.",i]),
         sfrac = 0,las = 1,cex = 2.5,cex.axis = 1.5,xaxt = "n")
  title(factor.levels[i],line = -2,adj = 0.05,cex.main = 2.5)
  axis(1,at = c(1:5,6.2),mgp = c(3.5, 1.5, 0),cex.axis = 1.8,
       labels = c(expression(epsilon['linst']),
                             expression(epsilon['adult']),expression('d'['linst']),
                             expression('d'['adult']),"s",expression('day'['pesticide'])))
  par(new=TRUE)
  
  #plot total effect sensitivity indices with CI
  plotCI(x = c(seq(1.3,5.3,by = 1),6.5),sens.out.S2.1.noStructuralChanges$T[,1,i],
         li=as.vector(sens.out.S2.1.noStructuralChanges$T[,"min. c.i.",i]), 
         ui=as.vector(sens.out.S2.1.noStructuralChanges$T[,"max. c.i.",i]),
         ylim = c(-0.1,1.1),xlim = c(1,6.7),
         xlab = "",ylab = "",
         pch = 17,sfrac = 0,cex = 2.5,
         axes = FALSE)

}
# legend
par(fig = c(0, 1, 0, 1), oma = c(1, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",horiz = T,
       c("first order effects","total effects"),
       pch = c(16,17), col = c("gray40", "black"),cex = 2.5)
dev.off()

# ------------------------------------------------------------------------------------ #
# Plot indices for sensitivity analysis including structural model changes
# (length of emergence period and eggs)
# ------------------------------------------------------------------------------------ #

# change order of length_eggs and length_emergence
# first order effects: 
length_eggs <- sens.out.S2.1.allParams$S[7,,]
sens.out.S2.1.allParams$S[7,,] <- sens.out.S2.1.allParams$S[8,,]
sens.out.S2.1.allParams$S[8,,] <- length_eggs
# total effects
length_eggsT <- sens.out.S2.1.allParams$T[7,,]
sens.out.S2.1.allParams$T[7,,] <- sens.out.S2.1.allParams$T[8,,]
sens.out.S2.1.allParams$T[8,,] <- length_eggsT

outfile <- "Figure_S3"
pdf(file = file.path(getwd(), paste0(outfile, ".pdf")),
    width = 25, height = 20)

par(mfrow = c(8,8), mar = c(2,1.1,1,2),oma = c(8,2,0,0))
for(i in 1:length(factor.levels)) {
  
  # plot first order sensitivity indices with CI: 
  plotCI(x = seq(1,10.1,by = 1.3),y = sens.out.S2.1.allParams$S[,1,i],
         col = gray.colors(8,start = 0.2,end = 0.8)[order(order(sens.out.S2.1.allParams$S[,1,i],decreasing = T))],
         ylim = c(-0.1,1.1),xlim = c(1,11),pch = 16,
         ylab = "",xlab = "",
         li=as.vector(sens.out.S2.1.allParams$S[,"min. c.i.",i]), 
         ui=as.vector(sens.out.S2.1.allParams$S[,"max. c.i.",i]),
         sfrac = 0,las = 1,cex = 2.5,cex.axis = 1.5,xaxt = "n")
  title(factor.levels[i],line = -2,adj = 0.05,cex.main = 2.5)
  axis(1,at = seq(1,10.1,by = 1.3),mgp = c(3.5, 1.5, 0),cex.axis = 1.25,labels = c(expression(epsilon['linst']),
                             expression(epsilon['adult']),expression('d'['linst']),
                             expression('d'['adult']),"s",expression('day'['pest.']),
                             expression('l'['adult']),expression('l'['eggs'])))
  par(new=TRUE)
  
  #plot total effect sensitivity indices with CI
  plotCI(x = seq(1.5,11,by = 1.3),sens.out.S2.1.allParams$T[,1,i],
         li=as.vector(sens.out.S2.1.allParams$T[,"min. c.i.",i]), 
         ui=as.vector(sens.out.S2.1.allParams$T[,"max. c.i.",i]),
         ylim = c(-0.1,1.1),xlim = c(1,11),
         xlab = "",ylab = "",
         pch = 17,sfrac = 0,cex = 2.5,
         axes = FALSE)

}
# legend
par(fig = c(0, 1, 0, 1), oma = c(1, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",horiz = T,
       c("first order effects","total effects"),
       pch = c(16,17), col = c("gray40", "black"),cex = 2.5)

dev.off()

# ----------------------------------------------------------- #
# Use Cluster analysis to evaluate similarity in sensitivity indices
# ----------------------------------------------------------- #

# ##################################################### #
# for sensitivity indices without structural parameters
# ##################################################### #

library(ape)
# hierarchical clustering for first order indices 
f.o.sensInd <- t(sens.out.S2.1.noStructuralChanges$S[,1,])
rownames(f.o.sensInd) <- factor.levels

# hierarchical Clustering
d <- dist(f.o.sensInd, method = "euclidean") # distance matrix
fit.f.o.part <- hclust(d, method="single")
# create clusters with high within cluster similarity
par(cex=1.1, mar = c(4,2,2,4))
plot(as.dendrogram(fit.f.o.part), xlab = "Distance", horiz=T)

# hierarchical clustering for total order indices 
total.sensInd <- t(sens.out.S2.1.noStructuralChanges$T[,1,])
rownames(total.sensInd) <- factor.levels

# hierarchical Clustering
d_tot <- dist(total.sensInd, method = "euclidean") # distance matrix
fit.t.o.part <- hclust(d_tot, method="single")
plot(as.phylo(fit.t.o.part)) # display dendogram

# join data and cluster
sensInd_nostruct <- cbind(f.o.sensInd, total.sensInd)
d_fin_no_struct <- dist(sensInd_nostruct, method = "euclidean") #
fit.nostruct.both <- hclust(d_fin_no_struct, method="single")
par(cex=1.1, mar = c(4,2,2,4))
plot(as.dendrogram(fit.nostruct.both), xlab = "Distance", horiz=T)

# ##################################################### #
# for sensitivity indices WITH structural parameters
# ##################################################### #

#hierarchical clustering for first order indices 
f.o.sensInd <- t(sens.out.S2.1.allParams$S[,1,])
rownames(f.o.sensInd) <- factor.levels

# hierarchical Clustering
d <- dist(f.o.sensInd, method = "euclidean") # distance matrix
fit.f.o.all <- hclust(d, method="complete")
plot(as.phylo(fit.f.o.all))

#hierarchical clustering for total order indices 
total.sensInd <- t(sens.out.S2.1.allParams$T[,1,])
rownames(total.sensInd) <- factor.levels

# hierarchical Clustering
d2 <- dist(total.sensInd, method = "euclidean") # distance matrix
fit.t.o.all <- hclust(d2, method="complete")
plot(as.phylo(fit.t.o.all))

# join data and cluster
sensInd_struct <- cbind(f.o.sensInd, total.sensInd)
d_fin <- dist(sensInd_struct, method = "euclidean") #
fit.t.o.both <- hclust(d_fin, method="single")
quartz()
par(cex=1, mar = c(4,2,2,4))
plot(as.dendrogram(fit.t.o.both), xlab = "Distance", horiz=T)
dev.off()
###############################################################
# Figure 4 and S4: combine cluster analysis with representative plots
###############################################################
# function to plot sensitivity analysis and cluster analysis of first order indices of 6 parameters

plot.6param.sens.ind <- function(simulation, legend = F, title.col = "black") 
			{
  			ind <- which(factor.levels == as.character(simulation))
  			# plot first order sensitivity indices with CI:
  			plotCI(x = seq(0.5,3,0.5),y = sens.out.S2.1.noStructuralChanges$S[,1,ind],
         	col = "gray50",
         	ylim = c(-0.2,1),xlim = c(0.4,3.5),pch = 21,
         	ylab = "sensitivity indices",xlab = "parameters",
         	li=as.vector(sens.out.S2.1.noStructuralChanges$S[,"min. c.i.",ind]), 
         	ui=as.vector(sens.out.S2.1.noStructuralChanges$S[,"max. c.i.",ind]),
         	sfrac = 0,las = 1,cex = 1.8,xaxt = "n")
			title(factor.levels[ind],line = -1.2,adj = 0.02,col.main = title.col)
			axis(1,at = seq(0.5,3,0.5),labels = c(expression(epsilon['linst']),
                           expression(epsilon['adult']),expression('d'['linst']),
                           expression('d'['adult']),"s",expression('day'['pesticide'])))
			par(new=TRUE)

			#plot total effect sensitivity indices with CI
			plotCI(x = seq(0.7,3.2,length.out = 6),sens.out.S2.1.noStructuralChanges$T[,1,ind],
     	    li=as.vector(sens.out.S2.1.noStructuralChanges$T[,"min. c.i.",ind]), 
       		ui=as.vector(sens.out.S2.1.noStructuralChanges$T[,"max. c.i.",ind]),
      		 ylim = c(-0.2,1),xlim = c(0.4,3.5),
       		xlab = "",ylab = "",
       		pch = 17,sfrac = 0,cex = 1.8,
       		axes = FALSE)
			if(legend == T) 
					{
					#legend
 					legend("topright",
      				  c("first order","total effects"),
       					 pch = c(1,17))
					}
			}

# ......................... #
# function for 8 parameters

plot.8param.sens.ind <- function(simulation,legend = F,title.col = "black") 
    {
    ind <- which(factor.levels == as.character(simulation))
 	 # plot first order sensitivity indices with CI:
  	plotCI(x = seq(0.5,4,0.5),y = sens.out.S2.1.allParams$S[,1,ind],
         col = "gray50",
         ylim = c(-0.2,1),xlim = c(0.4,4.5),pch = 21,
         ylab = "sensitivity indices",xlab = "parameters",
         li=as.vector(sens.out.S2.1.allParams$S[,"min. c.i.",ind]), 
         ui=as.vector(sens.out.S2.1.allParams$S[,"max. c.i.",ind]),
         sfrac = 0,las = 1,cex = 1.8,xaxt = "n")
  	title(factor.levels[ind],line = -1.2,adj = 0.02,col.main = title.col)
  	axis(1,at = seq(0.5,4,0.5),labels = c(expression(epsilon['linst']),
                             expression(epsilon['adult']),expression('d'['linst']),
                             expression('d'['adult']),"s",expression('day'['pesticide']),
                             expression('l'['adult']),expression('l'['eggs'])))
  	par(new=TRUE)
  
  	#plot total effect sensitivity indices with CI
  	plotCI(x = seq(0.7,4.2,length.out = 8),sens.out.S2.1.allParams$T[,1,ind],
         li=as.vector(sens.out.S2.1.allParams$T[,"min. c.i.",ind]), 
         ui=as.vector(sens.out.S2.1.allParams$T[,"max. c.i.",ind]),
         ylim = c(-0.2,1),xlim = c(0.4,4.5),
         xlab = "",ylab = "",
         pch = 17,sfrac = 0,cex = 1.8,
         axes = FALSE)
  	if(legend == T) 
  			{
    			#legend
   			 legend("top",
           c("first order","total effects"),
           pch = c(1,17))
  			}
	}
	
	
# .................................................................................................. #


# plot first order indices and cluster analysis of 6 parameters: 
pdf("Figure_4.pdf", height=8, width=14)

layout(matrix(c(1,2,
                1,3,
                1,4,
                1,5), 4, 2, byrow = TRUE), widths=c(1.5,1.2))
par(mar = c(1.5,4,1,4), oma = c(5,0,1,1))
plot(as.dendrogram(fit.nostruct.both), horiz=T, xlim = c(0.4,0))

# examples for each cluster:
# 1. 
plot.6param.sens.ind("50-50-75", legend = T)
# 2. 
plot.6param.sens.ind("0-0-0")
# 3.
plot.6param.sens.ind("75-50-50")
# 0.5-0.5-0.75
plot.6param.sens.ind("50-0-0")
dev.off()

# plot first order indices and cluster analysis of 8 parameters: 
pdf("Figure_S4.pdf", height=8, width=14)

layout(matrix(c(1,2,
                1,3,
                1,4,
                1,5), 4, 2, byrow = TRUE),widths=c(1.5,1.2))
par(mar = c(1.5,4,1,4), oma = c(5,0,1,1))
plot(as.dendrogram(fit.t.o.both), horiz=T, xlim = c(0.4,0))

# examples for each cluster:
# 1. 50-50-75
plot.8param.sens.ind("50-50-75", legend = T)
# 2. 50-25-50
plot.8param.sens.ind("50-25-50")
# 3.50-50-25
plot.8param.sens.ind("50-50-25")
# 0-25-0
plot.8param.sens.ind("0-25-0")
dev.off()
