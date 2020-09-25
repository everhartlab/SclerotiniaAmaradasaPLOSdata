comb <- read.csv("EC-AFLP-SSR.csv")
#plot(x=comb$EC50, y=comb$AFLP)
#plot(x=comb$EC50, y=comb$SSR)
#plot(x=comb$EC50, y=comb$Alleles)
fung <- c("ThiophanateMethyl", "Azoxystrobin", "Boscalid", "Pyraclostrobin", "Iprodione")   #Select the fungicide of choice
fung <- c("ThiophanateMethyl", "Boscalid")   #Select the fungicide of choice

#plotting only TM treated EC50 vs. AFLP
ec50 <- comb$EC50[as.logical(match(comb$TRT, fung, nomatch = F))]
aflp <- comb$AFLP[as.logical(match(comb$TRT, fung, nomatch = F))]
ssr <- comb$SSR[as.logical(match(comb$TRT, fung, nomatch = F))]
ssr2 <- ssr
ssr2[as.logical(match(ssr,0, nomatch = F))] <- NA
all <- comb$Alleles[as.logical(match(comb$TRT, fung, nomatch = F))]
cen <- comb$Cen[as.logical(match(comb$TRT, fung, nomatch = F))]
cenloc <- comb$CenLoc[as.logical(match(comb$TRT, fung, nomatch = F))]
id <- comb$ID[as.logical(match(comb$TRT, fung, nomatch = F))]

#for plotting AFLP data (change from g0 to g12-trt)
plot(x=ec50, y=aflp, xlim=range(-1,3.5), ylim=range(0,290), type='n')
text(ec50, aflp, label=id, col='black')
lm.plot <- lm(aflp~ec50, na.action=na.omit)
abline(lm.plot, col="red")
title(main="EC50 vs. Change in ALFP from g0 to g12-trt")
summary(lm.plot)

#for plotting only total number of alleles in AFLP data (all)
plot(x=ec50, y=all, xlim=range(-1,3.5), ylim=range(0,290), type='n')
text(ec50, all, label=id, col='black')
lm.plot <- lm(all~ec50, na.action=na.omit)
abline(lm.plot, col="red")
title(main="EC50 vs. Total Number of Alleles in g12")
summary(lm.plot)

#for plotting only censored alleles in AFLP data (cen)
plot(x=ec50, y=cen, xlim=range(-1,3.5), ylim=range(0,43), type='n')
text(ec50, cen, label=id, col='black')
lm.plot <- lm(cen~ec50, na.action=na.omit)
abline(lm.plot, col="red")
title(main="EC50 vs. Global Censored AFLP (omitted alleles variable in all controls)")
summary(lm.plot)

#for plotting local censored alleles in AFLP data (cenloc)
plot(x=ec50, y=cenloc, xlim=range(-1,3.5), ylim=range(0,255), type='n')
text(ec50, cenloc, label=id, col='black')
lm.plot <- lm(cenloc~ec50, na.action=na.omit)
abline(lm.plot, col="red")
title(main="EC50 vs. Local Censored AFLP (omitted alleles variable in control of each isolate)")
summary(lm.plot)

# for plotting SSR data (ie. ssr)
plot(x=ec50, y=ssr, xlim=range(-1,3.5), ylim=range(0,27), type='n')
text(ec50, ssr, label=id, col='black')
lm.plot <- lm(ssr~ec50, na.action=na.omit)
abline(lm.plot, col="red")
title(main="EC50 vs. SSR")
summary(lm.plot)

# for plotting SSR data (ie. ssr)
plot(x=ec50, y=ssr2, xlim=range(-1,3.5), ylim=range(0,27), type='n')
text(ec50, ssr2, label=id, col='black')
lm.plot <- lm(ssr2~ec50, na.action=na.omit)
abline(lm.plot, col="red")
summary(lm.plot)

###################  SSR vs. AFLP  ################################
# for plotting total AFLP alleles vs SSR data
plot(x=all, y=ssr, xlim=range(-1,311), ylim=range(0,27), type='n')
text(all, ssr, label=id, col='black')
lm.plot <- lm(ssr~all, na.action=na.omit)
abline(lm.plot, col="red")
title(main="non-censored (sum of alleles) AFLP vs. SSR")
summary(lm.plot)

# for plotting g0 vs. g12-trt AFLP vs SSR data
plot(x=all, y=ssr2, xlim=range(-1,311), ylim=range(0,27), type='n')
text(all, ssr2, label=id, col='black')
lm.plot <- lm(ssr2~all, na.action=na.omit)
abline(lm.plot, col="red")
title(main="non-censored (all alleles) AFLP vs. non-zero SSR")
summary(lm.plot)

# for plotting local Censored AFLP vs SSR data
plot(x=cen, y=ssr, xlim=range(-1,43), ylim=range(0,28), type='n')
text(cen, ssr, label=id, col='black')
lm.plot <- lm(ssr~cen, na.action=na.omit)
abline(lm.plot, col="red")
title(main="local censored AFLP vs. SSR")
summary(lm.plot)

# for plotting local Censored AFLP vs non-zero SSR data
plot(x=cen, y=ssr2, xlim=range(-1,14), ylim=range(0,27), type='n')
text(cen, ssr2, label=id, col='black')
lm.plot <- lm(ssr2~cen, na.action=na.omit)
abline(lm.plot, col="red")
title(main="local censored AFLP vs. non-zero SSR")
summary(lm.plot)

## EOF

