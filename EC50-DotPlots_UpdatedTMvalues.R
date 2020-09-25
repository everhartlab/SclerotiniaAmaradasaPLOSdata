setwd("~/Documents/people/sajeewa amaradasa/sublethal paper/sublethalstressmanuscript")
#EC50 <- read.csv("~/Documents/people/sajeewa amaradasa/sublethal paper/sublethalstressmanuscript/EC50.csv")
EC2 <- read.csv("~/Documents/people/sajeewa amaradasa/sublethal paper/sublethalstressmanuscript/EC50V2.csv")

# Change two highest values from the tm data from n (not significant) to i (increased significant)
EC2$sig.1[tm][8:9] <- "i"

#dotchart(EC2$Fold_Exp1[as.logical(match(EC2$Fungicide, "Boscalid", nomatch = F))], labels=EC2$IsolateID[as.logical(match(EC2$Fungicide, "Boscalid"))])
bos_1 <- EC2$Fold_Exp1[as.logical(match(EC2$Fungicide, "Boscalid", nomatch = F))]
bos_2 <- EC2$Fold_Exp2[as.logical(match(EC2$Fungicide, "Boscalid", nomatch = F))]
az_1 <- EC2$Fold_Exp1[as.logical(match(EC2$Fungicide, "Azoxystrobin", nomatch = F))]
az_2 <- EC2$Fold_Exp2[as.logical(match(EC2$Fungicide, "Azoxystrobin", nomatch = F))]
ip_1 <- EC2$Fold_Exp1[as.logical(match(EC2$Fungicide, "Iprodione", nomatch = F))]
ip_2 <- EC2$Fold_Exp2[as.logical(match(EC2$Fungicide, "Iprodione", nomatch = F))]
py_1 <- EC2$Fold_Exp1[as.logical(match(EC2$Fungicide, "Pyraclostrobin", nomatch = F))]
py_2 <- EC2$Fold_Exp2[as.logical(match(EC2$Fungicide, "Pyraclostrobin", nomatch = F))]
tm_1 <- EC2$Fold_Exp1[as.logical(match(EC2$Fungicide, "ThiophanateMethyl", nomatch = F))]
tm_2 <- EC2$Fold_Exp2[as.logical(match(EC2$Fungicide, "ThiophanateMethyl", nomatch = F))]

bs1 <- EC2$sig[as.logical(match(EC2$Fungicide, "Boscalid", nomatch = F))]
bs2 <- EC2$sig.1[as.logical(match(EC2$Fungicide, "Boscalid", nomatch = F))]
as1 <- EC2$sig[as.logical(match(EC2$Fungicide, "Azoxystrobin", nomatch = F))]
as2 <- EC2$sig.1[as.logical(match(EC2$Fungicide, "Azoxystrobin", nomatch = F))]
is1 <- EC2$sig[as.logical(match(EC2$Fungicide, "Iprodione", nomatch = F))]
is2 <- EC2$sig.1[as.logical(match(EC2$Fungicide, "Iprodione", nomatch = F))]
ps1 <- EC2$sig[as.logical(match(EC2$Fungicide, "Pyraclostrobin", nomatch = F))]
ps2 <- EC2$sig.1[as.logical(match(EC2$Fungicide, "Pyraclostrobin", nomatch = F))]
ts1 <- EC2$sig[as.logical(match(EC2$Fungicide, "ThiophanateMethyl", nomatch = F))]
ts2 <- EC2$sig.1[as.logical(match(EC2$Fungicide, "ThiophanateMethyl", nomatch = F))]

b1 <- as.logical(match(bs1, "n", nomatch=F))
b1[is.na(bs1)] <- NA
b2 <- as.logical(match(bs2, "n", nomatch=F))
b2[is.na(bs2)] <- NA
a1 <- as.logical(match(as1, "n", nomatch=F))
a1[is.na(as1)] <- NA
a2 <- as.logical(match(as2, "n", nomatch=F))
a2[is.na(as2)] <- NA
i1 <- as.logical(match(is1, "n", nomatch=F))
i1[is.na(is1)] <- NA
i2 <- as.logical(match(is2, "n", nomatch=F))
i2[is.na(is2)] <- NA
p1 <- as.logical(match(ps1, "n", nomatch=F))
p1[is.na(ps1)] <- NA
p2 <- as.logical(match(ps2, "n", nomatch=F))
p2[is.na(ps2)] <- NA
t1 <- as.logical(match(ts1, "n", nomatch=F))
t1[is.na(ts1)] <- NA
t2 <- as.logical(match(ts2, "n", nomatch=F))
t2[is.na(ts2)] <- NA

#bavg <- mean(c(bos_1,bos_2), na.rm=T) #5
#aavg <- mean(c(az_1,az_2), na.rm=T)  #3
#iavg <- mean(c(ip_1,ip_2), na.rm=T)  #2
#pavg <- mean(c(py_1,py_2), na.rm=T)  #1
#tavg <- mean(c(tm_1,tm_2), na.rm=T)  #4
#avg <- bavg
#avg <- append(bavg,c(aavg, iavg, pavg, tavg))


#set.seed(1)
##Don't print the axes labels
par(ann=FALSE)

##Plot first set of data.
##Need to check for sensible ranges
##Use the jitter function to spread data out.
plot(jitter(rep(0,9),amount=0.2), py_1, pch=21, bg=!p1, col=1,
     xlim=range(-0.5,9.5), ylim=range(-1,3.2),
     axes=FALSE,frame.plot=TRUE)
points(jitter(rep(1,9), amount=0.2), py_2, pch=23, bg=!p2, col=1)
points(jitter(rep(2,9), amount=0.2), ip_1, pch=21, bg=!i1, col=1)
points(jitter(rep(3,9), amount=0.2), ip_2, pch=23, bg=!i2, col=1)
points(jitter(rep(4,9), amount=0.2), az_1, pch=21, bg=!a1, col=1)
points(jitter(rep(5,9), amount=0.2), az_2, pch=23, bg=!a2, col=1)
points(jitter(rep(6,9), amount=0.2), tm_1, pch=21, bg=!t1, col=1)
points(jitter(rep(7,9), amount=0.2), tm_2, pch=23, bg=!t2, col=1)
points(jitter(rep(8,9), amount=0.2), bos_1, pch=21, bg=!b1, col=1)
points(jitter(rep(9,9), amount=0.2), bos_2, pch=23, bg=!b2, col=1)

##Add in the y-axis
axis(2, seq(-1,3.1,by=0.5))

##Add in the x-axis labels
mtext("Pyraclostrobin", side = 1, at=0.5)
mtext("Iprodione", side = 1, at=2.5)
mtext("Azoxystrobin", side = 1, at=4.5)
mtext("Thio. Methyl", side = 1, at=6.5)
mtext("Boscalid", side = 1, at=8.5)

##Add in the means
segments(-0.25, mean(py_1, na.rm = T), 0.25, mean(py_1, na.rm = T)) # -0.02002238
segments(0.75, mean(py_2, na.rm = T), 1.25, mean(py_2, na.rm = T))  # -0.553094
segments(1.75, mean(ip_1, na.rm = T), 2.25, mean(ip_1, na.rm = T))  # -0.09197669
segments(2.75, mean(ip_2, na.rm = T), 3.25, mean(ip_2, na.rm = T))  # -0.06792508
segments(3.75, mean(az_1, na.rm = T), 4.25, mean(az_1, na.rm = T))  #  0.5645106
segments(4.75, mean(az_2, na.rm = T), 5.25, mean(az_2, na.rm = T))  # -0.5724222
segments(5.75, mean(tm_1, na.rm = T), 6.25, mean(tm_1, na.rm = T))  #  0.4465313
segments(6.75, mean(tm_2, na.rm = T), 7.25, mean(tm_2, na.rm = T))  #  1.460614
segments(7.75, mean(bos_1, na.rm = T), 8.25, mean(bos_1, na.rm = T)) # 1.788216
segments(8.75, mean(bos_2, na.rm = T), 9.25, mean(bos_2, na.rm = T)) # 0.1497909
abline(h = 0, col = "gray60")

##Add in the legend
legend(0, 3.0, c("Experiment 1", "Experiment 2"), col=1, pch=c(21,23))
title(main = "Fold-change in EC-50 from G0 to G12")

#############################  EOF  ########################################

