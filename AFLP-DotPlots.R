setwd("~/Documents/people/sajeewa amaradasa/sublethal paper/sublethalstressmanuscript")
#EC50 <- read.csv("~/Documents/people/sajeewa amaradasa/sublethal paper/sublethalstressmanuscript/EC50.csv")
AFLP <- read.csv("~/Documents/people/sajeewa amaradasa/sublethal paper/sublethalstressmanuscript/AFLP2.csv")

#dotchart(AFLP$AFLP[as.logical(match(AFLP$Fungicide, "Boscalid", nomatch = F))], labels=AFLP$IsolateID[as.logical(match(AFLP$Fungicide, "Boscalid"))])
bos_1 <- AFLP$AFLP[as.logical(match(AFLP$Fungicide, "Boscalid", nomatch = F))]
bos_2 <- AFLP$AFLP2[as.logical(match(AFLP$Fungicide, "Boscalid", nomatch = F))]
az_1 <- AFLP$AFLP[as.logical(match(AFLP$Fungicide, "Azoxystrobin", nomatch = F))]
az_2 <- AFLP$AFLP2[as.logical(match(AFLP$Fungicide, "Azoxystrobin", nomatch = F))]
ip_1 <- AFLP$AFLP[as.logical(match(AFLP$Fungicide, "Iprodione", nomatch = F))]
ip_2 <- AFLP$AFLP2[as.logical(match(AFLP$Fungicide, "Iprodione", nomatch = F))]
py_1 <- AFLP$AFLP[as.logical(match(AFLP$Fungicide, "Pyraclostrobin", nomatch = F))]
py_2 <- AFLP$AFLP2[as.logical(match(AFLP$Fungicide, "Pyraclostrobin", nomatch = F))]
tm_1 <- AFLP$AFLP[as.logical(match(AFLP$Fungicide, "ThiophanateMethyl", nomatch = F))]
tm_2 <- AFLP$AFLP2[as.logical(match(AFLP$Fungicide, "ThiophanateMethyl", nomatch = F))]

#set.seed(1)
##Don't print the axes labels
par(ann=FALSE)

##Plot first set of data.
##Need to check for sensible ranges
##Use the jitter function to spread data out.
plot(jitter(rep(0,9),amount=0.15), py_1, pch=21, col=1,
     xlim=range(-0,9.5), ylim=range(-1,290),
     axes=FALSE,frame.plot=TRUE)
points(jitter(rep(1,9), amount=0.1), py_2, pch=21, col=2)
points(jitter(rep(2,9), amount=0.1), ip_1, pch=21, col=1)
points(jitter(rep(3,9), amount=0.1), ip_2, pch=21, col=2)
points(jitter(rep(4,9), amount=0.1), az_1, pch=21, col=1)
points(jitter(rep(5,9), amount=0.1), az_2, pch=21, col=2)
points(jitter(rep(6,9), amount=0.1), tm_1, pch=21, col=1)
points(jitter(rep(7,9), amount=0.1), tm_2, pch=21, col=2)
points(jitter(rep(8,9), amount=0.1), bos_1, pch=21, col=1)
points(jitter(rep(9,9), amount=0.1), bos_2, pch=21, col=2)

##Add in the y-axis
axis(2, seq(0,290,by=10))

##Add in the x-axis labels
mtext("Pyraclostrobin", side = 1, at=0.5)
mtext("Iprodione", side = 1, at=2.5)
mtext("Azoxystrobin", side = 1, at=4.5)
mtext("Thio. Methyl", side = 1, at=6.5)
mtext("Boscalid", side = 1, at=8.5)

##Add in the means
segments(-0.25, mean(py_1, na.rm = T), 0.25, mean(py_1, na.rm = T))
segments(0.75, mean(py_2, na.rm = T), 1.25, mean(py_2, na.rm = T))
segments(1.75, mean(ip_1, na.rm = T), 2.25, mean(ip_1, na.rm = T))
segments(2.75, mean(ip_2, na.rm = T), 3.25, mean(ip_2, na.rm = T))
segments(3.75, mean(az_1, na.rm = T), 4.25, mean(az_1, na.rm = T))
segments(4.75, mean(az_2, na.rm = T), 5.25, mean(az_2, na.rm = T))
segments(5.75, mean(tm_1, na.rm = T), 6.25, mean(tm_1, na.rm = T))
segments(6.75, mean(tm_2, na.rm = T), 7.25, mean(tm_2, na.rm = T))
segments(7.75, mean(bos_1, na.rm = T), 8.25, mean(bos_1, na.rm = T))
segments(8.75, mean(bos_2, na.rm = T), 9.25, mean(bos_2, na.rm = T))
abline(h = 0, col = "gray60")

title(main="Change in AFLP by Fungicide")

##Add in the legend
legend(0, 288, c("Experiment 1", "Experiment 2"), col=1:2, pch=1)


#############################  EOF  ########################################
