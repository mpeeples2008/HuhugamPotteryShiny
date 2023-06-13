dat <- read.csv(file="cer_codes.csv",header=T)
cer <- read.csv(file="Hohokam_pottery types.csv",header=T)

out <- matrix(NA,nrow(dat), 12)
colnames(out) <- colnames(cer)[3:14]

for (i in 1:nrow(dat)) {
  temp <- unique(dat[i,2:5])
  temp2 <- which(cer$Number %in% temp)
  temp3 <- colSums(cer[temp2,3:14])
  if (max(temp3,na.rm=T)>0) {out[i,which(temp3==max(temp3, na.rm=T))] <- 1
  }}

z <- colSums(out,na.rm=T)

tm1 <- c(13,157,678,678,9105,9105,6805,5708,5708,5708,5708,501)
tm2 <- c(1,1,53,53,587,587,546,594,594,594,594,4772)
tm3 <- c(1,16,39,39,1781,1781,2947,1285,1285,1285,1285,101)

z <- z/max(z)
tm1 <- tm1/max(tm1)
tm2 <- tm2/max(tm2)
tm3 <- tm3/max(tm3)

plot(z,type="l")
points(tm1,type="l",col='red')
points(tm2,type="l",col='blue')
points(tm3,type="l",col='darkgreen')

beg <- c(650,675,700,730,750,800,900,950,1020,1080,1100,1150)
end <- c(674,699,729,749,799,899,949,1019,1079,1099,1149,1299)
dat2 <- cbind(z,tm1,beg,end)

out.plot <- matrix(0,750,3)
out.plot[,1] <- seq(600,1349)

for (i in 1:nrow(out.plot)) {
  for (j in 1:nrow(dat2)) {
    if (out.plot[i,1] %in% seq(dat2[j,3],dat2[j,4])) {
      out.plot[i,3] <- dat2[j,2]
      out.plot[i,2] <- dat2[j,1]
    }
  }
}


library(ggplot2)

df1 <- as.matrix(cbind(out.plot[,1:2],rep("Current Analysis",nrow(out.plot))))
df2 <- as.matrix(cbind(out.plot[,c(1,3)],rep("1930s Excavation Typology",nrow(out.plot))))
df.all <- as.data.frame(rbind(df1,df2))
colnames(df.all) <- c("Year","Ceramic","Project")
df.all$Year <- as.numeric(df.all$Year)
df.all$Ceramic <- as.numeric(df.all$Ceramic)

ggplot(df.all) +
  geom_line(aes(x=Year,y=Ceramic,color=Project), size=3, alpha=0.5) +
  geom_vline(xintercept=beg) +
  ggtitle("Trash Mound 1 Dated Buffware Ceramics") +
  scale_x_continuous(breaks=seq(600, 1349, 50)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = rel(2)),
    axis.text.y = element_text(size = rel(2)),
    axis.title.x = element_text(size = rel(2)),
    axis.title.y = element_text(size = rel(2)),
    legend.text = element_text(size = rel(1))
  )