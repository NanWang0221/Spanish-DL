#!/bin/sh

#  engcvc.R
#
#
#  Created by Eleanor Chodroff on 11/7/15.
#
require(plyr)
require(reshape2)
require(boot)
require(piecewiseSEM)
require(ggExtra)
require(ggplot2)
require(lme4)

eng.cvc <- read.delim("/Volumes/MIXER6_cogsci/engCVC/cueAnalysis_engCVC.txt", header=FALSE)
names(eng.cvc) <- c("file", "phon","start","end","trial","vot","word","word_dur","vowel", "vowel_dur","f0_1","f0_2","f0_3","f0_4","f0_5","f0_6","f0_7","f0_8","f0_9","f0_10")
vdur <- read.delim("/Volumes/MIXER6_cogsci/engCVC/cueAnalysis_engCVC_vdur.txt", header=FALSE)
names(vdur) <- c("file", "phon", "start","end","trial","vot","word","word_dur","vowel","vowel_dur")
vdur$file <- gsub("_16kHz","",vdur$file)

eng.cvc$vowel <- NULL
eng.cvc$vowel_dur <-NULL
eng.cvc <- merge(eng.cvc, vdur, by=c("file","phon","start","end","trial","vot","word","word_dur"))

eng.cvc$usef0 <- ifelse(eng.cvc$f0_1 != "--undefined--", as.numeric(as.character(eng.cvc$f0_1)), ifelse(eng.cvc$f0_2 != "--undefined--", as.numeric(as.character(eng.cvc$f0_2)),
ifelse(eng.cvc$f0_3 != "--undefined--", as.numeric(as.character(eng.cvc$f0_3)),
ifelse(eng.cvc$f0_4 != "--undefined--",as.numeric(as.character(eng.cvc$f0_4)),ifelse(eng.cvc$f0_5 != "--undefined--",as.numeric(as.character(eng.cvc$f0_5)), as.numeric(as.character(eng.cvc$f0_6)))))))

eng.cvc <- subset(eng.cvc, vowel!="L")

####################
### CORRELATIONS ###
####################

eng.cvc$phon <- factor(eng.cvc$phon, levels=c("P","T","K","B","D","G"))
subj.means.vot <- ddply(eng.cvc, .(subj, phon), summarise, mean=mean(vot*1000), sd=sd(vot*1000))
sjm.vot <- dcast(subj.means.vot, subj~phon, value.var="mean")

subj.means.logvot <- ddply(eng.cvc, .(subj, phon), summarise, vot_mean=mean(log(vot*1000)), vot_sd=sd(log(vot*1000)))
sjm.logvot <- dcast(subj.means.logvot, subj~phon, value.var="vot_mean")

### correlations on residuals ###

model_residual <- lm(vot ~ 1 + son1.dur, eng.cvc1)
tmp <- residuals(model_residual)
eng.cvc1$res_vdur <- tmp

subj.means.sub <- ddply(eng.cvc1, .(subj, phon), summarise, vot_mean=mean(res_vdur), vot_sd=sd(res_vdur))
sjm.sub <- dcast(subj.means.sub, subj~phon, value.var="vot_mean")


cor.test(~P+T, sjm.sub)
cor.test(~P+K, sjm.sub)
cor.test(~T+K, sjm.sub)

cor.test(~B+D, sjm.sub)
cor.test(~B+G, sjm.sub)
cor.test(~D+G, sjm.sub)

cor.test(~P+B, sjm.sub)
cor.test(~T+D, sjm.sub)
cor.test(~K+G, sjm.sub)

cor.test(~P+T, sjm.logvot)
cor.test(~P+K, sjm.logvot)
cor.test(~T+K, sjm.logvot)

cor.test(~B+D, sjm.logvot)
cor.test(~B+G, sjm.logvot)
cor.test(~D+G, sjm.logvot)

cor.test(~P+B, sjm.logvot)
cor.test(~T+D, sjm.logvot)
cor.test(~K+G, sjm.logvot)

cor.test(~P+T, sjm.vot)
cor.test(~P+K, sjm.vot)
cor.test(~T+K, sjm.vot)

cor.test(~B+D, sjm.vot)
cor.test(~B+G, sjm.vot)
cor.test(~D+G, sjm.vot)

cor.test(~P+B, sjm.vot)
cor.test(~T+D, sjm.vot)
cor.test(~K+G, sjm.vot)

sjm.vot$rank <- c("01pktbdg","02pktbdg","03ptkbdg","04tpkbdg","05pktbdg","06pktbdg","07pktbgd","08kptbdg","09ptkbdg","10ptkbdg","11ptkbdg","12ptkbdg","13ptkbdg","14pktbgd","15kptbgd","16ptkbdg","17ptkbdg","18ptkbdg")

####### BOOTSTRAPPING #########

correl <- function(data, i) {
    cor(data[i, 1], data[i, 2])
}

results_PT <- boot(data=sjm.vot[,c(2,3)], statistic=correl, R=1000)
results_PK <- boot(data=sjm.vot[,c(2,4)], statistic=correl, R=1000)
results_TK <- boot(data=sjm.vot[,c(3,4)], statistic=correl, R=1000)

results_BD <- boot(data=sjm.vot[,c(5,6)], statistic=correl, R=1000)
results_BG <- boot(data=sjm.vot[,c(5,7)], statistic=correl, R=1000)
results_DG <- boot(data=sjm.vot[,c(6,7)], statistic=correl, R=1000)

results_PB <- boot(data=sjm.vot[,c(2,5)], statistic=correl, R=1000)
results_TD <- boot(data=sjm.vot[,c(3,6)], statistic=correl, R=1000)
results_KG <- boot(data=sjm.vot[,c(4,7)], statistic=correl, R=1000)

results_B <- boot(data=subset(subj.means.vot, phon=="B")[,4:5], statistic=correl, R=1000)
results_D <- boot(data=subset(subj.means.vot, phon=="D")[,4:5], statistic=correl, R=1000)
results_G <- boot(data=subset(subj.means.vot, phon=="G")[,4:5], statistic=correl, R=1000)
results_P <- boot(data=subset(subj.means.vot, phon=="P")[,4:5], statistic=correl, R=1000)
results_T <- boot(data=subset(subj.means.vot, phon=="T")[,4:5], statistic=correl, R=1000)
results_K <- boot(data=subset(subj.means.vot, phon=="K")[,4:5], statistic=correl, R=1000)
results_all <- boot(data=subj.means.vot[,4:5], statistic=correl, R=1000)


boot.ci(results_PT, type="bca")
boot.ci(results_TK, type="bca")
boot.ci(results_PK, type="bca")

boot.ci(results_BD, type="bca")
boot.ci(results_BG, type="bca")
boot.ci(results_DG, type="bca")

boot.ci(results_PB, type="bca")
boot.ci(results_TD, type="bca")
boot.ci(results_KG, type="bca")

boot.ci(results_B, type="bca")
boot.ci(results_D, type="bca")
boot.ci(results_G, type="bca")
boot.ci(results_P, type="bca")
boot.ci(results_T, type="bca")
boot.ci(results_K, type="bca")
boot.ci(results_all, type="bca")


###########################
#### SIMPLE REGRESSION ####
###########################

summary(lm(T ~ P, sjm.vot))
summary(lm(K ~ T, sjm.vot))
summary(lm(K ~ P, sjm.vot))

summary(lm(D ~ B, sjm.vot))
summary(lm(G ~ D, sjm.vot))
summary(lm(G ~ B, sjm.vot))

summary(lm(P ~ B, sjm.vot))
summary(lm(T ~ D, sjm.vot))
summary(lm(K ~ G, sjm.vot))


#############################
### MODELS: NEW CONTRASTS ###
#############################

eng.cvc1$vowel_height <- factor(eng.cvc1$vowel_height, levels=c('high', 'nonhigh'));
contrasts(eng.cvc1$vowel_height) <- vowel_height$contrast

eng.cvc1$poa <- factor(eng.cvc1$poa, levels=c("cor","dor","lab"));
contrasts(eng.cvc1$poa) <- cbind(poa$contrast1, poa$contrast2)

eng.cvc1$voice <- factor(eng.cvc1$voice, levels=c("vcl","vcd"));
contrasts(eng.cvc1$voice) <- voice$contrast

eng.cvc1$son1.dur.z <- scale(eng.cvc1$son1.dur)
eng.cvc1$Nson1.dur.d <- eng.cvc1$Nson1.dur * 10  #normalized sonorant duration in deciseconds
eng.cvc1$z.son1.dur <- (eng.cvc1$son1.dur - mean(eng.cvc1$son1.dur)) / (2*sd(eng.cvc1$son1.dur))

eng.cvc1$vowel_tense <- factor(eng.cvc1$vowel_tense, levels=c('tense','lax'))
contrasts(eng.cvc1$vowel_tense) <- vowel_tense$contrast

eng.cvc1$vot_ms <- eng.cvc1$vot*1000
eng.cvc1$Nvot_ms <- eng.cvc1$vot_ms - mean(eng.cvc1$vot_ms)
eng.cvc1$logvot_ms <- log(eng.cvc1$vot_ms)

### this one in JPhon revision! ###
fitsrz2 <- lmer(Nvot_ms ~ poa*voice + voice*son1.dur.z +vowel_tense*vowel_height +  (1 + poa*voice + son1.dur.z| subj) + (1 | body), eng.cvc1, REML=TRUE)

fitlogsrz2 <- lmer(logvot_ms ~ poa*voice + voice*son1.dur.z +vowel_tense*vowel_height +  (1 + poa*voice + son1.dur.z| subj) + (1 | body), eng.cvc1, REML=TRUE)

fitloglogsrz2 <- lmer(logvot_ms ~ poa*voice + voice*log(son1.dur) +vowel_tense*vowel_height +  (1 + poa*voice + log(son1.dur)| subj) + (1 | body), eng.cvc1, REML=TRUE)

####
fitsrz1 <- lmer(Nvot_ms ~ poa*voice + vowel_tense*vowel_height + son1.dur.z + (1 + poa*voice + son1.dur.z| subj) + (1 | body), eng.cvc1, REML=TRUE)
####

### FOR MODEL COMPARISON FOOTNOTE ###
fitsrz3 <- lmer(Nvot_ms ~ poa*voice + voice*son1.dur.z +vowel_tense*vowel_height +  (1 + poa*voice + son1.dur.z| subj) + (1 | body), eng.cvc1, REML=FALSE)

fitsrz4 <- lmer(Nvot_ms ~ poa*voice + voice*son1.dur.z +vowel_tense*vowel_height +  (1 + voice| subj) + (1 | body), eng.cvc1, REML=FALSE)

fitsrz5 <- lmer(Nvot_ms ~ poa*voice + voice*son1.dur.z +vowel_tense*vowel_height +  (1 | body), eng.cvc1, REML=FALSE)

fit_baseline <- lmer(Nvot_ms ~ 1 + (1|subj), eng.cvc1, REML=FALSE)
fit_baseline2 <- lmer(Nvot_ms ~ 1 + (1|subj) + (1|body), eng.cvc1, REML=FALSE)

#############
### PLOTS ###
#############

tiff("labPT.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=P, y=T)) + geom_point() + geom_smooth(aes(P,T), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("") + scale_x_continuous(limit=c(0,175)) + scale_y_continuous(limit=c(0,175)) + annotate("text", x = 130, y = 5, size = 7, label = "r = 0.95, p < 0.006", fontface = "italic") + annotate("text", x = 87.5, y = 175, size = 8, label = "pʰ") + annotate("text", x = 175, y = 87.5, size = 8, label = "tʰ");
ggMarginal(p, type="histogram", xparams = list(binwidth=7, colour="white", fill="firebrick4"), yparams = list(binwidth=7,colour="white", fill="red3"))
dev.off();

tiff("labTK.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=T, y=K)) + geom_point() + geom_smooth(aes(T,K), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,175)) + scale_y_continuous(limit=c(0,175)) + annotate("text", x = 130, y = 5, size = 7, label = "r = 0.95, p < 0.006", fontface = "italic") + annotate("text", x = 87.5, y = 175, size = 8, label = "tʰ") + annotate("text", x = 175, y = 87.5, size = 8, label = "kʰ")
ggMarginal(p, type="histogram", xparams = list(binwidth=7, colour="white", fill="red3"), yparams = list(binwidth=7,colour="white", fill="olivedrab4"))
dev.off();

tiff("labKP.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=K, y=P)) + geom_point() + geom_smooth(aes(K,P), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,175)) + scale_y_continuous(limit=c(0,175)) + annotate("text",x = 130, y = 5, size = 7, label = "r = 0.96, p < 0.006", fontface = "italic") + annotate("text", x = 87.5, y = 175, size = 8, label = "kʰ") + annotate("text", x = 175, y = 87.5, size = 8, label = "pʰ")
ggMarginal(p, type="histogram", xparams = list(binwidth=7,colour="white", fill="olivedrab4"), yparams = list(binwidth=7,colour="white", fill="firebrick4"))
dev.off();

tiff("labBD.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=B, y=D)) + geom_point() + geom_smooth(aes(B,D), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(5,45)) + scale_y_continuous(limit=c(5,45)) + annotate("text", x = 36, y = 6, size = 7, label = "r = 0.54, p = 0.006", fontface = "italic") + annotate("text", x = 25, y = 45, size = 8, label = "b") + annotate("text", x = 45, y = 25, size = 8, label = "d")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="slategray"), yparams = list(colour="white", fill="paleturquoise4"))
dev.off();

tiff("labDG.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=D, y=G)) + geom_point() + geom_smooth(aes(D,G), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(5,45)) + scale_y_continuous(limit=c(5,45)) + annotate("text", x = 36, y = 6,  size = 7, label = "r = 0.56, p < 0.006", fontface = "italic") + annotate("text", x = 25, y = 45, size = 8, label = "d") + annotate("text", x = 45, y = 25, size = 8, label = "g")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="paleturquoise4"), yparams = list(colour="white", fill="grey38"))
dev.off();

tiff("labGB.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=G, y=B)) + geom_point() + geom_smooth(aes(G,B), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(5,45)) + scale_y_continuous(limit=c(5,45)) + annotate("text",x = 36, y = 6,  size = 7, label = "r = 0.56, p < 0.006", fontface = "italic") + annotate("text", x = 25, y = 45, size = 8, label = "g") + annotate("text", x = 45, y = 25, size = 8, label = "b")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="grey38"), yparams = list(colour="white", fill="slategray"))
dev.off();

tiff("labvoicePB.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=P, y=B)) + geom_point() + geom_smooth(aes(P,B), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,175)) + scale_y_continuous(limit=c(5,45)) + annotate("text", x = 135, y = 6,  size = 7, label = "r = 0.21, p = 0.33", fontface = "italic") + annotate("text", x = 87.5, y = 45, size = 8, label = "pʰ") + annotate("text", x = 175, y = 25, size = 8, label = "b")
ggMarginal(p, type="histogram", xparams = list(binwidth=7, colour="white", fill="firebrick4"), yparams = list(colour="white", fill="slategray"))
dev.off();

tiff("labvoiceTD.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=T, y=D)) + geom_point() + geom_smooth(aes(T,D), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,175)) + scale_y_continuous(limit=c(5,45)) + annotate("text", x = 135, y = 6, size = 7, label = "r = 0.33, p = 0.12", fontface = "italic") + annotate("text", x = 87.5, y = 45, size = 8, label = "tʰ") + annotate("text", x = 175, y = 25, size = 8, label = "d")
ggMarginal(p, type="histogram", xparams = list(binwidth=7, colour="white", fill="red3"), yparams = list(colour="white", fill="paleturquoise4"))
dev.off();

tiff("labvoiceKG.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=K, y=G)) + geom_point() + geom_smooth(aes(K,G), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,175)) + scale_y_continuous(limit=c(5,45)) + annotate("text", x = 135, y = 6,  size = 7, label = "r = 0.18, p = 0.40", fontface = "italic") + annotate("text", x = 87.5, y = 45, size = 8, label = "kʰ") + annotate("text", x = 175, y = 25, size = 8, label = "g")
ggMarginal(p, type="histogram", xparams = list(binwidth=7,colour="white", fill="olivedrab4"), yparams = list(colour="white", fill="grey38"))
dev.off();
