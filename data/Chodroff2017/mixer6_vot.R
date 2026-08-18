#!/bin/sh

#  mixer6_vot.R
#  
#
#  Created by Eleanor Chodroff on 1/14/16
#  Updated 12-20-16

require(plyr)
require(ggplot2)
require(lme4)
require(reshape2)
require(gridExtra)
require(ggExtra)
require(boot)
require(piecewiseSEM)

raw_cues <- read.delim("/Volumes/MIXER6_cogsci/Workspace/analyze_cogsci/00autovot/cueAnalysis_new_all.txt", header=FALSE)
names(raw_cues) <- c("filename", "label", "start", "end", "trial", "vot", "word", "hyp_stop","vowel","vdur","word_int","prec1", "prec2", "follow1", "follow2", "pos","f0_1","f0_2","f0_3","f0_4","f0_5","f0_6","f0_7","f0_8","f0_9","f0_10","w_start","w_end","wdur","sent_start","sent_end","nWords","spk_rate")

raw_cues$stop <- ifelse(raw_cues$label %in% c("man", "man_v", "ch", "ch_v"), as.character(raw_cues$hyp_stop), as.character(raw_cues$label));
raw_cues$vot <- raw_cues$vot*1000
raw_cues$vdur <- raw_cues$vdur*1000
raw_cues$wdur <- raw_cues$wdur*1000
raw_cues$subj <- gsub("_[0-9]*_[0-9]*_[A-Z]","",raw_cues$filename)
raw_cues$oldsubj <- gsub("[0-9]*_[0-9]*_", "", raw_cues$filename)
raw_cues$oldsubj <- gsub("_[A-Z]","",raw_cues$oldsubj)
raw_cues$date <- gsub("_[0-9]*_[A-Z]","",raw_cues$filename)
raw_cues$date <- gsub("[0-9]*_","",raw_cues$date)
raw_cues$gender <- gsub("[0-9]*_[0-9]*_[0-9]*_", "", raw_cues$filename)
raw_cues$gender <- gsub("_stacked2", "", raw_cues$gender)
raw_cues$poa <- ifelse(raw_cues$stop %in% c("P","B"), "lab", ifelse(raw_cues$stop %in% c("T","D"), "cor","dor"))

raw_cues$spk_rate <- as.numeric(as.character(raw_cues$spk_rate))

raw_cues$usef0 <- ifelse(raw_cues$f0_1 != "--undefined--", as.numeric(as.character(raw_cues$f0_1)), ifelse(raw_cues$f0_2 != "--undefined--", as.numeric(as.character(raw_cues$f0_2)),
ifelse(raw_cues$f0_3 != "--undefined--", as.numeric(as.character(raw_cues$f0_3)),
ifelse(raw_cues$f0_4 != "--undefined--",as.numeric(as.character(raw_cues$f0_4)),ifelse(raw_cues$f0_5 != "--undefined--", as.numeric(as.character(raw_cues$f0_5), as.numeric(as.character(raw_cues$f0_6))))))))

subjects <- unique(raw_cues$subj)
dates.all <- c()
sess.all <- c()
subj.all <- c()
for (x in 1:length(subjects)) {
    s <- subjects[x]
    tmp <- subset(raw_cues, subj==s)
    dates <- sort(unique(tmp$date))
    dates.all <- append(dates.all, dates)
    for (d in 1:length(dates)) {
        sess.all <- append(sess.all, d)
        subj.all <- append(subj.all, s)
    }
}
session <- function(x,y) {
    match.x <- match(subj.all, x)
    match.y <- match(dates.all,y)
    combo <- match.x + match.y
    index <- which(combo==2)
    sess <- sess.all[index]
    return(sess)
}
raw_cues$session <- mapply(session, raw_cues$subj, raw_cues$date)


monosyllabic <- c("BACK", "BAD", "BE", "BEEN", "BIG", "BUNCH", "BUSH", "BUT", "BY", "CALLED", "CAN", "CAN'T", "CARE", "COME", "COOK", "COULD", "CUP", "DAY", "DAYS", "DEAL", "DID", "DIED","DO", "DOES", "DON'T", "DOWN", "GAMES", "GET", "GETS", "GO", "GOOD" , "GOES", "GOT", "GUESS", "KEEP", "KILLS", "KIND", "PAID", "PART", "PASS", "PAY", "PEACE", "POINT", "PUSHED", "PUT", "TAKE", "TAX", "TELL", "TIME", "TO", "TOO", "TOOK", "TWO")
disyllabic <- c("BANNING", "BEING", "BETTER", "BINGO", "BUSINESS", "CANNOT", "CAUSES", "COLLAR", "COLLEGE", "COOKING", "COULDN'T", "COUPLE", "DAUGHTER", "DIDN'T", "DOESN'T", "DOLLARS", "GETTING", "GOING", "GOODNESS", "GOTTEN", "KEEPING", "KIDDING", "PAPER", "PARDON", "PAYING", "PEOPLE", "PEOPLE'S", "PITY", "PIZZA", "PRETTY", "PROBLEMS", "PUBLIC", "TALKING", "TANNING", "TEACHER", "TOPIC", "TUNNELS")
polysyllabic <- c("BASICALLY", "BASKETBALL", "BENEFIT", "BIBLICAL", "COMMERCIALS", "COMPANY", "CORPORATE", "DANGEROUS", "DEFINITELY", "DIFFERENT", "PERSONALLY", "TERRIBLE", "TERRIFIED", "TOTALLY")

raw_cues$syll <- ifelse(raw_cues$word %in% monosyllabic, "one", ifelse(raw_cues$word %in% disyllabic, "two", "more"));
raw_cues$syll <- factor(raw_cues$syll, levels=c("one","two","more"))
contrasts(raw_cues$syll) <- contr.treatment(3)

func <- c("BE","BEEN","BUT","BY","DID","DO","DOES","DON'T","GET","GETS","GOT","TO","TOO","BEING","DIDN'T","DOESN'T","GETTING","GOTTEN","CAN","CAN'T","COULD","DOWN","TWO")
raw_cues$type <- ifelse(raw_cues$word %in% func, "func", "lex")


# remove non-existent bursts
cues <- subset(raw_cues, label!="man_v")
cues <- subset(cues, label!="ch_v")

# remove words which don't match the recorded place of articulation, do not have pre-vocalic stops or are too long
cues$wordinit <- substr(cues$word, 1,1)
cues$wordinit <- ifelse(cues$wordinit == "C", "K", cues$wordinit)

ch_nostop <- which(cues$wordinit!=cues$stop & cues$label%in%c("ch","man"))
ch_nostop <- cues[ch_nostop,]
ch_nostop$vowel <- ch_nostop$stop
ch_nostop$stop <- ch_nostop$wordinit
good_ch_nostop <- subset(ch_nostop, grepl("^[PTKBDG]", stop))

nostop <- which(cues$wordinit!=cues$stop)
nostop <- cues[nostop,]
xtabs(~nostop$word, drop=T)

cues <- subset(cues, cues$wordinit==cues$stop)
cues <- rbind(cues, good_ch_nostop)

cues <- subset(cues, !grepl("^TH", word))
cues <- subset(cues, !grepl("^TR", word))
cues <- subset(cues, !grepl("^PR", word))
cues <- subset(cues, grepl("^[AEIOU]", vowel))
cues$word <- droplevels(cues$word)
cues$vowel <- droplevels(cues$vowel)

# remove speakers not born in US
canadian <- c("120372", "120602", "120699","120710","120786")
other <- c("120278", "120308", "120440", "120465", "120513", "120566", "120572", "120698", "120762", "120807", "120822")
cues <- subset(cues, !subj %in% canadian)
cues <- subset(cues, !subj %in% other)

# remove speakers without all three sessions
xtabs(~subj+session, cues)
cues <- subset(cues, !subj %in% c("120301", "120361"))
cues <- subset(cues, stop %in% c("P","T","K","B","D","G"))
ddply(cues, .(stop), summarise, count=length(stop))
ddply(cues, .(vowel), summarise, count=length(vowel))

cues1 <- cues

cues1 <- subset(cues1, word!="TO")
cues1 <- subset(cues1, vowel!= "ER0")
cues1 <- subset(cues1, vowel!= "IH0")

ddply(cues1, .(stop), summarise, count=length(stop))
ddply(cues1, .(vowel), summarise, count=length(vowel))
ddply(cues1, .(word), summarise, count=length(word))

cues1$stop <- factor(cues1$stop, levels=c("P","T","K","B","D","G"))

subj.vot <- ddply(cues1, .(subj,stop), summarise, subj.avg=mean(vot), sd=sd(vot), count=length(subj));
subj.vot$se <- subj.vot$sd / sqrt(subj.vot$count);
subj.vot$plus25sd <- subj.vot$subj.avg + 2.5*subj.vot$sd
subj.vot$min25sd <- subj.vot$subj.avg - 2.5*subj.vot$sd

# remove stops plus or minus 2.5 sd away from speaker mean

cues1$outlier_high <- NA
cues1$outlier_low <- NA
for (i in 1:nrow(cues1)) {
    subj_id <- as.character(cues1$subj[i])
    stop_id <- as.character(cues1$stop[i])
    vot_id <- cues1$vot[i]
    row_num <- which(subj.vot$subj==subj_id & subj.vot$stop==stop_id)
    vot_high <- subj.vot$plus25sd[row_num]
    vot_low <- subj.vot$min25sd[row_num]
    cues1$outlier_high[i] <- ifelse(vot_id > vot_high,"outlier","incl")
    cues1$outlier_low[i] <- ifelse(vot_id < vot_low,"outlier","other")
}

cues1.all <- cues1
cues1 <- subset(cues1, outlier_high!="outlier")
cues1 <- subset(cues1, outlier_low!="outlier")

nogood <- which(is.na(cues1$spk_rate))
cues1 <- cues1[-nogood,]

cues1.allvowels <- cues1
cues1 <- subset(cues1, !grepl("0", vowel))
### VOWEL HEIGHT ###
vowelHeight <- read.csv("/Volumes/MIXER6_cogsci/Workspace/analyze_cogsci/VowelHeight.csv")
cues1 <- merge(cues1, vowelHeight, by="vowel", all=TRUE)
cues1$vowel_height <- ifelse(cues1$height=="high", "high","nonhigh")
#####################

### LEXICAL FREQUENCY ###
lexFreq <- read.delim("/Volumes/MIXER6_cogsci/Workspace/analyze_cogsci/ClearpondLexicalStatistics_new.txt")
lexFreq$orig_word <- toupper(lexFreq$orig_word)
lexFreq$logFreq <- log(lexFreq$Freq_per_million)
lexFreq$word <- lexFreq$orig_word
lexFreq$calc_Word <- lexFreq$Word
lexFreq$Word <- NULL

cues1 <- merge(cues1, lexFreq, by="word", all=TRUE)
#########################

#############
## NUMBERS ##
#############

ddply(cues1, .(stop), summarise, count=length(vot))
talkercount <- ddply(cues1, .(stop, subj), summarise, count=length(vot))
talkercount2 <- ddply(cues1, .(subj), summarise, count=length(vot))
summary(subset(talkercount, stop=="P"))
summary(subset(talkercount, stop=="T"))
summary(subset(talkercount, stop=="K"))
summary(subset(talkercount, stop=="B"))
summary(subset(talkercount, stop=="D"))
summary(subset(talkercount, stop=="G"))

wordtokens <- ddply(cues1, .(word, stop), summarise, count=length(word))
summary(subset(wordtokens, stop=="P"))
summary(subset(wordtokens, stop=="T"))
summary(subset(wordtokens, stop=="K"))
summary(subset(wordtokens, stop=="B"))
summary(subset(wordtokens, stop=="D"))
summary(subset(wordtokens, stop=="G"))

talkermeans <- ddply(cues1, .(stop,subj), summarise, mean_vot=mean(vot), sd_vot=sd(vot))

################
# CORRELATIONS #
################
cues1$stop <- factor(cues1$stop, levels=c("P","T","K","B","D","G"))

subj.means.vot <- ddply(cues1, .(subj, stop, gender), summarise, vot_mean=mean(vot), vot_sd=sd(vot))
subj.means.vot.F <- subset(subj.means.vot, gender=="F")
subj.means.vot.M <- subset(subj.means.vot, gender=="M")

sjm.vot.F <- dcast(subj.means.vot.F, subj~stop, value.var="vot_mean")
sjm.vot.M <- dcast(subj.means.vot.M, subj~stop, value.var="vot_mean")

sjm.vot <- dcast(subj.means.vot, subj~stop, value.var="vot_mean")

subj.means.logvot <- ddply(cues1, .(subj, stop, gender), summarise, vot_mean=mean(log(vot)), vot_sd=sd(log(vot)))
sjm.logvot <- dcast(subj.means.logvot, subj~stop, value.var="vot_mean")

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

cor.test(~P + T, sjm.vot.F)
cor.test(~P + K, sjm.vot.F)
cor.test(~T + K, sjm.vot.F)
cor.test(~B + D, sjm.vot.F)
cor.test(~B + G, sjm.vot.F)
cor.test(~D + G, sjm.vot.F)
cor.test(~P + B, sjm.vot.F)
cor.test(~T + D, sjm.vot.F)
cor.test(~K + G, sjm.vot.F)

cor.test(~P + T, sjm.vot.M)
cor.test(~P + K, sjm.vot.M)
cor.test(~T + K, sjm.vot.M)
cor.test(~B + D, sjm.vot.M)
cor.test(~B + G, sjm.vot.M)
cor.test(~D + G, sjm.vot.M)
cor.test(~P + B, sjm.vot.M)
cor.test(~T + D, sjm.vot.M)
cor.test(~K + G, sjm.vot.M)

sjm.vot$PT <- ifelse(sjm.vot$P < sjm.vot$T, "P<T", "T<P")
sjm.vot$TK <- ifelse(sjm.vot$T < sjm.vot$K, "T<K", "K<T")
sjm.vot$PTK <- ifelse(sjm.vot$PT == "P<T" & sjm.vot$TK == "T<K", "PTK", ifelse(sjm.vot$PT=="P<T" & sjm.vot$TK =="K<T", "PKT", "else"))

sjm.vot$BD <- ifelse(sjm.vot$B < sjm.vot$T, "B<D", "D<B")
sjm.vot$DG <- ifelse(sjm.vot$D < sjm.vot$G, "D<G", "G<D")
sjm.vot$BDG <- ifelse(sjm.vot$BD == "B<D" & sjm.vot$DG == "D<G", "BDG", ifelse(sjm.vot$BD=="B<D" & sjm.vot$DG =="G<D", "BGD", "else"))
sjm.vot$rank <- paste(sjm.vot$BDG, sjm.vot$PTK)


############ RESIDUALS #############
# correlations on residuals
cues1$Nvdur <- cues1$vdur - mean(cues1$vdur);
residuals_spkrate <- lm(Nvot ~ 1 + Nvdur, cues1)
tmp <- residuals(residuals_spkrate)
cues1$residuals_spkrate <- tmp

#residuals_spkrate <- lm(Nvot ~ 1 + Nspk_rate, cues1)
#tmp <- residuals(residuals_spkrate)
#cues1$residuals_spkrate <- tmp

residuals_spkrate <- lm(Nvot ~ 1 + z.spk_rate, cues1)
tmp <- residuals(residuals_spkrate)
cues1$residuals_spkrate <- tmp

subj.means.sub <- ddply(cues1, .(subj, stop, gender), summarise, vot_mean=mean(residuals_spkrate), vot_sd=sd(residuals_spkrate))
sjm.sub <- dcast(subj.means.sub, subj~stop, value.var="vot_mean")

vot_residual <- lm(vot ~ 1 + poa*voice + voice*z.vdur + vowel_height*tenseness + syll + pos + z.logFreq2, cues1)
summary(vot_residual)
tmp <- residuals(vot_residual)
cues1$vot_residuals <- tmp

subj.means.sub <- ddply(cues1, .(subj, stop, gender), summarise, vot_mean=mean(vot_residuals), vot_sd=sd(vot_residuals))
sjm.sub <- dcast(subj.means.sub, subj~stop, value.var="vot_mean")

vot_residual <- lm(vot ~ 1 + z.vdur + pos, cues1)
summary(vot_residual)
tmp <- residuals(vot_residual)
cues1$vot_residuals <- tmp

subj.means.sub <- ddply(cues1, .(subj, stop, gender), summarise, vot_mean=mean(vot_residuals), vot_sd=sd(vot_residuals))
sjm.sub <- dcast(subj.means.sub, subj~stop, value.var="vot_mean")




cor.test(~P+T, sjm.sub)
cor.test(~P+K, sjm.sub) 
cor.test(~T+K, sjm.sub)

cor.test(~B+D, sjm.sub)
cor.test(~B+G, sjm.sub) 
cor.test(~D+G, sjm.sub)

cor.test(~P+B, sjm.sub)
cor.test(~T+D, sjm.sub)
cor.test(~K+G, sjm.sub)


################################################
### correlations on vowel duration residuals ###
################################################

cues1$Nvdur <- cues1$vdur - mean(cues1$vdur)
res <- lm(Nvot ~ 1+Nvdur, cues1)
tmp <- residuals(res)
cues1$res_vdur <- tmp

subj.means.res_vdur <- ddply(cues1, .(subj, stop), summarise, vot_mean=mean(res_vdur), vot_sd=sd(res_vdur))
sjm.res_vdur <- dcast(subj.means.res_vdur, subj~stop, value.var="vot_mean")

cor.test(~P+T, sjm.res_vdur)
cor.test(~P+K, sjm.res_vdur) 
cor.test(~T+K, sjm.res_vdur)

cor.test(~B+D, sjm.res_vdur)
cor.test(~B+G, sjm.res_vdur) 
cor.test(~D+G, sjm.res_vdur)

cor.test(~P+B, sjm.res_vdur)
cor.test(~T+D, sjm.res_vdur)
cor.test(~K+G, sjm.res_vdur)


####### BOOTSTRAPPING #########

correl <- function(data, i) {
    cor(data[i, 1], data[i, 2])
}

### main correlations
results_PT <- boot(data=sjm.vot[,c(2,3)], statistic=correl, R=1000)
results_PK <- boot(data=sjm.vot[,c(2,4)], statistic=correl, R=1000)
results_TK <- boot(data=sjm.vot[,c(3,4)], statistic=correl, R=1000)

results_PB <- boot(data=sjm.vot[,c(2,5)], statistic=correl, R=1000)
results_TD <- boot(data=sjm.vot[,c(3,6)], statistic=correl, R=1000)
results_KG <- boot(data=sjm.vot[,c(4,7)], statistic=correl, R=1000)

results_BD <- boot(data=sjm.vot[,c(5,6)], statistic=correl, R=1000)
results_BG <- boot(data=sjm.vot[,c(5,7)], statistic=correl, R=1000)
results_DG <- boot(data=sjm.vot[,c(6,7)], statistic=correl, R=1000)

### RESIDUALS
results_PT <- boot(data=sjm.sub[,c(2,3)], statistic=correl, R=1000)
results_PK <- boot(data=sjm.sub[,c(2,4)], statistic=correl, R=1000)
results_TK <- boot(data=sjm.sub[,c(3,4)], statistic=correl, R=1000)

results_PB <- boot(data=sjm.sub[,c(2,5)], statistic=correl, R=1000)
results_TD <- boot(data=sjm.sub[,c(3,6)], statistic=correl, R=1000)
results_KG <- boot(data=sjm.sub[,c(4,7)], statistic=correl, R=1000)

results_BD <- boot(data=sjm.sub[,c(5,6)], statistic=correl, R=1000)
results_BG <- boot(data=sjm.sub[,c(5,7)], statistic=correl, R=1000)
results_DG <- boot(data=sjm.sub[,c(6,7)], statistic=correl, R=1000)

results_B <- boot(data=subset(subj.means.sub, stop=="B")[,4:5], statistic=correl, R=1000)
results_D <- boot(data=subset(subj.means.sub, stop=="D")[,4:5], statistic=correl, R=1000)
results_G <- boot(data=subset(subj.means.sub, stop=="G")[,4:5], statistic=correl, R=1000)
results_P <- boot(data=subset(subj.means.sub, stop=="P")[,4:5], statistic=correl, R=1000)
results_T <- boot(data=subset(subj.means.sub, stop=="T")[,4:5], statistic=correl, R=1000)
results_K <- boot(data=subset(subj.means.sub, stop=="K")[,4:5], statistic=correl, R=1000)
results_all <- boot(data=subj.means.sub[,4:5], statistic=correl, R=1000)


boot.ci(results_PT, type="bca")
boot.ci(results_PK, type="bca")
boot.ci(results_TK, type="bca")

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

# view results
plot(results_PK)

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

#########################
# MODELS: NEW CONTRASTS #
#########################

place <- read.delim("/Volumes/MIXER6_cogsci/Workspace/analyze_cogsci/00autovot/analyses/place.csv", sep=",", header=TRUE)
voice <- read.delim("/Volumes/MIXER6_cogsci/Workspace/analyze_cogsci/00autovot/analyses/voice.csv", sep=",", header=TRUE)
tenseness
syll
vowel_height
pos

cues1$poa <- factor(cues1$poa, levels=c("cor","dor","lab"));
contrasts(cues1$poa) <- cbind(place$contrast1, place$contrast2)

cues1$voice <- factor(cues1$voice, levels=c("vcl","vcd"));
contrasts(cues1$voice) <- voice$contrast

cues1$tenseness <- factor(cues1$tenseness, levels=c("tense","lax"))
contrasts(cues1$tenseness) <- tenseness$contrast

cues1$vowel_height <- factor(cues1$vowel_height, levels=c("high","nonhigh"))
contrasts(cues1$vowel_height) <- vowel_height$contrast

cues1$syll <- factor(cues1$syll, levels=c("two","more","one"));
contrasts(cues1$syll) <- cbind(syll$contrast1, syll$contrast2)

cues1$pos <- factor(cues1$pos, levels=c("utt_init", "utt_final", "postpause","prepause", "utt_mid"))
contrasts(cues1$pos) <- cbind(pos$contrast1, pos$contrast2, pos$contrast3, pos$contrast4)

cues1$Nvot <- cues1$vot - mean(cues1$vot);
cues1$z.spk_rate <- scale(cues1$spk_rate)
cues1$z.logFreq2<- scale(cues1$logFreq2)
cues1$z.vdur <- scale(cues1$vdur)
cues1$Nvdur <- cues1$vdur - mean(cues1$vdur);

########## full with weighted contrasts ############

#### this one in JPhon revision ####
fitz3 <- lmer(Nvot ~ poa*voice + voice*z.spk_rate + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + z.spk_rate|subj) + (1|word), cues1, REML=TRUE)
fitz5 <- lmer(Nvot ~ poa*voice + voice*z.vdur + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + z.vdur|subj) + (1|word), cues1, REML=TRUE)

fitz6r <- lmer(Nvot ~ poa*voice + voice*Nvdur + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + Nvdur|subj) + (1|word), cues1, REML=FALSE)
fitz5r <- lmer(Nvot ~ poa*voice + voice*z.vdur + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + z.vdur|subj) + (1|word), cues1, REML=FALSE)
fitz3r <- lmer(Nvot ~ poa*voice + voice*z.spk_rate + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + z.spk_rate|subj) + (1|word), cues1, REML=FALSE)

##fitz4 <- lmer(Nvot ~ poa*voice + voice*z.spk_rate + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + voice*z.spk_rate|subj) + (1|word), cues1, REML=TRUE) ## doesn't converge

###########
fitz1 <- lmer(Nvot ~ poa*voice + z.spk_rate + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + z.spk_rate|subj) + (1|word), cues1, REML=TRUE)

fitz2 <- lmer(Nvot ~ poa*voice * z.spk_rate + vowel_height*tenseness + syll + pos + z.logFreq2 + (1 + poa*voice + z.spk_rate|subj) + (1|word), cues1, REML=TRUE)

###########
## PLOTS ##
###########

tiff("PT.tiff", height = 7, width = 7, res = 1000, units="in")
p <- ggplot(sjm.vot, aes(x=P, y=T)) + geom_point() + geom_smooth(aes(P,T), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.83, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 100, size = 8, label = "pʰ") + annotate("text", x = 100, y = 59, size = 8, label = "tʰ")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="firebrick4"), yparams = list(colour="white", fill="red3"))
dev.off();

tiff("TK.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=T, y=K)) + geom_point() + geom_smooth(aes(T,K), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.77, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 100, size = 8, label = "tʰ") + annotate("text", x = 100, y = 59, size = 8, label = "kʰ")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="red3"), yparams = list(colour="white", fill="olivedrab4"))
dev.off();

tiff("KP.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=K, y=P)) + geom_point() + geom_smooth(aes(K,P), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.82, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 100, size = 8, label = "kʰ") + annotate("text", x = 100, y = 59, size = 8, label = "pʰ")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="olivedrab4"), yparams = list(colour="white", fill="firebrick4"))
dev.off();

pdf("/Users/Eleanor/Desktop/PT.pdf", height = 7, width = 7)
p <- ggplot(sjm.vot, aes(x=P, y=T)) + geom_point() + geom_smooth(aes(P,T), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.83, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 100, size = 8, label = "/p/") + annotate("text", x = 100, y = 59, size = 8, label = "/t/")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="firebrick4"), yparams = list(colour="white", fill="red3"))
dev.off();

pdf("/Users/Eleanor/Desktop/TK.pdf", height = 7, width = 7)
p <- ggplot(sjm.vot, aes(x=T, y=K)) + geom_point() + geom_smooth(aes(T,K), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.77, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 100, size = 8, label = "/t/") + annotate("text", x = 100, y = 59, size = 8, label = "/k/")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="red3"), yparams = list(colour="white", fill="olivedrab4"))
dev.off();

pdf("/Users/Eleanor/Desktop/KP.pdf", height = 7, width = 7)
p <- ggplot(sjm.vot, aes(x=K, y=P)) + geom_point() + geom_smooth(aes(K,P), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.82, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 100, size = 8, label = "/k/") + annotate("text", x = 100, y = 59, size = 8, label = "/p/")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="olivedrab4"), yparams = list(colour="white", fill="firebrick4"))
dev.off();
tiff("BD.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=B, y=D)) + geom_point() + geom_smooth(aes(B,D), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,30)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 23, y = 1, size = 7, label = "r = 0.07, p = 0.33", fontface = "italic") + annotate("text", x = 15, y = 30, size = 8, label = "b") + annotate("text", x = 30, y = 15, size = 8, label = "d")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="slategray"), yparams = list(colour="white", fill="paleturquoise4"))
dev.off();

tiff("DG.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot,aes(x=D, y=G)) + geom_point() + geom_smooth(aes(D,G), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,30))+ scale_y_continuous(limit=c(0,30)) + annotate("text", x = 23, y = 1,size = 7, label = "r = 0.33, p < 0.006", fontface = "italic") + annotate("text", x = 15, y = 30, size = 8, label = "d") + annotate("text", x = 30, y = 15, size = 8, label = "g")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="paleturquoise4"), yparams = list(colour="white", fill="grey38"))
dev.off();

tiff("GB.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=G, y=B)) + geom_point() + geom_smooth(aes(G,B), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,30)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 23, y = 1, size = 7, label = "r = 0.49, p < 0.006", fontface = "italic") + annotate("text", x = 15, y = 30, size = 8, label = "g") + annotate("text", x = 30, y = 15, size = 8, label = "b")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="grey38"), yparams = list(colour="white", fill="slategray"))
dev.off();

tiff("voicePB.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=P, y=B)) + geom_point() + geom_smooth(aes(P,B), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 80, y = 1, size = 7, label = "r = 0.15, p = 0.05", fontface = "italic") + annotate("text", x = 59, y = 30, size = 8, label = "pʰ") + annotate("text", x = 100, y = 15, size = 8, label = "b")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="firebrick4"), yparams = list(colour="white", fill="slategray"))
dev.off();

tiff("voiceTD.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=T, y=D)) + geom_point() + geom_smooth(aes(T,D), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 80, y = 1, size = 7, label = "r = 0.53, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 30, size = 8, label = "tʰ") + annotate("text", x = 100, y = 15, size = 8, label = "d")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="red3"), yparams = list(colour="white", fill="paleturquoise4"))
dev.off();

tiff("voiceKG.tiff", height = 7, width = 7, res=1000, units="in")
p <- ggplot(sjm.vot, aes(x=K, y=G)) + geom_point() + geom_smooth(aes(K,G), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 80, y = 1, size = 7, label = "r = 0.40, p < 0.006", fontface = "italic") + annotate("text", x = 59, y = 30, size = 8, label = "kʰ") + annotate("text", x = 100, y = 15, size = 8, label = "g")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="olivedrab4"), yparams = list(colour="white", fill="grey38"))
dev.off();



##### OLD ####
tiff("/Users/Eleanor/Desktop/PT.tiff");
p <- ggplot(sjm.vot, aes(x=P, y=T)) + geom_point() + geom_smooth(aes(P,T), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.83, p < 0.005", fontface = "italic") + annotate("text", x = 25, y = 98, size = 8, label = "pʰ - tʰ")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="firebrick4"), yparams = list(colour="white", fill="red3"))
dev.off();

tiff("/Users/Eleanor/Desktop/TK.tiff");
p <- ggplot(sjm.vot, aes(x=T, y=K)) + geom_point() + geom_smooth(aes(T,K), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.77, p < 0.005", fontface = "italic") + annotate("text", x = 25, y = 98, size = 8, label = "tʰ - kʰ")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="red3"), yparams = list(colour="white", fill="olivedrab4"))
dev.off();

tiff("/Users/Eleanor/Desktop/KP.tiff");
p <- ggplot(sjm.vot, aes(x=K, y=P)) + geom_point() + geom_smooth(aes(K,P), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(18,100)) + annotate("text", x = 80, y = 21, size = 7, label = "r = 0.82, p < 0.005", fontface = "italic") + annotate("text", x = 25, y = 98, size = 8, label = "kʰ - pʰ")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="olivedrab4"), yparams = list(colour="white", fill="firebrick4"))
dev.off();

tiff("/Users/Eleanor/Desktop/BD.tiff");
p <- ggplot(sjm.vot, aes(x=B, y=D)) + geom_point() + geom_smooth(aes(B,D), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,30)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 23, y = 1, size = 7, label = "r = 0.07, p = 0.33", fontface = "italic")+ annotate("text", x = 2, y = 29, size = 8, label = "b - d")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="slategray"), yparams = list(colour="white", fill="paleturquoise4"))
dev.off();

tiff("/Users/Eleanor/Desktop/DG.tiff");
p <- ggplot(sjm.vot,aes(x=D, y=G)) + geom_point() + geom_smooth(aes(D,G), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,30))+ scale_y_continuous(limit=c(0,30)) + annotate("text", x = 23, y = 1,size = 7, label = "r = 0.33, p < 0.005", fontface = "italic") + annotate("text", x = 2, y = 29, size = 8, label = "d - g")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="paleturquoise4"), yparams = list(colour="white", fill="grey38"))
dev.off();

tiff("/Users/Eleanor/Desktop/GB.tiff");
p <- ggplot(sjm.vot, aes(x=G, y=B)) + geom_point() + geom_smooth(aes(G,B), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(0,30)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 23, y = 1, size = 7, label = "r = 0.49, p < 0.005", fontface = "italic") + annotate("text", x = 2, y = 29, size = 8, label = "g - b")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="grey38"), yparams = list(colour="white", fill="slategray"))
dev.off();

tiff("/Users/Eleanor/Desktop/voicePB.tiff");
p <- ggplot(sjm.vot, aes(x=P, y=B)) + geom_point() + geom_smooth(aes(P,B), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 80, y = 1, size = 7, label = "r = 0.15, p = 0.05", fontface = "italic") + annotate("text", x = 24, y = 29, size = 8, label = "pʰ - b")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="firebrick4"), yparams = list(colour="white", fill="slategray"))
dev.off();

tiff("/Users/Eleanor/Desktop/voiceTD.tiff");
p <- ggplot(sjm.vot, aes(x=T, y=D)) + geom_point() + geom_smooth(aes(T,D), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 80, y = 1, size = 7, label = "r = 0.53, p < 0.005", fontface = "italic") + annotate("text", x = 24, y = 29, size = 8, label = "tʰ - d")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="red3"), yparams = list(colour="white", fill="paleturquoise4"))
dev.off();

tiff("/Users/Eleanor/Desktop/voiceKG.tiff");
p <- ggplot(sjm.vot, aes(x=K, y=G)) + geom_point() + geom_smooth(aes(K,G), method=lm, color = "black", size = 0.75, se=TRUE) + theme_bw(20) + xlab("") + ylab("")+ scale_x_continuous(limit=c(18,100)) + scale_y_continuous(limit=c(0,30)) + annotate("text", x = 80, y = 1, size = 7, label = "r = 0.40, p < 0.005", fontface = "italic") + annotate("text", x = 24, y = 29, size = 8, label = "kʰ - g")
ggMarginal(p, type="histogram", xparams = list(colour="white", fill="olivedrab4"), yparams = list(colour="white", fill="grey38"))
dev.off();


