################################################################################################################################################################################################
################################################################################################################################################################################################
####################################################################################  LOADING PACKAGES #########################################################################################
################################################################################################################################################################################################
################################################################################################################################################################################################

library("AER")
library("glmmTMB")
library("tidyverse")
library("tseries")
library("multcomp")
library("stargazer")
library("pscl")
library("Rfast")
library("gridExtra")
library("car")
library("boot")
library("multcomp")
library("restriktor")
library("lme4")
library("lintr")
library("pastecs")
library("quantreg")
library("multcomp")
library("cowplot")
library("gridExtra")
library("ggpubr")
library("ggsci")
library("mgcv")
options(scipen=999)

################################################################################################################################################################################################
################################################################################################################################################################################################
####################################################################################  BASIC MODEL  #############################################################################################
################################################################################################################################################################################################
################################################################################################################################################################################################
prop.test()
################################################################ DATA ################################################################

data_id <- read.csv("C:/Users/Honzík/OneDrive - Vysoká škola ekonomická v Praze/Connection/Plocha/Doktorát/tournament theory/id_gamenames.csv", header=TRUE, stringsAsFactors=FALSE, sep=";") # importing data
data_team <- read.csv("C:/Users/Honzík/OneDrive - Vysoká škola ekonomická v Praze/Connection/Plocha/Doktorát/tournament theory/all_team_results.csv", header=TRUE, stringsAsFactors=FALSE, sep=";") # importing data for team games
data_indi <- read.csv("C:/Users/Honzík/OneDrive - Vysoká škola ekonomická v Praze/Connection/Plocha/Doktorát/tournament theory/all_indi_results.csv", header=TRUE, stringsAsFactors=FALSE, sep=";") # importing data for individualistic games
######## Reforming structure ########

db <- rbind(data_team, data_indi)
db[db == "#NENÍ_K_DISPOZICI"] <- 0
db <- merge(data_id, db, by="GameId")
db <- db[,c(-1, -4, -5)]
db <- subset(db, Rank_1_prize != 0)
db$EndDate <- as.Date(db$EndDate, format="%Y-%m-%d")
db$EndDate <- format(db$EndDate, format="%Y")
db$EndDate <- factor(db$EndDate)

db$Location <- str_replace_all(db$Location, "(?i)\\b(?!\\w*Online);?", "")
db$Location[grepl("Online", db$Location, ignore.case=FALSE)] <- "Online"
db$Location[db$Location == "online"] <- "Online"
db$Location[db$Location != "Online"] <- "Offline"
db$Location <- factor(db$Location)

colnames(db)[7:14] <- c("r1_p", "r2_p", "r3_p", "r4_p", "r5_p", "r6_p", "r7_p", "r8_p")

db$r1_p <- as.integer(db$r1_p)
db$r2_p <- as.integer(db$r2_p)
db$r3_p <- as.integer(db$r3_p)
db$r4_p <- as.integer(db$r4_p)
db$r5_p <- as.integer(db$r5_p)
db$r6_p <- as.integer(db$r6_p)
db$r7_p <- as.integer(db$r7_p)
db$r8_p <- as.integer(db$r8_p)

db <- db[!(db$r1_p < db$r2_p | db$r2_p < db$r3_p | db$r3_p < db$r4_p | db$r4_p < db$r5_p | db$r5_p < db$r6_p | db$r6_p < db$r7_p | db$r7_p < db$r8_p),]

db <- db[!(db$r2_p == 0),]
db[(db$TotalUSDPrize == 0),]

db <- db[!(db$TotalUSDPrize == 0),]
db[db$GameName == "Attack on Titan Tribute Game",]

# db <- db[!(db$r1_p == 0 | db$r2_p == 0 | db$r3_p == 0 | db$r4_p == 0 | db$r5_p == 0 | 
#                          db$r6_p == 0 | db$r7_p == 0 | db$r8_p == 0),]

######## Adjusting the ps for inflation ########

c <- 1:24
a <- 1998:2021
b <- c(0.0155, 0.0219, 0.0338, 0.0283, 0.0159, 0.0227, 0.0268, 0.0339, 0.0323, 0.0285, 0.0384, -0.0036, 0.0164, 0.0316, 0.0207, 0.0146, 0.0162, 0.0012, 0.0126, 0.0213, 0.0244, 0.0181, 0.0125, 0)
c[24] <- 1
c[23] <- 0.9875

for (i in 1:22){c[23-i] = c[24-i]*(1-b[23-i])}

inflation <- data.frame(cbind(a, c))
colnames(inflation) <- c("EndDate", "CPI")
db <- merge(db, inflation, by="EndDate")

for (i in 6:14){db[, i] <- db[, i]*db[, 15]}

stat.desc(db)
######## Transforming from wide to long ########

colnames(db)[1:6] <- c("year", "game", "tournament", "type", "team", "tot_p")
db[is.na(db)] <- "Online"
as.factor(db$game)
summary(as.factor(db$team))
summary(as.factor(db$type))

length_desired = c(1,2,3,4,5,6,7,8)
db$s1_winp = 0
db$s2_winp = 0
db$s3_winp = 0
db$s4_winp = 0
db$s5_winp = 0
db$s6_winp = 0
db$s7_winp = 0
db$s8_winp = 0

for (i in 1:length(db$year)) {
  row_raw = unlist(unique(c(db[i, 7:14])))
  row = head(c(row_raw, 0 * length_desired), length(length_desired))
  for (j in 1:8) {
    db[i, j+15] = row[[j]]
  }
}

db <- db[,c(1:6,16:23)]

### ALL, strict inequality ##################################################################
for (i in 1:6) {db[,14+i] <- (db[,6+i] - db[,7+i] > db[,7+i] - db[,8+i]) | db[,7+i] == 0}

final_cond <- db$V15 & db$V16 & db$V17 & db$V18 & db$V19 & db$V20
trues <- ifelse(final_cond, 1, 0)
truesfinal <- ifelse(db$s1_winp-1.5*db$s2_winp>0, 1, 0)
sum(trues)/length(trues)
sum(truesfinal)/length(truesfinal)

for (i in 2011:2021){
  final_cond <- db[db$year == i,"V15"] & db[db$year == i,"V16"] & db[db$year == i,"V17"] & db[db$year == i,"V18"] & db[db$year == i,"V19"] & db[db$year == i,"V20"]
  trues <- ifelse(final_cond, 1, 0)
  truesfinal <- ifelse(db[db$year == i,"s1_winp"]-1.5*db[db$year == i,"s2_winp"]>0, 1, 0)
  print(paste0(i, ": ", sum(trues)/length(trues), ", final: ", sum(truesfinal)/length(truesfinal)))}


by_offline <- db[db$type=="Offline","V15"] & db[db$type=="Offline","V16"] & db[db$type=="Offline","V17"] & db[db$type=="Offline","V18"] & db[db$type=="Offline","V19"] & db[db$type=="Offline","V20"]
by_online <- db[db$type=="Online","V15"] & db[db$type=="Online","V16"] & db[db$type=="Online","V17"] & db[db$type=="Online","V18"] & db[db$type=="Online","V19"] & db[db$type=="Online","V20"]
offline_trues <- ifelse(by_offline, 1, 0)
online_trues <- ifelse(by_online, 1, 0)
offfinal <- ifelse(db[db$type=="Offline","s1_winp"]-1.5*db[db$type=="Offline","s2_winp"]>0, 1, 0)
onfinal <- ifelse(db[db$type=="Online","s1_winp"]-1.5*db[db$type=="Online","s2_winp"]>0, 1, 0)
print(paste0("Offline: ", sum(na.omit(offline_trues))/length(na.omit(offline_trues)), ", Online: ", sum(na.omit(online_trues))/length(na.omit(online_trues)),
             ", Final off: ", sum(na.omit(offfinal))/length(na.omit(offfinal)), ", Final on: ", sum(na.omit(onfinal))/length(na.omit(onfinal))))

by_team <- db[db$team==1,"V15"] & db[db$team==1,"V16"] & db[db$team==1,"V17"] & db[db$team==1,"V18"] & db[db$team==1,"V19"] & db[db$team==1,"V20"]
by_ind <- db[db$team==0,"V15"] & db[db$team==0,"V16"] & db[db$team==0,"V17"] & db[db$team==0,"V18"] & db[db$team==0,"V19"] & db[db$team==0,"V20"]
team_trues <- ifelse(by_team, 1, 0)
ind_trues <- ifelse(by_ind, 1, 0)
teamfinal <- ifelse(db[db$team==1,"s1_winp"]-1.5*db[db$team==1,"s2_winp"]>0, 1, 0)
indfinal <- ifelse(db[db$team==0,"s1_winp"]-1.5*db[db$team==0,"s2_winp"]>0, 1, 0)
print(paste0("Team: ", sum(na.omit(team_trues))/length(na.omit(team_trues)), ", Individual: ", sum(na.omit(ind_trues))/length(na.omit(ind_trues)),
             ", Final team: ", sum(na.omit(teamfinal))/length(na.omit(teamfinal)), ", Final ind: ", sum(na.omit(indfinal))/length(na.omit(indfinal))))

db <- db %>%  mutate(decile = ntile(tot_p, 10))

for (i in 1:10){
  final_cond <- db[db$decile == i,]$V15 & db[db$decile == i,]$V16 & db[db$decile == i,]$V17 & db[db$decile == i,]$V18 & db[db$decile == i,]$V19 & db[db$decile == i,]$V20
  trues <- ifelse(final_cond, 1, 0)
  truesfinal <- ifelse(db[db$decile == i,"s1_winp"]-1.5*db[db$decile == i,"s2_winp"]>0, 1, 0)
  print(paste0(i, ": ", sum(trues)/length(trues), ", final: ", sum(truesfinal)/length(truesfinal)))}


for (i in 2011:2021){
  rowos <- nrow(db[db$year == i,])
  print(paste0(i, ": ", rowos, ", ", rowos/315.62))
}

for (i in 1:10){
  rowos <- nrow(db[db$decile == i,])
  print(paste0(i, ": ", rowos, ", ", rowos/315.62))
}


### Means all for plotting
all <- db %>%
  summarize(mean_stage1 = mean(s1_winp), mean_stage2 = mean(s2_winp), mean_stage3 = mean(s3_winp), mean_stage4 = mean(s4_winp),
            mean_stage5 = mean(s5_winp), mean_stage6 = mean(s6_winp), mean_stage7 = mean(s7_winp), mean_stage8 = mean(s8_winp))

### Means by year for plotting
by_year <- db %>%
  group_by(year) %>%
  summarize(mean_stage1 = mean(s1_winp), mean_stage2 = mean(s2_winp), mean_stage3 = mean(s3_winp), mean_stage4 = mean(s4_winp),
            mean_stage5 = mean(s5_winp), mean_stage6 = mean(s6_winp), mean_stage7 = mean(s7_winp), mean_stage8 = mean(s8_winp))

### Means by type for plotting
by_type <- db %>%
  group_by(type) %>%
  summarize(mean_stage1 = mean(s1_winp), mean_stage2 = mean(s2_winp), mean_stage3 = mean(s3_winp), mean_stage4 = mean(s4_winp),
            mean_stage5 = mean(s5_winp), mean_stage6 = mean(s6_winp), mean_stage7 = mean(s7_winp), mean_stage8 = mean(s8_winp))

### Means by team for plotting
by_team <- db %>%
  group_by(team) %>%
  summarize(mean_stage1 = mean(s1_winp), mean_stage2 = mean(s2_winp), mean_stage3 = mean(s3_winp), mean_stage4 = mean(s4_winp),
            mean_stage5 = mean(s5_winp), mean_stage6 = mean(s6_winp), mean_stage7 = mean(s7_winp), mean_stage8 = mean(s8_winp))

team_graph_data <- as_tibble(cbind(team=c(rep("Team games",8), rep("Individual games",8)), rank=c(seq(1,8), seq(1,8)), prizes=t(bind_cols(by_team[2,2:9], by_team[1,2:9]))))
team_graph_data <- transform(team_graph_data, prizes=as.numeric(V3))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

scale_fill_manual(values=cbPalette)

team_graph <- team_graph_data %>% ggplot(aes(x=rank, y=prizes, fill=forcats::fct_rev(team))) +
  geom_bar(stat="identity", color="black", width=0.7, position=position_dodge(width=0.8)) +
  scale_fill_jco() +
  #scale_fill_manual(values=c('#d7d7d7', '#999999')) +
  scale_x_discrete(name="Level") +
  scale_y_continuous(name="Prize", breaks=c(0, 5000, 10000, 15000), labels=c("$0", "$5,000", "$10,000", "$15,000")) +
  theme(axis.title.x=element_text(face="bold", size=16, vjust=0),
        axis.title.y=element_text(face="bold", size=16, vjust=2),
        axis.line.y=element_line(color="black"),
        axis.line.x=element_line(color="black"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        legend.spacing = unit(0.9, "cm"),
        legend.position=c(0.6, 0.75),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

type_graph_data <- as_tibble(cbind(off=c(rep("Offline tournaments", 8), rep("Online tournaments", 8)), rank=c(seq(1,8), seq(1,8)), prizes=t(bind_cols(by_type[1,2:9], by_type[2,2:9]))))
type_graph_data <- transform(type_graph_data, prizes=as.numeric(V3))

type_graph <- type_graph_data %>% ggplot(aes(x=rank, y=prizes, fill=off)) +
  geom_bar(stat="identity", color="black", width=0.7, position=position_dodge(width=0.8)) +
  scale_fill_jco() + 
  scale_x_discrete(name="Level") +
  scale_y_continuous(name="Prize", breaks=c(0, 5000, 10000, 15000), labels=c("$0", "$5,000", "$10,000", "$15,000")) +
  theme(axis.title.x=element_text(face="bold", size=16, vjust=0),
        axis.title.y=element_text(face="bold", size=16, vjust=2),
        axis.line.y=element_line(color="black"),
        axis.line.x=element_line(color="black"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        legend.position=c(0.6, 0.75),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


year_graph_data <- as_tibble(cbind(year=c(rep("2011", 8), rep("2012", 8), rep("2013", 8), rep("2014", 8), rep("2015", 8), rep("2016", 8),
                                          rep("2017", 8), rep("2018", 8), rep("2019", 8), rep("2020", 8), rep("2021", 8)),
                                   rank=c(rep(seq(1,8),11)), prizes=t(bind_cols(by_year[1,2:9], by_year[2,2:9], by_year[3,2:9], by_year[4,2:9],
                                                                                by_year[5,2:9], by_year[6,2:9], by_year[7,2:9], by_year[8,2:9],
                                                                                by_year[9,2:9], by_year[10,2:9], by_year[11,2:9]))))

year_graph_data <- transform(year_graph_data, prizes=as.numeric(V3))

year_graph <- year_graph_data %>% ggplot(aes(x=rank, y=prizes, fill=year)) +
  geom_bar(stat="identity", color="black", width=0.7, position=position_dodge(width=0.8)) +
  scale_fill_jco() +
#  scale_fill_manual(values=c('#d7d7d7', '#999999')) +
  scale_x_discrete(name="Level") +
  scale_y_continuous(name="Prize", breaks=c(0, 5000, 10000, 15000), labels=c("$0", "$5,000", "$10,000", "$15,000")) +
  theme(axis.title.x=element_text(face="bold", size=16, vjust=0),
        axis.title.y=element_text(face="bold", size=16, vjust=2),
        axis.line.y=element_line(color="black"),
        axis.line.x=element_line(color="black"),
        axis.text.y=element_text(colour="black"),
        axis.text.x=element_text(colour="black"),
        legend.position=c(0.6, 0.75),
        legend.direction="horizontal",
        legend.spacing.x=unit(1.85, 'cm'),
        legend.text=element_text(margin=margin(l=-45)),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


by_decils <- db %>%
  group_by(cut(db$tot_p, 10)) %>%
  summarize(mean_stage1 = mean(s1_winp), mean_stage2 = mean(s2_winp), mean_stage3 = mean(s3_winp), mean_stage4 = mean(s4_winp),
            mean_stage5 = mean(s5_winp), mean_stage6 = mean(s6_winp), mean_stage7 = mean(s7_winp), mean_stage8 = mean(s8_winp))

decils_graph_data <- as_tibble(cbind(year=c(rep("D1", 8), rep("D2", 8), rep("D3", 8), rep("D4", 8), rep("D5", 8), rep("D6", 8),
                                          rep("D7", 8), rep("D8", 8), rep("D9", 8), rep("D10", 8)),
                                   rank=c(rep(seq(1,8),11)), prizes=t(bind_cols(by_decils[1,2:9], by_decils[2,2:9], by_decils[3,2:9], by_decils[4,2:9],
                                                                                by_decils[5,2:9], by_decils[6,2:9], by_decils[7,2:9], by_decils[8,2:9],
                                                                                by_decils[9,2:9], by_decils[10,2:9]))))



ggarrange(ggarrange(team_graph, type_graph, ncol = 2),
          year_graph, nrow = 2)

ggarrange(year_graph, ncol = 2,
          ggarrange(team_graph, type_graph, ncol = 1))


for (i in 1:6) {db[,9+i] <- (db[,1+i] - db[,2+i] > db[,2+i] - db[,3+i])}

final_cond <- db$V15 & db$V16 & db$V17 & db$V18 & db$V19 & db$V20
trues <- ifelse(final_cond, 1, 0)
sum(trues)/length(trues)

######## OLS model ########

for (i in 1:7) {db[,14+i] <- db[,6+i] - db[,7+i]}
colnames(db)[21] <- 'V21'
for (i in 1:nrow(db)) {db$n_stages[i] <- which.min(db[7:14][i,][db[7:14][i,]>0])}

for (i in 1:8){
  final_cond <- db[db$n_stages == i,]$V15 & db[db$n_stages == i,]$V16 & db[db$n_stages == i,]$V17 & db[db$n_stages == i,]$V18 & db[db$n_stages == i,]$V19 & db[db$n_stages == i,]$V20
  trues <- ifelse(final_cond, 1, 0)
  truesfinal <- ifelse(db[db$n_stages == i,"s1_winp"]-1.5*db[db$n_stages == i,"s2_winp"]>0, 1, 0)
  print(paste0(i, ": ", sum(trues)/length(trues), ", final: ", sum(truesfinal)/length(truesfinal)))}

for (i in 1:8){
  rowos <- nrow(db[db$n_stages == i,])
  print(paste0(i, ": ", rowos, ", ", rowos/315.62))
}


db_long <- gather(db, key = "level", value = "spread", 15:21)
db_long <- db_long[!(db_long$spread == 0),]

db_long$type <- factor(db_long$type)
db_long$game <- factor(db_long$game)
db_long$level <- factor(db_long$level)
db_long$team <- factor(db_long$team)
db_long$n_stages <- factor(db_long$n_stages)
stat.desc(db_long)

lm_basic <- lm(log(spread) ~ level + type + team + relevel(n_stages, ref="1") + game + year, data = transform(db_long, level = relevel(level, ref="V21")))
summary(lm_basic)
lm_basic_rob <- coeftest(lm_basic, vcov.=vcovHC(lm_basic, "HC1"))
lm_basic_rob
confint(lm_basic_rob)


glht_mod <- glht(model = lm_basic, linfct = c("-levelV16 <= 0",
                                              "levelV16 - levelV17 <= 0",
                                              "levelV17 - levelV18 <= 0",
                                              "levelV18 - levelV19 <= 0",
                                              "levelV19 - levelV20 <= 0",
                                              "levelV19 - levelV21 <= 0"))
summary(glht_mod)
############
qr_basic_small <- rq(log(spread) ~ level + type + team + relevel(n_stages, ref="1") + game + year, data = transform(db_long, level = relevel(level, ref="V21")), tau = 1:9/10)
cores <- parallel::detectCores()-1

sum <- summary(qr_basic_small, se = "ker", parallel = "multicore", ncpus = cores)
coefs <- lapply(sum, `[`, 3)

for (i in c(1, 3, 5, 7, 9)) {print(coefs[[i]]$coefficients[2:7, c(1,2,4)])}

################################################################################################################################################################################################
################################################################################################################################################################################################
#################################################################################### HETEROGENEITY MODEL #######################################################################################
################################################################################################################################################################################################
################################################################################################################################################################################################

################################################################ DATA ################################################################
d <- read.csv("C:/Users/Honzík/OneDrive - Vysoká škola ekonomická v Praze/Connection/Plocha/Doktorát/tournament theory/data_reworked2.csv", header=TRUE, stringsAsFactors=FALSE, sep=";")
d <- d[,c(1:5, 7, 10:11, 13:14, 16:17, 19:20, 22:23, 25:28, 33:34)]
d <- d[!(is.na(d$Team.1...rank)|is.na(d$Team.2...rank)),]

stat.desc(d[!duplicated(d$Tournament), ]$Prizepool*d[!duplicated(d$Tournament), ]$Adjustment.index)
stat.desc(d[!duplicated(d$Tournament), ]$N)
d <- d[,-c(1)]

colnames(d) <- c("N", "tier", "stage", "type", "year", "t1_rank", "t2_rank", "t1_m1_pts", "t2_m1_pts", "t1_m2_pts", "t2_m2_pts", "t1_m3_pts", "t2_m3_pts", "t1_m4_pts", "t2_m4_pts", "t1_m5_pts", "t2_m5_pts", "format", "ppool", "p_spread", "adj")
d$stage <- as.factor(d$stage)
d$type <- as.factor(d$type)
d$tier <- as.factor(d$tier)
d$format <- as.factor(d$format)
d$year <- as.factor(d$year)

d$klas1 <- 8 - log2(d$t1_rank)
d$klas2 <- 8 - log2(d$t2_rank)
d$het <- abs(d$klas1 - d$klas2)
d$rel_str <- d$klas1 - d$klas2

d$ppool <- d$ppool/1000000*d$adj
d$p_spread <- d$p_spread/1000000*d$adj
################################################################################################################################################################################################
d$t1_m1_w <- ifelse(d$t1_m1_pts > d$t2_m1_pts, 1, 0)
d$t1_m2_w <- ifelse(d$t1_m2_pts > d$t2_m2_pts, 1, 0)
d$t1_m3_w <- ifelse(d$t1_m3_pts > d$t2_m3_pts, 1, 0)
d$t1_m4_w <- ifelse(d$t1_m4_pts > d$t2_m4_pts, 1, 0)
d$t1_m5_w <- ifelse(d$t1_m5_pts > d$t2_m5_pts, 1, 0)

d$t2_m1_w <- ifelse(d$t1_m1_w == 0, 1, 0)
d$t2_m2_w <- ifelse(d$t1_m2_w == 0, 1, 0)
d$t2_m3_w <- ifelse(d$t1_m3_w == 0, 1, 0)
d$t2_m4_w <- ifelse(d$t1_m4_w == 0, 1, 0)
d$t2_m5_w <- ifelse(d$t1_m5_w == 0, 1, 0)
d[is.na(d)] <- 0

d$t1_pts <- d$t1_m1_pts + d$t1_m2_pts + d$t1_m3_pts + d$t1_m4_pts + d$t1_m5_pts
d$t2_pts <- d$t2_m1_pts + d$t2_m2_pts + d$t2_m3_pts + d$t2_m4_pts + d$t2_m5_pts
d$tot_pts <- d$t1_pts + d$t2_pts

d$t1_score <- d$t1_m1_w + d$t1_m2_w + d$t1_m3_w + d$t1_m4_w + d$t1_m5_w
d$t2_score <- d$t2_m1_w + d$t2_m2_w + d$t2_m3_w + d$t2_m4_w + d$t2_m5_w

d$t1_w <- ifelse(d$t1_score > d$t2_score, 1, 0)
d$t2_w <- 1-d$t1_w
d$t1_fav <- ifelse(d$t1_rank < d$t2_rank, 1, 0)
d$t2_fav <- 1-d$t1_fav
d$t1_fav_w <- ifelse(d$t1_fav + d$t1_w == 2, 1, 0)
d$t2_fav_w <- ifelse(d$t2_fav + d$t2_w == 2, 1, 0)
d$fav_w <- d$t1_fav_w + d$t2_fav_w
d$und_w <- 1-d$fav_w

d$w_pts <- ifelse(d$t1_w, d$t1_pts, d$t2_pts)
d$l_pts <- ifelse(d$t1_w, d$t2_pts, d$t1_pts)
d$fav_pts <- ifelse(d$t1_fav, d$t1_pts, d$t2_pts)
d$und_pts <- ifelse(d$t1_fav, d$t2_pts, d$t1_pts)

d$delta <- d$fav_pts - d$und_pts

d$n_maps <- d$t1_score + d$t2_score
d$t1_wperm <- d$t1_pts / d$n_maps
d$t2_wperm <- d$t2_pts / d$n_maps

d$und_wperm <- d$und_pts / d$n_maps
d$fav_wperm <- d$fav_pts / d$n_maps

d$l_wperm <- d$l_pts / d$n_maps
d$w_wperm <- d$w_pts / d$n_maps

d$t1_wr <- d$t1_pts / d$tot_pts
d$t2_wr <- d$t2_pts / d$tot_pts

d2 <- data.frame(d)
d2$t1_rank <- d$t2_rank
d2$t2_rank <- d$t1_rank
d2$t2_pts <- d$t1_pts
d2$t1_pts <- d$t2_pts
d2$t1_score <- d$t2_score
d2$t2_score <- d$t1_score
d2$t1_w <- d$t2_w
d2$t2_w <- d$t1_w
d2$t1_fav <- d$t2_fav
d2$t2_fav <- d$t1_fav
d2$t1_fav_w <- d$t2_fav_w
d2$t2_fav_w <- d$t1_fav_w
d2$t1_pts_perm <- d$t2_pts_wperm
d2$t2_pts_wperm <- d$t1_pts_wperm
d2$rel_str <- -d$rel_str
d2$t1_wr <- d2$t2_wr
d2$t2_wr <- d2$t1_wr
dd <- bind_rows(d, d2)

d$rankdiff = abs(d$t1_rank-d$t2_rank)
summary(d)
stat.desc(d)
stat.desc(dd)

ols_permap <- lm(t1_wperm ~ p_spread + t1_fav*rel_str + tier + relevel(type,2) + format + relevel(year,7) + N, data = dd)
ols_permap2 <- lm(t1_wperm ~ ppool + t1_fav*rel_str + relevel(stage,2) + tier + relevel(type,2) + format + relevel(year,7) + N, data = dd)
ols_pergame <- lm(t1_pts ~ p_spread + t1_fav*rel_str + tier + relevel(type,2) + format + relevel(year,7) + N, data = dd)
ols_pergame2 <- lm(t1_pts ~ ppool + t1_fav*rel_str + relevel(stage,2) + tier + relevel(type,2) + format + relevel(year,7) + N, data = dd)
ols_wr <- lm(t1_wr ~ p_spread + t1_fav*rel_str + tier + relevel(type,2) + format + relevel(year,7) + N, data = d)
ols_wr2 <- lm(t1_wr ~ ppool + t1_fav*rel_str + relevel(stage,2) + tier + relevel(type,2) + format + relevel(year,7) + N, data = d)

BIC(ols_permap, ols_permap2, ols_pergame, ols_pergame2, ols_wr, ols_wr2)

summary(ols_permap)
summary(ols_permap2)
summary(ols_pergame)
summary(ols_pergame2)
summary(ols_wr)
summary(ols_wr2)

coeftest(ols_permap, vcov = vcovHC)[c(2:4, 15, 14),c(1, 2, 4)]
coeftest(ols_permap2, vcov = vcovHC)[c(2:6, 17, 16),c(1, 2, 4)]
coeftest(ols_pergame, vcov = vcovHC)[c(2:4, 15, 14),c(1, 2, 4)]
coeftest(ols_pergame2, vcov = vcovHC)[c(2:6, 17, 16),c(1, 2, 4)]
coeftest(ols_wr, vcov = vcovHC)[c(2:4, 15, 14),c(1, 2, 4)]
coeftest(ols_wr2, vcov = vcovHC)[c(2:6, 17, 16),c(1, 2, 4)]
