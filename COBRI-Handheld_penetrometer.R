############################################
############################################
##
## 631.1 COBRI Handheld penetrometer
##
## This project uses R 4.1.2 
## with snapshot date 2021-11-02
##
############################################
############################################

# Setup

{
  # -------------------------------------------
  snapshot_date = "2021-11-02"
  options("repos" = paste0("https://mran.revolutionanalytics.com/snapshot/", snapshot_date))
  # -------------------------------------------
  
  # -------------------------------------------
  # sink options
  options(width = 150)
  # rJava memory option
  options(java.parameters = "-Xmx8000m")
  # -------------------------------------------
  
  # R packages
  # -------------------------------------------
  Rpackages_version = c("readxl_1.3.1", "ggplot2_3.3.5", "dplyr_1.0.7",
                        "gridExtra_2.3", "lme4_1.1-27.1", "lmerTest_3.1-3",
                        "emmeans_1.7.0", "pwr_1.3-0", "xtable_1.8-4",
                        "multcomp_1.4-17", "multcompView_0.1-8", "writexl_1.4.0",
                        "pbkrtest_0.5.1"
                        )
  path_Rpackages = "C:/R packages_412"
  # -------------------------------------------
  
  # version check and load packages
  # -------------------------------------------
  # R version check
  if(sessionInfo()$R.version$version.string != "R version 4.1.2 (2021-11-01)") stop("R.version must be 4.1.2 (2021-11-01)")
  
  # install packages
  Rpack = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[1])
  Rpack_version = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[2])
  if(!all(Rpack %in% list.files(path_Rpackages))){
    loadRpackages <- Rpack[!Rpack %in% list.files(path_Rpackages)]
    for(i in loadRpackages) install.packages(i, lib = path_Rpackages, repos = options("repos"), dependencies = T)
  }
  
  # load packages
  for(i in Rpack) eval(parse(text = paste0("library(", i, ", lib.loc = '", path_Rpackages, "')")))
}

############################################

# IMPORT DATA
Raw632 <- data.frame(read_excel("COBRI_HH_pen_Rawdata.xlsx", sheet="Rawdata", col_names=T,skip=0))
Reliab632 <- data.frame(read_excel("COBRI_HH_pen_Reliability.xlsx", sheet="rawdata", col_names=T,skip=0))

## REARRANGE WITH ONE COLUMN PER METRIC
{
  Variety <- c(Raw632$variety_nr, Raw632$variety_nr, Raw632$variety_nr)
  Variety <- factor(Variety, levels=c(1,2,3))
  Country <- c(Raw632$country, Raw632$country, Raw632$country)
  Time <- rep(1:3, each=162)
  lab_pen_all <- c(Raw632$lab_pen, Raw632$lab_pen, Raw632$lab_pen)
  lab_fir_all <- c(Raw632$lab_fir, Raw632$lab_fir, Raw632$lab_fir)
  Treatm <- c(Raw632$treatm_f, Raw632$treatm_f, Raw632$treatm_f)
  Block <- c(Raw632$block, Raw632$block, Raw632$block)
  pen_ave_all <- unlist(Raw632[c("pen_ave_1","pen_ave_2", "pen_ave_3")])
  Raw632_all <- data.frame(Country, Block, Variety, Time, Treatm, pen_ave_all, lab_pen_all, lab_fir_all)
  Raw632_all <- Raw632_all[!is.na(Raw632_all$pen_ave_all), ]
  rm(Variety, Country, Time, lab_pen_all, lab_fir_all, pen_ave_all, Treatm, Block)
}
##################################
#
# BAR GRAPHS for COMPARISON OF MEANS
#
##################################

## LAB (HAVE USED GEOMETRIC MEANS GIVEN DATA IS PERFECTLY BALANCED WITH ONE X)
dat_l3 <- Raw632_all[Raw632_all$Time==3,]
dat_l_pen <- dat_l3 %>%
  group_by(Variety, Country) %>%
    summarise(Mean = mean(lab_pen_all), SD = sd(lab_pen_all), SE = sd(lab_pen_all)/sqrt(n()))
dat_l_pen$Measure <- factor("Puncture Resistance", levels=c("Puncture Resistance","Tissue Firmness"))

dat_l_fir <- dat_l3 %>%
  group_by(Variety, Country) %>%
    summarise(Mean = mean(lab_fir_all), SD = sd(lab_fir_all), SE = sd(lab_fir_all)/sqrt(n()))
dat_l_fir$Measure <- factor("Tissue Firmness", levels=c("Puncture Resistance","Tissue Firmness"))

dat_l <- rbind(dat_l_pen, dat_l_fir)

### SIG DIFFERENCES BETWEEN LAB
country <- c("BE","BE","NL","NL","SE","SE")
para <- rep(c("lab_pen_all", "lab_fir_all"),3)

{
  mean_lab_ct <- NULL
  mean_lab_y <- NULL
  mean_lab_cont <- NULL
  mean_lab_p <- NULL

  for (i in 1:(length(country))){
    dat_ct <- Raw632_all[which(Raw632_all$Country ==country[i]),]
    eval(parse(text = paste0("lab_pen_lmer <- lmer(formula = ",para[i]," ~ Variety + (1|Block), data=dat_ct[which(dat_ct$Time ==1),])")))
    lab_pen_em <- emmeans(lab_pen_lmer, ~ Variety)
    lab_pen_em_sum <- summary(lab_pen_em)
    lab_pen_em_ph <- summary(contrast(lab_pen_em, method ='pairwise'))
    
    ct_i <- c(rep(country[i], nrow(lab_pen_em_sum)))
    y_i <- c(rep(para[i], nrow(lab_pen_em_sum)))
    cont_i <- as.character(lab_pen_em_ph$contrast)
    p_i <- lab_pen_em_ph$p.value
    
    mean_lab_ct <- c(mean_lab_ct, ct_i)
    mean_lab_y <- c(mean_lab_y, y_i)
    mean_lab_cont <- c(mean_lab_cont, cont_i)
    mean_lab_p <- c(mean_lab_p, p_i)
  }
  
  mean_lab <- data.frame(mean_lab_ct,mean_lab_y,mean_lab_cont,mean_lab_p)
  colnames(mean_lab) <- c("Country","Measure","Contrast","p-value")
  mean_lab
}

dat_l$Sig <- c("a","a","a","b","b","b","b","b","b","a","a","a","b","b","b","c","c","c")

p_l <- ggplot(data = dat_l, aes(x=Variety, y=Mean, fill=Variety)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  facet_grid(Country ~ Measure) +
  coord_cartesian(ylim=c(4,7.5)) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, position=position_dodge(.9)) +
  labs(x = "Laboratory", y = "Pressure (MPa)") +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(size = 11)) +
  theme(strip.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  geom_text(aes(label=Sig), y=7.5) + 
  scale_fill_manual(values=c("grey55", "#E69F00", "#56B4E9"))


#p_l

## FIELD
### Emmeans is only calculated for SE in period 1 owing to the perfectly balanced nature of all other times x places.
dat_f1_se <- Raw632_all[which(Raw632_all$Time==1 & Raw632_all$Country =="SE"),]
f1_pen_se.lmer <- lmer(formula = pen_ave_all ~ Variety + Treatm + (1|Block), data=dat_f1_se)
dat_f1_pen_se <- emmeans(f1_pen_se.lmer, ~ Variety, at = c("1","2","3"))
dat_f1_pen_se
summary(f1_pen_se.lmer)

### Var 1 = 5.124343, Var 2 = 5.626544, Var 3 = 5.989719
### These figures are used in the publication.

dat_f1 <- Raw632_all[Raw632_all$Time==1,]
dat_f1 <- dat_f1 %>%
  group_by(Variety, Country) %>%
    summarise(Mean = mean(pen_ave_all), SD = sd(pen_ave_all), SE = sd(pen_ave_all)/sqrt(n()))
dat_f1$Measure <- factor("-2 Months", levels=c("-2 Months","-1 Month","-0 Months"))

### Replace means for SE using this data

dat_f1$Mean[which(dat_f1$Country == "SE" & dat_f1$Variety == "1")] <- 5.124343
dat_f1$Mean[which(dat_f1$Country == "SE" & dat_f1$Variety == "2")] <- 5.626544
dat_f1$Mean[which(dat_f1$Country == "SE" & dat_f1$Variety == "3")] <- 5.989719

dat_f2 <- Raw632_all[Raw632_all$Time==2,]
dat_f2 <- dat_f2 %>%
  group_by(Variety, Country) %>%
    summarise(Mean = mean(pen_ave_all), SD = sd(pen_ave_all), SE = sd(pen_ave_all)/sqrt(n()))
dat_f2$Measure <- factor("-1 Month", levels=c("-2 Months","-1 Month","-0 Months"))

dat_f3 <- Raw632_all[Raw632_all$Time==3,]
dat_f3 <- dat_f3 %>%
  group_by(Variety, Country) %>%
    summarise(Mean = mean(pen_ave_all), SD = sd(pen_ave_all), SE = sd(pen_ave_all)/sqrt(n()))
dat_f3$Measure <- factor("-0 Months", levels=c("-2 Months","-1 Month","-0 Months"))

dat_f <- rbind(dat_f1, dat_f2, dat_f3)

### SIG DIFFERENCES BETWEEN FIELD
country <- c("BE","BE","BE","NL","NL","NL","SE","SE","SE")
tme <- rep(c(1,2,3),3)

{
  mean_field_ct <- NULL
  mean_field_tme <- NULL
  mean_field_cont <- NULL
  mean_field_p <- NULL
  
  for (i in 1:(length(country))){
    dat_ct <- Raw632_all[which(Raw632_all$Country ==country[i]),]
    eval(parse(text = paste0("field_pen_lmer <- lmer(formula = pen_ave_all ~ Variety + (1|Block), data=dat_ct[which(dat_ct$Time ==",tme[i],"),])")))
    field_pen_em <- emmeans(field_pen_lmer, ~ Variety)
    field_pen_em_sum <- summary(field_pen_em)
    field_pen_em_ph <- summary(contrast(field_pen_em, method ='pairwise'))
    
    ct_i <- c(rep(country[i], nrow(field_pen_em_sum)))
    tme_i <- c(rep(tme[i], nrow(field_pen_em_sum)))
    cont_i <- as.character(field_pen_em_ph$contrast)
    p_i <- field_pen_em_ph$p.value
    
    mean_field_ct <- c(mean_field_ct, ct_i)
    mean_field_tme <- c(mean_field_tme, tme_i)
    mean_field_cont <- c(mean_field_cont, cont_i)
    mean_field_p <- c(mean_field_p, p_i)
  }
  
  mean_field <- data.frame(mean_field_ct,mean_field_tme,mean_field_cont,mean_field_p)
  colnames(mean_field) <- c("Country","Occasion","Contrast","p-value")
  mean_field
}

dat_f$Sig <- c("a","a","a","b","b","b","c","c","b","a","a","a","b","b","b","c","c","c","a","a","a","b","b","b","c","b","c")

p_f <- ggplot(data = dat_f, aes(x=Variety, y=Mean, fill=Variety)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  facet_grid(Country ~ Measure)+
  coord_cartesian(ylim=c(4,7.5)) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, position=position_dodge(.9)) +
  labs(x = "Handheld", y = "Pressure (MPa)") +
  theme(axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 11)) +
  theme(strip.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  geom_text(aes(label=Sig), y=7.5) + 
  scale_fill_manual(values=c("grey55", "#E69F00", "#56B4E9"))

#p_f

# Export in ratio 1025 x 680
grid.arrange(p_l, p_f, ncol = 2, widths = 2:3)
#dev.off()

##########################
#
#  TABLE OF CORRELATIONS
#
##########################

country_corr  <- rep(c("BE","BE","BE","NL","NL","NL","SE","SE","SE"),2)
occ_corr      <- rep(c(1:3),6)
para_corr_lab <- c(rep("lab_pen_all",9), rep("lab_fir_all",9))

cor <- matrix(c(NA,NA,NA,NA), nrow = 1)
colnames(cor) <- c("corr_tab","V1","V1","V1")

for(j in 1:6){
  corr_tab <- matrix(c(NA,NA),2)
  for(i in (j*3-2):(j*3)){
    Raw632_corr <- Raw632_all[(Raw632_all$Country==country_corr[i]) & (Raw632_all$Time == occ_corr[i]),]
    eval(parse(text = paste0("corr <- cor.test(Raw632_corr$",para_corr_lab[i],", Raw632_corr$pen_ave_all)")))
    
    cor_pen <- as.data.frame(matrix(c(corr$estimate, corr$p.value)))
    corr_tab <- cbind(corr_tab, cor_pen)
  }
  cor <- rbind(cor,corr_tab)
}

{
  cor <- cor[-1,-1]
  
  colnames(cor) <- c("-2 Months", "-1 Month", "-0 Months")
  cor_name_cntry  <- rep(c("BE","BE","NL","NL","SE","SE"),2)
  cor_name_what <- c(rep(c("Estimate_pen_", "p-value_pen_"),3),rep(c("Estimate_fir_", "p-value_fir_"),3))
  cor_name <- paste0(cor_name_what, cor_name_cntry)
  rownames(cor) <- cor_name
  
  cor 
}

cor_resist_cor <- c(paste0("r = ",format(round(cor[1,1],4), nsmall=2)),paste0("r = ",format(round(cor[3,1],4), nsmall=2)),paste0("r = ",format(round(cor[5,1],4), nsmall=2)),
                    paste0("r = ",format(round(cor[1,2],4), nsmall=2)),paste0("r = ",format(round(cor[3,2],4), nsmall=2)),paste0("r = ",format(round(cor[5,2],4), nsmall=2)),
                    paste0("r = ",format(round(cor[1,3],4), nsmall=2)),paste0("r = ",format(round(cor[3,3],4), nsmall=2)),paste0("r = ",format(round(cor[5,3],4), nsmall=2)))
cor_resist_p   <- c(paste0("p = ",formatC(cor[2,1], format = "e", digits = 2)),paste0("p = ",formatC(cor[4,1], format = "e", digits = 2)),paste0("p = ",formatC(cor[6,1], format = "e", digits = 2)),
                    paste0("p = ",formatC(cor[2,2], format = "e", digits = 2)),paste0("p = ",formatC(cor[4,2], format = "e", digits = 2)),paste0("p = ",formatC(cor[6,2], format = "e", digits = 2)),
                    paste0("p = ",formatC(cor[2,3], format = "e", digits = 2)),paste0("p = ",formatC(cor[4,3], format = "e", digits = 2)),paste0("p = ",formatC(cor[6,3], format = "e", digits = 2)))

cor_firm_cor   <- c(paste0("r = ",format(round(cor[7,1],4), nsmall=2)),paste0("r = ",format(round(cor[9,1],4), nsmall=2)),paste0("r = ",format(round(cor[11,1],4), nsmall=2)),
                    paste0("r = ",format(round(cor[7,2],4), nsmall=2)),paste0("r = ",format(round(cor[9,2],4), nsmall=2)),paste0("r = ",format(round(cor[11,2],4), nsmall=2)),
                    paste0("r = ",format(round(cor[7,3],4), nsmall=2)),paste0("r = ",format(round(cor[9,3],4), nsmall=2)),paste0("r = ",format(round(cor[11,3],4), nsmall=2)))
cor_firm_p     <- c(paste0("p = ",formatC(cor[8,1], format = "e", digits = 2)),paste0("p = ",formatC(cor[10,1], format = "e", digits = 2)),paste0("p = ",formatC(cor[12,1], format = "e", digits = 2)),
                    paste0("p = ",formatC(cor[8,2], format = "e", digits = 2)),paste0("p = ",formatC(cor[10,2], format = "e", digits = 2)),paste0("p = ",formatC(cor[12,2], format = "e", digits = 2)),
                    paste0("p = ",formatC(cor[8,3], format = "e", digits = 2)),paste0("p = ",formatC(cor[10,3], format = "e", digits = 2)),paste0("p = ",formatC(cor[12,3], format = "e", digits = 2)))

#latex_cor <- xtable((cor),  align = c("c","c","c","c"), display = c("s","f","f","f"), digits = 4)

#print(latex_cor)

##################################
#
# Scatters of standardised values
#
##################################

# Mean of lab_pen and lab_fir, by country

# reorganise data to be like the above, with
# columns Country, Time_period, Lab_pen.sd, Lab_firm.sd, pen_ave.sd. 
# Then add country~Time period at facet_grid.
# add at 45 deg line.

dat_sd <- Raw632
dat_sd <- group_by(dat_sd, country) %>% mutate(lab_pen.sd = as.numeric(scale(lab_pen)))
dat_sd <- group_by(dat_sd, country) %>% mutate(lab_fir.sd = as.numeric(scale(lab_fir)))
dat_sd <- group_by(dat_sd, country) %>% mutate(pen_ave_1.sd = as.numeric(scale(pen_ave_1)))
dat_sd <- group_by(dat_sd, country) %>% mutate(pen_ave_2.sd = as.numeric(scale(pen_ave_2)))
dat_sd <- group_by(dat_sd, country) %>% mutate(pen_ave_3.sd = as.numeric(scale(pen_ave_3)))
#dat_s_BE <- dat_s[which(dat_s$country=="BE"),]

{
  Variety <- c(Raw632$variety_nr, Raw632$variety_nr, Raw632$variety_nr)
  Variety <- factor(Variety, levels=c(1,2,3))
  Country <- c(Raw632$country, Raw632$country, Raw632$country)
  Time <- factor(rep(c("-2 Months","-1 Month", "-0 Months"), each=162), levels = c("-2 Months","-1 Month", "-0 Months"))
  lab_pen.sd <- c(dat_sd$lab_pen.sd, dat_sd$lab_pen.sd, dat_sd$lab_pen.sd)
  lab_fir.sd <- c(dat_sd$lab_fir.sd, dat_sd$lab_fir.sd, dat_sd$lab_fir.sd)
  pen_ave.sd <- unlist(dat_sd[c("pen_ave_1.sd","pen_ave_2.sd", "pen_ave_3.sd")])
  Raw632_scatter <- data.frame(Country, Variety, Time, pen_ave.sd, lab_pen.sd, lab_fir.sd)
  Raw632_scatter <- Raw632_scatter[!is.na(Raw632_scatter$pen_ave.sd), ]
  rm(Variety, Country, Time, lab_pen.sd, lab_fir.sd, pen_ave.sd)
}

Time = factor(c(rep("-2 Months",times=3),rep("-1 Month", times=3),rep("-0 Months",times=3)))
Country = rep(c("BE","NL","SE"), times = 3)
Variety = factor(rep(1,9), levels =c(1,2,3))
ann_data_resist_cor <- data.frame(cor_resist_cor, Time, Country, Variety)
ann_data_resist_p <- data.frame(cor_resist_p, Time, Country, Variety)
ann_data_firm_cor <- data.frame(cor_firm_cor, Time, Country, Variety)
ann_data_firm_p <- data.frame(cor_firm_p, Time, Country, Variety)
rm(Time, Country, Variety)

# Export in ratio 1025 x 680

ggplot(Raw632_scatter, aes(x=lab_pen.sd, y=pen_ave.sd, color=Variety, shape = Variety)) +
  geom_point(aes(size=Variety)) +
  #geom_rug() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Puncture Resistance - Standardised") +
  ylab("Handheld Penetration - Standardised") +
  scale_shape_manual(values=c(18, 16, 17)) + 
  scale_size_manual(values=c(4,3,3)) +
  scale_color_manual(values=c("grey55", "#E69F00", "#56B4E9")) +
  theme(strip.text.x = element_text(size = 11)) +
  theme(strip.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  geom_text(data = ann_data_resist_cor, mapping = aes(x= 1.5, y = -1.5, label = cor_resist_cor),color="black", size = 4) +
  geom_text(data = ann_data_resist_p, mapping = aes(x= 1.5, y = -2.1, label = cor_resist_p),color="black", size =3.5) +
  facet_grid(Country~Time)

#dev.off()

# Export in ratio 1025 x 680

ggplot(Raw632_scatter, aes(x=lab_fir.sd, y=pen_ave.sd, color=Variety, shape = Variety)) +
  geom_point(aes(size = Variety)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Tissue Firmness - Standardised") +
  ylab("Handheld Penetration - Standardised") +
  scale_shape_manual(values=c(18, 16, 17)) + 
  scale_size_manual(values=c(4,3,3)) +
  scale_color_manual(values=c("grey55", "#E69F00", "#56B4E9")) +
  theme(strip.text.x = element_text(size = 11)) +
  theme(strip.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  geom_text(data = ann_data_firm_cor, mapping = aes(x= 1.5, y = -1.5, label = cor_firm_cor),color="black", size =4) +
  geom_text(data = ann_data_firm_p, mapping = aes(x= 1.5, y = -2.2, label = cor_firm_p),color="black", size =3.5) +
  facet_grid(Country~Time)
  # + geom_smooth(method=lm, aes(fill=lab_pen.sd))

#dev.off()




##########################
#
#  Power Test
#
##########################


dat_pow <- Raw632[,c(1:15,40:50)]
#set.seed(123)
#random <- sample(1:10, replace = FALSE)
random <- c("_03","_08","_04","_07","_06","_01","_10","_09","_02","_05")
names(dat_pow)[names(dat_pow)=="pen_ave_3"] <- "pen_ave_all"

for (i in 1:(length(random)-1)){
  rm <- grepl(random[i], colnames(dat_pow))
  dat_pow[rm] <- as.numeric("")
  #dat_pow$pen_ave_9 <- rowMeans(dat_pow[,16:25])
  eval(parse(text = paste0("dat_pow$pen_ave_",10-i, " <- rowMeans(dat_pow[,16:25], na.rm = T)")))
}
names(dat_pow)[names(dat_pow)=="pen_ave_all"] <- "pen_ave_10"
names(dat_pow)[names(dat_pow)=="variety_nr"] <- "Variety"
dat_pow$Variety <- as.factor(dat_pow$Variety)

dat_t.test_m <- dat_pow[dat_pow$country == "NL" & (dat_pow$Variety == 2 | dat_pow$Variety == 3),]
t_stat_10 <- t.test(pen_ave_10 ~ Variety, data = dat_t.test_m)$p.value
t_stat_09 <- t.test(pen_ave_9 ~ Variety, data = dat_t.test_m)$p.value
t_stat_08 <- t.test(pen_ave_8 ~ Variety, data = dat_t.test_m)$p.value
t_stat_07 <- t.test(pen_ave_7 ~ Variety, data = dat_t.test_m)$p.value
t_stat_06 <- t.test(pen_ave_6 ~ Variety, data = dat_t.test_m)$p.value
t_stat_05 <- t.test(pen_ave_5 ~ Variety, data = dat_t.test_m)$p.value
t_stat_04 <- t.test(pen_ave_4 ~ Variety, data = dat_t.test_m)$p.value
t_stat_03 <- t.test(pen_ave_3 ~ Variety, data = dat_t.test_m)$p.value
t_stat_02 <- t.test(pen_ave_2 ~ Variety, data = dat_t.test_m)$p.value
t_stat_01 <- t.test(pen_ave_1 ~ Variety, data = dat_t.test_m)$p.value

dat_pow_m <- dat_pow %>%
  group_by(Variety, country) %>%
    summarise(Mean = mean(pen_ave_10), SD = sd(pen_ave_10), SE = sd(pen_ave_10)/sqrt(n()))
dat_pow_m$obs = 10

for (i in 1:(length(random)-1)){
  eval(parse(text = paste0("dat_pow_mi <- dat_pow %>%
  group_by(Variety, country) %>%
    summarise(Mean = mean(pen_ave_",10-i,"), SD = sd(pen_ave_",10-i,"), SE = sd(pen_ave_",10-i,")/sqrt(n()))
dat_pow_mi$obs = 10-",i,"
dat_pow_m <- rbind(dat_pow_m, dat_pow_mi)")))
}

# Line graph

ggplot(data=dat_pow_m, aes(x=obs, y=Mean, group=Variety, color=Variety)) +
  geom_line(size =1)+
  geom_point(aes(shape=Variety)) + 
  scale_x_continuous(breaks=seq(0,10,2)) +
  labs(x = "Number of samples per plot", y = "Pressure (MPa)") +
  scale_shape_manual(values=c(18, 16, 17)) + 
  scale_size_manual(values=c(6,6,6)) +
  scale_color_manual(values=c("grey55", "#E69F00", "#56B4E9")) +
  theme(strip.text.x = element_text(size = 11)) +
  theme(strip.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  geom_errorbar(aes(ymin=Mean-1.96*SE, ymax=Mean+1.96*SE), width=.2) +
  facet_grid(~country)

# Export as png with name "Power_line" in ratio 1025 x 485

## ACTUAL POWER TEST

sd_se3_1 <- sd(Raw632_all$pen_ave_all[Raw632_all$Country=="SE"& Raw632_all$Time ==3 & Raw632_all$Variety == 1])
sd_se3_2 <- sd(Raw632_all$pen_ave_all[Raw632_all$Country=="SE"& Raw632_all$Time ==3 & Raw632_all$Variety == 2])
sd_se3_3 <- sd(Raw632_all$pen_ave_all[Raw632_all$Country=="SE"& Raw632_all$Time ==3 & Raw632_all$Variety == 3])
sd_se3 <- mean(c(sd_se3_1, sd_se3_2, sd_se3_3))
sd_main <- 0.2836 # value of sd_se3
sd_SE2020 <- 0.1826
sd_SE2019 <- 0.1389

### Set test parameters - can only have one SD, but as many Ns and Diffs as you want
ns_to_test   <- c(4,30)
diffs_to_test <- c(0.01, 0.04, 0.1, 0.3, 0.6, 0.97)
sd_to_test   <- c(sd_main, sd_SE2019, sd_SE2020)

### Run test parameters - output should be a table with 4 columns: Known, calc_main, calc_2019, calc_2020
#p_t_fun <-function(d=NULL, n=NULL){
#  pwr.t.test(d=d, n=n, power=0.9, sig.level=0.05,type="one.sample",alternative="two.sided")
#}

p_t_tab <- data.frame(c(diffs_to_test,ns_to_test))

for (i in 1:length(sd_to_test)){
  out_i <- NA
  d <-diffs_to_test/sd_to_test[i]
  for (j in 1:length(diffs_to_test)){
    out_j <- pwr.t.test(d=d[j], power=0.9, sig.level=0.05,type="one.sample",alternative="two.sided")
    out_i <- c(out_i, out_j$n)
  }
  for (k in 1:length(ns_to_test)){
    out_k <- pwr.t.test(n=ns_to_test[k], power=0.9, sig.level=0.05,type="one.sample",alternative="two.sided")
    out_k <- out_k$d * sd_to_test[i]
    out_i <- c(out_i, out_k)
  }
  p_t_tab <- data.frame(p_t_tab, out_i[2:length(out_i)])
}

names(p_t_tab) <- c("Given","Main", "SE2019", "SE2020")
  
p_t_tab

write_xlsx(p_t_tab, "power_test.xlsx")

  
############################
#
#  COMPARISON OF OPERATORS
#
############################

# IMPORT DATA
Raw632_op <- data.frame(read_excel("632_Rawdata.xlsx", sheet="Rawdata_op", col_names=T,skip=0))

## Combine times to one vector
{
  Variety <- c(Raw632_op$variety_nr, Raw632_op$variety_nr)
  Variety <- factor(Variety, levels=c(1,2,3))
  Time <- rep(2:3, each=108)
  lab_pen_all <- c(Raw632_op$lab_pen, Raw632_op$lab_pen)
  lab_fir_all <- c(Raw632_op$lab_fir, Raw632_op$lab_fir)
  Treatm <- c(Raw632_op$treatm_f, Raw632_op$treatm_f)
  Block <- c(Raw632_op$block, Raw632_op$block)
  Operator <- c(Raw632_op$operator, Raw632_op$operator)
  Operator <- replace(Operator, Operator == "SWE_1", "SE 1")
  Operator <- replace(Operator, Operator == "SWE_2", "SE 2")
  pen_ave_all <- unlist(Raw632_op[c("pen_ave_2", "pen_ave_3")])
  Raw632_op_all <- data.frame(Operator, Block, Variety, Time, Treatm, pen_ave_all, lab_pen_all, lab_fir_all)
  Raw632_op_all <- Raw632_op_all[!is.na(Raw632_op_all$pen_ave_all), ]
  rm(Variety, Operator, Time, lab_pen_all, lab_fir_all, pen_ave_all, Treatm, Block)
}
  
## BAR GRAPHS USING ARITHMETIC MEANS

dat_op_f2 <- Raw632_op_all[Raw632_op_all$Time==2,]
dat_op_f2 <- dat_op_f2 %>%
  group_by(Variety, Operator) %>%
    summarise(Mean = mean(pen_ave_all), SD = sd(pen_ave_all), SE = sd(pen_ave_all)/sqrt(n()))
dat_op_f2$Measure <- factor("-1 Month", levels=c("-1 Month","-0 Months"))

dat_op_f3 <- Raw632_op_all[Raw632_op_all$Time==3,]
dat_op_f3 <- dat_op_f3 %>%
  group_by(Variety, Operator) %>%
    summarise(Mean = mean(pen_ave_all), SD = sd(pen_ave_all), SE = sd(pen_ave_all)/sqrt(n()))
dat_op_f3$Measure <- factor("-0 Months", levels=c("-1 Month","-0 Months"))

dat_op <- rbind(dat_op_f2, dat_op_f3)

p_values <- c("p = 0.0027", NA, "p = 0.6780", NA, "p = 0.9998", NA, "p = 0.00010", NA, "p = 0.0567", NA, "p = 0.9735", NA)

p_op <- ggplot(data = dat_op, aes(x=Variety, y=Mean, fill=Operator)) +
  geom_bar(stat="identity", color="black", position="dodge") + 
  facet_grid( ~ Measure)+
  coord_cartesian(ylim=c(4,7.5)) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, position=position_dodge(.9)) +
  labs(x = "Variety", y = "Pressure (MPa)") +
  theme(axis.title.y = element_blank()) +
  scale_fill_manual(values=c("#004B87", "#FFCD00")) +
  theme(strip.text.x = element_text(size = 11)) +
  theme(strip.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  geom_text(aes(label=p_values), y=7.5, size = 3.5)

p_op

# Export as png with name "Operators" in ratio 1025 x 400

## BAR GRAPHS USING EMMEANS

dat_op_2 <- Raw632_op_all[Raw632_op_all$Time==2,]
lmer2 <- lmer(pen_ave_all ~ Variety + Operator + Variety*Operator + (1|Block), data = dat_op_2)
summary(lmer2)
anova(lmer2)
em2 <- emmeans(lmer2, ~ Variety + Operator)
em_g2 <- as.data.frame(summary(em2))
em_g2
em_ph2 <- contrast(em2, method ='pairwise')
em_ph2

dat_op_3 <- Raw632_op_all[Raw632_op_all$Time==3,]
lmer3 <- lmer(pen_ave_all ~ Variety + Operator + Variety*Operator + (1|Block), data = dat_op_3)
summary(lmer3)
anova(lmer3)
em3 <- emmeans(lmer3, ~ Variety + Operator)
em_g3 <- as.data.frame(summary(em3))
em_g3
em_ph3 <- contrast(em3, method ='pairwise')
em_ph3

em_g <- rbind(em_g2, em_g3)

em_g$Measure <- c("-1 Month","-1 Month","-1 Month","-1 Month","-1 Month","-1 Month","-0 Months","-0 Months","-0 Months","-0 Months","-0 Months","-0 Months") 
em_g$Measure <- factor(em_g$Measure, levels = c('-1 Month', '-0 Months'))

# This graph using EMMEANS gives the same averages as the arithmetic means, but I couldn't be bothered pulling out SD (which I already had with arithmetic means)
# So, use the one above, not this one.
#p_op_em <- ggplot(data = em_g, aes(x=Variety, y=emmean, fill=Operator)) +
#  geom_bar(stat="identity", color="black", position="dodge") + 
#  facet_grid( ~ Measure)+
#  coord_cartesian(ylim=c(4,7.5)) +
#  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2, position=position_dodge(.9)) +
#  labs(x = "Variety", y = "Pressure (MPa)") +
#  theme(axis.title.y = element_blank()) 

#p_op_em

###########
# Re-doing all previous tests

## SIG DIFFERENCES BETWEEN FIELD
dat_ct <- Raw632_op_all[which(Raw632_op_all$Operator == "SE 2"),]
#dat_ct <- dat_ct[which(dat_ct$Time == 2),]
dat_ct <- dat_ct[which(dat_ct$Time == 3),]
dat_ct$Block <- as.factor(dat_ct$Block)
field_pen_lmer <- lmer(formula = pen_ave_all ~ Variety + (1|Block), data=dat_ct)
field_pen_em <- emmeans(field_pen_lmer, ~ Variety)
field_pen_em_sum <- summary(field_pen_em)
field_pen_em_ph <- summary(contrast(field_pen_em, method ='pairwise'))
field_pen_em_ph

## Correlations
cor_op_SE2 <- c(cor.test(dat_op_2$lab_pen_all, dat_op_2$pen_ave_all)[["estimate"]][["cor"]],
                cor.test(dat_op_2$lab_pen_all, dat_op_2$pen_ave_all)[["p.value"]],
                cor.test(dat_op_2$lab_fir_all, dat_op_2$pen_ave_all)[["estimate"]][["cor"]],
                cor.test(dat_op_2$lab_fir_all, dat_op_2$pen_ave_all)[["p.value"]],
                cor.test(dat_op_3$lab_pen_all, dat_op_3$pen_ave_all)[["estimate"]][["cor"]],
                cor.test(dat_op_3$lab_pen_all, dat_op_3$pen_ave_all)[["p.value"]],
                cor.test(dat_op_3$lab_fir_all, dat_op_3$pen_ave_all)[["estimate"]][["cor"]],
                cor.test(dat_op_3$lab_fir_all, dat_op_3$pen_ave_all)[["p.value"]])
cor_op_SE2

## Power
sd_se3_1 <- sd(Raw632_op_all$pen_ave_all[Raw632_op_all$Operator=="SE 2"& Raw632_op_all$Time ==3 & Raw632_op_all$Variety == 1])
sd_se3_2 <- sd(Raw632_op_all$pen_ave_all[Raw632_op_all$Operator=="SE 2"& Raw632_op_all$Time ==3 & Raw632_op_all$Variety == 2])
sd_se3_3 <- sd(Raw632_op_all$pen_ave_all[Raw632_op_all$Operator=="SE 2"& Raw632_op_all$Time ==3 & Raw632_op_all$Variety == 3])
sd_se3 <- mean(c(sd_se3_1, sd_se3_2, sd_se3_3))
                         
                         
############################
#
#  RELIABILITY
#
############################

Reliab632$ID <- as.factor(Reliab632$ID)
#Reliab632$pen_avg_a3 <- Reliab632$pen_avg_a2
#Reliab632$pen_avg_a2 <- Reliab632$pen_avg_a3
Reliab632$pen_avg_a2 <- ifelse(Reliab632$year == 2019, Reliab632$pen_avg_a2*1.4159, Reliab632$pen_avg_a2)
Reliab632$pen_avg_a2 <- ifelse(Reliab632$year == 2020, Reliab632$pen_avg_a2*1.4159, Reliab632$pen_avg_a2)
Reliab632$pen_avg_a2 <- ifelse(Reliab632$year == 2021, Reliab632$pen_avg_a2*0.90618, Reliab632$pen_avg_a2)
  
# 2019                         
m_val_19 <- lmer(pen_avg_a2 ~ ID + (1|miniblock), data = Reliab632[which(Reliab632$year == 2019),])
summary(m_val_19)
anova(m_val_19)
em_val_19 <- emmeans(m_val_19, ~ ID)
em_g_val_19 <- as.data.frame(summary(em_val_19))
em_g_val_19[order(em_g_val_19$emmean),]
#em_ph_val_19 <- contrast(em_val_19, method ='pairwise')
#em_ph_val_19
em_letters_val_19 <- cld(em_val_19, Letters = c(letters, rep(LETTERS, each = 100)), sort = T, reversed = T, alpha = 0.05) 
em_letters_val_19 
em_letters_val_19_tab <- em_letters_val_19[,c(1,2,7)]
colnames(em_letters_val_19_tab) <- c("ID", "LSMean 2019", "group 2019")
                                                              
# 2020                         
m_val_20 <- lmer(pen_avg_a2 ~ ID + (1|block), data = Reliab632[which(Reliab632$year == 2020),])
summary(m_val_20)
anova(m_val_20)
em_val_20 <- emmeans(m_val_20, ~ ID)
em_g_val_20 <- as.data.frame(summary(em_val_20))
em_g_val_20[order(em_g_val_20$emmean),]
#em_ph_val_20 <- contrast(em_val_20, method ='pairwise')
#em_ph_val_20
em_letters_val_20 <- cld(em_val_20, Letters = c(letters, rep(LETTERS, each = 100)), sort = T, reversed = T, alpha = 0.05) 
em_letters_val_20 
em_letters_val_20_tab <- em_letters_val_20[,c(1,2,7)]
colnames(em_letters_val_20_tab) <- c("ID", "LSMean 2020", "group 2020")
                                                              
# 2021                         
m_val_21 <- lmer(pen_avg_a2 ~ ID + (1|block), data = Reliab632[which(Reliab632$year == 2021),])
summary(m_val_21)
anova(m_val_21)
em_val_21 <- emmeans(m_val_21, ~ ID)
em_g_val_21 <- as.data.frame(summary(em_val_21))
em_g_val_21[order(em_g_val_21$emmean),]
#em_ph_val_21 <- contrast(em_val_21, method ='pairwise')
#em_ph_val_21
em_letters_val_21 <- cld(em_val_21, Letters = c(letters, rep(LETTERS, each = 100)), sort = T, reversed = T, alpha = 0.05) 
em_letters_val_21
em_letters_val_21_tab <- em_letters_val_21[,c(1,2,7)]
colnames(em_letters_val_21_tab) <- c("ID", "LSMean 2021", "group 2021")
                                                              
# MERGE RESULTS FOR OUTPUT (Good if I can get the letters in here too)
Reliab_tab <- merge(em_letters_val_19_tab, em_letters_val_20_tab, by = "ID", all = T)
Reliab_tab <- merge(Reliab_tab, em_letters_val_21_tab, by = "ID", all = T)
Reliab_tab
                                                              
## neaten up this table for the correlations and for presentation.
Reliab_tab <- arrange(Reliab_tab, desc(`LSMean 2021`))
Reliab_tab <- arrange(Reliab_tab, desc(`LSMean 2019`))
Reliab_tab <- arrange(Reliab_tab, desc(`LSMean 2020`))
Reliab_tab

write_xlsx(Reliab_tab, "Reliab.xlsx")

## graph

Reliab_tab_g <- Reliab_tab[which(Reliab_tab$ID %in% c(1,2,3,5,6,7,8,11,12,13,15,18,19,21,23,24,25,27,31,32))
                           ,c("ID","LSMean 2019", "LSMean 2020", "LSMean 2021")]

Reliab_tab_g19 <- Reliab_tab_g[,c("ID","LSMean 2020", "LSMean 2019")]
Reliab_tab_g19$Pair <- as.factor("2020:2019")
Reliab_tab_g21 <- Reliab_tab_g[,c("ID","LSMean 2020", "LSMean 2021")]
Reliab_tab_g21$Pair <- as.factor("2020:2021")

colnames(Reliab_tab_g19) <- c("ID", "LSMean2020", "LSMean1921", "Pair")
colnames(Reliab_tab_g21) <- c("ID", "LSMean2020", "LSMean1921", "Pair")

Reliab_tab_g <- rbind(Reliab_tab_g19, Reliab_tab_g21)
                                                              
ggplot(Reliab_tab_g, aes(x=LSMean2020, y=LSMean1921, col = Pair, shape = Pair)) +
  geom_point(aes(size=Pair)) +
  geom_smooth(method = lm, se = F) +
  xlab("Handheld Penetration (MPa) - 2020") +
  ylab("Handheld Penetration (MPa) - 2019 and 2021") +
  annotate("text", x = 6.625, y = 6.4, label = "r = 0.9449") +
  annotate("text", x = 6.625, y = 6, label = "r = 0.8566") +
  scale_shape_manual(values=c(17, 16)) +
  scale_size_manual(values=c(3.5,3.5)) +
  scale_color_manual(values=c("#00a15f", "#e6bb3c")) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) 
  #geom_text(data = "r = 0.9449", mapping = aes(x= 1.5, y = -1.5, label = cor_resist_cor),color="black", size = 4) +
  #geom_text(data = "r = 0.8566", mapping = aes(x= 1.5, y = -2.1, label = cor_resist_p),color="black", size =3.5) "#004B87", "#FFCD00"

# CORRELATIONS 
Reliab_cor_1920 <- cor.test(Reliab_tab$`LSMean 2019`, Reliab_tab$`LSMean 2020`,
                    method = "pearson")
Reliab_cor_1921 <- cor.test(Reliab_tab$`LSMean 2019`, Reliab_tab$`LSMean 2021`,
                    method = "pearson")
Reliab_cor_2021 <- cor.test(Reliab_tab$`LSMean 2020`, Reliab_tab$`LSMean 2021`,
                    method = "pearson")                                                              
v1920 <- c(Reliab_cor_1920$estimate, Reliab_cor_1920$p.value)
v1921 <- c(Reliab_cor_1921$estimate, Reliab_cor_1921$p.value)
v2021 <- c(Reliab_cor_2021$estimate, Reliab_cor_2021$p.value)
Reliab_cors <- data.frame(matrix(c(v1920, v1921, v2021),nrow = 2))
colnames(Reliab_cors) <- c("19:20", "19:21", "20:21")
rownames(Reliab_cors) <- c("Estimate", "p-value")

write_xlsx(Reliab_cors, "Reliab_cors.xlsx")
