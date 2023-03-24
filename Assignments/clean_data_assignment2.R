rm(list = ls())

setwd('~/Downloads')

library(data.table)

#### 2012 LINES
load('./LINES/DS0001/36680-0001-Data.rda')
lines <- data.table(da36680.0001)

# partisanship measure
vars <- c('PTYID_RPTYID', 'THERMPRE_THDPTY', 'THERMPRE_THRPTY',
         'CSES_PTYCLOST', 'CSES_DPTYLIKE', 'CSES_RPTYLIKE')
lines[ , (vars) := lapply(.SD, as.numeric), .SDcols = vars]
colnames(lines) <- tolower(names(lines))

lines[(ptyid_rptyid==1 | ptyid_rptyid>=4), partyid:=0.5]
lines[(ptyid_rptyid==2), partyid:=1]
lines[(ptyid_rptyid==3), partyid:=0]

lines[cses_ptyclost==1 & is.na(partyid), partyid:=1]
lines[cses_ptyclost==2 & is.na(partyid), partyid:=0]
lines[cses_ptyclost==3 & is.na(partyid), partyid:=0.5]

vars <- tolower(c('THERMPRE_THDPTY', 'THERMPRE_THRPTY',
          'CSES_DPTYLIKE', 'CSES_RPTYLIKE'))
range01 <- function(x){(x-min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))}
lines[ , (vars) := lapply(.SD, range01), .SDcols = vars]
lines[!is.na(cses_dptylike), partisanship:=(partyid+cses_dptylike-cses_rptylike)]
lines[is.na(cses_dptylike), partisanship:=(partyid+thermpre_thdpty-thermpre_thrpty)]
lines[,partisanship:=range01(partisanship)]
lines[,democrat:=ifelse(partisanship >= quantile(partisanship, .25, na.rm = TRUE),1,0)]

# latino identity measure (importance + linked fate)
lines[as.numeric(ident_hispid)==1, hispid:=5]
lines[as.numeric(ident_hispid)==2, hispid:=4]
lines[as.numeric(ident_hispid)==3, hispid:=3]
lines[as.numeric(ident_hispid)==4, hispid:=2]
lines[as.numeric(ident_hispid)==5, hispid:=1]

lines[as.numeric(link_lnkhisp)==2, hisplink:=1]
lines[(as.numeric(link_lnkhisp)==1 & as.numeric(link_lnkhispamt)==3), hisplink:=2]
lines[(as.numeric(link_lnkhisp)==1 & as.numeric(link_lnkhispamt)==2), hisplink:=3]
lines[(as.numeric(link_lnkhisp)==1 & as.numeric(link_lnkhispamt)==1), hisplink:=4]

lines[,hispattach:=range01(hispid+hisplink)]

# perceived discrimination against Hispanics
lines[as.numeric(discrim_dischisp)==5, hispdisc:=1]
lines[as.numeric(discrim_dischisp)==4, hispdisc:=2]
lines[as.numeric(discrim_dischisp)==3, hispdisc:=3]
lines[as.numeric(discrim_dischisp)==2, hispdisc:=4]
lines[as.numeric(discrim_dischisp)==1, hispdisc:=5]
lines[, hispdisc:=range01(hispdisc)]
lines[, hispdisc_high:=as.factor(ifelse(hispdisc>=0.75,1,0))]

# political engagement (ask question about the mean---low levels of participation)
lines[as.numeric(mobilpo_rmob)==1, rmob:=1]
lines[as.numeric(mobilpo_rmob)==2, rmob:=0]
lines[as.numeric(mobilpo_rrally)==1, rrally:=1]
lines[as.numeric(mobilpo_rrally)==2, rrally:=0]
lines[as.numeric(mobilpo_rbuttn)==1, rbuttn:=1]
lines[as.numeric(mobilpo_rbuttn)==2, rbuttn:=0]
lines[as.numeric(mobilpo_ctbcand)==1, ctbcand:=1]
lines[as.numeric(mobilpo_ctbcand)==2, ctbcand:=0]
lines[as.numeric(mobilpo_rcampwk)==1, rcampwk:=1]
lines[as.numeric(mobilpo_rcampwk)==2, rcampwk:=0]
lines[,polengagement:=range01(rmob+rrally+rbuttn+ctbcand+rcampwk)]

# ideology (lib-conservative)
lines[,ideology:=as.numeric(libcpre_lcint)]
lines[ideology==8, ideology:=4]
lines[(is.na(ideology) & as.numeric(libcpre_lcchoose)==3), ideology:=4]
lines[(is.na(ideology) & as.numeric(libcpre_lcchoose)==2), ideology:=5]
lines[(is.na(ideology) & as.numeric(libcpre_lcchoose)==1), ideology:=3]

# immigration policy (higher values is more liberal, inclusionary)
lines[as.numeric(immigpo_immlevel)==1, immigpolicy:=5]
lines[as.numeric(immigpo_immlevel)==2, immigpolicy:=4]
lines[as.numeric(immigpo_immlevel)==3, immigpolicy:=3]
lines[as.numeric(immigpo_immlevel)==4, immigpolicy:=2]
lines[as.numeric(immigpo_immlevel)==5, immigpolicy:=1]
lines[,immigpolicy:=range01(immigpolicy)]

# abortion (higher values more liberal, pro-choice)
lines[,abortion:=as.numeric(abortpre_abselfstd_post)]
lines[is.na(abortion), abortion:=as.numeric(abortpre_abselfstd)]
lines[abortion==5, abortion:=NA]

# US patriotism
lines[as.numeric(patriot_flag)==1, us_flag:=5]
lines[as.numeric(patriot_flag)==2, us_flag:=4]
lines[as.numeric(patriot_flag)==3, us_flag:=3]
lines[as.numeric(patriot_flag)==4, us_flag:=2]
lines[as.numeric(patriot_flag)==5, us_flag:=1]
lines[,us_love:=as.numeric(patriot_love)]
lines[, uspatriot:=range01(us_flag+us_love)]

# Country of origin patriotism
lines[as.numeric(patriot_flag_coo)==1, origin_flag:=5]
lines[as.numeric(patriot_flag_coo)==2, origin_flag:=4]
lines[as.numeric(patriot_flag_coo)==3, origin_flag:=3]
lines[as.numeric(patriot_flag_coo)==4, origin_flag:=2]
lines[as.numeric(patriot_flag_coo)==5, origin_flag:=1]
lines[,origin_love:=as.numeric(patriot_love_coo)]
lines[, originpatriot:=range01(origin_flag+origin_love)]

# demographics: citizen, country/region of origin
lines[as.numeric(naturalize)==1, citizen:=1]
lines[as.numeric(naturalize)==2, citizen:=0]
lines[(is.na(citizen) & as.numeric(naturalize_post)==1), citizen:=1]
lines[(is.na(citizen) & as.numeric(naturalize_post)==2), citizen:=0]
lines[(is.na(citizen) & as.numeric(naturalize_post2)==1), citizen:=1]
lines[(is.na(citizen) & as.numeric(naturalize_post2)==2), citizen:=0]
lines[,citizen:=as.factor(citizen)]

lines[as.numeric(dem_hispborn)==12, origin:='mexican']
lines[as.numeric(dem_hispborn)==7, origin:='dominican']
lines[as.numeric(dem_hispborn)==6, origin:='cuban']
lines[(as.numeric(dem_hispborn)==9 | as.numeric(dem_hispborn)==10 | as.numeric(dem_hispborn)==11 | as.numeric(dem_hispborn)==13),
      origin:='central american']
lines[(as.numeric(dem_hispborn)==1 | as.numeric(dem_hispborn)==2 | as.numeric(dem_hispborn)==3 | as.numeric(dem_hispborn)==4 |
         as.numeric(dem_hispborn)==5 | as.numeric(dem_hispborn)==8 | as.numeric(dem_hispborn)==16 | as.numeric(dem_hispborn)==19 |
         as.numeric(dem_hispborn)==20),
      origin:='south american']

fwrite(lines[,.(origin, citizen, polengagement, uspatriot, partisanship, hispattach, hispdisc_high)], file='./lines.csv')

##### analysis ######
## Section 1
# bar plot with polengagement by country of origin
ggplot(data = lines[!is.na(origin), .(avg_engagement=mean(polengagement, na.rm = TRUE)),by=origin],
       aes(x = reorder(origin, -avg_engagement), y=avg_engagement)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = round(avg_engagement,2)), vjust=-0.3, size=3.5) +
  xlab(NULL) +
  ylab('average political engagement')
ggsave('./pol_engagement_barplot.png', width=6, height=4.85)

# bar plot with patriotism by country of origin
ggplot(data = lines[!is.na(origin), .(avg_patriiotism=mean(uspatriot, na.rm = TRUE)),by=origin],
       aes(x = reorder(origin, -avg_patriiotism), y=avg_patriiotism)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = round(avg_patriiotism,2)), vjust=-0.3, size=3.5) +
  xlab(NULL) +
  ylab('average US patriiotism')
ggsave('./patriotism_barplot.png', width=6, height=4.85)


# density plot patriotism by naturalization
ggplot(lines[!is.na(citizen)], aes(x = uspatriot, ..scaled..))+
  geom_density(aes(fill = as.factor(citizen), color = as.factor(citizen)), alpha = 0.5) +
  ylab('density') + xlab('patriotism toward US') +
  scale_fill_discrete(name = "citizen", labels = c("No", "Yes")) +
  scale_color_discrete(name = "citizen", labels = c("No", "Yes"))
ggsave('./patriotism_density.png', width=6, height=4.85)


### Section 2
# identity convergence
ggplot(lines, aes(x = hispattach, y = partisanship)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab('Latino attachment') +
  ylab('Democratic attachment')
ggsave('./identity_correlation.png', width=6, height=4.85)

summary(lm(partisanship~hispattach, data=lines))

ggplot(lines[!is.na(hispdisc_high)], aes(x = hispattach, y = partisanship, color=hispdisc_high)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab('Latino attachment') +
  ylab('Democratic attachment') +
  scale_color_discrete(name = "discrimination", labels = c("low", "high"))
ggsave('./identity_correlation_discrimination.png', width=6, height=4.85)


### no evidence of identity convergence theory
summary(lm(partisanship~hispattach*hispdisc, data=lines))
summary(lm(partisanship~hispattach, data=lines, subset = hispdisc_high==1))
summary(lm(partisanship~hispattach, data=lines, subset = hispdisc_high==0))

# control for ideology
summary(lm(partisanship~hispattach + ideology, data=lines, subset = hispdisc_high==1))
summary(lm(partisanship~hispattach + ideology, data=lines, subset = hispdisc_high==0))

# control for citizen
summary(lm(partisanship~hispattach + citizen, data=lines, subset = hispdisc_high==1))
summary(lm(partisanship~hispattach + citizen, data=lines, subset = hispdisc_high==0))

### dichotomizing partisanship (as in the paper)
summary(lm(democrat~hispattach*hispdisc, data=lines))
