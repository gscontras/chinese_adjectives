library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
source("../results/helpers.R")

setwd("~/git/chinese_adjectives/experiments/4-faultless-disagreement/Submiterator-master")

num_round_dirs = 12
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    '../Submiterator-master/round', i, '/faultless-disagreement-chinese.csv', sep='')) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df1, select=c("workerid","firstutterance","predicate","class","noun","nounclass","slide_number","response","language","age","assess","education","lived","proficiency","homeLanguage","yearsLived","outsideLanguage","gender"))

# only lived both before and after 8 in Chinese-speaking country
d = d[d$lived=="both8",]
# only 5plus years in Chinese-speaking country
d = d[d$yearsLived=="5plus",]
# only native Chinese speakers
d = d[d$language!="Cantonese"&d$language!="English"&d$language!="上海话"&d$language!="广东话"&d$language!="粤语",]


length(unique(d$workerid)) # n=35 (108)

summary(d)


#write.csv(d,"../results/chinese-faultless.csv")

adj_agr = aggregate(response~predicate*class,FUN=mean,data=d)
d_agr = aggregate(response~predicate,FUN=mean,data=d)
adj_agr

class_agr = aggregate(response~class,FUN=mean,data=d)
class_agr

class_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))

ggplot(data=class_s,aes(x=reorder(class,-response,mean),y=response))+
  geom_bar(stat="identity",fill="lightgray",color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.1))+
  geom_hline(yintercept=0.5,linetype="dashed") + 
  xlab("\nadjective class")+
  ylab("subjectivity score\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#ggsave("../results/class_subjectivity.pdf",height=3)


#### comparison with faultless disgareement

o = read.csv("../../3-order-preference-both/results/chinese-naturalness-duplicated.csv",header=T)

o_agr = aggregate(correctresponse~predicate,data=o,FUN=mean)

o_agr$subjectivity = d_agr$response[match(o_agr$predicate,d_agr$predicate)]

gof(o_agr$correctresponse,o_agr$subjectivity)
# r = 0.69, r2 = 0.48
results <- boot(data=o_agr, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") 
# 95%   ( 0.2666,  0.6647 )   

ggplot(o_agr, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  #geom_smooth()+
  stat_smooth(method="lm",color="black")+
  #geom_text(aes(label=predicate),size=2.5,vjust=1.5)+
  ylab("preferred distance from noun\n")+
  xlab("\nsubjectivity score")+
  ylim(0,1)+
  #xlim(0,1)+
  theme_bw()
#ggsave("../results/naturalness-subjectivity.pdf",height=3,width=4)
#ggsave("../results/LSA-naturalness-subjectivity.png",height=2.8,width=3)
#ggsave("../results/chinese-scatter.pdf",height=2.75,width=3.15)
