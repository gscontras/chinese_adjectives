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

d = subset(df1, select=c("workerid","firstutterance","predicate","class","noun","nounclass","slide_number","response","language","age","assess","education","lived","proficiency","homeLanguage","yearsLived","outsideLanguage"))

# only lived both before and after 8 in Chinese-speaking country
d = d[d$lived=="both8",]
# only 5plus years in Chinese-speaking country
d = d[d$yearsLived=="5plus",]
# only native Chinese speakers
d = d[d$language!="Cantonese"&d$language!="English"&d$language!="上海话"&d$language!="广东话"&d$language!="粤语",]


length(unique(d$workerid)) # n=25

summary(d)


#write.csv(d,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/order-preference-trimmed.csv")

adj_agr = aggregate(response~predicate*class,FUN=mean,data=d)
adj_agr

class_agr = aggregate(response~class,FUN=mean,data=d)
class_agr

class_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))

ggplot(data=class_s,aes(x=reorder(class,-response,mean),y=response))+
  geom_bar(stat="identity",fill="lightgray",color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.1))+
  geom_hline(yintercept=0.5,linetype="dashed") + 
  xlab("\nadjective class")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#ggsave("../results/class_distance.pdf",height=3)
#ggsave("../results/LSA_class_distance.png",height=2,width=4.3)


class_s = bootsSummary(data=agr, measurevar="correctresponse", groupvars=c("correctclass","condition"))

ggplot(data=class_s,aes(x=reorder(correctclass,-correctresponse,mean),y=correctresponse,fill=condition))+
  geom_bar(stat="identity",color="black",position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),position=position_dodge(0.9))+
  geom_hline(yintercept=0.5,linetype="dashed") + 
  xlab("\nadjective class")+
  ylab("preferred\ndistance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#ggsave("../results/class_distance.pdf",height=3)
