library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)

setwd("~/git/chinese_adjectives/experiments/2-order-preference-expanded/Submiterator-master")

num_round_dirs = 15
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    '../Submiterator-master/round', i, '/chinese-order3.csv', sep='')) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df1, select=c("workerid","nounEnglish","gender","nounclass","slide_number", "predicate1English", "predicate2English", "class1","class2","response","sense","condition","language","age","assess","education","lived","proficiency","homeLanguage","yearsLived","outsideLanguage"))

# only lived both before and after 8 in Chinese-speaking country
d = d[d$lived=="both8",]
# only 5plus years in Chinese-speaking country
d = d[d$yearsLived=="5plus",]
# only native Chinese speakers
d = d[d$language!="Cantonese"&d$language!=""&d$language!="中文（Chinese, both Manderin and Cantonese）",]


length(unique(d$workerid)) # n=20

summary(d)

d$predicate1 = d$predicate1English
d$predicate2 = d$predicate2English
d$noun = d$nounEnglish

#write.csv(d,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/order-preference-trimmed.csv")

#####
## sense analysis
#####

table(d$sense,d$condition)

s = d[d$sense=="no",]
s = subset(s, select=c("predicate1English","predicate2English","nounEnglish","condition"))
s = s[!is.na(s$predicate1English),]
write.csv(s,"check-for-weird.csv")

#####
## duplicate observations by first predicate
#####

library(tidyr)
o <- d
o$makesSense = "yes"
o[!is.na(o$sense),]$makesSense = "no"
o = o[o$makesSense!="no",]
o$rightpredicate1 = o$predicate2
o$rightpredicate2 = o$predicate1
o$rightresponse = 1-o$response
agr = o %>% 
        select(predicate1,rightpredicate1,response,rightresponse,workerid,noun,nounclass,class1,class2,makesSense,condition) %>%
        gather(predicateposition,predicate,predicate1:rightpredicate1,-workerid,-noun,-nounclass,-class1,-class2,-makesSense,-condition)
agr$correctresponse = agr$response
agr[agr$predicateposition == "rightpredicate1",]$correctresponse = agr[agr$predicateposition == "rightpredicate1",]$rightresponse
agr$correctclass = agr$class1
agr[agr$predicateposition == "rightpredicate1",]$correctclass = agr[agr$predicateposition == "rightpredicate1",]$class2
head(agr[agr$predicateposition == "rightpredicate1",])
agr$response = NULL
agr$rightresponse = NULL
agr$class1 = NULL
agr$class2 = NULL
nrow(agr) #1092 total, without sense
#write.csv(agr,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv")

adj_agr = aggregate(correctresponse~predicate*correctclass,FUN=mean,data=agr)
adj_agr

class_agr = aggregate(correctresponse~correctclass,FUN=mean,data=agr)
class_agr

class_s = bootsSummary(data=agr, measurevar="correctresponse", groupvars=c("correctclass"))

ggplot(data=class_s,aes(x=reorder(correctclass,-correctresponse,mean),y=correctresponse))+
  geom_bar(stat="identity",fill="lightgray",color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1))+
  geom_hline(yintercept=0.5,linetype="dashed") + 
  xlab("\nadjective class")+
  ylab("preferred\ndistance from noun\n")+
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
