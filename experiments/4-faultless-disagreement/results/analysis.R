library(ggplot2)
library(reshape2)
library(lme4)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master")

d = read.table("faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("faultless-disagreement-2-subject_information.tsv",sep="\t",header=T)
head(s)
d$language = s$language[match(d$workerid,s$workerid)]
summary(d)

model.7 = lm(response~predicate, data=d)
model.11 = lm(response~predicate+noun:predicate, data=d)
anova(model.7,model.11)
summary(model.7)
summary(model.11)

pairwise.t.test(d$response, d$class, p.adj = "bonf")

aggregate(response~class,data=d,mean)

d$class <- factor(d$class,levels=c("quality","size","age","texture","color","shape","material"))

table(d$class,d$nounclass)

## class plot
d_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))
# save data for aggregate plot
#write.csv(d_s,"~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/faultless.csv")

class_plot <- ggplot(d_s, aes(x=reorder(class,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("\nadjective class") +
  ylim(0,1) +
  theme_bw()
class_plot
ggsave("../results/class_plot.pdf",height=3)


## predicate plot by class
p_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","predicate"))
p_s$predicate <- factor(p_s$predicate,ordered=is.ordered(p_s$predicate))
pred_plot <- ggplot(p_s, aes(x=reorder(predicate,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
pred_plot
ggsave("../results/pred_plot.pdf",height=5)

## predicate plot by class and noun
n_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","predicate","nounclass"))
n_s$predicate <- factor(n_s$predicate,ordered=is.ordered(n_s$predicate))
noun_plot <- ggplot(n_s, aes(x=reorder(predicate,-response,mean),y=response,fill=nounclass)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
noun_plot
ggsave("../results/noun_plot.pdf",height=5)
