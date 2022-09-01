library(ggplot2)
library(psych)
library(car)
library(multcomp)

setwd('~/Desktop/R_analysis_lab/data')
df <- read.csv("osmo-shock_600.csv")

df$Accession <- factor(df$Accession)
attach(df)
table(Accession)

TukeyHSD(aov(Chlorophyll~Accession))

res <- aov(Chlorophyll ~ Accession, data = df)

tuk <- glht(res, linfct = mcp(Accession = "Tukey"))

mltv <- cld(tuk, decreasing = F)

abc <- mltv[["mcletters"]][["Letters"]]


g <- ggplot(data=df, aes(x=Accession, y=Chlorophyll)) +
  stat_summary(fun="mean", geom="bar",position=position_dodge(width = 0.9),width=0.8,colour="black",alpha=0.6) +
  scale_fill_manual(values=c("#f5f5f5","#000000")) +
  theme_classic() +
  stat_summary(fun="mean",fun.min=function(x)mean(x)-(sd(x)/sqrt(3)),fun.max=function(x)mean(x)+(sd(x)/sqrt(3)),position=position_dodge(width = 0.9)) +
  geom_point(position = position_dodge(width = 0.9)) +
  stat_summary(geom = 'text', label =abc, fun = max, vjust = -5) +
  theme(axis.title.x = element_text(size=9, family = "Arial"), 
        axis.title.y = element_text(size=9, family = "Arial"), 
        axis.text.x = element_text(size=9, colour = 1, family = "Arial"), 
        axis.text.y = element_text(size =9, colour = 1, family = "Arial"))

print(g)
