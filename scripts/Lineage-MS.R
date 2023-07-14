library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(RColorBrewer)
library(cowplot)
coul<-brewer.pal(12, "Set3") 
coul<-colorRampPalette(coul)(23)
theme_set(theme_cowplot())

##linegae per month
LinMonth<- read.delim("data/bar_lineages/Lin.txt", header=TRUE,sep = "\t")
LinMonth$Date<-as.Date.character(LinMonth$date)
LinMonth$month <-format(as.Date(LinMonth$date), "%Y-%m")

ggplot(LinMonth, mapping= aes(fill=lin, x=month)) + 
  geom_bar(colour = "black", position="fill") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        axis.title=element_text(size=12,face="bold"),
        axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_fill_manual(values = c("#8DD3C7", "#C5E8BC", "#F6F9B4", "#DDDBC6", "#C6BACE", "#DB9CA5", "#E78B86", "#BD98A1",
                               "#9DAAB9", "#BEB29A", "#E2B871", "#D7C865", "#C5D577", "#D6D5A6", "#EED0D4", "#EAD2DE",
                               "#D9CED6", "#C9ACCA", "#BE8EBF", "#C3B4C0", "#CDE5C0", "#E5EB99", "#FFED6F")) +
  xlab("Month (2020-2021)") +
  ylab("Proportion of lineages") +
  labs(fill = "Lineage")

