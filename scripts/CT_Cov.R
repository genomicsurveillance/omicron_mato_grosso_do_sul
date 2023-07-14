

library(ggplot2) #need to install 

ct_cov = read.csv("data/ct/CT-Cov-Lineage.csv", header=TRUE, row.names=1,sep = ",")
colnames(ct_cov)

ggplot(ct_cov, aes(x = CT,y = Coverage, col=lin)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.x = element_text(size = 8, hjust=0.5, vjust=0.5, colour = "black"),
        axis.text.y = element_text(size = 8, hjust=0.5, vjust=0.5, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("ct vs coverage")


