
library(data.table)
library(tidyverse)
library(dplyr)
library(gridExtra)


cases_ms_month <- fread("data/cases/data_ms_per_month.csv", sep=";")
hosp_ms_month <- fread("data/cases/data_ms_hosp_covid_per_month.csv", sep=";")

hosp_ms_month$faixaEtaria <- factor(hosp_ms_month$faixaEtaria, levels = c("0 a 4 anos", "5 a 9 anos", "10 a 19 anos", "20 a 59 anos", "60 a 69 anos", "70+ anos"))

hosp_ms_month$faixaEtariaEng <- NA
hosp_ms_month$faixaEtariaEng[which(hosp_ms_month$faixaEtaria == "0 a 4 anos")] <- "0 to 4 years"
hosp_ms_month$faixaEtariaEng[which(hosp_ms_month$faixaEtaria == "5 a 9 anos")] <- "5 to 9 years"
hosp_ms_month$faixaEtariaEng[which(hosp_ms_month$faixaEtaria == "10 a 19 anos")] <- "10 to 19 years"
hosp_ms_month$faixaEtariaEng[which(hosp_ms_month$faixaEtaria == "20 a 59 anos")] <- "20 to 59 years"
hosp_ms_month$faixaEtariaEng[which(hosp_ms_month$faixaEtaria == "60 a 69 anos")] <- "60 to 69 years"
hosp_ms_month$faixaEtariaEng[which(hosp_ms_month$faixaEtaria == "70+ anos")] <- "70+ years"

hosp_ms_month$faixaEtariaEng <- factor(hosp_ms_month$faixaEtariaEng, levels = c("0 to 4 years", "5 to 9 years", "10 to 19 years", "20 to 59 years", "60 to 69 years", "70+ years"))

n_month <- max(hosp_ms_month$total)

Sys.setlocale("LC_TIME", "en_US")

p1engmonth <- ggplot()+
  geom_bar(data=hosp_ms_month, aes(x=month,y=total,fill=faixaEtariaEng),stat="identity")+
  geom_line(data=cases_ms_month, aes(x=month,y=casosNovos*0.02),size=2, color="#7fcdbb")+
  scale_y_continuous(sec.axis = sec_axis( trans=(~./0.02), name="Number of COVID-19 cases") ) +
  scale_x_date(labels = scales::label_date_short(format = c("%Y", "%b"), sep = "\n"), expand = c(0,0), breaks = "2 months") +
  scale_fill_manual("", values = c("0 to 4 years" = "#08306B", "5 to 9 years" = "#045A8D",    "10 to 19 years" = "#FEC44F",
                                   "20 to 59 years" = "#3477B2", "60 to 69 years" = "#A63603", "70+ years" = "#EC7014"))+
  labs(x="Event date",y="Number of hospitalizations due to COVID-19",fill="",title = "Mato Grosso do Sul")+
  theme_classic()+
  theme(legend.key.height = unit(0.2, "cm")) +
  guides(fill=guide_legend(ncol=3))+
  theme(legend.position = "bottom",legend.box = "vertical")+
  theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(size=14),
        text=element_text(size=16),
        title = element_text(size=16))

p2engmonth <- ggplot()+
  geom_bar(data=hosp_ms_month, aes(x=month,y=total,fill=faixaEtariaEng),stat="identity")+
  geom_line(data=cases_ms_month, aes(x=month,y=obitosNovos*1.25),size=2, color="#000000")+
  scale_y_continuous(sec.axis = sec_axis( trans=(~./1.25), name="Number of COVID-19 deaths") ) +
  scale_x_date(labels = scales::label_date_short(format = c("%Y", "%b"), sep = "\n"), expand = c(0,0), breaks = "2 months") +
  scale_fill_manual("", values = c("0 to 4 years" = "#08306B", "5 to 9 years" = "#045A8D",    "10 to 19 years" = "#FEC44F",
                                   "20 to 59 years" = "#3477B2", "60 to 69 years" = "#A63603", "70+ years" = "#EC7014"))+
  labs(x="Event date",y="Number of hospitalizations due to COVID-19",fill="",title = "Mato Grosso do Sul")+
  theme_classic()+
  theme(legend.key.height = unit(0.2, "cm")) +
  guides(fill=guide_legend(ncol=3))+
  theme(legend.position = "bottom",legend.box = "vertical")+
  theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.text=element_text(size=14),
        text=element_text(size=16),
        title = element_text(size=16))

p <- arrangeGrob(p1engmonth, p2engmonth, ncol=1)

ggsave(filename = paste0("cases_hosp_sars_month.pdf"), plot = p, device = "pdf", dpi = 300, width = 14, height = 12)
