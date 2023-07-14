library("geobr")
library("ggplot2")
library("tidyverse")
library("RColorBrewer")
library("lubridate")
library("tidyr")
library("stringr")
library("gdata")
library("dplyr")
library("viridis")
library("sf")
library("maptools")
library("leaflet")
library("gridExtra")
library("data.table")
library("scales")

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

muni <- read_municipality(code_muni="MS",year=2020)
muni$city <- toupper(rm_accent(muni$name_muni))
sample <- read.csv("data/mapa/mapa_MS_SARS.txt", header = TRUE, sep = "\t")
sample$City <- toupper(rm_accent(sample$City))
muni$city <- toupper(rm_accent(muni$name_muni))
sample_agre <- sample %>%
  group_by(City)  %>%
  summarise(NCasos = n()) %>%
  drop_na()
muni_agre <- left_join(muni, sample_agre, by=c("city"="City"))
maxsars <- sum(muni_agre$NCasos[which(!is.na(muni_agre$NCasos))])
SARS <- ggplot() +
  geom_sf(data=muni_agre, aes(fill=NCasos), color=NA, size=.15) +
  geom_sf(data=muni, fill="transparent", color="black", size=.4, show.legend = FALSE) +
  scale_fill_distiller(palette = "Greens", name=paste0("Genome sequencing (n=",maxsars,")"), direction = 1,
                       na.value = "white", limits=c(0,max(muni_agre$NCasos))) +
  labs(subtitle="SARS-CoV-2", size=48) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=10))
SARS
#ggsave(filename = paste0("map_sars.pdf"), plot = SARS, device = "pdf", dpi = 300, width = 17.5, height = 7.5)
