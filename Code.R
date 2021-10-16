library(tidytuesdayR)
library(tidyverse)
library(reshape2)
tuesdata <- tidytuesdayR::tt_load('2021-10-12')
tuesdata
tuesdata$`capture-fisheries-vs-aquaculture`->production
production
glimpse(production)
production%>%
  filter(Code=="OWID_WRL")%>%
  select(-c(Entity,Code))%>%
  distinct(Year,.keep_all = TRUE)->global
data.frame(global)->global
colnames(global)


melt(global,id.vars ="Year", measure.vars = c("Aquaculture.production..metric.tons.","Capture.fisheries.production..metric.tons."),value.name = "value" )->m1
m1$value<-round((m1$value)/1000)


ggplot(m1,aes(x=Year, y=value, fill=variable))+geom_col(width=.6, colour="white")+
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  scale_x_continuous(breaks=c(1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015))+
  scale_fill_manual(values = c("#0392cf", "#7bc043"),labels = c("Aquaculture (in thousand metric tons)", "Capture fisheries (in thousand metric tons)"))+
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_text(colour = "white"),
        legend.text = element_text(colour = "white"),
        legend.background = element_rect("black"),
        legend.title = element_blank(),
        legend.position = "top")+
  labs(title="SEAFOOD PRODUCTION: AQUACULTURE VERSUS WILD FISH CATCH",
       subtitle = "Aquaculture is the farming of aquatic organisms including fish, molluscs, crustaceans and aquatic plants. On the other hand, capture
fishery production is the volume of wild fish catches landed for all commercial, industrial, recreational and subsistence purposes",
       caption="Data from Our World in Data|Design and analysis: @annapurani93" )+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5),
        plot.caption = element_text(colour = "white",size=8))->p

ggsave("stackbar.pdf",p,width =12,height=8)


