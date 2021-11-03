library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)
library(RColorBrewer)
display.brewer.all(type = "all")
library(plotly)
library(GGally)
library(ggpubr)

distinct(strawb_chem_D,State)
distinct(strawb_chem_D,type)

###### delete columns
strawb_pest_cut <- strawb_pest[,-c(1,5,6,10,12,13,17)]
view(strawb_pest_cut)
distinct(strawb_pest_cut,discription)

## delete (D) in value
strawb_pest_cut_D <- strawb_pest_cut[strawb_pest_cut$Value != " (D)",]
view(strawb_pest_cut_D)

## delete NA Bee.Toxins 
strawb_pest_cut_D_databee<- strawb_pest_cut_D[strawb_pest_cut_D$Bee.Toxins != "",]
view(strawb_pest_cut_D_databee)
f_data<-strawb_pest_cut_D_databee

view(f_data)

distinct(f_data,State)

distinct(f_data,Year)

distinct(f_data,discription)

distinct(f_data,units)

distinct(f_data,type)

##########################################
###### EDA
# 2019CaliforniaBee.Toxin+sum(value)
CA_2019<- filter(f_data,f_data$Year=="2019"&f_data$State=="CALIFORNIA")#70
CA_2018<- filter(f_data,f_data$Year=="2018"&f_data$State=="CALIFORNIA")#60
CA_2016<- filter(f_data,f_data$Year=="2016"&f_data$State=="CALIFORNIA")#60

##################EDA
#Specific
#1.
all_Year<-f_data %>% group_by(Year,State) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=T))
all_Year %>% ggplot(aes(x=Year,y=sum_value,color=State))+
  geom_point(size=8)+
  geom_line(linetype = "dashed",size=5)+
  labs(x='Year',y='Value',title='Pesticide&Fungicide usage in each State through Year')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("State",values=c("CALIFORNIA"="#7c3f43","FLORIDA"="#b67378","WASHINGTON"="#cdcdcd"))

  
all_Bee<-f_data %>% group_by(Bee.Toxins,State) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=T))
all_Bee %>% ggplot(aes(x=Bee.Toxins,y=sum_value,fill=State))+
  geom_bar(stat='identity',position='dodge',size=3)+
  labs(x='Bee.Toxins',y='Value',title='Bee.Toxins vs Pesticide&Fungicide usage in each State')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("State",values=c("CALIFORNIA"="#4c729b","FLORIDA"="#a2b4dc","WASHINGTON"="#8d8aa9"))
    
all_type<-f_data %>% group_by(type,State) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=T))
all_type %>% ggplot(aes(x=type,y=sum_value,fill=State))+
  geom_bar(stat='identity',size=3)+
  labs(x='Type',y='Value',title='Type vs Pesticide&Fungicide usage in each State')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("State",values=c("CALIFORNIA"="#bba19e","FLORIDA"="#d2baa9","WASHINGTON"="#b49382"))
  
#2.
#2.1 Type
Year_2019<-filter(f_data,f_data$Year=="2019")
all_2019_Type<-Year_2019 %>% group_by(State,type) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
all_2019_Type %>% ggplot(aes(x=type,y=sum_value,fill=State))+
  geom_bar(stat="identity",size=3)+
  labs(x='Type',y='Value',title='Type vs Value of each State in 2019')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
scale_fill_manual("State",values=c("CALIFORNIA"="#bba19e","FLORIDA"="#d2baa9","WASHINGTON"="#b49382"))

Year_2018<-filter(f_data,f_data$Year=="2018")
all_2018_Type<-Year_2018 %>% group_by(State,type) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
all_2018_Type %>% ggplot(aes(x=type,y=sum_value,fill=State))+
  geom_bar(stat="identity",size=3)+
  labs(x='Type',y='Value',title='Type vs Value of each State in 2018')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("State",values=c("CALIFORNIA"="#bba19e","FLORIDA"="#d2baa9","WASHINGTON"="#b49382"))

Year_2016<-filter(f_data,f_data$Year=="2016")
all_2016_Type<-Year_2016 %>% group_by(State,type) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
all_2016_Type %>% ggplot(aes(x=type,y=sum_value,fill=State))+
  geom_bar(stat="identity",size=3)+
  labs(x='Type',y='Value',title='Type vs Value of each State in 2016')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("State",values=c("CALIFORNIA"="#bba19e","FLORIDA"="#d2baa9","WASHINGTON"="#b49382"))

# Bee.Toxins
all_2019_Bee.Toxins<-Year_2019 %>% group_by(State,Bee.Toxins) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
all_2019_Bee.Toxins %>% ggplot(aes(x=Bee.Toxins,y=sum_value,fill=State))+
  geom_bar(stat="identity",position='dodge',size=3)+
  labs(x='Bee.Toxins',y='Value',title='Bee.Toxins vs Value of each State in 2019')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("State",values=c("CALIFORNIA"="#4c729b","FLORIDA"="#a2b4dc","WASHINGTON"="#8d8aa9"))

all_2018_Bee.Toxins<-Year_2018 %>% group_by(State,Bee.Toxins) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
all_2018_Bee.Toxins %>% ggplot(aes(x=Bee.Toxins,y=sum_value,fill=State))+
  geom_bar(stat="identity",position='dodge',size=3)+
  labs(x='Bee.Toxins',y='Value',title='Bee.Toxins vs Value of each State in 2018')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("State",values=c("CALIFORNIA"="#4c729b","FLORIDA"="#a2b4dc","WASHINGTON"="#8d8aa9"))

all_2016_Bee.Toxins<-Year_2016 %>% group_by(State,Bee.Toxins) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
all_2016_Bee.Toxins %>% ggplot(aes(x=Bee.Toxins,y=sum_value,fill=State))+
  geom_bar(stat="identity",position='dodge',size=3)+
  labs(x='Bee.Toxins',y='Value',title='Bee.Toxins vs Value of each State in 2016')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("State",values=c("CALIFORNIA"="#4c729b","FLORIDA"="#a2b4dc","WASHINGTON"="#8d8aa9"))


# insecticide近年增量
insect<-filter(f_data,f_data$type=="INSECTICIDE")
insect_State_Year<-insect %>% group_by(Year,State) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
insect_State_Year %>% ggplot(aes(x=Year,y=sum_value,color=State))+geom_line(stat='identity',size=3)+
  geom_point()+
  labs(x='Year',y='Dose of Insecticide',title='Insecticide Usage in California & Florida')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))

#####Pictures in shiny app
#CA_2019
CA_pest_sum_2019<-CA_2019 %>% group_by(Pesticide,Bee.Toxins) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
view(CA_pest_sum_2019)
CA_pest_sum_2019 %>% ggplot(aes(x=sum_value,y=Bee.Toxins,group=Pesticide,color=Pesticide))+
  geom_point(stat="identity",size=14)+
  labs(x='Dose of each Pesticide',y='To Bee.Toxin',title='Pesticide Usage/Bee Toxins in 2019 California')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))

CA_pest_sum_2018<-CA_2018 %>% group_by(Pesticide,Bee.Toxins) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
CA_pest_sum_2018 %>% ggplot(aes(x=sum_value,y=Bee.Toxins,group=Pesticide,color=Pesticide))+
  geom_point(stat="identity",size=14)+
  labs(x='Pesticide',y='Dose of each Pesticide',title='Pesticide Usage in 2018 California')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))    

CA_pest_sum_2016<-CA_2016 %>% group_by(Pesticide,Bee.Toxins) %>% summarise(sum_value=sum(as.numeric(Value),na.rm=TRUE))
CA_pest_sum_2016 %>% ggplot(aes(x=sum_value,y=Bee.Toxins,group=Pesticide,color=Pesticide))+
  geom_point(stat="identity",size=14)+
  labs(x='Pesticide',y='Dose of each Pesticide',title='Pesticide Usage in 2016 California')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))     

#Bee.Toxins count
f_data$Bee.Toxins<-factor(f_data$Bee.Toxins)
ggplot(data = strawb_pest_cut_databee)+
  geom_bar(mapping = aes(x = Year, fill = as.factor(Bee.Toxins)))+
  labs(x='Year',y='Count',title='Bee.Toxins Count number In Each Year')+
  theme(axis.text.x=element_text(color='black',size=23),
        axis.text.y = element_text(color='black',size=25))+
  theme(axis.title.x=element_text(size=28),axis.title.y = element_text(size=28))+
  theme(text=element_text(family="Times New Roman",size=28))+
  theme(text=element_text(size=28, family="Times New Roman"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Bee.Toxins",values=c("slight"="#b4d9b0","moderate"="#c6c99e","high"="#b3c174"))


################# draw a histgram to anaylse strawb$dname
count(type)<-as.data.frame(table(f_data$type)) #Count the number of occurrences for each type
p_f_data_type<-ggplot(type,aes(x=Var1,y=Freq,main="The number of each Chemical type"))+
  geom_bar(stat="identity",fill="lightgray",color="gray")+
  xlab("Planting Methods")+
  ylab("Number")+
  geom_text(aes(label=Freq,hjust="middle"))
  theme(text=element_text(size=13, family="Comic Sans MS"))
p_f_data_type
















