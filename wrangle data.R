##  for data info see
##  https://quickstats.nass.usda.gov/

##  Note: CV Coefficient of variation. Available for the 2012 Census of Agriculture
##        only. County-level CVs are generalized.

Sys.setlocale(category = "LC_CTYPE", locale = "C")
library(tidyverse)
library(magrittr)

#### Load data
setwd("/Users/mac/Desktop/615-assignment/615 Midterm Project")
strawb<-read.csv("Strawberries.csv")
setwd("/Users/mac/Desktop/615-assignment/615 Midterm Project")
pest<-read.csv("Pesticides.csv")


#### Drop the no-info columns

drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

strawb <- drop_no_info_cols(strawb)

#Separate Data.Item into 2 columns
strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "discription", "units"),
                     sep = ",",
                     fill = "right")

#### explore
distinct(strawb, Strawberries)

distinct(strawb, items)

distinct(strawb, discription)

distinct(strawb, units)

#Separate Domain into 2 columns(?)
strawb %<>% separate(col=Domain,
                     into = c("dname","type"),
                     sep = ",",
                     fill = "right")
distinct(strawb,dname)
distinct(strawb,type)
distinct(strawb,State)

# make a copy of Domain.Category 
strawb %<>%
  mutate(Chemicals=Domain.Category) %>%
  relocate(Chemicals, .after=Domain.Category)

## vector of logicals for each row with "CHEM" at 
## the start of strawb$Chemicals
bb <- strawb$Chemicals %>% str_detect("CHEM")
sum(bb)

## index 
ind_C <- (!bb)*(1:dim(strawb)[1])
## 
r1 <- ind_C[ind_C > 0]
## set entries in Chemicals column to " " if they don't start with CHEM
strawb$Chemicals[r1] <- " "


## now we need a list of chemicals

strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(type = str_trim(type))
distinct(strawb, details)
distinct(strawb, type)


strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE")|
                                   (type=="OTHER"))
distinct(strawb,type)
distinct(strawb,dname)
distinct(strawb,items)

distinct(dname,Type)
#### draw a histgram to anaylse strawb$dname
dname$Type<-dname$Var1
p<-ggplot(dname,aes(x=Type,y=Freq,fill=Type))+geom_bar(stat='identity')+
  labs(x='Planting Methods',y='Number',title='Number of Type')+
  theme(axis.text.x = element_text(color="black",size=20),
        axis.text.y = element_text(color="black",size=22 ))+ 
  theme(axis.title.x =element_text(size=25), axis.title.y=element_text(size=25))+
  geom_text(aes(label=Freq),size=9,color='black',vjust= 0.0000001,hjust=.5)+
  theme(text=element_text(family="Times New Roman",size=25))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Type",values=c("CHEMICAL"="#685948","FERTILIZER"="#dda069","ORGANIC STATUS"="#e5b68b","TOTAL"="#bc9777"))
p

####### separate details into 2 columns--only use chem to analyse
strawb_chem %<>% separate(col = details,
                          into = c("Pesticide", "Pesticides_code"),
                          sep = "=",
                          fill = "right")
strawb_chem$Pesticide<-tolower(strawb_chem$Pesticide)
strawb_chem$Pesticide=str_trim(strawb_chem$Pesticide)

distinct(strawb_chem,Pesticide)
distinct(strawb_chem,type)
distinct(pest_no_NA,Pesticide)

distinct(strawb_chem,dname)
distinct(strawb_chem,Year)


## delete (D) in value
strawb_chem_D <- strawb_chem[strawb_chem$Value != " (D)",]


############ pest
pest_no<-pest[!apply(pest =="", 1, all),]

colnames(pest_no)[1]="Pesticide"
pest_no$Pesticide<-tolower(pest_no$Pesticide)
a<-pest_no %>% `$`(Pesticide) %>% unique() 
pest_both<-filter(strawb_chem,Pesticide %in% a)


######wrangle
strawb_pest<-dplyr::left_join(pest_both,pest_no,by='Pesticide')



