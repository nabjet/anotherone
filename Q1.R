remove(list = ls()) #clear the environment

#Load Libraries
library(dplyr)
library(rstudioapi)
library(tidyr)
library(ggplot2)
library(data.table)

#Set working directory
#current_path <- getActiveDocumentContext()$path
setwd('C:/Rstudio/anotherone/')

#Load the Dataset
E <- read.csv2("employment_data.csv")

#Answer to question 1
View(E)
unique(E)
unique(E$Sector)     #there are 4 unique sectors
unique(E$Country)    #There are 34 unique countries
                     # the categories are 
                     #1. Total -all NACE activities 
                     #2. Manufacturing
                     #3. Wholesale and retail trade, transport, accommodation and food service activities
                     #4. Construction"

#Answer to question 2 : Drop observations for 
#“European Union - 27 countries (from 2020)” and “Euro area - 19 countries
#(from 2015)”.
#E <- E[-c(1,2,35,36,69,70),]   
E <- E[-(E$Country == 'European Union - 27 countries (from 2020)'),] #Drop 
E <- E[-(E$Country == 'Euro area - 19 countries  (from 2015)'),] #Drop 
E <- E[!duplicated(as.list(E))] ## remove duplicated/redundant columns

#Answer to question 3
# countries
b <- E$Country[is.na(E$X2019.Q4)]
b
#dropped those countries
E <- E[ is.na(E$X2019.Q4) %in% "FALSE",]
#b <- E$Country[ !is.na(E$X2019.Q4)]


#Answer to question 4
class(E$Country) #Object class of Country is a character.

#Answer to question 5 : Rename country name to Germany
E[E == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
#E[4,1]='Germany'      
#E[30,1]='Germany'     
#E[56,1]='Germany'     
#E[82,1]='Germany' 
#rownames(E)<-NULL

#Answer to question 6 : Remove the "X" from the column names
B <- names(E)[grep('.Q',names(E))]
names(E)[grep('.Q',names(E))] <- sub('.','',B)


#Answer to question 7 : DataSet "M" for 2019.Q2 and 2020.Q2
#M <- data.frame(E[,12],E[,16])
M <- data.frame(E$'2020.Q1',E$'2020.Q2')

M <- M %>% mutate(Per_change =((as.numeric(M$E..2020.Q2.)-as.numeric(M$E..2020.Q1.))/(as.numeric(M$E..2020.Q1.)))* 100 )

colnames(M)[colnames(M)%in%"E...12..1"] <- '2019.Q2'
colnames(M)[colnames(M)%in%"X2020.Q2"] <- '2020.Q2'
colnames(M)[colnames(M)%in%"E...12."] <- 'Per_Diff'


#Answer to question 8 : plot a graph of the Percentage Differences

Q_ret <- (M$Y)[E$Sector == E[72,2]] ## whole sale, retail values
Q_ret_countries <- (E$Country)[E$Sector == E[72,2]]## whole sale, retail countries
P_ret <- data.frame(Q_ret,Q_ret_countries) ## contains countries and their perc change
Q_man <- (M$Y)[E$Sector == 'Manufacturing'] ## manufacturing values
Q_man_countries <- E$Country[E$Sector == 'Manufacturing']
P_man <- data.frame(Q_man,Q_man_countries)# manufacture dataset
P_ret <- arrange(P_ret,desc(P_ret$Q_ret))# retail, ... dataset reordered desc
P_man <- arrange(P_man,desc(P_man$Q_man))# manufac dataset reordered desc
barplot(P_man$Q_man,names.arg = P_man$Q_man_countries, main = 'Percentage difference in Manufacturing by Country')
# for manufacter, Portugal has the lowest
barplot(P_ret$Q_ret,names.arg = P_ret$Q_ret_countries,main = 'Percentage difference in Retail,..., by Country')
# for retail the lowest is Spain


#Answers to question 9 : DataSet of Employment in wholesale and retail
Employment <- E %>% filter(Sector == E[70,2])
Employment <- Employment[,-2]
#Transforming the DataSet to 390 rows 3 columns
Employment <- gather(Employment, key = 'quarters', 'values', - Country)


#Answers to Question 10 : Percentage difference btw quarters
Employment <- Employment %>% mutate(per_change = NA)
for (i in 1:11) {
  Employment[((i+3)*26)+c(1:26),4] <- 100*((as.numeric(Employment[((i+3)*26)+c(1:26),3])-as.numeric(Employment[((i-1)*26 +c(1:26)),3]))/as.numeric(Employment[((i-1)*26 +c(1:26)),3]))
  
}
## dropping year 2017
Employment <- Employment %>% slice(105:390)



## Answer to question 11
Count_1 <- Employment %>% filter(Employment$Country == 'Germany') ## selection GErmany
Count_2 <- Employment %>% filter(Employment$Country == 'Bulgaria')
Count_1 <- Count_1[,-c(1,3)]
Count_2 <- Count_2[,-c(1,3)]
colnames(Count_2) <- c('Quarters', 'Bulgaria')
colnames(Count_1) <- c('Quarters', 'Germany')
PlotData <- merge(Count_1,Count_2,by = 'Quarters')
PlotData <- gather(PlotData,key = 'Country',value = 'change', -Quarters)
## plot
ggplot(data = PlotData, aes(x = Quarters,y = change )) +geom_path(aes(color = Country),group = 'quarters',na.rm = TRUE) + scale_color_manual(values = c('darkred','blue'))


## Question 12
#Overview of DataSet
#dim(E)
#class(E)
#names(E)
#colnames(E)
#head(E)

#dim(R)
#class(R)
#names(R)
#colnames(R)
#head(R)
