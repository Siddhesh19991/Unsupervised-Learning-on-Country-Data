update.packages(ask = FALSE, checkBuilt = TRUE)

Country.data <- read.csv("~/Downloads/Datasets/Country-data.csv")

library(caret)
library(dplyr)

#to check 

k<-kmeans(Country.data[,-1],centers = 3,nstart = 20)

col<-k$cluster

plot(Country.data$income,Country.data$gdpp,col=col)

pre<-preProcess(Country.data[,-1],method = c("center","scale"))
country<-predict(pre,Country.data[,-1])

#or you can just apply 
#scale(country.data[,-1])

country<-cbind(Country.data[,1],country)
names(country)[1]<-"country"


k1<-kmeans(country[,-1],centers = 3,nstart = 20)
col1<-k1$cluster
plot(country$income,country$gdpp,col=col1)


#determining the ideal number of clusters

wss<-0

for(i in 1:15){
  
  km.out<-kmeans(country[,-1],centers = i,nstart = 20)
  wss[i]<-km.out$tot.withinss
}

plot(1:15,wss,type="b")

#3 seems like an ideal number of clusters
#but first lets apply PCA

pr<-prcomp(x=country[,-1])

country2<-pr$x[,1:5]
country2<-cbind(Country.data[,1],country2)
country2<-as.data.frame(country2)

names(country2)[1]<-"country"

k2<-kmeans(country2[,-1],centers = 3,nstart = 20)
col2<-k2$cluster
plot(country2$PC1,country2$PC2,col=col2)

kvalue<-k2$cluster

#now using hierarchiacl clustering 

dis_matrix<-dist(country2[,-1])
a<-hclust(d=dis_matrix)
plot(a)

target_country<-cbind(Country.data,kvalue)

target_country%>%select(gdpp,kvalue)%>%group_by(kvalue)%>%summarise(total=sum(gdpp))

# kvalue   total
# <int>   <int>
# 1       544862
# 2      1529800
# 3       90352


#we can see that we should be investing our funds in cluster 3 
#lets look at the data and further analyse if this is true

# a<-subset(target_country,kvalue==1)
# b<-subset(target_country,kvalue==2)
# c<-subset(target_country,kvalue==3)

colMeans(a[,-1])
# child_mort      exports       health      imports       income    inflation   life_expec 
# 21.927381    40.243917     6.200952    47.473404 12305.595238     7.600905    72.814286 
# total_fer         gdpp       kvalue 
# 2.307500  6486.452381     1.000000 
 colMeans(c[,-1])
# child_mort     exports      health     imports      income   inflation  life_expec   total_fer 
# 92.961702   29.151277    6.388511   42.323404 3942.404255   12.019681   59.187234    5.008085 
# gdpp      kvalue 
# 1922.382979    3.000000 

#therefore we should invest in cluster 3(c)

#on looking at these clusters we see that there is a high child mortality rate and low life expectancy
#therefore there is need to invest in health industry for these countries
 
d<-c%>%select(country,life_expec)%>%arrange(life_expec)

#                     country   life_expec
#  1                     Haiti       32.1
#  2                   Lesotho       46.5
#  3  Central African Republic       47.5
#  4                    Zambia       52.0
#  5                    Malawi       53.1
#  6              South Africa       54.3
#  7                Mozambique       54.5
#  8              Sierra Leone       55.0
#  9             Guinea-Bissau       55.6
#  10              Afghanistan       56.2
#  11            Cote d'Ivoire       56.3
# 12                     Chad       56.5
# 13                   Uganda       56.8
# 14                 Botswana       57.1
# 15                 Cameroon       57.3
# 16         Congo, Dem. Rep.       57.5
# 17                  Burundi       57.7
# 18             Burkina Faso       57.9
# 19                   Guinea       58.0
# 20                  Namibia       58.6


e<-c%>%select(country,child_mort)%>%arrange(desc(child_mort))

#                     country child_mort
# 1                     Haiti      208.0
# 2              Sierra Leone      160.0
# 3                      Chad      150.0
# 4  Central African Republic      149.0
# 5                      Mali      137.0
# 6                   Nigeria      130.0
# 7                     Niger      123.0
# 8                    Angola      119.0
# 9              Burkina Faso      116.0
# 10         Congo, Dem. Rep.      116.0
# 11            Guinea-Bissau      114.0
# 12                    Benin      111.0
# 13            Cote d'Ivoire      111.0
# 14        Equatorial Guinea      111.0
# 15                   Guinea      109.0
# 16                 Cameroon      108.0
# 17               Mozambique      101.0
# 18                  Lesotho       99.7
# 19               Mauritania       97.4
# 20                  Burundi       93.6
 
#the top 3 countries to invest more in are Haiti,Central African Republic,Cote d'Ivoire based on its poor health conditions 
