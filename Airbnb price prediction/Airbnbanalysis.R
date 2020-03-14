data=read.csv("data2.csv",header=T,na.strings = c("","NA"))
data
data$neighbourhood[data$neighbourhood_group=="Brooklyn"]
sd(data$price[data$neighbourhood_group=="Brooklyn"])
sd(data$price[data$neighbourhood_group=="Manhattan"])
sd(data$price[data$neighbourhood_group=="Bronx"])
sd(data$price[data$neighbourhood_group=="Queens"])
sd(data$price[data$neighbourhood_group=="Staten Island"])
sd(data$price)
hist(data$price)
bartlett.test(data$price~data$neighbourhood_group)
a=log(data$price)
a
data$transprice=a
data$transprice
sd(data$transprice[data$neighbourhood_group=="Brooklyn"])
sd(data$transprice[data$neighbourhood_group=="Manhattan"])
sd(data$transprice[data$neighbourhood_group=="Bronx"])
sd(data$transprice[data$neighbourhood_group=="Queens"])
sd(data$transprice[data$neighbourhood_group=="Staten Island"])
sd(data$transprice)
data
setwd("G://Uni//3rd year//Semester 2//STL//project 2")

sum1=0
for(i in 1:length(data$neighbourhood_group)){
  if(data$neighbourhood_group[i]=="Manhattan")
    sum1=sum1+1
  
}
sum1

sum2=0
for(i in 1:length(data$neighbourhood_group)){
  if(data$neighbourhood_group[i]=="Brooklyn")
    sum2=sum2+1
  
}
sum2

sum3=0
for(i in 1:length(data$neighbourhood_group)){
  if(data$neighbourhood_group[i]=="Bronx")
    sum3=sum3+1
  
}
sum3

sum4=0
for(i in 1:length(data$neighbourhood_group)){
  if(data$neighbourhood_group[i]=="Queens")
    sum4=sum4+1
  
}
sum4

sum5=0
for(i in 1:length(data$neighbourhood_group)){
  if(data$neighbourhood_group[i]=="Staten Island")
    sum5=sum5+1
  
}
sum5

calc=c(sum1,sum2,sum3,sum4,sum5)
calc
barplot(calc)

warnings()
length(data$neighbourhood_group)
data$neighbourhood_group=="Manhattan"

setwd("G://Uni//3rd year//Semester 2//STL//project 1")
a=read.csv("new_1.csv")
View(a)
dff=data.frame(data$longitude,data$latitude,data$price)
dff
aa=write.csv(dfff,"G://Uni//3rd year//Semester 2//STL//project 1//coordinates.csv", row.names = FALSE)
dfff <- setNames(dff, c("latitude","longitude","price"))


q=data.frame(data$price[data$neighbourhood_group=="Manhattan"])

q1=data.frame(data$price[data$neighbourhood_group=="Brooklyn"])
q2=data.frame(data$price[data$neighbourhood_group=="Staten Island"])
q3=data.frame(data$price[data$neighbourhood_group=="Bronx"])
q4=data.frame(data$price[data$neighbourhood_group=="Queens"])
q=data$price[data$neighbourhood_group=="Manhattan"]

write.csv(q,"G://Uni//3rd year//Semester 2//STL//project 1//Manhattan.csv", row.names = FALSE)
write.csv(q1,"G://Uni//3rd year//Semester 2//STL//project 1//Brooklyn.csv", row.names = FALSE)
write.csv(q2,"G://Uni//3rd year//Semester 2//STL//project 1//Staten_Island.csv", row.names = FALSE)
write.csv(q3,"G://Uni//3rd year//Semester 2//STL//project 1//Bronx.csv", row.names = FALSE)
write.csv(q4,"G://Uni//3rd year//Semester 2//STL//project 1//Queens.csv", row.names = FALSE)

plot(data$calculated_host_listings_count)
plot(data$calculated_host_listings_count,)

hlc=c(data$calculated_host_listings_count)
hlc
for(i in 1 : length(data$calculated_host_listings_count)){
  if ( data$calculated_host_listings_count[i]==1) {
    data$calculated_host_listings_count[i]="Group_1"
 
  } else if ( data$calculated_host_listings_count[i]>1 & data$calculated_host_listings_count[i]<100) {
    data$calculated_host_listings_count[i]="Group_2"
   
  } else if ( data$calculated_host_listings_count[i]>100 & data$calculated_host_listings_count[i]<200) {
    data$calculated_host_listings_count[i]="Group_3"
    
  } else if ( data$calculated_host_listings_count[i]>200 & data$calculated_host_listings_count[i]<300) {
    data$calculated_host_listings_count[i]="Group_4"
   
  } else {
    data$calculated_host_listings_count[i]="Group_5"
  }
}
View(data)
warnings()

for(i in 1 : length(data$calculated_host_listings_count)){
  if ( data$calculated_host_listings_count[i]==1) {
    data$calculated_host_listings_count[i]="Group_1"
}
}
View(data)

x=numeric(0)
for(i in 1:length(data$calculated_host_listings_count)){
  if(data$calculated_host_listings_count[]==1)
    x[i]=data$price[i]
}
x

data$price[which(data$calculated_host_listings_count==1)]

data1=read.csv("new_1.csv")
hlc=c(data1$calculated_host_listings_count)
hlc
for(i in 1 : length(hlc)){
  if ( hlc[i]==1) {
    data1$calculated_host_listings_count[i]="Group_1"
    
  } else if ( (hlc[i]>1) & (hlc[i]<20) ){
    data1$calculated_host_listings_count[i]="Group_2"
    
  } else if ( (hlc[i]>20) & (hlc[i]<50) ){
    data1$calculated_host_listings_count[i]="Group_3"
    
  } else if ( (hlc[i]>50) & (hlc[i]<103)) {
    data1$calculated_host_listings_count[i]="Group_4"
    
  } else {
    data1$calculated_host_listings_count[i]="Group_5"
  }
}
View(data1)
Host_listing_count=data1$calculated_host_listings_count
Price=data1$price
df1=data.frame(Host_listing_count,Price,row.names=NULL)

levels(df1$Host_listing_count)
data_groups=ordered(df1$Host_listing_count,levels=c("Group_1", "Group_2" ,"Group_3", "Group_4", "Group_5"))
anova1=aov(df1$Price~df1$Host_listing_count,data=df1)
summary(anova1)


library(ggpubr)
install.packages("ggpubr")
ggboxplot()

tuk=TukeyHSD(anova1)
tuk
install.packages("DTK")

ggboxplot(df2,x=" xxx ",yvar, 
          color = "df$xx", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c( "Group_1" ,"Group_2", "Group_3" ,"Group_4", "Group_5"),
          ylab = "Weight", xlab = "Group")
df1

xxx=df1$xx
yyy=df1$yy
df2=data.frame(xxx,yyy,row.names = NULL)
yvar=c(yyy)















library(DTK)
DTK.test(x = df1$Price, f = df1$Host_listing_count, a = 0.05)
#Loading data
data1=read.csv("new_1.csv")

hlc=c(data1$calculated_host_listings_count)
hlc

#Creating 5 groups according to the Calculated_host_listing_count
for(i in 1 : length(hlc)){
  if ( hlc[i]==1) {
    data1$calculated_host_listings_count[i]="Group_1"
    
  } else if ( (hlc[i]>1) & (hlc[i]<20) ){
    data1$calculated_host_listings_count[i]="Group_2"
    
  } else if ( (hlc[i]>20) & (hlc[i]<50) ){
    data1$calculated_host_listings_count[i]="Group_3"
    
  } else if ( (hlc[i]>50) & (hlc[i]<103)) {
    data1$calculated_host_listings_count[i]="Group_4"
    
  } else {
    data1$calculated_host_listings_count[i]="Group_5"
  }
}
#Creating a dataframe of Calculated host listing count and price
Host_listing_count=data1$calculated_host_listings_count
Price=data1$price
df1=data.frame(Host_listing_count,Price,row.names=NULL)

levels(df1$Host_listing_count)
data_groups=ordered(df1$Host_listing_count,levels=c("Group_1", "Group_2" ,"Group_3", "Group_4", "Group_5"))

# ANOVA for price according to host listing count groups
anova1=aov(df1$Price~df1$Host_listing_count,data=df1)
summary(anova1)

# Post hoc test for different groups
library(DTK)
DTK.test(x = df1$Price, f = df1$Host_listing_count, a = 0.05)








