setwd("G://Uni//3rd year//Semester 2//STL//project 1")
data1=read.csv("new_1.csv")

hlc=c(data1$calculated_host_listings_count)
hlc


for(i in 1 : length(hlc)){
  if ( hlc[i]==1) {
    data1$calculated_host_listings_count[i]="Group_1"
    
  } else if ( (hlc[i]>1) & (hlc[i]<50) ){
    data1$calculated_host_listings_count[i]="Group_2"
    
 
    
  } else {
    data1$calculated_host_listings_count[i]="Group_3"
  }
}
#Creating a dataframe of Calculated host listing count and price
Host_listing_count=data1$calculated_host_listings_count
Price=data1$price
df1=data.frame(Host_listing_count,Price,row.names=NULL)

levels(df1$Host_listing_count)
data_groups=ordered(df1$Host_listing_count,levels=c("Group_1", "Group_2" ,"Group_3"))

# ANOVA for price according to host listing count groups
anova1=aov(df1$Price~df1$Host_listing_count,data=df1)
summary(anova1)

# Post hoc test for different groups
library(DTK)
DTK.test(x = df1$Price, f = df1$Host_listing_count, a = 0.05)
length(data1$price[data1$calculated_host_listings_count=="Group_1"])

View(data1)
length(data1$price)