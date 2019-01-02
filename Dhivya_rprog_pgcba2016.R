# =====================================================
# Description: Analysis of the given FMCG Data
# Data: FMCG data set_R.csv
# =====================================================

# =====================================================
# Name:Dhivya Swaminathan
# =====================================================


#########################################################################################################
###Question 1
#loading the excel file to a variable in r 
fmcg_data=read.csv("E:/Applied analytics/FMCG data set_R.csv")

#looking for the type of the variable defined above
class(fmcg_data)

#replacing NA s as 0
fmcg_data[is.na(fmcg_data)]=0

#########################################################################################################
### Question 2
#creating and displaying a variable that stores all the relevant data about variant-1163
variant_1163=subset(fmcg_data,Variant=="variant-1163")
variant_1163

#creating and displaying a variable that stores the Value of Sales of variant-1163 across three years
variant_1163_ValS=variant_1163[,c(6,11:46)]
variant_1163_ValS

#creating and displaying a variable that stores the Volume of Sales of variant-1163 across three years
variant_1163_VolS=variant_1163[,c(6,47:82)]
variant_1163_VolS

#creating and displaying a variable that stores the No of stores having variant-1163 across three years
variant_1163_NoS=variant_1163[,c(6,83:118)]
variant_1163_NoS


#pivoting data by finding sum of Value of sales across all years for the purpose of finding correlation
SumVals1=data.frame(1:7687,0);
for(i in 1:length(fmcg_data[,1])){
  for(j in 1:36){
    SumVals1[,i]=SumVals1[,i]+fmcg_data[,j+10];
  }
}

#pivoting data by finding sum of Volume of sales across all years for the purpose of finding correlation
SumVols2=data.frame(1:7687,0);
for(i in 1:length(fmcg_data[,1])){
  for(j in 1:36){
    SumVols2[,i]=SumVols2[,i]+fmcg_data[,j+46];
  }
}

#pivoting data by finding sum of No of stores across all years for the purpose of finding correlation
SumNoS1=data.frame(1:7687,0);
for(i in 1:length(fmcg_data[,1])){
  for(j in 1:36){
    SumNoS1[,i]=SumNoS1[,i]+fmcg_data[,j+82];
  }
}

#creating another vector having price calculated from revenue(value of sales) and volume of sales
Price1=data.frame(1:7687,0);
for(i in 1:length(fmcg_data[,1])){
  Price1[,i]=(SumVals1[,i]*1000)/(SumVols2[,i]*100000);
}
Price1[is.na(Price1)]=0


#creating data frame for finding correlation
fmcg_corr=data.frame(fmcg_data[,1:10],SumVals1[,2],SumVols2[,2],SumNoS1[,2])
fmcg_corr1=data.frame(SumVals1[,2],SumVols2[,2],SumNoS1[,2])
fmcg_corr_price_Volume=data.frame(Price1[,2],SumVols2[,2])
fmcg_corr_price_Volume_NoS=data.frame(Price1[,2],SumVols2[,2],SumNoS1[,2])
fmcg_corr_price_Volume_NoS_2=subset(fmcg_corr_price_Volume_NoS,fmcg_corr_price_Volume_NoS$SumVols2...2.>0)
fmcg_corr_priceLess1000_Volume_NoS=subset(fmcg_corr_price_Volume_NoS_2,fmcg_corr_price_Volume_NoS_2$Price1...2.<1000)

#finding correlation
cor(fmcg_corr1)
cor(fmcg_corr_price_Volume_NoS_2)
cor(fmcg_corr_priceLess1000_Volume_NoS)

#Extracting data for Volume sales and stores across three years
variant_1163_VolS_NoS=variant_1163[,c(1:10,47:118)]

Sum_Vols_2011=data.frame(1:10,0);
Sum_Vols_2012=data.frame(1:10,0);
Sum_Vols_2013=data.frame(1:10,0);

Sum_Nos_2011=data.frame(1:10,0);
Sum_Nos_2012=data.frame(1:10,0);
Sum_Nos_2013=data.frame(1:10,0);
for(i in 1:length(variant_1163_VolS_NoS[,1])){
  for(j in 1:12){
    Sum_Vols_2011$X0[i]=Sum_Vols_2011$X0[i]+variant_1163_VolS_NoS[i,j+10];
    Sum_Vols_2012$X0[i]=Sum_Vols_2012$X0[i]+variant_1163_VolS_NoS[i,j+22];
    Sum_Vols_2013$X0[i]=Sum_Vols_2013$X0[i]+variant_1163_VolS_NoS[i,j+34];
    
    Sum_Nos_2011$X0[i]=Sum_Nos_2011$X0[i]+variant_1163_VolS_NoS[i,j+46];
    Sum_Nos_2012$X0[i]=Sum_Nos_2012$X0[i]+variant_1163_VolS_NoS[i,j+58];
    Sum_Nos_2013$X0[i]=Sum_Nos_2013$X0[i]+variant_1163_VolS_NoS[i,j+70];
  }
}

variant_1163_vol_years=data.frame(Sum_Vols_2011$X0,Sum_Vols_2012$X0,Sum_Vols_2013$X0);
variant_1163_nos_years=data.frame(Sum_Nos_2011$X0,Sum_Nos_2012$X0,Sum_Nos_2013$X0);

variant_1163_all=data.frame(variant_1163[1:10],variant_1163_vol_years*100000,variant_1163_nos_years*1000);

#finding volume sold across stores over years using for loop
VolAcrossNoS_2011=data.frame(1:10,0);
for(i in 1:length(variant_1163_all[,1])){
  VolAcrossNoS_2011[,i]=variant_1163_all$Sum_Vols_2011.X0/variant_1163_all$Sum_Nos_2011.X0;
}

VolAcrossNoS_2012=data.frame(1:10,0);
for(i in 1:length(variant_1163_all[,1])){
  VolAcrossNoS_2012[,i]=variant_1163_all$Sum_Vols_2012.X0/variant_1163_all$Sum_Nos_2012.X0;
}

VolAcrossNoS_2013=data.frame(1:10,0);
for(i in 1:length(variant_1163_all[,1])){
  VolAcrossNoS_2013[,i]=variant_1163_all$Sum_Vols_2013.X0/variant_1163_all$Sum_Nos_2013.X0;
}

#finding volume sold across stores over years using vectorization
variant_1163_all$VolAcrossNoS_2011=variant_1163_all$Sum_Vols_2011.X0/variant_1163_all$Sum_Nos_2011.X0;
variant_1163_all$VolAcrossNoS_2012=variant_1163_all$Sum_Vols_2012.X0/variant_1163_all$Sum_Nos_2012.X0;
variant_1163_all$VolAcrossNoS_2013=variant_1163_all$Sum_Vols_2013.X0/variant_1163_all$Sum_Nos_2013.X0;

#
#Assuming that the question infers - Find in which year, variant -1163 had higher (volume sold/no of stores) ratio 
#i am proceeding to find the sum of Volume and no of stores for three years
#
variant_1163_all[is.na(variant_1163_all)]=0

#used the below code to convert INF data to 0, but this converts all the date in the column to 0
#variant_1163_all[is.infinite(variant_1163_all)]=0
#hence, using this code
variant_1163_all$VolAcrossNoS_2013[9]=0
variant_1163_all$VolAcrossNoS_2013[10]=0

VolAcrossNoS_sum_2011=0
for(i in 1:10){
  VolAcrossNoS_sum_2011=VolAcrossNoS_sum_2011+variant_1163_all$VolAcrossNoS_2011[i];
}

VolAcrossNoS_sum_2012=0
for(i in 1:10){
  VolAcrossNoS_sum_2012=VolAcrossNoS_sum_2012+variant_1163_all[i,18];
}

VolAcrossNoS_sum_2013=0
for(i in 1:10){
  VolAcrossNoS_sum_2013=VolAcrossNoS_sum_2013+variant_1163_all[i,19];
}

#using inbuilt function in R for finding sum of rows
Vol_Nos_2011=sum(variant_1163_all$VolAcrossNoS_2011)
Vol_Nos_2012=sum(variant_1163_all$VolAcrossNoS_2012)
Vol_Nos_2013=sum(variant_1163_all$VolAcrossNoS_2013)

#########################################################################################################
### Question 3.1
#creating a variable with volume of sales for three years
Final_sort_data=c(Vol_Nos_2011,Vol_Nos_2012,Vol_Nos_2013)

#Ordinary sorting 
temp=0;
for(i in 1:3){
  for(j in 1:3){
    if(Final_sort_data[i]>Final_sort_data[j]){
      temp=Final_sort_data[i]
      Final_sort_data[i]=Final_sort_data[j]
      Final_sort_data[j]=temp;
    }
  }
}
Final_sort_data

#creating a variable with volume of sales for three years
Final_sort_data_bub=data.frame(Vol_Nos_2011,Vol_Nos_2012,Vol_Nos_2013)
length(Final_sort_data_bub)

#Bubble sort for variant 1163 vol across stores across years
temp1=0
for(i in 1:length(Final_sort_data_bub)){
  for(j in 2:(length(Final_sort_data_bub))){
    if(Final_sort_data_bub[j-1]>Final_sort_data_bub[j]){
      temp1=Final_sort_data_bub[j-1]
      Final_sort_data_bub[j-1]=Final_sort_data_bub[j]
      Final_sort_data_bub[j]=temp1;
    }
  }
}

#NOTE: Because it is in ascending order, the data makes sense.
#Otherwise,After sorting, the values are getting sorted, but the columns aren't
#(found while trying Descending): code as below
#temp1=0
#for(i in 1:length(Final_sort_data_bub)){
#  for(j in 2:(length(Final_sort_data_bub))){
#    if(Final_sort_data_bub[j-1]<Final_sort_data_bub[j]){
#      temp1=Final_sort_data_bub[j-1]
#      Final_sort_data_bub[j-1]=Final_sort_data_bub[j]
#      Final_sort_data_bub[j]=temp1;
#    }
#  }
#}

#Displaying sorted values
Final_sort_data_bub


#Assuming, i have to find sum of all three years
#and sort the 10 items of variant-1163
Final_sort_data_bub_2=data.frame(variant_1163_all[1:10],VolAcrossNoS_2011[,1],VolAcrossNoS_2012[,1],VolAcrossNoS_2013[,1])
Final_sort_data_bub_2[is.na(Final_sort_data_bub_2)]=0
Final_sort_data_bub_2[9,13]=0
Final_sort_data_bub_2[10,13]=0


VolAcrossNoS_Years=data.frame(1:10,0);
for(i in 1:length(variant_1163_all[,1])){
  VolAcrossNoS_Years[i,2]=Final_sort_data_bub_2$VolAcrossNoS_2011...1.[i]+Final_sort_data_bub_2$VolAcrossNoS_2012...1.[i]+Final_sort_data_bub_2$VolAcrossNoS_2013...1.[i]
}

Final_for_sort=data.frame(variant_1163_all[1:10],VolAcrossNoS_Years[,2])

#Bubble sort for assumption2
temp1=0
for(i in 1:length(Final_for_sort[,1])){
  for(j in 2:(length(Final_for_sort[,1]))){
    if(Final_for_sort[j-1,11]>Final_for_sort[j,11]){
      temp1=Final_for_sort[j-1,]
      Final_for_sort[j-1,]=Final_for_sort[j,]
      Final_for_sort[j,]=temp1;
    }
  }
}

#########################################################################################################
### Question 3.2

#
#variant -1162, variant -1163, variant -1164 
#belong to m-388,subbrand-568,brand-434, segment-(1,5),benefit-5 and bodypart-1 
#and therefore, taking them as competitors
#

### Variant-1162

#filtering only variant-1162
variant_1162=subset(fmcg_data,Variant=="variant-1162")
#creating and displaying a variable that stores the Value of Sales of variant-1162 across three years
variant_1162_ValS=variant_1162[,c(6,11:46)]
variant_1162_ValS

#creating and displaying a variable that stores the Volume of Sales of variant-1162 across three years
variant_1162_VolS=variant_1162[,c(6,47:82)]
variant_1162_VolS

#creating and displaying a variable that stores the No of stores having variant-1162 across three years
variant_1162_NoS=variant_1162[,c(6,83:118)]
variant_1162_NoS

#Extracting data for Volume sales and stores across three years
variant_1162_VolS_NoS=variant_1162[,c(1:10,47:118)]

Sum_Vols_2011_1162=data.frame(1:6,0);
Sum_Vols_2012_1162=data.frame(1:6,0);
Sum_Vols_2013_1162=data.frame(1:6,0);

Sum_Nos_2011_1162=data.frame(1:6,0);
Sum_Nos_2012_1162=data.frame(1:6,0);
Sum_Nos_2013_1162=data.frame(1:6,0);
for(i in 1:length(variant_1162_VolS_NoS[,1])){
  for(j in 1:12){
    Sum_Vols_2011_1162$X0[i]=Sum_Vols_2011_1162$X0[i]+variant_1162_VolS_NoS[i,j+10];
    Sum_Vols_2012_1162$X0[i]=Sum_Vols_2012_1162$X0[i]+variant_1162_VolS_NoS[i,j+22];
    Sum_Vols_2013_1162$X0[i]=Sum_Vols_2013_1162$X0[i]+variant_1162_VolS_NoS[i,j+34];
    
    Sum_Nos_2011_1162$X0[i]=Sum_Nos_2011_1162$X0[i]+variant_1162_VolS_NoS[i,j+46];
    Sum_Nos_2012_1162$X0[i]=Sum_Nos_2012_1162$X0[i]+variant_1162_VolS_NoS[i,j+58];
    Sum_Nos_2013_1162$X0[i]=Sum_Nos_2013_1162$X0[i]+variant_1162_VolS_NoS[i,j+70];
  }
}

variant_1162_vol_years=data.frame(Sum_Vols_2011_1162$X0,Sum_Vols_2012_1162$X0,Sum_Vols_2013_1162$X0);
variant_1162_nos_years=data.frame(Sum_Nos_2011_1162$X0,Sum_Nos_2012_1162$X0,Sum_Nos_2013_1162$X0);

#this data frame has all qualitative details plus 
#vol of sales of three yrs (3 columns) and no of stores for three years (3 columns)
variant_1162_all=data.frame(variant_1162[,1:10],variant_1162_vol_years*100000,variant_1162_nos_years*1000);

#finding volume sold across stores over years using vectorization
variant_1162_all$VolAcrossNoS_2011=variant_1162_all$Sum_Vols_2011_1162.X0/variant_1162_all$Sum_Nos_2011_1162.X0;
variant_1162_all$VolAcrossNoS_2012=variant_1162_all$Sum_Vols_2012_1162.X0/variant_1162_all$Sum_Nos_2012_1162.X0;
variant_1162_all$VolAcrossNoS_2013=variant_1162_all$Sum_Vols_2013_1162.X0/variant_1162_all$Sum_Nos_2013_1162.X0;

#Assigning 0 to all NA values
variant_1162_all[is.na(variant_1162_all)]=0

#Data frame having only the metric Volume/no of stores
variant_1162_VolNos=variant_1162_all[c(1:10,17:19)]

#using inbuilt function in R for finding sum of rows
Vol_Nos_2011_1162=sum(variant_1162_all$VolAcrossNoS_2011)
Vol_Nos_2012_1162=sum(variant_1162_all$VolAcrossNoS_2012)
Vol_Nos_2013_1162=sum(variant_1162_all$VolAcrossNoS_2013)

#creating a variable with volume of sales for three years
Final_sort_data_1162=data.frame("Variant-1162",Vol_Nos_2011_1162,Vol_Nos_2012_1162,Vol_Nos_2013_1162)



### Variant-1164

#filtering only variant-1164
variant_1164=subset(fmcg_data,Variant=="variant-1164")
#creating and displaying a variable that stores the Value of Sales of variant-1164 across three years
variant_1164_ValS=variant_1164[,c(6,11:46)]
variant_1164_ValS

#creating and displaying a variable that stores the Volume of Sales of variant-1164 across three years
variant_1164_VolS=variant_1164[,c(6,47:82)]
variant_1164_VolS

#creating and displaying a variable that stores the No of stores having variant-1164 across three years
variant_1164_NoS=variant_1164[,c(6,83:118)]
variant_1164_NoS

#Extracting data for Volume sales and stores across three years
variant_1164_VolS_NoS=variant_1164[,c(1:10,47:118)]

Sum_Vols_2011_1164=data.frame(1:7,0);
Sum_Vols_2012_1164=data.frame(1:7,0);
Sum_Vols_2013_1164=data.frame(1:7,0);

Sum_Nos_2011_1164=data.frame(1:7,0);
Sum_Nos_2012_1164=data.frame(1:7,0);
Sum_Nos_2013_1164=data.frame(1:7,0);
for(i in 1:length(variant_1164_VolS_NoS[,1])){
  for(j in 1:12){
    Sum_Vols_2011_1164$X0[i]=Sum_Vols_2011_1164$X0[i]+variant_1164_VolS_NoS[i,j+10];
    Sum_Vols_2012_1164$X0[i]=Sum_Vols_2012_1164$X0[i]+variant_1164_VolS_NoS[i,j+22];
    Sum_Vols_2013_1164$X0[i]=Sum_Vols_2013_1164$X0[i]+variant_1164_VolS_NoS[i,j+34];
    
    Sum_Nos_2011_1164$X0[i]=Sum_Nos_2011_1164$X0[i]+variant_1164_VolS_NoS[i,j+46];
    Sum_Nos_2012_1164$X0[i]=Sum_Nos_2012_1164$X0[i]+variant_1164_VolS_NoS[i,j+58];
    Sum_Nos_2013_1164$X0[i]=Sum_Nos_2013_1164$X0[i]+variant_1164_VolS_NoS[i,j+70];
  }
}

variant_1164_vol_years=data.frame(Sum_Vols_2011_1164$X0,Sum_Vols_2012_1164$X0,Sum_Vols_2013_1164$X0);
variant_1164_nos_years=data.frame(Sum_Nos_2011_1164$X0,Sum_Nos_2012_1164$X0,Sum_Nos_2013_1164$X0);

#this data frame has all qualitative details plus 
#vol of sales of three yrs (3 columns) and no of stores for three years (3 columns)
variant_1164_all=data.frame(variant_1164[,1:10],variant_1164_vol_years*100000,variant_1164_nos_years*1000);

#finding volume sold across stores over years using vectorization
variant_1164_all$VolAcrossNoS_2011=variant_1164_all$Sum_Vols_2011_1164.X0/variant_1164_all$Sum_Nos_2011_1164.X0;
variant_1164_all$VolAcrossNoS_2012=variant_1164_all$Sum_Vols_2012_1164.X0/variant_1164_all$Sum_Nos_2012_1164.X0;
variant_1164_all$VolAcrossNoS_2013=variant_1164_all$Sum_Vols_2013_1164.X0/variant_1164_all$Sum_Nos_2013_1164.X0;

#Assigning 0 to all NA values
variant_1164_all[is.na(variant_1164_all)]=0

#Data frame having only the metric Volume/no of stores
variant_1164_VolNos=variant_1164_all[c(1:10,17:19)]

#using inbuilt function in R for finding sum of rows
Vol_Nos_2011_1164=sum(variant_1164_all$VolAcrossNoS_2011)
Vol_Nos_2012_1164=sum(variant_1164_all$VolAcrossNoS_2012)
Vol_Nos_2013_1164=sum(variant_1164_all$VolAcrossNoS_2013)

#creating a variable with volume of sales for three years
Final_sort_data_1164=data.frame("Variant-1164",Vol_Nos_2011_1164,Vol_Nos_2012_1164,Vol_Nos_2013_1164)

Final_sort_data_1163=data.frame("Variant-1163",Vol_Nos_2011,Vol_Nos_2012,Vol_Nos_2013)

Final_1162=data.frame(Final_sort_data_1162$Vol_Nos_2011_1162+Final_sort_data_1162$Vol_Nos_2012_1162+Final_sort_data_1162$Vol_Nos_2013_1162)
Final_1164=data.frame(Final_sort_data_1164$Vol_Nos_2011_1164+Final_sort_data_1164$Vol_Nos_2012_1164+Final_sort_data_1164$Vol_Nos_2013_1164)
Final_1163=data.frame(Final_sort_data_1163$Vol_Nos_2011+Final_sort_data_1163$Vol_Nos_2012+Final_sort_data_1163$Vol_Nos_2013)

Final=data.frame(Final_1162,Final_1163,Final_1164)
colnames(Final)=c("Variant-1162","Variant-1163","Variant-1164")
sort(Final)
length(Final)

#Bubble sort for three variants, but using this method, the values are sorted, but not the column names
temp1=0
for(i in 1:length(Final)){
  for(j in 2:(length(Final))){
    if(Final[j-1]>Final[j]){
      temp1=Final[j-1]
      Final[j-1]=Final[j]
      Final[j]=temp1;
    }
  }
}

Final_1162_1=(Final_sort_data_1162$Vol_Nos_2011_1162+Final_sort_data_1162$Vol_Nos_2012_1162+Final_sort_data_1162$Vol_Nos_2013_1162)
Final_1164_1=(Final_sort_data_1164$Vol_Nos_2011_1164+Final_sort_data_1164$Vol_Nos_2012_1164+Final_sort_data_1164$Vol_Nos_2013_1164)
Final_1163_1=(Final_sort_data_1163$Vol_Nos_2011+Final_sort_data_1163$Vol_Nos_2012+Final_sort_data_1163$Vol_Nos_2013)

#
#to sort the column names also
#
Final_values=c(Final_1162_1,Final_1163_1,Final_1164_1)
Final_Names=c("Variant-1162","Variant-1163","Variant-1164")
Final_2=data.frame(Final_Names,Final_values)

#Bubble sort
temp1=0
for(i in 1:length(Final_2[,2])){
  for(j in 2:(length(Final_2[,2]))){
    if(Final_2[j-1,2]>Final_2[j,2]){
      temp1=Final_2[j-1,]
      Final_2[j-1,]=Final_2[j,]
      Final_2[j,]=temp1;
    }
  }
}



#########################################################################################################
### Question 3.3
# When to use for,while, repeat

#For Loop:
# For loop is used to iterate a finite number of times 
# i.e., when we know how many number of times we want to run a loop
# for example : Print all even numbers between 1 to 10

for(i in 1:10){
  if(i%%2==0){
  print(i)
  }
}


#While Loop:
#While loop is used to iterate when the number of iterations is not known
#i.e., when we do not know how many times the loop needs to run
# for example: Print yes as long as x is less than 5 

x=1
while(x<=5)
{
  x=x+1
  print("YES")
}


#Repeat loop:
#Repeat loop is very similar to while loop except that 
#Repeat loop executes executes atleast once regardless of the condition

y=3
repeat {
  print(y)
  y= y-1
  if(y!=2){
    break
  }
}


#########################################################################################################
###Question 3.4
#vectorization and loops
#Vectorization is used to find the sum of columns
#But,using vectorization, we cannot find the sum of rows
#Here is where for loops are used.
#For example
x=c(2,4,6,8)
y=c(1,3,5,7)
z=data.frame(x,y)

#vectorization (adding columns)
z1=z$x/z$y;
z1

#for loops (adding rows)
z2=0
for(i in 1:4){
  z2[i]=x[i]+y[i]
}



#########################################################################################################
### Question 4
#loading libraries for plotting graphs
library(ggplot2)
library(UsingR)

#finding the distribution for Value of sales
ValS=as.numeric(SumVals1$X0)
hist(ValS,main="Value of sales Histogram",xlab="Value of sales")
qqnorm(ValS)
qqline(ValS)
val=rgamma(100,1)
qqnorm(val)
qqline(val)

#finding the distribution for Volume of sales
VolS=as.numeric(SumVols2$X0)
hist(VolS,main="Volume of sales Histogram",xlab="Volume of sales")
qqnorm(VolS)
qqline(VolS)
vol=rgamma(100,1)
qqnorm(vol)
qqline(vol)

#finding the distribution for Number of stores
NoS=as.numeric(SumNoS1$X0)
hist(NoS,main="Number of stores Histogram",xlab="Number of stores")
qqnorm(NoS)
qqline(NoS)
nos=rgamma(100,1)
qqnorm(nos)
qqline(nos)

Vol_vs_Val<-ggplot(fmcg_corr1,aes(SumVals1...2.,SumVols2...2.))
Vol_vs_Val
Vol_vs_Val+geom_point()
Vol_vs_Val+geom_point(aes(size=SumNoS1...2.))


price_vs_Vol_NoS<-ggplot(fmcg_corr_priceLess1000_Volume_NoS,aes(Price1...2.,SumVols2...2.))
price_vs_Vol_NoS
price_vs_Vol_NoS+geom_point()
price_vs_Vol_NoS+geom_point(aes(size=SumNoS1...2.))

clusters_Vol <- hclust(dist(fmcg_corr_price_Volume_NoS_2[,2]))
plot(clusters_Vol)

set.seed(20)
KCluster_vol <- kmeans(fmcg_corr_price_Volume_NoS_2[,2:3],3,nstart = 20)
KCluster_vol

KCluster_vol$cluster <- as.factor(KCluster_vol$cluster)
ggplot(fmcg_corr_price_Volume_NoS_2, aes(fmcg_corr_price_Volume_NoS_2[,2],fmcg_corr_price_Volume_NoS_2[,3], color = KCluster_vol$cluster)) + geom_point()

barplot(Final_values,names=Final_Names,col=rainbow(3))

###Competitor analysis
#For finding competitors for variant-1163, 
#For the same benefit,bodypart,manufacturer, brand, subbrand,segment, 
#variant -1163 has one competitor variant 1164
Competitors=subset(fmcg_data,Manufacturer=="M-388" & Subbrand=="Subbrand-568" & Segment=="segment-1" & Primarybenefit=="benefit-5" & Bodypart=="body.part-1")

Competitors_val=data.frame(1:17,0);
Competitors_vol=data.frame(1:17,0);
Competitors_Nos=data.frame(1:17,0);

for(j in 1:17){
  for(i in 11:46){
  Competitors_val[j,2]=Competitors_val[j,2]+Competitors[j,i]
  Competitors_vol[j,2]=Competitors_vol[j,2]+Competitors[j,i+36]
  Competitors_Nos[j,2]=Competitors_Nos[j,2]+Competitors[j,i+72]
  }
}

Competitors_Val_1=data.frame(Competitors[,6],Competitors_val[,2])
colnames(Competitors_Val_1)=c("Variant","ValueSales")

Competitors_Vol_1=data.frame(Competitors[,6],Competitors_vol[,2])
colnames(Competitors_Vol_1)=c("Variant","VolumeSales")

Competitors_Nos_1=data.frame(Competitors[,6],Competitors_Nos[,2])
colnames(Competitors_Nos_1)=c("Variant","No of stores")

Competitors_val_sum_1163=0
Competitors_val_sum_1163=0
Competitors_Val_1163=subset(Competitors_Val_1,Variant="variant-1163")
Competitors_Val_1164=subset(Competitors_Val_1,Variant="variant-1164")

for(i in 1:17){
  Competitors_val_sum_1163=Competitors_val_sum_1163+Competitors_Val_1[i,2]
}
barplot(Competitors_Val_1$ValueSales,col=c("darkblue","red"),main="Value sales of variant 1163 and its competitor variant 1164",xlab = "Value of sales",legend.text = c("Variant-1163","Variant-1164"),horiz = TRUE)

for(i in 1:17){
  Competitors_vol_sum_1163=Competitors_vol_sum_1163+Competitors_Vol_1[i,2]
}
barplot(Competitors_Vol_1$VolumeSales,col=c("darkblue","red"),main="Volume Sales of variant 1163 and its competitor variant 1164",xlab = "Value of sales",legend.text = c("Variant-1163","Variant-1164"),horiz = TRUE)

for(i in 1:17){
  Competitors_Nos_sum_1163=Competitors_Nos_sum_1163+Competitors_Nos_1[i,2]
}
barplot(Competitors_Nos_1$`No of stores`,col=c("darkblue","red"),main="No of stores of variant 1163 and its competitor variant 1164",xlab = "Value of sales",legend.text = c("Variant-1163","Variant-1164"),horiz = TRUE)
