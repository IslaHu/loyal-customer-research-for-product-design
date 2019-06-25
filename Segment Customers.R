rm(list = ls())
setwd("~/Desktop/MKT Research/Case5 - Flavor")
itemAttributes <- read.table('itemsAttributes(1).txt', header = T)
survItemSales <- read.table('survItemSales.txt', header = T)
storeItemSales <- read.table('storeItemSales.txt', header = T)
survQuestions <- read.table('survQuestions.txt', sep = "", header = T)
hshldDemograph <- read.table('hshldDemograph.txt', header = T)
survResponses <- read.table('survResponses.txt', header = T)
randItemSales <- read.table('randItemSales.txt', header = T)
names(itemAttributes) <- c('ItemNum', 'Brand', 'zerofat', 'lowfat', 'highfat', 'organic', 'allnatural', 'probiotic', 'fiber', 'fob', 'SBScup', 'NCITcup', 'Flavor1')
names(randItemSales) <- c("ItemNum", "HouseholdNum", "TransactionNum", "Date", "StoreNum", "visitID", "Units", "Coupon", "Sales")
names(storeItemSales) <- c("ItemNum", "StoreNum", "Week_Ending", "Units", "Sales")
names(survItemSales) <- c("ItemNum", "HouseholdNum", "UserID", "TransactionNum", "Date", "StoreNum", "visitID", "Units", "Coupon", "Sales")
names(hshldDemograph) <- c("HouseholdNum", "Income", "FamilySize", "NumAdults", "RandData", "IncomeBin", "FSBin", "AdultBin")

## ---------- Current Purchase Data - find out GY -----------------
## 1.which brand has Greek Yogurt?
# Brands that produce GY: DANNON, YO (Should be Yoplait), FAGE, CHOBANI, OIKOS, STONYFIELD, LIBERTE, YOGREEK, MAPLE HILL
# Brands that do not produce GY:
# YOCRUNCH: No, we no longer make YoCrunch Greek yogurt
# REDWOOD: goat yogurt
# BIO-K: probiotic yogurt
# OLDCHATHAM: black sheep yogurt
GyBrand <-  c('DANNON', 'FAGE', 'YO', 'CHOBANI', 'OIKOS',
              'STONYFIELD', 'LIBERTE', 'YOGREEK', 'MAPLE HILL')
itemAttributes$isGyBrand <- itemAttributes$Brand %in% GyBrand

## 2.find out purchase recording of Greek Yogurt according to Brand + Price
# "The national average price of 4-6 ounce Greek yogurt is $1 - $1.5"
# Source: https://www.dairyfoods.com/articles/90440-consumers-pay-the-price-for-greek-yogurt-retail-prices-up-156-in-2-weeks-for-a-32-ounce-package
randItemSales$Price <- (randItemSales$Sales + randItemSales$Coupon) / randItemSales$Units
randItemSales$isGyPrice <- randItemSales$Price >= 1 & randItemSales$Price <= 2

survItemSales$Price <- (survItemSales$Sales + survItemSales$Coupon) / survItemSales$Units
survItemSales$isGyPrice <- survItemSales$Price >= 1 & survItemSales$Price <= 2

## ------------- Survey Data - Cluster - Failed ---------------
library(cluster) 
library(fpc)
library(sqldf)
## 3.partition consumers into different segmentations
set.seed(17)
toclust = survResponses[ , 7:14]
toclust[is.na(toclust)] <- 0
wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
plot(1:15, wss/wss[1], type="b", xlab="Number of Clusters",
     ylab="% Variation Explained")
# determine the optimal number of clusters 
pm1 = pamk(toclust,scaling=TRUE)
pm1$nc  # 10........

## ------------ Survey Participants' Current Data - Cluster - Success ------------------
toclustOrig <- sqldf("select UserID, avg(zerofat), avg(lowfat), avg(highfat), avg(organic), avg(allnatural), avg(probiotic), avg(fiber), avg(fob), avg(SBScup), avg(NCITcup)
                 from survItemSales join itemAttributes on survItemSales.ItemNum = itemAttributes.ItemNum
                 where isGyPrice = 1 and isGyBrand = 1
                 group by UserID")
toclust <- toclustOrig[ ,-1]
set.seed(17)
toclust[is.na(toclust)] <- 0
wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
plot(1:15, wss/wss[1], type="b", xlab="Number of Clusters",
     ylab="% Variation Explained")
# determine the optimal number of clusters 
pm1 = pamk(toclust,scaling=TRUE)
pm1$nc  # 3

## so we split consumers into 3 groups 
km = kmeans(toclust,3,iter.max = 20, nstart=2)  
toclustOrig$Groups <- km$cluster

percsize = paste(1:3," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
pie(km$size,labels=percsize)

# plot the clusters  ////////???????
plotcluster(toclust, km$cluster)
km

## ---------------- Survey Response - flover preference of each group - Failed-----------------
colnames(survResponses)[15:37] <- c("Almond","Banana","Black Cherry","Blueberry","Caramel","Chai","Chocolate","Cinnamon","Coconut",
                                    "Honey","Key Lime Pie","Lemon","Mango","Maple","Peach","Pineapple","Plain","Pomegranate","Raspberry",
                                    "Strawberry","Strawberry Banana","Vanilla","Vanilla Banana")
SegFlavor <- sqldf("select ID, Groups, Almond,Banana,`Black Cherry`,Blueberry,Caramel,Chai,Chocolate,Cinnamon,Coconut,
                        Honey,`Key Lime Pie`,Lemon,Mango,Maple,Peach,Pineapple,Plain,Pomegranate,Raspberry,
                        Strawberry,`Strawberry Banana`,Vanilla,`Vanilla Banana` 
                   from toclustOrig join survResponses on toclustOrig.UserID = survResponses.ID")
write.csv(SegFlavor, file = "Survey-Segments-Flavors-Q12.csv")

## 0 (Regularly), 1 (Occasionally), 2 (Never)
library(RPostgreSQL)
library(sqldf)
SegFlavor[is.na(SegFlavor)] <- 2
sqldf("select Groups, Almond, count(*) over(order by Groups) from SegFlavor")

## ------------------ Segment Flavor -----------------
SegFlavor <- read_csv("Segments-final data.csv")
f0 <- function(x) sum(x==0)/length(x)
Flavor0 <- aggregate(.~Groups, data = SegFlavor[,-2:-1], f0)
f1 <- function(x) sum(x==1)/length(x)
Flavor1 <- aggregate(.~Groups, data = SegFlavor[,-2:-1], f1)
f2 <- function(x) sum(x==1)/length(x)
Flavor2 <- aggregate(.~Groups, data = SegFlavor[,-2:-1], f2)

SegFla <- rbind(Flavor0, Flavor1, Flavor2)
SegFla$Choose <- c(0,0,0,1,1,1,2,2,2)

write.csv(SegFla, file = "SegFla.csv")





