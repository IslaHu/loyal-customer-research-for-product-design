library(ggplot2)
library(wordcloud)
library(tm)
library(foreign)

rm(list = ls())
setwd("~/Desktop/MKT Research/Case - Wegmans")

# read the spss data
spssDataLab <- read.spss("Wegmans Survey 1.sav",to.data.frame=TRUE,use.value.labels=TRUE,trim_values=TRUE)
spssData <- read.spss("Wegmans Survey 1.sav",to.data.frame=TRUE,use.value.labels=FALSE)


#1.depict the portion of female vs. male
popSex = c(.87, .13) # female vs. male
sampleSex = table(spssData$Q33)[2:3] # female vs. male
chisq.test(sampleSex,p = popSex)
# p-value < .05 , which means the sample is differ from the population considering genders.
cbind(popSex, prop.table(table(spssData$Q33)[2:3]))
# weights, if reweighting
w = popSex/prop.table(table(spssData$Q33)[2:3])
wsamp = w[spssData$Q33]        

#2.analyze the average importance ratings (Q6)
# create indexing for importance variables
colnames(spssData)[47:59] = c("All natural", "Blended", "Calorie level", "Consistency", "Fat level",                                              
                              "Fruit on the bottom", "Organic", "Price", "Protein level", "rbST free",                                             
                              "Side by side cup", "Taste", "Texture")
attributes = c("All natural", "Blended", "Calorie level", "Consistency", "Fat level",                                              
               "Fruit on the bottom", "Organic", "Price", "Protein level", "rbST free",                                             
               "Side by side cup", "Taste", "Texture")
# calculate means and standard errors for importance
importMeans = colMeans(spssData[ ,attributes], na.rm = TRUE)
importSE = apply(spssData[ ,attributes], 2, sd, na.rm = TRUE) / sqrt(colSums(!is.na(spssData[ ,attributes])))
importAttri = data.frame(attributes, importMeans, importSE)

# create error bar plots for the importance variables
dodge = position_dodge(width=.75)
gp = ggplot(importAttri,aes(y=importMeans,x=attributes,ymax=importMeans+importSE,ymin=importMeans-importSE))
gp + geom_bar(position=dodge,stat="identity",col=1,fill=2,width=.75) +
        geom_errorbar(position=dodge,width=1) + labs(x="Importance", y="Mean Rating (1-7)")

# we can run t-tests on these, but with the s.e., we already have the gist
# for example, the t-test compares the importance of "All nature" and "Blended"
t.test(spssData[,attributes[1]],spssData[,attributes[2]],paired=TRUE)  

#3.examine the free text reasons for selecting a particular brand of Greek Yogurt (Q17‐19)

#4.compare the brand attribute ratings for Fage (Q24) versus Oikos (Q30) given by those who purchased a brand in the past month.
brandAttri = c("All natural", "Price", "Taste")
for(i in 1:3){
        # cat(paste("******", brandAttri[i],"for Fage (Q24)" , "******"), fill = TRUE)
        # print(colMeans(spssData[spssData[4] == "Yes", 106+i-1], na.rm = TRUE))
        # print(apply(spssData[spssData[4] == "Yes", 1+i-1], 2, sd, na.rm = TRUE))
        # cat(paste("******", brandAttri[i],"for Oikos (Q30)" , "******"), fill = TRUE)
        # print(colMeans(spssData[spssData[4] == "Yes", 132+i-1], na.rm = TRUE))
        # print(apply(spssData[spssData[4] == "Yes", 132+i-1], 2, sd, na.rm = TRUE))
        cat(paste("******", brandAttri[i],"t-test" , "******"), fill = TRUE)
        print(t.test(spssData$Question24Allnatural[spssData[4] == "Yes"],spssData$Question24Allnatural[spssData[4] == "Yes"], paired = T))
        cat(paste("******", brandAttri[i],"Chi-test" , "******"), fill = TRUE)
        print(chisq.test(spssData$Question24Allnatural[spssData[4] == "Yes"],spssData$Question24Allnatural[spssData[4] == "Yes"]))
}

#5.people who use Greek yogurt for cooking vs. others
spssData$forCook = spssData$Question20Breakfast + spssData$Question20Dessert + spssData$Question20Dinner + spssData$Question20Lunch != 0
chisq.test(prop.table(table(spssData$forCook, spssData$`All natural`), 1)) # ????????
chisq.test(spssData$forCook, spssData$`All natural`) #??????????????
t.test(spssData$`All natural`[spssData$forCook == TRUE],spssData$`All natural`[spssData$forCook == FALSE],)
chisq.test(prop.table(table(spssData$forCook, spssData$Organic), 1))
t.test(spssData$`All natural`[spssData$forCook == TRUE],spssData$Organic[spssData$forCook == FALSE],)
chisq.test(prop.table(table(spssData$forCook, spssData$`rbST free`), 1))
t.test(spssData$`All natural`[spssData$forCook == TRUE],spssData$`rbST free`[spssData$forCook == FALSE],)
# people who use Greek yogurt for cooking rate the same for the importance of all natural/organic/rbst free
chisq.test(prop.table(table(spssData$forCook, spssData$Price), 1))
t.test(spssData$`All natural`[spssData$forCook == TRUE],spssData$Price[spssData$forCook == FALSE],)
# they don't think price is more important
