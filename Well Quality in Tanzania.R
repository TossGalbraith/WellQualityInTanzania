library(lubridate)
library(multcompView)
library(arules)
library(discretization)
library(regclass)


#Data dictionary:  https://www.drivendata.org/competitions/7/pump-it-up-data-mining-the-water-table/page/25/

WATER <- read.csv("watertrain.csv",stringsAsFactors = TRUE)
LABELS <- read.csv("waterlabels.csv",stringsAsFactors = TRUE)
WATER <- merge(WATER,LABELS,by="id")


##Cleaning of data:
#Narrow scope to what kind of wells to consider
#Eliminate identifier variables
#Transform numerical variables to symmetry
#Discretize numerical variables into sets of categories when necessary
#Handle dates
#Combine rare levels

#Consider only functional and non-functional wells by removing the functional needs repair level
WATER <- droplevels( subset(WATER,status_group!="functional needs repair")) 

#Rename levels to good and bad
levels( WATER$status_group ) <- c("Good","Bad")

WATER$id <- NULL  #id is an identifier variable
hist(WATER$amount_tsh)
summary(WATER$amount_tsh)
hist(log10( 1 + WATER$amount_tsh) )
WATER$amount_tsh <- log10(WATER$amount_tsh+1)  #amount_tsh is highly skewed, with a minimum of 0, so look at log of 1+value

#I think the 0s indicate "unknown", creating categories here to represent range of amount_tsh; it is not specified in data dictionary
plot(status_group ~ amount_tsh, data=WATER)
x <- WATER$amount_tsh
summary( cut(x,c(-1,0,0.5,1.5,2,2.5,3,3.5,3.75,10)) )
x <- cut(x,c(-1,0,0.5,1.5,2,2.5,3,3.5,3.75,10))
levels(x) <- c("Unknown","LessThan0.5","0.5to1.5","1.5to2","2to2.5","2.5to3","3to3.5","3.5to3.75","3.75And")
WATER$amount_tsh <- x


WATER$date_recorded <- ymd(WATER$date_recorded)  #Convert this field into an actual date
WATER$date_recorded <- year( WATER$date_recorded ) #Consider only the year the well was built
WATER <- droplevels( subset(WATER,date_recorded>=2011) )  #Consider only wells whose status was recorded in 2011 and later (goes out to 2013)
WATER$date_recorded <- NULL   #Get rid of date recorded



#Funder

tail(sort(table(WATER$funder)),20)  #Look at 20 most common values
#Issues: there is a "blank" level, and a level that is "0", which might represent unknown?  Would need to ask data providers
x <- combine_rare_levels(WATER$funder,threshold=599)$values  #Combine all levels that appear 599 times or fewer
levels(x) #Look at values of this variable.  rename blank
levels(x)[which(levels(x) %in% c("")) ] <- "Unknown"
levels(x)[which(levels(x) %in% c("Combined")) ] <- "Other"
WATER$funder <- x  #replace values in funder with combined levels

summary(WATER$gps_height)
mean(WATER$gps_height==0)
#Issue: a value of 0 means "unknown", not "0".  To "fix" this, need to convert to categorical variable where 0 means Unknown
gps.disc <- c()
x <- WATER$gps_height
table( discretize(x,breaks=8,method="cluster") )
cats <- cut(x,c(-100,-0.5,0,250,500,750,1000,1500,1750,2000,3000) )
levels(cats)[1] <- "LessThan0"
levels(cats)[2] <- "0orUnknown"
summary(cats)
WATER$gps_height <- cats
mosaic(status_group ~ gps_height,data=WATER,inside=TRUE,equal=TRUE)



# installer

tail(sort(table(WATER$installer)),20)  #20 most common values
#Issues:  District Council has two different spellings.  Is Commu the same as Community ?  There is blank and 0.
#Is DANIDA different than DANID? Need to ask data provider.  Only fixing the blank level
x <- combine_rare_levels(WATER$installer,threshold=700)$values  #Combine all levels that appear 700 times or fewer
levels(x)
levels(x)[which(levels(x) %in% c("")) ] <- "Unknown"
levels(x)[which(levels(x) %in% c("Combined")) ] <- "Other"
WATER$installer <- x



#All locations in Tanzania have longitude > 0 and latitude < 0, but sometimes data has wrong values
#Get rid of rows having bogus locations
#Then, delete latitude/longitude since there is other informative geographic info (gps height, region, basin, etc.)
WATER <- droplevels( subset(WATER,longitude > 0 & latitude < 0 ) )
WATER$latitude <- NULL
WATER$longitude <- NULL


#Get rid of name because most are unique, and not much representation of levels that appear more than once
tail( sort(table(WATER$wpt_name)),20 )
WATER$wpt_name <- NULL


#num_private is mostly 0
hist(WATER$num_private)
summary(WATER$num_private)
mean(WATER$num_private==0)
WATER$num_private <- factor( ifelse(WATER$num_private==0,"Zero","NonZero") )


#Issues with basin:  spaces and / can cause issues in names; replace them with nothing, i.e. ""
levels(WATER$basin)
levels(WATER$basin) <- gsub(" ","",levels(WATER$basin))
levels(WATER$basin) <- gsub("/","",levels(WATER$basin))
levels(WATER$basin)



#subvillage

tail(sort(table(WATER$subvillage)),20)  #Look at 20 most common values

#Issues:  there's an "I", a "1", a "blank", and an "M".  Should consult data providers.  Combine levels and fix the blank
x <- combine_rare_levels(WATER$subvillage,threshold=130)$values
levels(x)
levels(x)[which(levels(x) %in% c("")) ] <- "Unknown"
levels(x)[which(levels(x) %in% c("Combined")) ] <- "Other"
summary(x)
WATER$subvillage <- x
nlevels(WATER$subvillage)

#region has spaces in the name, so getting rid of them
levels(WATER$region) <- gsub(" ","",levels(WATER$region))


#region code, district code, lga, and ward are more or less redundant with other columns in data; get rid of them
WATER$region_code <- NULL
WATER$district_code <- NULL
WATER$lga <- NULL
WATER$ward <- NULL

#population is skewed, but minimum value is 0 (out in the middle of nowhere, or maybe not recorded?) 
#so considering log of 1 + value
hist(WATER$population)
summary(WATER$population)
hist(  log10(WATER$population+1)  )

#Make categories, because I think 0 is unknown
x <- WATER$population
summary(discretize(x,breaks=12))
summary( cut(x,c(-1,0,1,100,150,250,350,500,50000)))
x <- cut(x,c(-1,0,1,100,150,250,350,500,50000))
levels(x) <- c("Unknown","Just1","Upto100","100to150","150to250","250to350","350to500","500andMore")
WATER$population <- x



#public_meeting:  True, False, and unknown represented as a blank.  Rename blank to Unknown
levels(WATER$public_meeting)
levels(WATER$public_meeting)[1] <- "Unknown"

#Same value for all entries in recorded by
WATER$recorded_by <- NULL

#scheme name is redundant with scheme management; delete
WATER$scheme_name <- NULL

#scheme management has a blank and unknown; combine those and also get rid of spaces
levels(WATER$scheme_management)
levels(WATER$scheme_management)[c(1,3)] <- "Unknown"
levels(WATER$scheme_management) <- gsub(" ","",levels(WATER$scheme_management))



#permit is true, false, and unknown (but with a blank); change blank to unknown
levels(WATER$permit)
levels(WATER$permit)[1] <- "Unknown"


#construction year. VERY IMPORTANT VARIABLE, but it has issues since MANY values are 0 (unknown)
#Need to make this a categorical variable, giving 0 a level called unknown, but this forces categories for construction year. I'll arbitrarily do this by decade
summary(WATER$construction_year)
cons.disc <- c()
x <- WATER$construction_year
summary( cut(x,c(0,1959,1969,1979,1989,1999,2009,2019),include.lowest = TRUE,
             labels=c("Unknown","1960s","1970s","1980s","1990s","2000s","2010s")) )

WATER$construction_year <- cut(x,c(0,1959,1969,1979,1989,1999,2009,2019),include.lowest = TRUE,
                               labels=c("Unknown","1960s","1970s","1980s","1990s","2000s","2010s"))

mosaic( status_group ~ construction_year, data=WATER,inside=TRUE,equal=TRUE)
summary(WATER$construction_year)


#extraction_type and type_group are redundant with type_class; I'm going to get rid of them then remove space and - in levels of extraction_type_class
WATER$extraction_type <- NULL
WATER$extraction_type_group <- NULL
levels(WATER$extraction_type_class) <- gsub(" ","",levels(WATER$extraction_type_class))
levels(WATER$extraction_type_class) <- gsub("-","",levels(WATER$extraction_type_class))


#management group and management are redundant; get rid of spaces and dash in management
WATER$management_group <- NULL
levels(WATER$management) <- gsub(" ","",levels(WATER$management))
levels(WATER$management) <- gsub("-","",levels(WATER$management))


#payment_type and payment redundant.  Get rid of spaces in payment
WATER$payment_type <- NULL
levels(WATER$payment) <- gsub(" ","",levels(WATER$payment))

#quality_group, water_quality redundant, get rid of one
WATER$water_quality <- NULL
levels(WATER$quality_group)  #LOOKS FINE!

#quantity_group and quantity redundant
WATER$quantity_group <- NULL
levels(WATER$quantity) #LOOKS FINE!

#source, source_type, and source_class redundant.  Choose one and fix levels by giving a few new names
WATER$source <- NULL
levels(WATER$source_type)
levels(WATER$source_type)[c(4,5,6)] <- c("rainwater","riverlake","shallowwell")
WATER$source_class <- NULL

#waterpoint_type has some rare levels, and some spaces in level names; lump dam in with other
table(WATER$waterpoint_type)
levels(WATER$waterpoint_type) <- c("cattletrough","communalstandpipe","communalstandpipemultiple","other","handpump","improvedspring","other")
table(WATER$waterpoint_type)
#waterpoint_type_group is redundant
WATER$waterpoint_type_group <- NULL


#Get rid of unused levels R is keeping record of, then get rid of any row missing values (there aren't any)
WATER <- droplevels(WATER)
mean(complete.cases(WATER))
WATER <- droplevels(WATER[complete.cases(WATER),])

cats <- which( unlist(lapply(WATER,function(x)tail(class(x),1))) == "factor" )
unlist( lapply( WATER[,cats],function(x) min(table(x))) )


mosaic(status_group ~ amount_tsh, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ funder, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ gps_height, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ installer, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ num_private, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ basin, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ subvillage, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ region, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ population, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ public_meeting, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ scheme_management, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ permit, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ construction_year, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ extraction_type_class, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ management, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ payment, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ quality_group, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ quantity, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ source_type, data=WATER,inside=TRUE,equal=TRUE)
mosaic(status_group ~ waterpoint_type, data=WATER,inside=TRUE,equal=TRUE)



#Fit a model (vanilla partition here) on the *entirety* of the data to get an idea of which variables are "important" in predicting the well's functionality status. 
TREE <- rpart(status_group ~ ., data=WATER)
summarize_tree(TREE)



#Investigate quantity


#Step 1: accounting for lurking variables fitting a model
train.rows <- sample(1:nrow(WATER),size=0.7*nrow(WATER))
TRAIN <- WATER[train.rows,]
HOLDOUT <- WATER[-train.rows,]

#Logistic regression model, everything but quantity as predictors
M <- glm( status_group ~ . -quantity, data=TRAIN, family="binomial" )



#Step 2: making predictions for what bad well probabilities should be
predictions <- predict(M,newdata=HOLDOUT,type="response")

#Add to HOLDOUT data the predicted probability
HOLDOUT$PredictedProbability <- predictions

#Actual proportion of bad wells for quantity (naive comparison)
SUMMARY <- aggregate(  status_group=="Bad" ~ quantity , data=HOLDOUT, FUN=mean)
names(SUMMARY)[2] <- "Actual"
AOV <- aov(status_group=="Bad" ~ quantity, data=HOLDOUT)
TUKEY <- TukeyHSD(AOV)
multcompLetters4(AOV,TUKEY)
LETTERS <- multcompLetters4(AOV,TUKEY)
SUMMARY$letters.naivecomparison <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]

#Average predicted probability for each quantity (add this to the "SUMMARY" dataframe)
aggregate(PredictedProbability ~ quantity, data=HOLDOUT, FUN=mean)
SUMMARY$Predicted <- aggregate(PredictedProbability ~ quantity, data=HOLDOUT, FUN=mean)[,2]

#Difference between the actual proportion of bad wells for each basin and the predicted probability for each basin
SUMMARY$Difference <- SUMMARY$Actual - SUMMARY$Predicted

#Add a column letting us know how many of each quantity were in the data
SUMMARY$n <- table(HOLDOUT$quantity)

#Print to the screen the result, ordering by size of the difference
SUMMARY[order(SUMMARY$Difference),]



#Step 3: looking for differences after accounting for lurking variables
HOLDOUT$Actual <- as.numeric(HOLDOUT$status_group=="Bad")
HOLDOUT$Difference <- HOLDOUT$Actual - HOLDOUT$PredictedProbability

AOV <- aov(Difference ~ quantity, data=HOLDOUT)
summary(AOV)
TUKEY <- TukeyHSD(AOV)
multcompLetters4(AOV,TUKEY)
LETTERS <- multcompLetters4(AOV,TUKEY)

#Add the connecting letters to the "SUMMARY" dataframe we have been constructing
SUMMARY$letters.actualvspred <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]

#Sort by differences between actual proportion of bad wells vs. predicted probability of bad well
SUMMARY[order(SUMMARY$Difference),]

#######################################################################################################################################################################################

#Investigate waterpoint_type


#Step 1: accounting for lurking variables fitting a model
train.rows <- sample(1:nrow(WATER),size=0.7*nrow(WATER))
TRAIN <- WATER[train.rows,]
HOLDOUT <- WATER[-train.rows,]

#Logistic regression model, everything but waterpoint_type as predictors
M <- glm( status_group ~ . -waterpoint_type, data=TRAIN, family="binomial" )



#Step 2: making predictions for what bad well probabilities should be
predictions <- predict(M,newdata=HOLDOUT,type="response")

#Add to HOLDOUT data the predicted probability
HOLDOUT$PredictedProbability <- predictions

#Actual proportion of bad wells for waterpoint_type (naive comparison)
SUMMARY <- aggregate(  status_group=="Bad" ~ waterpoint_type , data=HOLDOUT, FUN=mean)
names(SUMMARY)[2] <- "Actual"
AOV <- aov(status_group=="Bad" ~ waterpoint_type, data=HOLDOUT)
TUKEY <- TukeyHSD(AOV)
multcompLetters4(AOV,TUKEY)
LETTERS <- multcompLetters4(AOV,TUKEY)
SUMMARY$letters.naivecomparison <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]

#Average predicted probability for each waterpoint_type (add this to the "SUMMARY" dataframe)
aggregate(PredictedProbability ~ waterpoint_type, data=HOLDOUT, FUN=mean)
SUMMARY$Predicted <- aggregate(PredictedProbability ~ waterpoint_type, data=HOLDOUT, FUN=mean)[,2]

#Difference between the actual proportion of bad wells for each basin and the predicted probability for each basin
SUMMARY$Difference <- SUMMARY$Actual - SUMMARY$Predicted

#Add a column letting us know how many of each waterpoint_type were in the data
SUMMARY$n <- table(HOLDOUT$waterpoint_type)

#Print to the screen the result, ordering by size of the difference
SUMMARY[order(SUMMARY$Difference),]



#Step 3: looking for differences after accounting for lurking variables
HOLDOUT$Actual <- as.numeric(HOLDOUT$status_group=="Bad")
HOLDOUT$Difference <- HOLDOUT$Actual - HOLDOUT$PredictedProbability

AOV <- aov(Difference ~ waterpoint_type, data=HOLDOUT)
summary(AOV)
TUKEY <- TukeyHSD(AOV)
multcompLetters4(AOV,TUKEY)
LETTERS <- multcompLetters4(AOV,TUKEY)

#Add the connecting letters to the "SUMMARY" dataframe
SUMMARY$letters.actualvspred <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]

#Sort by differences between actual proportion of bad wells vs. predicted probability of bad well
SUMMARY[order(SUMMARY$Difference),]

#######################################################################################################################################################################################

#Investigate waterpoint_type


#Step 1: accounting for lurking variables fitting a model
train.rows <- sample(1:nrow(WATER),size=0.7*nrow(WATER))
TRAIN <- WATER[train.rows,]
HOLDOUT <- WATER[-train.rows,]

#Logistic regression model, everything but waterpoint_type as predictors
M <- glm( status_group ~ . -waterpoint_type, data=TRAIN, family="binomial" )



#Step 2: making predictions for what bad well probabilities should be
predictions <- predict(M,newdata=HOLDOUT,type="response")

#Add to HOLDOUT data the predicted probability
HOLDOUT$PredictedProbability <- predictions

#Actual proportion of bad wells for waterpoint_type (naive comparison)
SUMMARY <- aggregate(  status_group=="Bad" ~ waterpoint_type , data=HOLDOUT, FUN=mean)
names(SUMMARY)[2] <- "Actual"
AOV <- aov(status_group=="Bad" ~ waterpoint_type, data=HOLDOUT)
TUKEY <- TukeyHSD(AOV)
multcompLetters4(AOV,TUKEY)
LETTERS <- multcompLetters4(AOV,TUKEY)
SUMMARY$letters.naivecomparison <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]

#Average predicted probability for each waterpoint_type (add this to the "SUMMARY" dataframe)
aggregate(PredictedProbability ~ waterpoint_type, data=HOLDOUT, FUN=mean)
SUMMARY$Predicted <- aggregate(PredictedProbability ~ waterpoint_type, data=HOLDOUT, FUN=mean)[,2]

#Difference between the actual proportion of bad wells for each basin and the predicted probability for each basin
SUMMARY$Difference <- SUMMARY$Actual - SUMMARY$Predicted

#Add a column letting us know how many of each waterpoint_type were in the data
SUMMARY$n <- table(HOLDOUT$waterpoint_type)

#Print to the screen the result, ordering by size of the difference
SUMMARY[order(SUMMARY$Difference),]



#Step 3: looking for differences after accounting for lurking variables
HOLDOUT$Actual <- as.numeric(HOLDOUT$status_group=="Bad")
HOLDOUT$Difference <- HOLDOUT$Actual - HOLDOUT$PredictedProbability

AOV <- aov(Difference ~ waterpoint_type, data=HOLDOUT)
summary(AOV)
TUKEY <- TukeyHSD(AOV)
multcompLetters4(AOV,TUKEY)
LETTERS <- multcompLetters4(AOV,TUKEY)

#Add the connecting letters to the "SUMMARY" dataframe we have been constructing
SUMMARY$letters.actualvspred <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]

#Sort by differences between actual proportion of bad wells vs. predicted probability of bad well
SUMMARY[order(SUMMARY$Difference),]

