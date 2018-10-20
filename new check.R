#################################### Business Understanding #####################################
# ElecKart is an e-commerce firm specialising in electronic products. Over the last one year,
# they had spent a significant amount of money in marketing.
# They are about to create a marketing budget for the next year which includes spending on commercials,
# online campaigns, and pricing & promotion strategies. 
# you have to recommend the optimal budget allocation for different marketing levers for the next year.
#################################################################################################

## loading the data available
#install.packages('DataExplorer',  dependencies = TRUE)

#install.packages("MASS") for StepAIC
#install.packages("car") for VIF

library(DataExplorer)

library(xlsx)

sales_electronics <- read.csv("ConsumerElectronics.csv")

str(sales_electronics)

#Ploting for structure of df using DataExplorer 	
plot_str(sales_electronics)


#checking for missing values in df using DataExplorer
plot_missing(sales_electronics)
# From the Graph we can know there are 0.3% missing rows for pincode, cust_id and gmv column



prod_list <- read.xlsx("Media data and other information.xlsx",1)

str(prod_list)

media_invest <- read.xlsx("Media data and other information.xlsx",2,header = T)

str(media_invest)

colnames(media_invest) <- c("Year","Month","Total Investment","TV","Digital","Sponsorship","Content Marketing","Online marketing","Affiliates",
                            "SEM","Radio","Other")

media_invest <- media_invest[-1,]


special_sale <- read.xlsx("Media data and other information.xlsx",3)

nps_score <- read.xlsx("Media data and other information.xlsx",4, header = F)


### check for missing value in the dataset available.



sum(is.na(sales_electronics$order_id))



sum(is.na(sales_electronics$order_item_id))

#0

sum(is.na(sales_electronics$deliverybdays))

#0

sum(is.na(sales_electronics[,10:12]))
#0

sum(is.na(sales_electronics[,15:20]))
#0

sum(is.na(sales_electronics$cust_id))
#4904

sum(is.na(sales_electronics$pincode))
#4904

sum(is.na(sales_electronics$gmv))
#4904

# it seems that the 4904 oreders are not taken correctly.we can also remove these orders from dataset 
# which is 0.29 % of total orders


# removed 4904 orders from the dataset.

sales_electronics_1 <- sales_electronics[-which(is.na(sales_electronics$gmv)),]

# eda

# Let's change "Charged Off" level to "1" and "COD" to "0"
sales_electronics_1$s1_fact.order_payment_type <- ifelse(sales_electronics_1$s1_fact.order_payment_type=="COD",1,0)


#remove column product_analytic_super_category.

sales_electronics_1 <- sales_electronics_1[, -15]


levels(sales_electronics_1$product_analytic_sub_category)


levels(sales_electronics_1$product_analytic_vertical)


# selecting only three sub-categories  - camera accessory, home audio and gaming accessory

sales_electronics_2 <- sales_electronics_1

library(dplyr)


sales_electronics_2 <- filter(sales_electronics_2,product_analytic_sub_category %in% c("CameraAccessory","HomeAudio", "GamingAccessory") )


# select data from july 2015 to june 2016.


sales_electronics_2$order_date<- as.character(sales_electronics_2$order_date)


sales_electronics_2$order_date<- format(as.Date(sales_electronics_2$order_date, "%Y-%m-%d"), "%Y-%m-%d")


sales_electronics_2 <- subset(sales_electronics_2, sales_electronics_2$order_date >= "2015-07-01" & sales_electronics_2$order_date <= "2016-06-30")


# creating week column for the respective date.

library(lubridate)
sales_electronics_2$week <-lubridate::week(ymd(sales_electronics_2$order_date))


# join week and year so that we will have "year-week" column.

sales_electronics_2$year_week <- paste(sales_electronics_2$Year, sales_electronics_2$week, sep = "-")

# extract year and month from order date.

library(stringr)

sales_electronics_2$year_month <- str_sub(sales_electronics_2$order_date, 1, 7)

# Creating derived metrics as "Discount".


sales_electronics_2$discount   <-  sales_electronics_2$product_mrp - sales_electronics_2$gmv


# changing values to 0 for No Delay
sales_electronics_2 <- sales_electronics_2 %>%
  mutate( 
    deliverybdays  = ifelse(deliverybdays == "\\N", 0, deliverybdays),
    deliverycdays  = ifelse(deliverycdays == "\\N", 0, deliverycdays)
  )



# Checking unique values for category columns
sapply(sales_electronics_2[c("product_analytic_category", "product_analytic_sub_category", "product_analytic_vertical")], function(x) length(unique(x)) )
#  product_analytic_category     product_analytic_sub_category     product_analytic_vertical 
#  3                                      3                            51 

# creating Special day column

for(i in 1:length(sales_electronics_2$order_date)){
  if(sales_electronics_2$order_date[i] == "2015-07-18" | sales_electronics_2$order_date[i] == "2015-07-19")
    
  { sales_electronics_2$Special_day[i] <- "1"}
  
  else if(sales_electronics_2$order_date[i] >= "2015-08-15" & sales_electronics_2$order_date[i] <= "2015-08-17") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  else if(sales_electronics_2$order_date[i] >= "2015-08-15" & sales_electronics_2$order_date[i] <= "2015-08-17") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  else if(sales_electronics_2$order_date[i] >= "2015-08-28" & sales_electronics_2$order_date[i] <= "2015-08-30") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  else if(sales_electronics_2$order_date[i] >= "2015-10-15" & sales_electronics_2$order_date[i] <= "2015-10-17") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  else if(sales_electronics_2$order_date[i] >= "2015-11-07" & sales_electronics_2$order_date[i] <= "2015-11-14") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  else if(sales_electronics_2$order_date[i] >= "2015-12-25" & sales_electronics_2$order_date[i] <= "2016-01-03") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  
  else if(sales_electronics_2$order_date[i] >= "2016-01-20" & sales_electronics_2$order_date[i] <= "2016-01-22") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  
  else if(sales_electronics_2$order_date[i] >= "2016-02-01" & sales_electronics_2$order_date[i] <= "2016-02-02") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  
  
  else if(sales_electronics_2$order_date[i] >= "2016-02-20" & sales_electronics_2$order_date[i] <= "2016-02-21") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  
  else if(sales_electronics_2$order_date[i] >= "2016-02-14" & sales_electronics_2$order_date[i] <= "2016-02-15") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  
  
  else if(sales_electronics_2$order_date[i] >= "2016-03-07" & sales_electronics_2$order_date[i] <= "2016-03-09") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  
  
  else if(sales_electronics_2$order_date[i] >= "2016-05-25" & sales_electronics_2$order_date[i] <= "2016-05-27") 
  { sales_electronics_2$Special_day[i] <- "1"}
  
  else  {
    sales_electronics_2$Special_day[i] <- "0"
    break}
}

typeof(sales_electronics_2$Special_day)
#Character

sales_electronics_2$Special_day <- as.numeric(sales_electronics_2$Special_day)


week_month_aggregate <- aggregate(sales_electronics_2[,c(7,8:12,18,19,23,24)],
                                  by = list(sales_electronics_2$year_week,sales_electronics_2$year_month,sales_electronics_2$product_analytic_sub_category,sales_electronics_2$product_analytic_vertical),mean, na.rm = T)

str(week_month_aggregate)

colnames(week_month_aggregate) <- c("year Week","year_month","product_analytic_sub_category","product_analytic_vertical", "GMV", "No.of units sold", "deliverybdays","deliverycdays","Payment type","sla","MRP","Procurement sla","discount", "special day")


week_month_aggregate <- arrange(week_month_aggregate,week_month_aggregate$`year Week`)






nps_score <-as.data.frame(t(nps_score))

colnames(nps_score) <- c("year_month", "NPS")

nps_score <- nps_score[-1,]

nps_score1 <- nps_score

typeof(nps_score1$year_month)

library(stringr)

nps_score1$year_month <- str_replace_all(nps_score1$year_month,"'","-")

typeof(nps_score1$year_month)

# check and removing missing value in nps dataframe

sum(is.na(nps_score1$year_month))

nps_score1 <- nps_score1[-which(is.na(nps_score1$year_month)), ]

# formating column in date format

nps_score1$year_month <- as.Date(paste0(nps_score1$year_month,'-01'), format("%b-%y-%d"))

nps_score1[3,1] <- "2015-09-01"

nps_score1$year_month <- str_sub(nps_score1$year_month, 1,7)

#merging nps score to week_month_aggregate

merge_data <- merge(week_month_aggregate,nps_score1, by = "year_month", all.x = T)

# creating "year_month" in media_invest dataframe.

media_invest$Month <- as.character(media_invest$Month)

media_invest$Month <- as.numeric(media_invest$Month)

media_invest$Month <- sprintf("%02d", media_invest$Month)

media_invest$year_month <- paste(media_invest$Year, media_invest$Month, sep = '-')


media_invest <- media_invest[,-c(1:2)]


# Merge the media invest values to the "merge_data".

merge_data1 <- merge(merge_data,media_invest, by = "year_month", all.x = T)

#creating a dataframe giving the times year_month appearing in dataframe "merge_data1".

library(dplyr)
cal <- arrange(as.data.frame(table(merge_data1$year_month)),Var1)

colnames(cal) <- c("year_month", "freq")

# combine the cal dataframe to merge_data1.

merge_data2 <- merge(merge_data1,cal, by = "year_month", all.x = T)

# dividing the investment value to distribute the values among the number of times "year_month" appearing in dataframe.

merge_data2$`Total Investment` <- as.numeric(as.character(merge_data2$`Total Investment`))/as.numeric(merge_data2$freq)


merge_data2$TV <- as.numeric(as.character(merge_data2$TV))/as.numeric(merge_data2$freq)
merge_data2$Digital <- as.numeric(as.character(merge_data2$Digital))/as.numeric(merge_data2$freq)
merge_data2$Sponsorship<- as.numeric(as.character(merge_data2$Sponsorship))/as.numeric(merge_data2$freq)
merge_data2$`Content Marketing` <- as.numeric(as.character(merge_data2$`Content Marketing`))/as.numeric(merge_data2$freq)
merge_data2$`Online marketing` <- as.numeric(as.character(merge_data2$`Online marketing`))/as.numeric(merge_data2$freq)
merge_data2$Affiliates <- as.numeric(as.character(merge_data2$Affiliates))/as.numeric(merge_data2$freq)
merge_data2$SEM <- as.numeric(as.character(merge_data2$SEM))/as.numeric(merge_data2$freq)
merge_data2$Radio <- as.numeric(as.character(merge_data2$Radio))/as.numeric(merge_data2$freq)
merge_data2$Other <- as.numeric(as.character(merge_data2$Other))/as.numeric(merge_data2$freq)


str(merge_data2)

library(ggplot2)

plot_histogram(merge_data2)


plot_density(merge_data2)


create_report(merge_data2)




ggplot(merge_data2, aes(x=merge_data2$year_month, y=merge_data2$`Total Investment`,colour=product_analytic_sub_category)) +geom_bar(stat = 'identity') + facet_wrap("product_analytic_sub_category")+  theme(axis.text.x = element_text(angle = 90))

ggplot(merge_data2, aes(x=merge_data2$year_month, y=merge_data2$`special day`,colour=product_analytic_sub_category)) +geom_bar(stat = 'identity') + facet_wrap("product_analytic_sub_category")+  theme(axis.text.x = element_text(angle = 90))

ggplot(merge_data2, aes(x=merge_data2$year_month, y=merge_data2$GMV,colour=product_analytic_sub_category)) +geom_bar(stat = 'identity') + facet_wrap("product_analytic_sub_category")+  theme(axis.text.x = element_text(angle = 90))

ggplot(merge_data2, aes(x=merge_data2$year_month, y=merge_data2$`No.of units sold`,colour=product_analytic_sub_category)) +geom_bar(stat = 'identity') + facet_wrap("product_analytic_sub_category")+  theme(axis.text.x = element_text(angle = 90))

ggplot(merge_data2, aes(x=merge_data2$year_month, y=merge_data2$`No.of units sold`,colour=product_analytic_sub_category)) +geom_bar(stat = 'identity') + facet_wrap("product_analytic_sub_category")+  theme(axis.text.x = element_text(angle = 90))

ggplot(merge_data2, aes(x=merge_data2$year_month, y=merge_data2$deliverybdays,colour=product_analytic_sub_category)) +geom_bar(stat = 'identity') + facet_wrap("product_analytic_sub_category")+  theme(axis.text.x = element_text(angle = 90))

ggplot(merge_data2, aes(x=merge_data2$year_month, y=merge_data2$deliverycdays,colour=product_analytic_sub_category)) +geom_bar(stat = 'identity') + facet_wrap("product_analytic_sub_category")+  theme(axis.text.x = element_text(angle = 90))


str(merge_data2)


model_df <- merge_data2

library(MASS)
library(car)


# imputing the missing value in "raio" and "other" column with 0.

model_df$Radio[which(is.na(model_df$Radio))] <- 0

model_df$Other[which(is.na(model_df$Other))] <- 0

# Scaling to the positive values.

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

range02 <- function(x){(x-mean(x))/sd(x)}


model_df[,c(5:14)] <- range01(model_df[,c(5:14)])

model_df$NPS <- as.numeric((model_df$NPS))

model_df$NPS <- range01(model_df$NPS)


# scaling the numerical attributes.

model_df[,c(16:23)] <-  range01(model_df[,c(16:23)])

model_df[,c(24:25)] <- range01(data.matrix(model_df[,c(24:25)]))


# Modelling for cameraAccessory SUBCATEGORY

cameraAccessory <- filter(model_df,product_analytic_sub_category %in% c("CameraAccessory") )


# Create the dummy variables

# For product_analytic_sub_category
dummy_1 <- data.frame(model.matrix( ~product_analytic_sub_category, data = cameraAccessory))
dummy_1<-dummy_1[,-1]

# For product_analytic_sub_category
dummy_2 <- data.frame(model.matrix( ~product_analytic_vertical, data = cameraAccessory))
dummy_2<-dummy_2[,-1]


# Combine the dummy variables and the numeric columns of model_df dataset.

model_df1 <- cbind(cameraAccessory[,c(5:25)], dummy_1,dummy_2)


cor(model_df1$GMV, model_df1$MRP)

# since MRP and GMV are higly correlated, we thus remove the MRP from dataset.

model_df1 <- model_df1[,-7]

str(model_df1)

# Divide you data in 70:30 

train1= model_df1[c(1:664),]

test1 = model_df1[c(665:948),]

# Develop the first model 

model_1 <-lm(train1$GMV~.,data=train1[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")

#-------------------------------------------------------------------------------------------


# Run the step object

step





model_2 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount + NPS + `Content Marketing` + `Online marketing` + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])

library(car)
summary(model_2)
vif(model_2)

# remove "NPS".
model_3 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount  + `Content Marketing` + `Online marketing` + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_3)
vif(model_3)

# remove product_analytic_verticalCameraBatteryCharger

model_4 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount  + `Content Marketing` + `Online marketing` + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery  + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])

summary(model_4)
vif(model_4)

# remove deliverybdays

model_5 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount  + `Content Marketing` + `Online marketing` + 
                product_analytic_verticalBinoculars  + 
                product_analytic_verticalCameraBattery  + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])

summary(model_5)
vif(model_5)

#remove  product_analytic_verticalCameraRemoteControl

model_6 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount  + `Content Marketing` + `Online marketing` + 
                product_analytic_verticalBinoculars  + 
                product_analytic_verticalCameraBattery  + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount  + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])

summary(model_6)
vif(model_6)

#remove product_analytic_verticalFilter
model_7 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount  + `Content Marketing` + `Online marketing` + 
                product_analytic_verticalBinoculars  + 
                product_analytic_verticalCameraBattery  + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount  + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])

summary(model_7)
vif(model_7)

#Remove `Online marketing`

model_8 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount  + `Content Marketing`  + 
                product_analytic_verticalBinoculars  + 
                product_analytic_verticalCameraBattery  + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount  + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_8)
vif(model_8)

# Remove product_analytic_verticalCameraMount


model_9 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount  + `Content Marketing`  + 
                product_analytic_verticalBinoculars  + 
                product_analytic_verticalCameraBattery  + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_9)


# Remove product_analytic_verticalBinoculars

model_10 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                 sla + discount  + `Content Marketing`  + 
                 
                 product_analytic_verticalCameraBattery  + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalLens + 
                 product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_10)


# Remove `Content Marketing`

model_11 <- lm(formula = train1$GMV ~ `No.of units sold` + `Payment type` + 
                 sla + discount    + 
                 
                 product_analytic_verticalCameraBattery  + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalLens + 
                 product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_11)


# Remove `No.of units sold`

model_12 <- lm(formula = train1$GMV ~  + `Payment type` + 
                 sla + discount    + 
                 
                 product_analytic_verticalCameraBattery  + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalLens + 
                 product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_12)


# Remove product_analytic_verticalCameraBattery

model_13 <- lm(formula = train1$GMV ~  + `Payment type` + 
                 sla + discount    + 
                 
                 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalLens + 
                 product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_13)


# Remove `Payment type`

model_14 <- lm(formula = train1$GMV ~  +  
                 sla + discount    + 
                 
                 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                 
                 product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                 product_analytic_verticalLens + 
                 product_analytic_verticalTeleconverter, data = train1[, -1])


summary(model_14)




Predict_1 <- predict(model_14,test1[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test1$test_GMV <- Predict_1

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test1$GMV,test1$test_GMV)
cor(test1$GMV,test1$test_GMV)^2

# In final model we get sla, discount, CameraBatteryGrip, CameraLEDLight,

# CameraTripod , ExtensionTube,Lens, Teleconverter as our driving variable in determining the GMV for
# the CameraAccessory sub category. we got training Adjusted R^2 as 0.615 and test adjusted R^2 as 0.42


# Modelling for HomeAudio SUBCATEGORY

HomeAudio <- filter(model_df,product_analytic_sub_category %in% c("HomeAudio") )


# Create the dummy variables

# For product_analytic_sub_category
dummy_3 <- data.frame(model.matrix( ~product_analytic_sub_category, data = HomeAudio))
dummy_3<-dummy_3[,-1]

# For product_analytic_sub_category
dummy_4 <- data.frame(model.matrix( ~product_analytic_vertical, data = HomeAudio))
dummy_4<-dummy_4[,-1]


# Combine the dummy variables and the numeric columns of HomeAudio dataset.

model_df1a <- cbind(HomeAudio[,c(5:25)], dummy_3,dummy_4)
str(model_df1a)


model_df1a[,c(20:107)] <- range01(data.matrix(model_df1a[,c(20:107)]))

# removing MRP due to highly correlated to GMV.

model_df1a <- model_df1a[,-7]

# Divide you data in 70:30 

train2= model_df1a[c(1:333),]

test2 = model_df1a[c(334:475),]

# Develop the first model 

model_1 <-lm(train2$GMV~.,data=train2[,-1])
summary(model_1)


#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")

#-------------------------------------------------------------------------------------------


# Run the step object

step





model_2 <- lm(formula = train2$GMV ~ `No.of units sold` + deliverybdays + 
                deliverycdays + `Payment type` + sla + `Procurement sla` + 
                discount + NPS + `Total Investment` + TV + Sponsorship + 
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

library(car)
summary(model_2)
vif(model_2)

# remove deliverybdays

model_3 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                deliverycdays + `Payment type` + sla + `Procurement sla` + 
                discount + NPS + `Total Investment` + TV + Sponsorship + 
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

summary(model_3)
vif(model_3)

# remove Sponsorship

model_4 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                deliverycdays + `Payment type` + sla + `Procurement sla` + 
                discount + NPS + `Total Investment` + TV +
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

summary(model_4)
vif(model_4)

# remove deliverycdays

model_5 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                `Payment type` + sla + `Procurement sla` + 
                discount + NPS + `Total Investment` + TV +
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

summary(model_5)
vif(model_5)

#remove  `Total Investment`

model_6 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                `Payment type` + sla + `Procurement sla` + 
                discount + NPS  + TV +
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

summary(model_6)
vif(model_6)

#remove TV

model_7 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                `Payment type` + sla + `Procurement sla` + 
                discount + NPS +
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

summary(model_7)
vif(model_7)

#remove `Procurement sla`
model_8 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                `Payment type` + sla  + 
                discount + NPS +
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

summary(model_8)
vif(model_8)

#remove  `Payment type` 
model_9 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                + sla  + 
                discount + NPS +
                `Online marketing` + product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                product_analytic_verticalSoundMixer, data = train2[, -1])

summary(model_9)
vif(model_9)

# Remove sla, NPS, `Online marketing`

model_10 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                 + 
                 discount  +
                 product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                 product_analytic_verticalDock + product_analytic_verticalFMRadio + 
                 product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                 product_analytic_verticalKaraokePlayer + product_analytic_verticalSlingBox + 
                 product_analytic_verticalSoundMixer, data = train2[, -1])


summary(model_10)
vif(model_10)

# Remove product_analytic_verticalFMRadio, product_analytic_verticalSlingBox

model_11 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                 + 
                 discount  +
                 product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                 product_analytic_verticalDock  + 
                 product_analytic_verticalHiFiSystem + product_analytic_verticalHomeAudioSpeaker + 
                 product_analytic_verticalKaraokePlayer  + 
                 product_analytic_verticalSoundMixer, data = train2[, -1])


summary(model_11)
vif(model_11)

# Remove product_analytic_verticalHomeAudioSpeaker

model_12 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                 + 
                 discount  +
                 product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                 product_analytic_verticalDock  + 
                 product_analytic_verticalHiFiSystem  + 
                 product_analytic_verticalKaraokePlayer  + 
                 product_analytic_verticalSoundMixer, data = train2[, -1])



summary(model_12)
vif(model_12)

# Remove product_analytic_verticalDock

model_13 <- lm(formula = train2$GMV ~ `No.of units sold` +  
                 + 
                 discount  +
                 product_analytic_verticalBoomBox + product_analytic_verticalDJController + 
                 
                 product_analytic_verticalHiFiSystem  + 
                 product_analytic_verticalKaraokePlayer  + 
                 product_analytic_verticalSoundMixer, data = train2[, -1])


summary(model_13)
vif(model_13)



Predict_2 <- predict(model_13,test2[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test2$test_GMV <- Predict_2

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test2$GMV,test2$test_GMV)
cor(test2$GMV,test2$test_GMV)^2

# In final model we get `No.of units sold`+discount + product_analytic_verticalBoomBox + 
# product_analytic_verticalDJController + product_analytic_verticalHiFiSystem + 
# product_analytic_verticalKaraokePlayer + product_analytic_verticalSoundMixer as our driving variable in 
# determiing the GMV for HomeAudio subcategory.
# here we have considered the basic linear model whereas we have timestamped data available we can work 
# over the adstock effect and find the driving variables in determining the GMV.
# getting 0.89 as training adjusted R^2 and 0.90 as Test data R^2.






# Modelling for GamingAccessory SUBCATEGORY

GamingAccesory <- filter(model_df,product_analytic_sub_category %in% c("GamingAccessory") )

# Create the dummy variables

# For product_analytic_sub_category
dummy_5 <- data.frame(model.matrix( ~product_analytic_sub_category, data = GamingAccesory))
dummy_5<-dummy_5[,-1]

# For product_analytic_sub_category
dummy_6 <- data.frame(model.matrix( ~product_analytic_vertical, data = GamingAccesory))
dummy_6<-dummy_6[,-1]


# Combine the dummy variables and the numeric columns of GamingAccesory dataset.

model_df1b <- cbind(GamingAccesory[,c(5:25)], dummy_5,dummy_6)


str(model_df1b)

# Divide you data in 70:30 

train3= model_df1b[c(1:506),]

test3 = model_df1b[c(507:723),]

# Develop the first model 

model_1 <-lm(train3$GMV~.,data=train3[,-1])
summary(model_1)


#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")

#-------------------------------------------------------------------------------------------


# Run the step object

step

model_2 <- lm(formula = train3$GMV ~ deliverycdays + `Payment type` + sla + 
                MRP + discount + `Total Investment` + TV + Digital + `Content Marketing` + 
                `Online marketing` + product_analytic_verticalGamePad + deliverybdays + 
                Other, data = train3[, -1])




library(car)
summary(model_2)
vif(model_2)


# remove deliverycdays
model_3 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                MRP + discount + `Total Investment` + TV + Digital + `Content Marketing` + 
                `Online marketing` + product_analytic_verticalGamePad + deliverybdays + 
                Other, data = train3[, -1])



summary(model_3)
vif(model_3)

# remove TV

model_4 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                MRP + discount + `Total Investment` + Digital + `Content Marketing` + 
                `Online marketing` + product_analytic_verticalGamePad + deliverybdays + 
                Other, data = train3[, -1])


summary(model_4)
vif(model_4)

# remove others

model_5 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                MRP + discount + `Total Investment` + Digital + `Content Marketing` + 
                `Online marketing` + product_analytic_verticalGamePad + deliverybdays
              , data = train3[, -1])


summary(model_5)
vif(model_5)

#remove  `Online marketing`

model_6 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                MRP + discount + `Total Investment` + Digital + `Content Marketing` + 
                product_analytic_verticalGamePad + deliverybdays
              , data = train3[, -1])

summary(model_6)
vif(model_6)

#remove product_analytic_verticalGamingAdapter

model_7 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                MRP + discount + `Total Investment` + `Content Marketing` + 
                product_analytic_verticalGamePad + deliverybdays
              , data = train3[, -1])


summary(model_7)
vif(model_7)

#remove `Content Marketing`

model_8 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                MRP + discount + `Total Investment` + 
                product_analytic_verticalGamePad + deliverybdays
              , data = train3[, -1])


summary(model_8)
vif(model_8)


#remove deliverybdays

model_9 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                MRP + discount + `Total Investment` + 
                product_analytic_verticalGamePad 
              , data = train3[, -1])


summary(model_9)
vif(model_9)


#remove `Total Investment`

model_10 <- lm(formula = train3$GMV ~  `Payment type` + sla + 
                 MRP + discount + 
                 product_analytic_verticalGamePad 
               , data = train3[, -1])



summary(model_10)
vif(model_10)

#remove sla

model_11 <- lm(formula = train3$GMV ~  `Payment type`  + 
                 MRP + discount + 
                 product_analytic_verticalGamePad 
               , data = train3[, -1])


summary(model_11)
vif(model_11)




Predict_3 <- predict(model_11,test3[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test3$test_GMV <- Predict_3

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test3$GMV,test3$test_GMV)
cor(test3$GMV,test3$test_GMV)^2

# In final model we get `Payment type` , MRP, discount`, product_analytic_verticalGamePad as our driving variable in determiing the GMV.
# here we have considered the basic linear model whereas we have timestamped data available we can work 
# over the adstock effect and find the driving variables in determining the GMV. Here we got Adjusted R^2 as 1
# for test and train dataset. The presence of MRP variable making it overfit. If we remove the MRP variable it
# would leads to a very low Adjusted R^2 near to Zero.

##################################################################################################
# Multiplicative , Distributive, Koyck and Multiplicative + Distributive model
##################################################################################################

################################## Multiplicative Model ##########################################


# Modelling for cameraAccessory SUBCATEGORY


str(model_df1)

# Looking at the structure of model_df1 we see that all values are positive but few columns
# are having "0"s in it, which we need to remove  from it for logarithmic transformation.

range02 <- function(x){(x-mean(x))/sd(x)}


model_df1[,c(19:106)] <- range02(data.matrix(model_df1[,c(19:106)]))

str(model_df1)

# we see that though zeros have been removed from dataset, but negative is still there I would 
# add "1" to the column so that my values comes out to be positive.

model_df1[,c(19:106)] <- model_df1[,c(19:106)] + 1

str(model_df1)

# Now dataset seems to be fine for logarithmic transformation.




# Divide you data in 70:30 

train_m= (model_df1[c(1:664),])

test_m = (model_df1[c(665:948),])


# Develop the first model 

str(train_m)

model_1 <-lm(log(train_m$GMV)~.,data=train_m[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step


plot(step)



model_2 <- lm(formula = log(train_m$GMV) ~ `No.of units sold` + deliverybdays + 
                deliverycdays + `Payment type` + sla + discount + NPS + TV + 
                Digital + `Content Marketing` + `Online marketing` + Affiliates + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter + product_analytic_verticalTelescope, 
              data = train_m[, -1])

summary(model_2)
vif(model_2)

# Remove deliverybdays


model_3 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                deliverycdays + `Payment type` + sla + discount + NPS + TV + 
                Digital + `Content Marketing` + `Online marketing` + Affiliates + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter + product_analytic_verticalTelescope, 
              data = train_m[, -1])

summary(model_3)
vif(model_3)


# Remove deliverycdays


model_4 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                `Payment type` + sla + discount + NPS + TV + 
                Digital + `Content Marketing` + `Online marketing` + Affiliates + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter + product_analytic_verticalTelescope, 
              data = train_m[, -1])

summary(model_4)
vif(model_4)


# Remove Digital


model_5 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                `Payment type` + sla + discount + NPS + TV + 
                `Content Marketing` + `Online marketing` + Affiliates + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter + product_analytic_verticalTelescope, 
              data = train_m[, -1])

summary(model_5)
vif(model_5)


# Remove Affiliates


model_5 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                `Payment type` + sla + discount + NPS + TV + 
                `Content Marketing` + `Online marketing`  + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter + product_analytic_verticalTelescope, 
              data = train_m[, -1])

summary(model_5)
vif(model_5)

# Remove TV


model_6 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                `Payment type` + sla + discount + NPS  + 
                `Content Marketing` + `Online marketing`  + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter + product_analytic_verticalTelescope, 
              data = train_m[, -1])

summary(model_6)
vif(model_6)

# Remove `Content Marketing` 


model_7 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                `Payment type` + sla + discount + NPS  + 
                `Online marketing`  + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter + product_analytic_verticalTelescope, 
              data = train_m[, -1])

summary(model_7)
vif(model_7)


# Remove product_analytic_verticalTelescope


model_8 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                `Payment type` + sla + discount + NPS  + 
                `Online marketing`  + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap + 
                product_analytic_verticalTeleconverter , 
              data = train_m[, -1])

summary(model_8)
vif(model_8)


# Remove product_analytic_verticalTeleconverter


model_9 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                `Payment type` + sla + discount + NPS  + 
                `Online marketing`  + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter + 
                product_analytic_verticalLens + product_analytic_verticalStrap  
              , 
              data = train_m[, -1])

summary(model_9)
vif(model_9)


# Remove product_analytic_verticalFlashShoeAdapter


model_10 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                 `Payment type` + sla + discount + NPS  + 
                 `Online marketing`  + 
                 product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash  + 
                 product_analytic_verticalLens + product_analytic_verticalStrap  
               , 
               data = train_m[, -1])

summary(model_10)
vif(model_10)

# Remove `Online marketing`


model_11 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                 `Payment type` + sla + discount + NPS  + 
                 
                 product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash  + 
                 product_analytic_verticalLens + product_analytic_verticalStrap  
               , 
               data = train_m[, -1])

summary(model_11)
vif(model_11)

# Remove NPS


model_12 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                 `Payment type` + sla + discount   + 
                 
                 product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash  + 
                 product_analytic_verticalLens + product_analytic_verticalStrap  
               , 
               data = train_m[, -1])

summary(model_12)
vif(model_12)

# Remove product_analytic_verticalStrap


model_13 <- lm(formula = log(train_m$GMV) ~ `No.of units sold`  + 
                 `Payment type` + sla + discount   + 
                 
                 product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash  + 
                 product_analytic_verticalLens   
               , 
               data = train_m[, -1])

summary(model_13)
vif(model_13)


# Remove `No.of units sold`


model_14 <- lm(formula = log(train_m$GMV) ~  
                 `Payment type` + sla + discount   + 
                 
                 product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash  + 
                 product_analytic_verticalLens   
               , 
               data = train_m[, -1])

summary(model_14)
vif(model_14)



# Remove product_analytic_verticalBinoculars


model_15 <- lm(formula = log(train_m$GMV) ~  
                 `Payment type` + sla + discount   + 
                 
                 product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash  + 
                 product_analytic_verticalLens   
               , 
               data = train_m[, -1])

summary(model_15)
vif(model_15)


# Remove `Payment type`


model_16 <- lm(formula = log(train_m$GMV) ~  
                 sla + discount   + 
                 
                 product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash  + 
                 product_analytic_verticalLens   
               , 
               data = train_m[, -1])

summary(model_16)
vif(model_16)

# Remove product_analytic_verticalFlash


model_17 <- lm(formula = log(train_m$GMV) ~  
                 sla + discount   + 
                 
                 product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 
                 product_analytic_verticalLens   
               , 
               data = train_m[, -1])

summary(model_17)
vif(model_17)

# Remove sla


model_18 <- lm(formula = log(train_m$GMV) ~  
                 discount   + 
                 
                 product_analytic_verticalCameraAccessory + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
                 product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 
                 product_analytic_verticalLens   
               , 
               data = train_m[, -1])

summary(model_18)
vif(model_18)



# Here discount + product_analytic_verticalCameraAccessory + 
# product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
#  product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraEyeCup + 
#  product_analytic_verticalCameraLEDLight + product_analytic_verticalCameraRemoteControl + 
#  product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
#  product_analytic_verticalLens



Predict_4 <- predict(model_18,test_m[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_m$test_GMV <- Predict_4

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test_m$GMV,test_m$test_GMV)
cor(test_m$GMV,test_m$test_GMV)^2

# But from this we could not get the Marketing channels important for the business. Thus we now 
# we will take only marketing channel in modelling. The training R^2 is 0.73 and test R^2 is 0.38


# Here we couldn't get Marketing Channels in final model. Thus We would take only Marketing Channels
# in Modelling.


train_m1 <- train_m[,c(1,11:20)]
test_m1 <- test_m[,c(1,11:20)]

# Develop the first model 

str(train_m1)

model_1 <-lm(log(train_m1$GMV)~.,data=train_m1[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step


model_2 <- lm(formula = log(train_m1$GMV) ~ `Total Investment` + Digital + 
                Sponsorship + `Content Marketing` + `Online marketing` + 
                Affiliates + SEM, data = train_m1[, -1])


summary(model_2)
vif(model_2)

# Remove `Total Investment`

model_3 <- lm(formula = log(train_m1$GMV) ~  Digital + 
                Sponsorship + `Content Marketing` + `Online marketing` + 
                Affiliates + SEM, data = train_m1[, -1])


summary(model_3)
vif(model_3)


# Remove Sponsorship

model_4 <- lm(formula = log(train_m1$GMV) ~  Digital + 
                `Content Marketing` + `Online marketing` + 
                Affiliates + SEM, data = train_m1[, -1])


summary(model_4)
vif(model_4)

# Remove Affiliates

model_5 <- lm(formula = log(train_m1$GMV) ~  Digital + 
                `Content Marketing` + `Online marketing` + 
                SEM, data = train_m1[, -1])


summary(model_5)
vif(model_5)


# Remove SEM

model_6 <- lm(formula = log(train_m1$GMV) ~  Digital + 
                `Content Marketing` + `Online marketing` 
              , data = train_m1[, -1])


summary(model_6)
vif(model_6)




Predict_5 <- predict(model_6,test_m1[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_m1$test_GMV <- Predict_5

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test_m1$GMV,test_m1$test_GMV)
cor(test_m1$GMV,test_m1$test_GMV)^2

# In the final model we got three marketing channels Digital, `Content Marketing`,`Online marketing` 
# are major deciding variable for CameraAccessory sub category, though the correlation of  GMV to the any 
# of the advertising channel is very weak as seen by low adjusted R^2 value. Training adjusted R^2 as 0.0065
# and test data adjusted R^2 as 0.00096

#------------------------------------------------------------------------------------------------------#
# Modelling for homeAudio SUBCATEGORY
#------------------------------------------------------------------------------------------------------#



str(model_df1a)

# Looking at the structure of model_df1 we see that all values are positive but few columns
# are having "0"s in it, which we need to remove  from it for logarithmic transformation.

range02 <- function(x){(x-mean(x))/sd(x)}


model_df1a[,c(19:106)] <- range02(data.matrix(model_df1a[,c(19:106)]))

str(model_df1a)

# we see that though zeros have been removed from dataset, but negative is still there I would 
# add "1" to the column so that my values comes out to be positive.

model_df1a[,c(19:106)] <- model_df1a[,c(19:106)] + 1

str(model_df1a)

# Now dataset seems to be fine for logarithmic transformation.


# Divide you data in 70:30 

train_mh= (model_df1a[c(1:333),c(1,11:20)])

test_mh = (model_df1a[c(334:475),c(1,11:20)])


# Develop the first model 

str(train_mh)

model_1 <-lm(log(train_mh$GMV)~.,data=train_mh[,-1])
summary(model_1)

# adjusted R^2 comes out to be negative, thus we need to bring more significant variable into model.

train_mh= (model_df1a[c(1:333),c(1,2,5,8:20)])

test_mh = (model_df1a[c(334:475),c(1,2,5,8:20)])


model_1 <-lm(log(train_mh$GMV)~.,data=train_mh[,-1])
summary(model_1)



#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step

model_2 <- lm(formula = log(train_mh$GMV) ~ `No.of units sold` + `Payment type` + 
                discount + NPS + `Total Investment` + TV + Sponsorship + 
                `Online marketing`, data = train_mh[, -1])
summary(model_2)
vif(model_2)


# Remove `No.of units sold`

model_3 <- lm(formula = log(train_mh$GMV) ~  `Payment type` + 
                discount + NPS + `Total Investment` + TV + Sponsorship + 
                `Online marketing`, data = train_mh[, -1])
summary(model_3)
vif(model_3)


# Remove NPS

model_4 <- lm(formula = log(train_mh$GMV) ~  `Payment type` + 
                discount  + `Total Investment` + TV + Sponsorship + 
                `Online marketing`, data = train_mh[, -1])
summary(model_4)
vif(model_4)


# Remove Sponsorship

model_5 <- lm(formula = log(train_mh$GMV) ~  `Payment type` + 
                discount  + `Total Investment` + TV  + 
                `Online marketing`, data = train_mh[, -1])
summary(model_5)
vif(model_5)


# Remove `Online marketing`

model_6 <- lm(formula = log(train_mh$GMV) ~  `Payment type` + 
                discount  + `Total Investment` + TV   
              , data = train_mh[, -1])
summary(model_6)
vif(model_6)



Predict_6 <- predict(model_6,test_mh[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_mh$test_GMV <- Predict_6

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test_mh$GMV,test_mh$test_GMV)
cor(test_mh$GMV,test_mh$test_GMV)^2

# In the final model we got three variables`Payment type` ,discount,`Total Investment`,TV 

# as major driving variable for HomeAudio sub category, The correlation between GMV and the independent 
# variable found to be good.The training adjusted R^2 and the test adjusted R^2 quite close. 
# The adjusted R^2 of train data set as 0.55 and adjusted R^2 for test data set ~ 0.60.

# --------------------------------------------------------------------------------------------------#
# Modelling for GamingAccessory SUBCATEGORY
#---------------------------------------------------------------------------------------------------#

str(model_df1b)

# Looking at the structure of model_df1 we see that all values are positive but few columns
# are having "0"s in it, which we need to remove  from it for logarithmic transformation.

range02 <- function(x){(x-mean(x))/sd(x)}

model_df1b <- model_df1b[,-7]

model_df1b[,c(19:106)] <- range02(data.matrix(model_df1b[,c(19:106)]))

str(model_df1a)

# we see that though zeros have been removed from dataset, but negative is still there I would 
# add "1" to the column so that my values comes out to be positive.

model_df1b[,c(19:106)] <- model_df1b[,c(19:106)] + 1

str(model_df1a)

# Now dataset seems to be fine for logarithmic transformation.


# Divide you data in 70:30 

train_mg= (model_df1b[c(1:506),c(1,11:20)])

test_mg = (model_df1b[c(507:723),c(1,11:20)])


# Develop the first model 

str(train_mg)

model_1 <-lm(log(train_mg$GMV)~.,data=train_mg[,-1])
summary(model_1)

# adjusted R^2 comes out to be negative, thus we need to bring more significant variable into model.

train_mg= (model_df1b[c(1:506),c(1,2,5,8:20)])

test_mg = (model_df1b[c(507:723),c(1,2,5,8:20)])


model_1 <-lm(log(train_mg$GMV)~.,data=train_mg[,-1])
summary(model_1)



#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step

model_2 <- lm(formula = log(train_mg$GMV) ~ `Payment type` + `Total Investment` + 
                Digital + Sponsorship + `Content Marketing`, data = train_mg[, 
                                                                             -1])
summary(model_2)
vif(model_2)


# Remove Sponsorship

model_3 <- lm(formula = log(train_mg$GMV) ~ `Payment type` + `Total Investment` + 
                Digital +  `Content Marketing`, data = train_mg[, 
                                                                -1])
summary(model_3)
vif(model_3)


# Remove `Total Investment`

model_4 <- lm(formula = log(train_mg$GMV) ~ `Payment type`  + 
                Digital +  `Content Marketing`, data = train_mg[, 
                                                                -1])
summary(model_4)
vif(model_4)

# Remove `Total Investment`

model_5 <- lm(formula = log(train_mg$GMV) ~ `Payment type`  + 
                Digital +  `Content Marketing`, data = train_mg[, 
                                                                -1])
summary(model_5)
vif(model_5)



Predict_7 <- predict(model_5,test_mg[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_mg$test_GMV <- Predict_7

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test_mg$GMV,test_mg$test_GMV)
cor(test_mg$GMV,test_mg$test_GMV)^2

# In the final model we got three variables `Payment type`, Digital,`Content Marketing` 
# as major driving variable for GamingAccessory, The correlation between GMV and the independent variable
# found to be very low.
# The adjusted R^2 of test data set 0.019 and train data set of R^2 as 0.070

#######################################################################################################
#                                        Distributive Model                                           #
#######################################################################################################

# Here we need to create lag for non marketing variables and adstock for Advertising Channels.



str(model_df)

Model_dfd <- aggregate(model_df[,c(5,6,9,13,14,16:25)], by = list(model_df$`year Week`,model_df$product_analytic_sub_category), sum, na.rm = T)

names(Model_dfd)[names(Model_dfd) == "Group.1"] <- "Year-week"

names(Model_dfd)[names(Model_dfd) == "Group.2"] <- "product_analytic_sub_category"

# install.packages("Hmisc")
library(Hmisc)

Model_dfd$lag_no_of_unit_sold <- Lag(Model_dfd$`No.of units sold`, +1)

Model_dfd$lag_payment_type <- Lag(Model_dfd$`Payment type`, +1)

Model_dfd$lag_discount <- Lag(Model_dfd$discount, +1)

Model_dfd$lag_special <- Lag(Model_dfd$`special day`, +1)

# Now will create adstock taking asstock as 20% of the previous week value.

Model_dfd$Ad_Tot_investment <- 0.2 * (Lag(Model_dfd$`Total Investment`, +1))

Model_dfd$Ad_TV <- 0.2 * Lag(Model_dfd$TV, +1)

Model_dfd$Ad_digital <- 0.2 * Lag(Model_dfd$Digital, +1)


Model_dfd$Ad_sponsorship <- 0.2 * Lag(Model_dfd$Sponsorship, +1)

Model_dfd$Ad_Content_Marketing <- 0.2 * Lag(Model_dfd$`Content Marketing`, +1)

Model_dfd$Ad_online_marketing <- 0.2 * Lag(Model_dfd$`Online marketing`, +1)

Model_dfd$Ad_affliate <- 0.2 * Lag(Model_dfd$Affiliates, +1)

Model_dfd$Ad_SEM <- 0.2 * Lag(Model_dfd$SEM, +1)

Model_dfd$Ad_Radio <- 0.2 * Lag(Model_dfd$Radio, +1)

Model_dfd$Ad_other <- 0.2 * Lag(Model_dfd$Other, +1)


# removing original advertising marketing channels columns and keeping only Adstock effent columns.


Model_dfd <- Model_dfd[,-c(8:16)]

Model_dfd[1,c(9:22)] <- 0

# Now we have prepared dataset for distributive model. We now apply lm function
#_________________________________________________________________________________________________________________#
# Lets take cameraAccessory sub product category.
#_________________________________________________________________________________________________________________#

camera_distributive <- filter(Model_dfd,product_analytic_sub_category %in% c("CameraAccessory") )

camera_distributive <- camera_distributive[,-c(1:2)]

# Divide you data in 70:30 

train_dc= camera_distributive[c(1:35),]

test_dc = camera_distributive[c(36:52),]


# Develop the first model 

str(train_dc)

model_1 <-lm(train_dc$GMV~.,data=train_dc[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step


model_2 <- lm(formula = train_dc$GMV ~ `No.of units sold` + discount + `special day` + 
                lag_no_of_unit_sold + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_Content_Marketing + Ad_online_marketing + Ad_SEM, data = train_dc[, 
                                                                                     -1])
summary(model_2)
vif(model_2)

# Remove Ad_online_marketing


model_3 <- lm(formula = train_dc$GMV ~ `No.of units sold` + discount + `special day` + 
                lag_no_of_unit_sold + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_Content_Marketing  + Ad_SEM, data = train_dc[, 
                                                                -1])
summary(model_3)
vif(model_3)

# Remove `No.of units sold`


model_4 <- lm(formula = train_dc$GMV ~  + discount + `special day` + 
                lag_no_of_unit_sold + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_Content_Marketing  + Ad_SEM, data = train_dc[, 
                                                                -1])
summary(model_4)
vif(model_4)


# Remove `special day`


model_5 <- lm(formula = train_dc$GMV ~  + discount  + 
                lag_no_of_unit_sold + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_Content_Marketing  + Ad_SEM, data = train_dc[, 
                                                                -1])
summary(model_5)
vif(model_5)

# Remove Ad_digital


model_6 <- lm(formula = train_dc$GMV ~  + discount  + 
                lag_no_of_unit_sold + Ad_TV  + Ad_sponsorship + 
                Ad_Content_Marketing  + Ad_SEM, data = train_dc[, -1])

summary(model_6)
vif(model_6)


# Remove Ad_sponsorship


model_7 <- lm(formula = train_dc$GMV ~  + discount  + 
                lag_no_of_unit_sold + Ad_TV   + 
                Ad_Content_Marketing  + Ad_SEM, data = train_dc[, -1])

summary(model_7)
vif(model_7)


# Remove lag_no_of_unit_sold


model_8 <- lm(formula = train_dc$GMV ~  + discount  + 
                Ad_TV   + 
                Ad_Content_Marketing  + Ad_SEM, data = train_dc[, -1])

summary(model_8)
vif(model_8)

# Remove Ad_SEM


model_9 <- lm(formula = train_dc$GMV ~  + discount  + 
                Ad_TV   + 
                Ad_Content_Marketing, data = train_dc[, -1])

summary(model_9)
vif(model_9)



Predict_8 <- predict(model_9,test_dc[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_dc$test_GMV <- Predict_8

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test_dc$GMV,test_dc$test_GMV)
cor(test_dc$GMV,test_dc$test_GMV)^2


# We finally get discount, Ad_TV and Ad_Content_Marketing as our driving levers to improve the revenue 
# response for cameraAccessory sub product category. we get the train adjusted R^2 as 0.89 and the test 
# dataset adjusted R^2 as 0.92.



#_______________________________________________________________________________________________________#

# Lets take HomeAudio sub product category.

#_______________________________________________________________________________________________________#

HomeAudio_distributive <- filter(Model_dfd,product_analytic_sub_category %in% c("HomeAudio") )

HomeAudio_distributive <- HomeAudio_distributive[,-c(1:2)]

# Divide you data in 70:30 

train_dh= HomeAudio_distributive[c(1:34),]

test_dh = HomeAudio_distributive[c(35:51),]


# Develop the first model 

str(train_dh)

model_1 <-lm(train_dh$GMV~.,data=train_dh[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step

model_2 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other + lag_no_of_unit_sold + lag_discount + lag_special + 
                Ad_Tot_investment + Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dh[, -1])
summary(model_2)
vif(model_2)


# Remove lag_no_of_unit_sold

model_3 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other  + lag_discount + lag_special + 
                Ad_Tot_investment + Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dh[, -1])
summary(model_3)
vif(model_3)

# Remove Ad_affliate

model_4 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other  + lag_discount + lag_special + 
                Ad_Tot_investment + Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                Ad_SEM + Ad_Radio, data = train_dh[, -1])
summary(model_4)
vif(model_4)



# Remove lag_discount

model_5 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other  + lag_special + 
                Ad_Tot_investment + Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                Ad_SEM + Ad_Radio, data = train_dh[, -1])
summary(model_5)
vif(model_5)


# Remove lag_special

model_6 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other   + 
                Ad_Tot_investment + Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                Ad_SEM + Ad_Radio, data = train_dh[, -1])
summary(model_6)
vif(model_6)


# Remove Ad_SEM

model_7 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other   + 
                Ad_Tot_investment + Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                Ad_Radio, data = train_dh[, -1])
summary(model_7)
vif(model_7)


# Remove Ad_online_marketing

model_8 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other   + 
                Ad_Tot_investment + Ad_digital + Ad_sponsorship  + 
                Ad_Radio, data = train_dh[, -1])
summary(model_8)
vif(model_8)


# Remove Ad_digital

model_9 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other   + 
                Ad_Tot_investment  + Ad_sponsorship  + 
                Ad_Radio, data = train_dh[, -1])
summary(model_9)
vif(model_9)


# Remove Ad_sponsorship

model_10 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount + `special day` + 
                 Other   + 
                 Ad_Tot_investment    + 
                 Ad_Radio, data = train_dh[, -1])
summary(model_10)
vif(model_10)


# Remove `special day` due to high vif

model_11 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount  + 
                 Other   + 
                 Ad_Tot_investment    + 
                 Ad_Radio, data = train_dh[, -1])
summary(model_11)
vif(model_11)


# Remove Ad_Tot_investment

model_12 <- lm(formula = train_dh$GMV ~ `No.of units sold` + discount  + 
                 Other   + 
                 + 
                 Ad_Radio, data = train_dh[, -1])
summary(model_12)
vif(model_12)


predict_9 <- predict(model_12, test_dh[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_dh$test_GMV <- predict_9

#-------------------------------------------------------------------------------------------

# Calculate R^2

cor(test_dh$GMV,test_dh$test_GMV)

cor(test_dh$GMV,test_dh$test_GMV)^2

# We finally get `No.of units sold`, discount, Other and Ad_Radio as our driving levers to improve the revenue 
# response for HomeAudio sub product category. 
# we get the train adjusted R^2 as 0.83 and the test dataset adjusted R^2 as 0.92.



#_______________________________________________________________________________________________________#

# Lets take GamingAccessory sub product category.

#_______________________________________________________________________________________________________#

GamingAccessory_distributive <- filter(Model_dfd,product_analytic_sub_category %in% c("GamingAccessory") )

GamingAccessory_distributive <- GamingAccessory_distributive[,-c(1:2)]

# Divide you data in 70:30 

train_dg= GamingAccessory_distributive[c(1:36),]

test_dg = GamingAccessory_distributive[c(35:53),]


# Develop the first model 

str(train_dg)

model_1 <-lm(train_dg$GMV~.,data=train_dg[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step



model_2 <- lm(formula = train_dg$GMV ~ `No.of units sold` + `Payment type` + 
                discount + `special day` + Other + lag_no_of_unit_sold + 
                lag_payment_type + lag_discount + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_2)
vif(model_2)

# Remove `Payment type`


model_3 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                discount + `special day` + Other + lag_no_of_unit_sold + 
                lag_payment_type + lag_discount + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_3)
vif(model_3)


# Remove lag_payment_type


model_4 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                discount + `special day` + Other + lag_no_of_unit_sold + 
                lag_discount + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_4)
vif(model_4)

# Remove `special day`


model_5 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                discount  + Other + lag_no_of_unit_sold + 
                lag_discount + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_5)
vif(model_5)

# Remove Other


model_6 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                discount   + lag_no_of_unit_sold + 
                lag_discount + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_6)
vif(model_6)

# Remove lag_discount


model_7 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                discount   + lag_no_of_unit_sold + 
                Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_7)
vif(model_7)

# Remove Ad_TV


model_8 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                discount   + lag_no_of_unit_sold + 
                Ad_Tot_investment  + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_8)
vif(model_8)

# Remove Ad_Content_Marketing


model_9 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                discount   + lag_no_of_unit_sold + 
                Ad_Tot_investment  + 
                Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_9)
vif(model_9)

# Remove Ad_Radio


model_10 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 discount   + lag_no_of_unit_sold + 
                 Ad_Tot_investment  + 
                 Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                 Ad_affliate + Ad_SEM , data = train_dg[, -1])

summary(model_10)
vif(model_10)

# Remove lag_no_of_unit_sold


model_11 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 discount    + 
                 Ad_Tot_investment  + 
                 Ad_digital + Ad_sponsorship + Ad_online_marketing + 
                 Ad_affliate + Ad_SEM , data = train_dg[, -1])

summary(model_11)
vif(model_11)


# Remove Ad_online_marketing


model_12 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 discount    + 
                 Ad_Tot_investment  + 
                 Ad_digital + Ad_sponsorship +  
                 Ad_affliate + Ad_SEM , data = train_dg[, -1])

summary(model_12)
vif(model_12)

# Remove Ad_SEM


model_13 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 discount    + 
                 Ad_Tot_investment  + 
                 Ad_digital + Ad_sponsorship +  
                 Ad_affliate  , data = train_dg[, -1])

summary(model_13)
vif(model_13)

# Remove Ad_Tot_investment


model_14 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 discount    + 
                 
                 Ad_digital + Ad_sponsorship +  
                 Ad_affliate  , data = train_dg[, -1])

summary(model_14)
vif(model_14)

# Remove Ad_sponsorship


model_15 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 discount    + 
                 
                 Ad_digital  +  
                 Ad_affliate  , data = train_dg[, -1])

summary(model_15)
vif(model_15)

# Remove Ad_affliate


model_16 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 discount    + 
                 
                 Ad_digital   
               , data = train_dg[, -1])

summary(model_16)
vif(model_16)

# Remove discount


model_17 <- lm(formula = train_dg$GMV ~ `No.of units sold`  + 
                 
                 
                 Ad_digital   
               , data = train_dg[, -1])

summary(model_17)
vif(model_17)

predict_10 <- predict(model_17, test_dg[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_dg$test_GMV <- predict_10

# calculate test R^2

cor(test_dg$GMV,test_dg$test_GMV)

cor(test_dg$GMV,test_dg$test_GMV)^2

# We finally get `No.of units sold` and Ad_digital as our driving levers to improve the revenue 
# response for GamingAccessory sub product category. 
# we get the train adjusted R^2 as 0.966 and the test dataset adjusted R^2 as 0.916.

##################################################################################################################
#                                   Multiplicative + Distributive Modelling                                     #
##################################################################################################################

# Multiplicative + Distributive Modelling is applying logarithm to the distributive modelling.

# We already have prepared the dataset for the distributive model, bringing the same dataset for this modelling.



#_______________________________________________________________________________________________________#

# Lets take CameraAccessory sub product category.

#_______________________________________________________________________________________________________#

# Develop the first model 

str(train_dc)

model_1 <-lm(log(train_dc$GMV)~.,data=train_dc[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step

model_2 <- lm(formula = log(train_dc$GMV) ~ `No.of units sold` + lag_special + 
                Ad_Tot_investment + Ad_sponsorship + Ad_Content_Marketing + 
                Ad_SEM, data = train_dc[, -1])
summary(model_2)
vif(model_2)

# Remove Ad_SEM

model_3 <- lm(formula = log(train_dc$GMV) ~ `No.of units sold` + lag_special + 
                Ad_Tot_investment + Ad_sponsorship + Ad_Content_Marketing
                , data = train_dc[, -1])
summary(model_3)
vif(model_3)

# Remove Ad_Content_Marketing

model_4 <- lm(formula = log(train_dc$GMV) ~ `No.of units sold` + lag_special + 
                Ad_Tot_investment + Ad_sponsorship
              , data = train_dc[, -1])
summary(model_4)
vif(model_4)

# Remove Ad_Tot_investment

model_5 <- lm(formula = log(train_dc$GMV) ~ `No.of units sold` + lag_special + 
                 Ad_sponsorship
              , data = train_dc[, -1])
summary(model_5)
vif(model_5)

# Remove lag_special

model_6 <- lm(formula = log(train_dc$GMV) ~ `No.of units sold`  + 
                Ad_sponsorship
              , data = train_dc[, -1])
summary(model_6)
vif(model_6)



predict_11 <- predict(model_6, test_dc[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_dc$test_GMV <- predict_11

# calculate test R^2

cor(test_dc$GMV,test_dc$test_GMV)

cor(test_dc$GMV,test_dc$test_GMV)^2



# From here we get `No.of units sold` and Ad_sponsorship as our driving levers to improve the revenue 
# response for CameraAccessory sub product category. 
# we get the train adjusted R^2 as 0.66 and the test dataset adjusted R^2 as 0.81


#_______________________________________________________________________________________________________#

# Lets take HomeAudio sub product category.

#_______________________________________________________________________________________________________#

# Develop the first model 

str(train_dh)

model_1 <-lm(log(train_dh$GMV)~.,data=train_dh[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step


model_2 <- lm(formula = log(train_dh$GMV) ~ `No.of units sold` + discount + 
                `special day` + Other + lag_payment_type + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio, data = train_dh[, -1])

summary(model_2)
vif(model_2)


# Remove lag_payment_type

model_3 <- lm(formula = log(train_dh$GMV) ~ `No.of units sold` + discount + 
                `special day` + Other  + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio, data = train_dh[, -1])

summary(model_3)
vif(model_3)

# Remove Other

model_4 <- lm(formula = log(train_dh$GMV) ~ `No.of units sold` + discount + 
                `special day`   + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio, data = train_dh[, -1])

summary(model_4)
vif(model_4)

# Remove `special day`

model_5 <- lm(formula = log(train_dh$GMV) ~ `No.of units sold` + discount 
                   + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio, data = train_dh[, -1])

summary(model_5)
vif(model_5)

# Remove `No.of units sold`

model_6 <- lm(formula = log(train_dh$GMV) ~  discount 
              + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio, data = train_dh[, -1])

summary(model_6)
vif(model_6)

# Remove Ad_Radio

model_7 <- lm(formula = log(train_dh$GMV) ~  discount 
              + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing , data = train_dh[, -1])

summary(model_7)
vif(model_7)


# Remove Ad_Radio

model_8 <- lm(formula = log(train_dh$GMV) ~  discount 
              + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing , data = train_dh[, -1])

summary(model_8)
vif(model_8)

# Remove Ad_online_marketing

model_9 <- lm(formula = log(train_dh$GMV) ~  discount 
              + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship  
                 , data = train_dh[, -1])

summary(model_9)
vif(model_9)

# Remove Ad_digital

model_10 <- lm(formula = log(train_dh$GMV) ~  discount 
               + lag_discount + 
                 lag_special + Ad_Tot_investment  + Ad_sponsorship  
               , data = train_dh[, -1])

summary(model_10)
vif(model_10)

# Remove Ad_Tot_investment

model_11 <- lm(formula = log(train_dh$GMV) ~  discount 
               + lag_discount + 
                 lag_special   + Ad_sponsorship  
               , data = train_dh[, -1])

summary(model_11)
vif(model_11)


predict_12 <- predict(model_11, test_dh[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_dh$test_GMV <- predict_12

# calculate test R^2

cor(test_dh$GMV,test_dh$test_GMV)

cor(test_dh$GMV,test_dh$test_GMV)^2



# From here we get discount, lag_discount, lag_special and Ad_sponsorship as our driving levers to improve
# the revenue response for HomeAudio sub product category. 
# we get the train adjusted R^2 as 0.87 and the test dataset adjusted R^2 as 0.87.




#_______________________________________________________________________________________________________#

# Lets take GamingAccessory sub product category.

#_______________________________________________________________________________________________________#

# Develop the first model 

str(train_dg)

model_1 <-lm(log(train_dg$GMV)~.,data=train_dg[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step

model_2 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold` + `special day` + 
                lag_no_of_unit_sold + lag_special + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_2)
vif(model_2)

# Remove lag_special


model_3 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold` + `special day` + 
                lag_no_of_unit_sold  + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_3)
vif(model_3)

# Remove `special day`


model_4 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                lag_no_of_unit_sold  + Ad_Tot_investment + Ad_TV + 
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_4)
vif(model_4)


# Remove Ad_Tot_investment


model_5 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                lag_no_of_unit_sold    + Ad_TV +
                Ad_digital + Ad_sponsorship + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_5)
vif(model_5)

# Remove Ad_sponsorship


model_6 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                lag_no_of_unit_sold    + Ad_TV +
                Ad_digital  + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM + Ad_Radio, data = train_dg[, -1])

summary(model_6)
vif(model_6)


# Remove Ad_Radio


model_7 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                lag_no_of_unit_sold    + Ad_TV +
                Ad_digital  + Ad_Content_Marketing + Ad_online_marketing + 
                Ad_affliate + Ad_SEM , data = train_dg[, -1])

summary(model_7)
vif(model_7)


# Remove Ad_affliate


model_8 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                lag_no_of_unit_sold    + Ad_TV +
                Ad_digital  + Ad_Content_Marketing + Ad_online_marketing + 
                 Ad_SEM , data = train_dg[, -1])

summary(model_8)
vif(model_8)

# Remove Ad_Content_Marketing


model_9 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                lag_no_of_unit_sold    + Ad_TV +
                Ad_digital   + Ad_online_marketing + 
                Ad_SEM , data = train_dg[, -1])

summary(model_9)
vif(model_9)

# Remove lag_no_of_unit_sold


model_10 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                Ad_TV +
                Ad_digital   + Ad_online_marketing + 
                Ad_SEM , data = train_dg[, -1])

summary(model_10)
vif(model_10)

# Remove Ad_online_marketing


model_11 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                 Ad_TV +
                 Ad_digital    + 
                 Ad_SEM , data = train_dg[, -1])

summary(model_11)
vif(model_11)


# Remove Ad_TV


model_12 <- lm(formula = log(train_dg$GMV) ~ `No.of units sold`  + 
                 
                 Ad_digital    + 
                 Ad_SEM , data = train_dg[, -1])

summary(model_12)
vif(model_12)


predict_13 <- predict(model_12, test_dg[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_dg$test_GMV <- predict_13

# calculate test R^2

cor(test_dg$GMV,test_dg$test_GMV)

cor(test_dg$GMV,test_dg$test_GMV)^2



# From here we get discount, lag_discount, lag_special and Ad_sponsorship as our driving levers to improve
# the revenue response for HomeAudio sub product category. 
# we get the train adjusted R^2 as 0.74 and the test dataset adjusted R^2 as 0.878.



##################################################################################################################
#                                              Koyck Modelling                                                   #
##################################################################################################################

# Here we would take the lag of our target variable GMV as well as the lag of other non- marketing variables and
# the adstock of marketing variables.

# We already have prepared the dataset for the distributive model, bringing the same dataset for this modelling.



#_______________________________________________________________________________________________________#

# Lets take CameraAccessory sub product category.

#_______________________________________________________________________________________________________#


camera_koyck <- camera_distributive
camera_koyck$lag_GMV <- Lag(camera_koyck$GMV, +1)

camera_koyck$lag_GMV[1] <- 0

# Divide you data in 70:30 

train_kc= camera_koyck[c(1:35),]

test_kc = camera_koyck[c(36:52),]


# Develop the first model 

str(train_kc)

model_1 <-lm(train_kc$GMV~.,data=train_kc[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step


model_2 <- lm(formula = train_kc$GMV ~ `No.of units sold` + `Payment type` + 
                discount + `special day` + lag_discount + Ad_TV + Ad_digital + 
                Ad_sponsorship + Ad_Content_Marketing + Ad_SEM + lag_GMV, 
              data = train_kc[, -1])

summary(model_2)

vif(model_2)

# Remove `No.of units sold`

model_3 <- lm(formula = train_kc$GMV ~  `Payment type` + 
                discount + `special day` + lag_discount + Ad_TV + Ad_digital + 
                Ad_sponsorship + Ad_Content_Marketing + Ad_SEM + lag_GMV, 
              data = train_kc[, -1])

summary(model_3)

vif(model_3)


# Remove Ad_sponsorship

model_4 <- lm(formula = train_kc$GMV ~  `Payment type` + 
                discount + `special day` + lag_discount + Ad_TV + Ad_digital + 
                 Ad_Content_Marketing + Ad_SEM + lag_GMV, 
              data = train_kc[, -1])

summary(model_4)

vif(model_4)


# Remove Ad_digital

model_5 <- lm(formula = train_kc$GMV ~  `Payment type` + 
                discount + `special day` + lag_discount + Ad_TV  + 
                Ad_Content_Marketing + Ad_SEM + lag_GMV, 
              data = train_kc[, -1])

summary(model_5)

vif(model_5)

# Remove `special day`

model_6 <- lm(formula = train_kc$GMV ~  `Payment type` + 
                discount  + lag_discount + Ad_TV  + 
                Ad_Content_Marketing + Ad_SEM + lag_GMV, 
              data = train_kc[, -1])

summary(model_6)

vif(model_6)

# Remove `Payment type`

model_7 <- lm(formula = train_kc$GMV ~   
                discount  + lag_discount + Ad_TV  + 
                Ad_Content_Marketing + Ad_SEM + lag_GMV, 
              data = train_kc[, -1])

summary(model_7)

vif(model_7)


# Remove lag_GMV

model_8 <- lm(formula = train_kc$GMV ~   
                discount  + lag_discount + Ad_TV  + 
                Ad_Content_Marketing + Ad_SEM , 
              data = train_kc[, -1])

summary(model_8)

vif(model_8)

# Remove lag_discount

model_9 <- lm(formula = train_kc$GMV ~   
                discount   + Ad_TV  + 
                Ad_Content_Marketing + Ad_SEM , 
              data = train_kc[, -1])

summary(model_9)

vif(model_9)

# Remove Ad_SEM

model_10 <- lm(formula = train_kc$GMV ~   
                discount   + Ad_TV  + 
                Ad_Content_Marketing  , 
              data = train_kc[, -1])

summary(model_10)

vif(model_10)


predict_14 <- predict(model_10, test_kc[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_kc$test_GMV <- predict_14

# calculate test R^2

cor(test_kc$GMV,test_kc$test_GMV)

cor(test_kc$GMV,test_kc$test_GMV)^2



# From here we get discount, Ad_TV and Ad_Content_Marketing as our driving levers to improve
# the revenue response for CameraAccessary sub product category. 
# we get the train adjusted R^2 as 0.898 and the test dataset adjusted R^2 as 0.920


#_______________________________________________________________________________________________________#

# Lets take HomeAudio sub product category.

#_______________________________________________________________________________________________________#


Home_koyck <- HomeAudio_distributive
Home_koyck$lag_GMV <- Lag(Home_koyck$GMV, +1)

Home_koyck$lag_GMV[1] <- 0

# Divide you data in 70:30 

train_kh= Home_koyck[c(1:35),]

test_kh = Home_koyck[c(36:51),]


# Develop the first model 

str(train_kh)

model_1 <-lm(train_kh$GMV~.,data=train_kh[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step

model_2 <- lm(formula = train_kh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other + lag_no_of_unit_sold + lag_payment_type + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_2)                                                                          
vif(model_2)


# Remove lag_payment_type

model_3 <- lm(formula = train_kh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other + lag_no_of_unit_sold  + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_3)                                                                          
vif(model_3)


# Remove lag_no_of_unit_sold

model_4 <- lm(formula = train_kh$GMV ~ `No.of units sold` + discount + `special day` + 
                Other   + lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_4)                                                                          
vif(model_4)


# Remove Other

model_5 <- lm(formula = train_kh$GMV ~ `No.of units sold` + discount + `special day` + 
                 lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_5)                                                                          
vif(model_5)

# Remove Ad_online_marketing

model_6 <- lm(formula = train_kh$GMV ~ `No.of units sold` + discount + `special day` + 
                lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                 Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_6)                                                                          
vif(model_6)

# Remove `No.of units sold`

model_7 <- lm(formula = train_kh$GMV ~  discount + `special day` + 
                lag_discount + 
                lag_special + Ad_Tot_investment + Ad_digital + Ad_sponsorship + 
                Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_7)                                                                          
vif(model_7)


# Remove Ad_Tot_investment

model_8 <- lm(formula = train_kh$GMV ~  discount + `special day` + 
                lag_discount + 
                lag_special  + Ad_digital + Ad_sponsorship + 
                Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_8)                                                                          
vif(model_8)


# Remove Ad_sponsorship

model_9 <- lm(formula = train_kh$GMV ~  discount + `special day` + 
                lag_discount + 
                lag_special  + Ad_digital  + 
                Ad_Radio + lag_GMV, data = train_kh[, -1])

summary(model_9)                                                                          
vif(model_9)


# Remove Ad_Radio

model_10 <- lm(formula = train_kh$GMV ~  discount + `special day` + 
                lag_discount + 
                lag_special  + Ad_digital  + 
                 lag_GMV, data = train_kh[, -1])

summary(model_10)                                                                          
vif(model_10)


# Remove `special day`

model_11 <- lm(formula = train_kh$GMV ~  discount  + 
                 lag_discount + 
                 lag_special  + Ad_digital  + 
                 lag_GMV, data = train_kh[, -1])

summary(model_11)                                                                          
vif(model_11)



predict_15 <- predict(model_11, test_kh[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_kh$test_GMV <- predict_15

# calculate test R^2

cor(test_kh$GMV,test_kh$test_GMV)

cor(test_kh$GMV,test_kh$test_GMV)^2



# From here we get discount, `special day`, lag_discount, lag_special, Ad_digital and lag_GMV 
# as our driving levers to improve the revenue response for HomeAudio sub product category. 
# we get the train adjusted R^2 as 0.880 and the test dataset adjusted R^2 as 0.833



#_______________________________________________________________________________________________________#

# Lets take GamingAccessory sub product category.

#_______________________________________________________________________________________________________#


Gaming_koyck <- GamingAccessory_distributive
Gaming_koyck$lag_GMV <- Lag(Gaming_koyck$GMV, +1)

Gaming_koyck$lag_GMV[1] <- 0

# Divide you data in 70:30 

train_kg= Gaming_koyck[c(1:35),]

test_kg = Gaming_koyck[c(36:53),]


# Develop the first model 

str(train_kg)

model_1 <-lm(train_kg$GMV~.,data=train_kg[,-1])
summary(model_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")




#-------------------------------------------------------------------------------------------


# Run the step object

step


model_2 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount + `special day` + 
                Other + lag_no_of_unit_sold + lag_payment_type + lag_discount + 
                Ad_Tot_investment + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio + lag_GMV, 
              data = train_kg[, -1])

summary(model_2)
vif(model_2)


# Remove lag_discount


model_3 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount + `special day` + 
                Other + lag_no_of_unit_sold + lag_payment_type  + 
                Ad_Tot_investment + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio + lag_GMV, 
              data = train_kg[, -1])

summary(model_3)
vif(model_3)


# Remove `special day`


model_4 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                Other + lag_no_of_unit_sold + lag_payment_type  + 
                Ad_Tot_investment + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio + lag_GMV, 
              data = train_kg[, -1])

summary(model_4)
vif(model_4)


# Remove lag_payment_type


model_5 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                Other + lag_no_of_unit_sold   + 
                Ad_Tot_investment + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio + lag_GMV, 
              data = train_kg[, -1])

summary(model_5)
vif(model_5)


# Remove Other


model_6 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                 lag_no_of_unit_sold   + 
                Ad_Tot_investment + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio + lag_GMV, 
              data = train_kg[, -1])

summary(model_6)
vif(model_6)

# Remove lag_GMV


model_7 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                lag_no_of_unit_sold   + 
                Ad_Tot_investment + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio , 
              data = train_kg[, -1])

summary(model_7)
vif(model_7)



# Remove lag_GMV


model_8 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                lag_no_of_unit_sold   + 
                Ad_Tot_investment + Ad_TV + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio , 
              data = train_kg[, -1])

summary(model_8)
vif(model_8)


# Remove Ad_TV


model_9 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                lag_no_of_unit_sold   + 
                Ad_Tot_investment  + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM + Ad_Radio , 
              data = train_kg[, -1])

summary(model_9)
vif(model_9)


# Remove Ad_Radio


model_10 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                lag_no_of_unit_sold   + 
                Ad_Tot_investment  + Ad_digital + Ad_sponsorship + 
                Ad_online_marketing + Ad_affliate + Ad_SEM  , 
              data = train_kg[, -1])

summary(model_10)
vif(model_10)


# Remove lag_no_of_unit_sold


model_11 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                    
                 Ad_Tot_investment  + Ad_digital + Ad_sponsorship + 
                 Ad_online_marketing + Ad_affliate + Ad_SEM  , 
               data = train_kg[, -1])

summary(model_11)
vif(model_11)

# Remove Ad_SEM


model_12 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                 
                 Ad_Tot_investment  + Ad_digital + Ad_sponsorship + 
                 Ad_online_marketing + Ad_affliate   , 
               data = train_kg[, -1])

summary(model_12)
vif(model_12)


# Remove Ad_affliate


model_13 <- lm(formula = train_kg$GMV ~ `No.of units sold` + discount  + 
                 
                 Ad_Tot_investment  + Ad_digital + Ad_sponsorship + 
                 Ad_online_marketing    , 
               data = train_kg[, -1])

summary(model_13)
vif(model_13)


# Remove discount


model_14 <- lm(formula = train_kg$GMV ~ `No.of units sold` +    
                 
                 Ad_Tot_investment  + Ad_digital + Ad_sponsorship + 
                 Ad_online_marketing    , 
               data = train_kg[, -1])

summary(model_14)
vif(model_14)


# Remove Ad_online_marketing


model_15 <- lm(formula = train_kg$GMV ~ `No.of units sold` +    
                 
                 Ad_Tot_investment  + Ad_digital + Ad_sponsorship  
                     , 
               data = train_kg[, -1])

summary(model_15)
vif(model_15)


# Remove Ad_sponsorship


model_16 <- lm(formula = train_kg$GMV ~ `No.of units sold` +    
                 
                 Ad_Tot_investment  + Ad_digital   
               , 
               data = train_kg[, -1])

summary(model_16)
vif(model_16)


# Remove Ad_Tot_investment


model_17 <- lm(formula = train_kg$GMV ~ `No.of units sold` +    
                 
                  Ad_digital   
               , 
               data = train_kg[, -1])

summary(model_17)
vif(model_17)



predict_16 <- predict(model_17, test_kg[,-1])

#-------------------------------------------------------------------------------------------

# Add a new column "test_GMV" into the test dataset

test_kg$test_GMV <- predict_16

# calculate test R^2

cor(test_kg$GMV,test_kg$test_GMV)

cor(test_kg$GMV,test_kg$test_GMV)^2



# From here we get `No.of units sold` and Ad_digital as our driving levers to improve the revenue response
# for GamingAccessory sub product category. 
# we get the train adjusted R^2 as 0.967 and the test dataset adjusted R^2 as 0.915




