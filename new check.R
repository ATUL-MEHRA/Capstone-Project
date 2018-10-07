
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
  }
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



write.csv(merge_data2,"merge_data2.csv")

str(merge_data2)


model_df <- merge_data2

library(MASS)
library(car)


# imputing the missing value in "raio" and "other" column with 0.

model_df$Radio[which(is.na(model_df$Radio))] <- 0

model_df$Other[which(is.na(model_df$Other))] <- 0

# Scaling to the positive values.

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

model_df[,c(5:14)] <- range01(model_df[,c(5:14)])

model_df$NPS <- as.numeric((model_df$NPS))

model_df$NPS <- range01(model_df$NPS)


# scaling the numerical attributes.

model_df[,c(16:23)] <-  range01(model_df[,c(16:23)])

model_df[,c(24:25)] <- range01(model_df[,c(24:25)])


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
  
# CameraTripod , ExtensionTube,Lens, Teleconverter as our driving variable in determiing the GMV.




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

# In final model we get `No.of units sold` + +discount + product_analytic_verticalBoomBox + 
# product_analytic_verticalDJController + product_analytic_verticalHiFiSystem + 
 # product_analytic_verticalKaraokePlayer + product_analytic_verticalSoundMixer as our driving variable in determiing the GMV.
# here we have considered the basic linear model whereas we have timestamped data available we can work 
# over the adstock effect and find the driving variables in determining the GMV.







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
# over the adstock effect and find the driving variables in determining the GMV.


# Multiplicative , Distributive, Koyck and Multiplicative + Distributive model



# Multiplicative models


# Develop the first model 

model_1m <-lm(log(train1$GMV)~.,data=train1[,-1])

summary(model_1m)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step <- stepAIC(model_1m, direction="both")



#-------------------------------------------------------------------------------------------


# Run the step object

step


plot(step)


model_2 <- lm(formula = train$GMV ~ `No.of units sold` + `Payment type` + 
                sla + discount + NPS + `Content Marketing` + `Online marketing` + 
                product_analytic_verticalBinoculars + product_analytic_verticalCameraAccessory + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraBatteryGrip + product_analytic_verticalCameraLEDLight + 
                product_analytic_verticalCameraMount + product_analytic_verticalCameraRemoteControl + 
                product_analytic_verticalCameraTripod + product_analytic_verticalExtensionTube + 
                product_analytic_verticalFilter + product_analytic_verticalLens + 
                product_analytic_verticalTeleconverter, data = train[, -1])



















