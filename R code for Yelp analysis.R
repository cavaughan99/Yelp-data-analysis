##last updated: 03-19-17
library("jsonlite")
library("tidyr")
library("car")
library("stringr")
library("lme4")
library("dplyr")

##call up data file and create temporary df bus_data

setwd("/Documents...")
json_file_1 = "yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"
bus_data <- fromJSON(sprintf("[%s]", paste(readLines(json_file_1), collapse=",")))

##view bus_data
View(bus_data)

##Since my analysis is limited to restaurants, create subset of bus_data that contains only businesses categorized as restaurants

##Use grepl command to create variable "restaurant" that is TRUE if categories contains the word "Restaurants"
bus_data$restaurant <- as.numeric(grepl("Restaurants", bus_data$categories))

##look at subset of data to see whether "restaurant" variable was successfully created
bus_data_c<-select(bus_data, restaurant, categories)
View(bus_data_c)

##create new df res_data, the subset of data that has only restaurants
res_data <- bus_data[!(bus_data$restaurant==0), ]

##check that subsetting worked as expected
View(res_data)
table(res_data$restaurant)

##separate different categories of businesses into separate columns in order to see what the different restaurant categories are
##probably DON'T need to do this again now that restaurant subcategory indicators have been created (see code below).

##res_data_cat <- res_data %>% 
        ##separate(categories, c('cat1','cat2','cat3','cat4','cat5','cat6','cat7','cat8','cat9','cat10','cat11'), sep = ', ')
##clean up cat1 var by removing extraneous characters
##res_data_cat$cat1 <- gsub("^c","",res_data_cat$cat1)
##res_data_cat$cat1 <- gsub("[[:punct:]]","",res_data_cat$cat1)
##clean up cat2 and all other vars by removing extraneous characters
##res_data_cat$cat2 <- gsub("[[:punct:]]","", res_data_cat$cat2)
##res_data_cat$cat3 <- gsub("[[:punct:]]","", res_data_cat$cat3)
##res_data_cat$cat4 <- gsub("[[:punct:]]","", res_data_cat$cat4)
##res_data_cat$cat5 <- gsub("[[:punct:]]","", res_data_cat$cat5)
##res_data_cat$cat6 <- gsub("[[:punct:]]","", res_data_cat$cat6)
##res_data_cat$cat7 <- gsub("[[:punct:]]","", res_data_cat$cat7)
##res_data_cat$cat8 <- gsub("[[:punct:]]","", res_data_cat$cat8)
##res_data_cat$cat9 <- gsub("[[:punct:]]","", res_data_cat$cat9)
##res_data_cat$cat10 <- gsub("[[:punct:]]","", res_data_cat$cat10)
##res_data_cat$cat11 <- gsub("[[:punct:]]","", res_data_cat$cat11)

##View(res_data_cat)
##subset category variables to prepare to stack them on top of each other to form a single variable
##catset <- subset(res_data_cat, select = cat1:cat11)
##View(catset)
##stack category variables on top of each other
##catset2 <- stack(catset[1:11])
##examine resulting dataframe and look at frequencies of each category of restaurant using "table" function
##View(catset2)
##table(catset2$values)

##create dataframe (where each row contains a unique category of restaurant) that includes a count of the number of restaurants in each category
##catcount <- count(catset2, values)
##examine structure and contents of new dataframe
##str(catcount)
##View(catcount)
##There are 310 categories of restaurants; filter dataframe to include only categories with >=50 restaurants
##catcount2 <- filter(catcount, n>=100)
##str(catcount2)
##View(catcount2)
##68 unique categories with >= 100 resataurants

## DATA WRANGLING
## create separate binary indicators for each category of restaurant (for all 68 categories of restaurants with at least 100 restaurants) 
## using information from split out categories in analysis below where you create cat1-cat11
res_data$ArtsEntertainment <- as.numeric(grepl("Arts & Entertainment", res_data$categories))
res_data$AsianFusion <- as.numeric(grepl("Asian Fusion", res_data$categories))
res_data$Bagels <- as.numeric(grepl("Bagels", res_data$categories))
res_data$Bakeries <- as.numeric(grepl("Bakeries", res_data$categories))
res_data$Barbeque <- as.numeric(grepl("Barbeque", res_data$categories))
res_data$Bars <- as.numeric(grepl("Bars", res_data$categories))
res_data$BreakfastBrunch <- as.numeric(grepl("Breakfast & Brunch", res_data$categories))
res_data$British <- as.numeric(grepl("British", res_data$categories))
res_data$Buffets <- as.numeric(grepl("Buffets", res_data$categories))
res_data$Burgers <- as.numeric(grepl("Burgers", res_data$categories))
res_data$Cafes <- as.numeric(grepl("Cafes", res_data$categories))
res_data$CajunCreole <- as.numeric(grepl("Cajun/Creole", res_data$categories))
res_data$Caribbean <- as.numeric(grepl("Caribbean", res_data$categories))
res_data$ChickenWings <- as.numeric(grepl("Chicken Wings", res_data$categories))
res_data$Chinese <- as.numeric(grepl("Chinese", res_data$categories))
res_data$CocktailBars <- as.numeric(grepl("Cocktail Bars", res_data$categories))
res_data$CoffeeTea <- as.numeric(grepl("Coffee & Tea", res_data$categories))
res_data$ComfortFood <- as.numeric(grepl("Comfort Food", res_data$categories))
res_data$Delis <- as.numeric(grepl("Delis", res_data$categories))
res_data$Desserts <- as.numeric(grepl("Desserts", res_data$categories))
res_data$Diners <- as.numeric(grepl("Diners", res_data$categories))
res_data$Event <- as.numeric(grepl("Event Planning & Services", res_data$categories))
res_data$FastFood <- as.numeric(grepl("Fast Food", res_data$categories))
res_data$FishChips <- as.numeric(grepl("Fish & Chips", res_data$categories))
res_data$Food <- as.numeric(grepl("Food", res_data$categories))
res_data$FoodTrucks <- as.numeric(grepl("Food Trucks", res_data$categories))
res_data$French <- as.numeric(grepl("French", res_data$categories))
res_data$Gastropubs <- as.numeric(grepl("Gastropubs", res_data$categories))
res_data$German <- as.numeric(grepl("German", res_data$categories))
res_data$GlutenFree <- as.numeric(grepl("Gluten-Free", res_data$categories))
res_data$Greek <- as.numeric(grepl("Greek", res_data$categories))
res_data$Hawaiian <- as.numeric(grepl("Hawaiian", res_data$categories))
res_data$HotDogs <- as.numeric(grepl("Hot Dogs", res_data$categories))
res_data$IceCream <- as.numeric(grepl("Ice Cream & Frozen Yogurt", res_data$categories))
res_data$Indian <- as.numeric(grepl("Indian", res_data$categories))
res_data$Italian <- as.numeric(grepl("Italian", res_data$categories))
res_data$Japanese <- as.numeric(grepl("Japanese", res_data$categories))
res_data$JuiceBars <- as.numeric(grepl("Juice Bars & Smoothies", res_data$categories))
res_data$Korean <- as.numeric(grepl("Korean", res_data$categories))
res_data$LatAmer <- as.numeric(grepl("Latin American", res_data$categories))
res_data$Lounges <- as.numeric(grepl("Lounges", res_data$categories))
res_data$Mediterranean <- as.numeric(grepl("Mediterranean", res_data$categories))
res_data$Mexican <- as.numeric(grepl("Mexican", res_data$categories))
res_data$MidEast <- as.numeric(grepl("Middle Eastern", res_data$categories))
res_data$Nightlife <- as.numeric(grepl("Nightlife", res_data$categories))
res_data$Pizza <- as.numeric(grepl("Pizza", res_data$categories))
res_data$Pubs <- as.numeric(grepl("Pubs", res_data$categories))
res_data$Salad <- as.numeric(grepl("Salad", res_data$categories))
res_data$Sandwiches <- as.numeric(grepl("Sandwiches", res_data$categories))
res_data$Seafood <- as.numeric(grepl("Seafood", res_data$categories))
res_data$SoulFood <- as.numeric(grepl("Soul Food", res_data$categories))
res_data$Soup <- as.numeric(grepl("Soup", res_data$categories))
res_data$Southern <- as.numeric(grepl("Southern", res_data$categories))
res_data$Specialty <- as.numeric(grepl("Specialty Food", res_data$categories))
res_data$SportsBars <- as.numeric(grepl("Sports Bars", res_data$categories))
res_data$Steakhouses <- as.numeric(grepl("Steakhouses", res_data$categories))
res_data$SushiBars <- as.numeric(grepl("Sushi Bars", res_data$categories))
res_data$TapasBars <- as.numeric(grepl("Tapas Bars", res_data$categories))
res_data$TapasSmall <- as.numeric(grepl("Tapas/Small Plates", res_data$categories))
res_data$TexMex <- as.numeric(grepl("Tex-Mex", res_data$categories))
res_data$Thai <- as.numeric(grepl("Thai", res_data$categories))
res_data$Vegan <- as.numeric(grepl("Vegan", res_data$categories))
res_data$Vegetarian <- as.numeric(grepl("Vegetarian", res_data$categories))
res_data$Vietnamese <- as.numeric(grepl("Vietnamese", res_data$categories))
res_data$WineBars <- as.numeric(grepl("Wine Bars", res_data$categories))

##wrangle the difficult categories that had parentheses
res_data$categories3 <- gsub("[[:punct:]]","", res_data$categories)
print(res_data$categories3)
res_data$AmericanNew <- as.numeric(grepl("American New", res_data$categories3))
res_data$AmericanTrad <- as.numeric(grepl("American Traditional", res_data$categories3))
res_data$CanadianNew <- as.numeric(grepl("Canadian New", res_data$categories3))
##create variable to indicate number of restaurant subcategories associated with restaurant 

res_data$res_sum <- with (res_data, (AmericanNew + AmericanTrad + ArtsEntertainment + AsianFusion + Bagels + Bakeries +
           Barbeque + Bars + BreakfastBrunch + British + Buffets + Burgers + 
           Cafes + CajunCreole + CanadianNew + Caribbean + ChickenWings + Chinese + 
           CocktailBars + CoffeeTea + ComfortFood + Delis + Desserts + Diners + 
           Event + FastFood + FishChips + FoodTrucks + French + Gastropubs + 
           German + GlutenFree + Greek + Hawaiian + HotDogs + IceCream + 
           Indian + Italian + Japanese + JuiceBars + Korean + LatAmer + 
           Lounges + Mediterranean + Mexican + MidEast + Nightlife + Pizza + 
           Pubs + Salad + Sandwiches + Seafood + SoulFood + Soup + 
           Southern + Specialty + SportsBars + Steakhouses + SushiBars + TapasBars + 
           TapasSmall + TexMex + Thai + Vegan + Vegetarian + Vietnamese + 
           WineBars))



##check that res_sum was computed
table(res_data$res_sum)


##examine univariate descriptive statistics and missingness for prospective predictors of interest

##wi-fi
str(res_data$attributes$`Wi-Fi`)
table(res_data$attributes$`Wi-Fi`, exclude=NULL)
mean(is.na(res_data$attributes$'Wi-Fi'))

##noise-level
str(res_data$attributes$`Noise Level`)
table(res_data$attributes$`Noise Level`, exclude=NULL)
mean(is.na(res_data$attributes$`Noise Level`))

##outdoor seating
str(res_data$attributes$`Outdoor Seating`)
table(res_data$attributes$`Outdoor Seating`, exclude=NULL)
mean(is.na(res_data$attributes$`Outdoor Seating`))

##parking
table(res_data$attributes$Parking$garage, exclude=NULL)
mean(is.na(res_data$attributes$Parking$garage))

table(res_data$attributes$Parking$street, exclude=NULL)
mean(is.na(res_data$attributes$Parking$street))

table(res_data$attributes$Parking$validated, exclude=NULL)
mean(is.na(res_data$attributes$Parking$validated))

table(res_data$attributes$Parking$lot, exclude=NULL)
mean(is.na(res_data$attributes$Parking$lot))

table(res_data$attributes$Parking$valet, exclude=NULL)
mean(is.na(res_data$attributes$Parking$valet))

##takes reservations
table(res_data$attributes$`Takes Reservations`, exclude=NULL)
mean(is.na(res_data$attributes$`Takes Reservations`))

##waiter service
table(res_data$attributes$`Waiter Service`, exclude=NULL)
mean(is.na(res_data$attributes$`Waiter Service`))

##price range
str(res_data$attributes$`Price Range`)
table(res_data$attributes$`Price Range`, exclude=NULL)
mean(is.na(res_data$attributes$`Price Range`))

##attire
table(res_data$attributes$Attire, exclude=NULL)
mean(is.na(res_data$attributes$Attire))

##ambience
table(res_data$attributes$Ambience$romantic, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$romantic))
table(res_data$attributes$Ambience$intimate, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$intimate))
table(res_data$attributes$Ambience$classy, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$classy))
table(res_data$attributes$Ambience$hipster, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$hipster))
table(res_data$attributes$Ambience$divey, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$divey))
table(res_data$attributes$Ambience$touristy, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$touristy))
table(res_data$attributes$Ambience$trendy, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$trendy))
table(res_data$attributes$Ambience$upscale, exclude=NULL)
mean(is.na(res_data$attributes$Ambience$upscale))

##music type
table(res_data$attributes$Music$dj, exclude=NULL)
mean(is.na(res_data$attributes$Music$dj))
table(res_data$attributes$Music$background_music, exclude=NULL)
mean(is.na(res_data$attributes$Music$background_music))
table(res_data$attributes$Music$jukebox, exclude=NULL)
mean(is.na(res_data$attributes$Music$jukebox))
table(res_data$attributes$Music$live, exclude=NULL)
mean(is.na(res_data$attributes$Music$live))
table(res_data$attributes$Music$video, exclude=NULL)
mean(is.na(res_data$attributes$Music$video))
table(res_data$attributes$Music$karaoke, exclude=NULL)
mean(is.na(res_data$attributes$Music$karaoke))

##good for dancing
table(res_data$attributes$`Good For Dancing`, exclude=NULL)
mean(is.na(res_data$attributes$`Good For Dancing`))

##good for groups
table(res_data$attributes$`Good For Groups`, exclude=NULL)
mean(is.na(res_data$attributes$`Good For Groups`))

##good for kids
str(res_data$attributes$`Good for Kids`)
table(res_data$attributes$`Good for Kids`, exclude=NULL)
mean(is.na(res_data$attributes$`Good for Kids`))

##ALCOHOL-RELATED VARS
##happy hour
table(res_data$attributes$`Happy Hour`, exclude=NULL)
mean(is.na(res_data$attributes$`Happy Hour`))

##alcohol
table(res_data$attributes$Alcohol, exclude=NULL)
mean(is.na(res_data$attributes$Alcohol))
table(res_data$attributes$BYOB, exclude=NULL)
mean(is.na(res_data$attributes$BYOB))
table(res_data$attributes$`BYOB/Corkage`, exclude=NULL)
mean(is.na(res_data$attributes$`BYOB/Corkage`))

##SMOKING
table(res_data$attributes$Smoking, exclude=NULL)
mean(is.na(res_data$attributes$Smoking))

##Good for which meal (dessert, lunch, dinner, etc.)
table(res_data$attributes$`Good For`$dessert, exclude=NULL)
mean(is.na(res_data$attributes$`Good For`$dessert))
table(res_data$attributes$`Good For`$lunch, exclude=NULL)
mean(is.na(res_data$attributes$`Good For`$lunch))
table(res_data$attributes$`Good For`$dinner, exclude=NULL)
mean(is.na(res_data$attributes$`Good For`$dinner))
table(res_data$attributes$`Good For`$brunch, exclude=NULL)
mean(is.na(res_data$attributes$`Good For`$brunch))
table(res_data$attributes$`Good For`$breakfast, exclude=NULL)
mean(is.na(res_data$attributes$`Good For`$breakfast))
table(res_data$attributes$`Good For`$latenight, exclude=NULL)
mean(is.na(res_data$attributes$`Good For`$latenight))

##Dietary restrictions (dairy-free, vegan, etc.)
table(res_data$attributes$`Dietary Restrictions`$`dairy-free`, exclude=NULL)
mean(is.na(res_data$attributes$`Dietary Restrictions`$`dairy-free`))
table(res_data$attributes$`Dietary Restrictions`$`gluten-free`, exclude=NULL)
mean(is.na(res_data$attributes$`Dietary Restrictions`$`gluten-free`))
table(res_data$attributes$`Dietary Restrictions`$vegan, exclude=NULL)
mean(is.na(res_data$attributes$`Dietary Restrictions`$vegan))
table(res_data$attributes$`Dietary Restrictions`$vegetarian, exclude=NULL)
mean(is.na(res_data$attributes$`Dietary Restrictions`$vegetarian))
table(res_data$attributes$`Dietary Restrictions`$kosher, exclude=NULL)
mean(is.na(res_data$attributes$`Dietary Restrictions`$kosher))
table(res_data$attributes$`Dietary Restrictions`$halal, exclude=NULL)
mean(is.na(res_data$attributes$`Dietary Restrictions`$halal))
table(res_data$attributes$`Dietary Restrictions`$`soy-free`, exclude=NULL)
mean(is.na(res_data$attributes$`Dietary Restrictions`$`soy-free`))

##Take-out (attributes$'Take-out')
table(res_data$attributes$`Take-out`, exclude=NULL)
mean(is.na(res_data$attributes$`Take-out`))
##Drive-thru (attributes$'Drive-Thru')
table(res_data$attributes$`Drive-Thru`, exclude=NULL)
mean(is.na(res_data$attributes$`Drive-Thru`))
table(res_data$attributes$Caters, exclude=NULL)
mean(is.na(res_data$attributes$Caters))
table(res_data$attributes$Delivery, exclude=NULL)
mean(is.na(res_data$attributes$Delivery))
table(res_data$attributes$`Order at Counter`, exclude=NULL)
mean(is.na(res_data$attributes$`Order at Counter`))
table(res_data$attributes$`Has TV`, exclude=NULL)
mean(is.na(res_data$attributes$`Has TV`))
table(res_data$attributes$`Accepts Credit Cards`, exclude=NULL)
mean(is.na(res_data$attributes$`Accepts Credit Cards`))
table(res_data$attributes$`Coat Check`, exclude=NULL)
mean(is.na(res_data$attributes$`Coat Check`))
table(res_data$attributes$`Wheelchair Accessible`, exclude=NULL)
mean(is.na(res_data$attributes$`Wheelchair Accessible`))
table(res_data$attributes$`Dogs Allowed`, exclude=NULL)
mean(is.na(res_data$attributes$`Dogs Allowed`))
table(res_data$attributes$`Open 24 Hours`, exclude=NULL)
mean(is.na(res_data$attributes$`Open 24 Hours`))

## other vars of interest

## city, state
table(res_data$city, exclude=NULL)
count(res_data, city)
table(res_data$state, exclude=NULL)
count(res_data, state)
## neighborhoods: convert original format of list to a character variable to examine frequency distribution
## note that there is a meaningless category "character(0)" that contains 14,679 restaurants, which means
## there is a lot of missingness on this variable and suggests it should probably not 
## be included in the model 
res_data$neighborhoods <- as.character(lapply(res_data$neighborhoods,
                        function(x) { return(as.character(x))} ))

str(res_data$neighborhoods)
table(res_data$neighborhoods, exclude=NULL)

##review_count
table(res_data$review_count, exclude=NULL)
hist(res_data$review_count)
mean(res_data$review_count)
sd(res_data$review_count)
min(res_data$review_count)
max(res_data$review_count)
##stars
table(res_data$stars, exclude=NULL)
hist(res_data$stars)
mean(res_data$stars)
sd(res_data$stars)

##DERIVED VARIABLES
##for categorical variables with missingness, let missing data be its own category; 
##for binary vars, 0 = no, 1 = yes, 2 = missing
##for any continuous variables with missingness, assign a nonmissing value to missing data (0) and 
##create a binary indicator for missingness coded 
##as 1 if missing and 0 if not missing

##      TAKE-OUT
##create 3-category variable that includes missingness
res_data$take_out_cat <- as.numeric(res_data$attributes$`Take-out`)
res_data$take_out_cat[as.numeric(is.na(res_data$take_out_cat)) == 1] <- 2
##check 3-category var
table(res_data$take_out_cat)
##create 2 dummy vars to represent 3-category variable with missingness, 
##where "no" is reference category
##one indicator is for "yes" vs. all else 
res_data$take_out[res_data$take_out_cat == 1] <- 1
res_data$take_out[res_data$take_out_cat == 0 | res_data$take_out_cat == 2] <- 0
table(res_data$take_out)
##the other indicator is for missingness vs. all else
res_data$take_out_miss[res_data$take_out_cat == 2] <- 1
res_data$take_out_miss[res_data$take_out_cat == 1 | res_data$take_out_cat == 0] <- 0
table(res_data$take_out_miss)
##drop original var from dataset
##res_data$attributes$`Take-out` <- NULL

##      WI-FI
##create wifi variable recoded from character to numeric variable: 0 = no, 1 = yes, 2 = missing
res_data$wifi_cat[res_data$attributes$`Wi-Fi`  == 'no'] <- 0
res_data$wifi_cat[res_data$attributes$`Wi-Fi` == 'free' | res_data$attributes$`Wi-Fi` == 'paid'] <- 1
res_data$wifi_cat[as.numeric(is.na(res_data$attributes$`Wi-Fi`)) == 1] <- 2
res_data$wifi_cat <- as.numeric(res_data$wifi)
##create 2 dummy coded vars ("yes" vs. all else and missing vs. all else) to represent 3-category wi-fi var
res_data$wifi[res_data$wifi_cat == 1] <- 1
res_data$wifi[res_data$wifi_cat == 0 | res_data$wifi_cat == 2] <- 0
res_data$wifi_miss[res_data$wifi_cat == 2] <- 1
res_data$wifi_miss[res_data$wifi_cat == 0 | res_data$wifi_cat == 1] <- 0
##check wifi variable
table(res_data$wifi_cat, res_data$attributes$`Wi-Fi`)
table(res_data$wifi_cat, res_data$wifi)
table(res_data$wifi_cat, res_data$wifi_miss)
##drop original wifi variable
##res_data$attributes$`Wi-Fi` <- NULL

##      OUTDOOR SEATING
##outdoor seating variable
res_data$outdoor_cat <- as.numeric(res_data$attributes$`Outdoor Seating`)
res_data$outdoor_cat[as.numeric(is.na(res_data$attributes$`Outdoor Seating`)) == 1] <- 2
table(res_data$outdoor_cat, res_data$attributes$`Outdoor Seating`)
##create 2 dummy coded vars ("yes" vs. all else and missing vs. all else) to represent 3-category var
res_data$outdoor[res_data$outdoor_cat == 1] <- 1
res_data$outdoor[res_data$outdoor_cat == 0 | res_data$outdoor_cat == 2] <- 0
res_data$outdoor_miss[res_data$outdoor_cat == 2] <- 1
res_data$outdoor_miss[res_data$outdoor_cat == 0 | res_data$outdoor_cat == 1] <- 0
##check dummy-coded vars
table(res_data$outdoor_cat, res_data$outdoor)
table(res_data$outdoor_cat, res_data$outdoor_miss)

##      PARKING
table(res_data$attributes$Parking$garage)
table(res_data$attributes$Parking$street)
table(res_data$attributes$Parking$validated)
table(res_data$attributes$Parking$lot)
table(res_data$attributes$Parking$valet)

## parking garage
res_data$garage_cat <- as.numeric(res_data$attributes$Parking$garage)
res_data$garage_cat[as.numeric(is.na(res_data$attributes$Parking$garage)) == 1] <- 2
res_data$garage[res_data$garage_cat == 1] <- 1
res_data$garage[res_data$garage_cat == 0 | res_data$garage_cat == 2] <- 0
res_data$garage_miss[res_data$garage_cat == 2] <- 1
res_data$garage_miss[res_data$garage_cat == 0 | res_data$garage_cat == 1] <- 0
table(res_data$garage_cat, res_data$garage)
table(res_data$garage_cat, res_data$garage_miss)

## street parking
res_data$street_cat <- as.numeric(res_data$attributes$Parking$street)
res_data$street_cat[as.numeric(is.na(res_data$attributes$Parking$street)) == 1] <- 2
res_data$street[res_data$street_cat == 1] <- 1
res_data$street[res_data$street_cat == 0 | res_data$street_cat == 2] <- 0
res_data$street_miss[res_data$street_cat == 2] <- 1
res_data$street_miss[res_data$street_cat == 0 | res_data$street_cat == 1] <- 0
table(res_data$street_cat, res_data$street)
table(res_data$street_cat, res_data$street_miss)

## parking validation
res_data$validate_cat <- as.numeric(res_data$attributes$Parking$validated)
res_data$validate_cat[as.numeric(is.na(res_data$attributes$Parking$validated)) == 1] <- 2
res_data$validate[res_data$validate_cat == 1] <- 1
res_data$validate[res_data$validate_cat == 0 | res_data$validate_cat == 2] <- 0
res_data$validate_miss[res_data$validate_cat == 2] <- 1
res_data$validate_miss[res_data$validate_cat == 0 | res_data$validate_cat == 1] <- 0
table(res_data$validate_cat, res_data$validate)
table(res_data$validate_cat, res_data$validate_miss)

## parking lot
res_data$lot_cat <- as.numeric(res_data$attributes$Parking$lot)
res_data$lot_cat[as.numeric(is.na(res_data$attributes$Parking$lot)) == 1] <- 2
res_data$lot[res_data$lot_cat == 1] <- 1
res_data$lot[res_data$lot_cat == 0 | res_data$lot_cat == 2] <- 0
res_data$lot_miss[res_data$lot_cat == 2] <- 1
res_data$lot_miss[res_data$lot_cat == 0 | res_data$lot_cat == 1] <- 0
table(res_data$lot_cat, res_data$lot)
table(res_data$lot_cat, res_data$lot_miss)

## valet parking
res_data$valet_cat <- as.numeric(res_data$attributes$Parking$valet)
res_data$valet_cat[as.numeric(is.na(res_data$attributes$Parking$valet)) == 1] <- 2
res_data$valet[res_data$valet_cat == 1] <- 1
res_data$valet[res_data$valet_cat == 0 | res_data$valet_cat == 2] <- 0
res_data$valet_miss[res_data$valet_cat == 2] <- 1
res_data$valet_miss[res_data$valet_cat == 0 | res_data$valet_cat == 1] <- 0
table(res_data$valet_cat, res_data$valet)
table(res_data$valet_cat, res_data$valet_miss)
class(res_data$valet)

##create summary parking variable that indicates whether restaurant offers 
##at least one of the 5 types of parking (1), offers none of them (0), or is missing (2)
##create new vars with missingness coded as 99
res_data$garage_cat_new <- res_data$garage_cat
res_data$garage_cat_new[res_data$garage_cat == 2] <- 99
table(res_data$garage_cat_new, res_data$garage_cat)
res_data$street_cat_new <- res_data$street_cat
res_data$street_cat_new[res_data$street_cat == 2] <- 99
table(res_data$street_cat_new, res_data$street_cat)
res_data$validate_cat_new <- res_data$validate_cat
res_data$validate_cat_new[res_data$validate_cat == 2] <- 99
table(res_data$validate_cat_new, res_data$validate_cat)
res_data$lot_cat_new <- res_data$lot_cat
res_data$lot_cat_new[res_data$lot_cat == 2] <- 99
table(res_data$lot_cat_new, res_data$lot_cat)
res_data$valet_cat_new <- res_data$valet_cat
res_data$valet_cat_new[res_data$valet_cat == 2] <- 99
table(res_data$valet_cat_new, res_data$valet_cat)

##create sum that represents different types of parking
res_data$parking_sum = res_data$garage_cat_new + res_data$street_cat_new + res_data$validate_cat_new + res_data$lot_cat_new + res_data$valet_cat_new
table(res_data$parking_sum, exclude=NULL)

##create three-category variable coded 1 if restaurant has any type of parking, 0 if missing on all 5 types of parking or on 4 of 5, and 2 if not in one of other categories (missing)
res_data$parking_cat[(res_data$parking_sum >= 1 & res_data$parking_sum <= 5) | res_data$parking_sum == 100 | res_data$parking_sum == 101] <- 1
res_data$parking_cat[res_data$parking_sum == 0 | res_data$parking_sum == 99] <- 0
res_data$parking_cat[res_data$parking_sum == 396 | res_data$parking_sum == 495] <- 2
table(res_data$parking_cat, res_data$parking_sum, exclude = NULL)

##create 2 dummy coded indicators to represent three-category variable for parking
res_data$parking[res_data$parking_cat == 1] <- 1
res_data$parking[res_data$parking_cat == 0 | res_data$parking_cat == 2] <- 0
res_data$parking_miss[res_data$parking_cat == 2] <- 1
res_data$parking_miss[res_data$parking_cat == 0 | res_data$parking_cat == 1] <- 0
table(res_data$parking_cat, res_data$parking)
table(res_data$parking_cat, res_data$parking_miss)

##takes reservations
res_data$takesres_cat <- as.numeric(res_data$attributes$`Takes Reservations`)
res_data$takesres_cat[as.numeric(is.na(res_data$attributes$`Takes Reservations`)) == 1] <- 2
res_data$takesres[res_data$takesres_cat == 1] <- 1
res_data$takesres[res_data$takesres_cat == 0 | res_data$takesres_cat == 2] <- 0
res_data$takesres_miss[res_data$takesres_cat == 2] <- 1
res_data$takesres_miss[res_data$takesres_cat == 0 | res_data$takesres_cat == 1] <- 0
table(res_data$takesres_cat, res_data$takesres)
table(res_data$takesres_cat, res_data$takesres_miss)

##waiter service
res_data$waiter_cat <- as.numeric(res_data$attributes$`Waiter Service`)
res_data$waiter_cat[as.numeric(is.na(res_data$attributes$`Waiter Service`)) == 1] <- 2
res_data$waiter[res_data$waiter_cat == 1] <- 1
res_data$waiter[res_data$waiter_cat == 0 | res_data$waiter_cat == 2] <- 0
res_data$waiter_miss[res_data$waiter_cat == 2] <- 1
res_data$waiter_miss[res_data$waiter_cat == 0 | res_data$waiter_cat == 1] <- 0
table(res_data$waiter_cat, res_data$waiter)
table(res_data$waiter_cat, res_data$waiter_miss)
##res_data$attributes$`Waiter Service` <- NULL

##caters
res_data$caters_cat <- as.numeric(res_data$attributes$Caters)
res_data$caters_cat[as.numeric(is.na(res_data$attributes$Caters)) == 1] <- 2
res_data$caters[res_data$caters_cat == 1] <- 1
res_data$caters[res_data$caters_cat == 0 | res_data$caters_cat == 2] <- 0
res_data$caters_miss[res_data$caters_cat == 2] <- 1
res_data$caters_miss[res_data$caters_cat == 0 | res_data$caters_cat == 1] <- 0
table(res_data$caters_cat, res_data$caters)
table(res_data$caters_cat, res_data$caters_miss)

##delivery
res_data$delivery_cat <- as.numeric(res_data$attributes$Delivery)
res_data$delivery_cat[as.numeric(is.na(res_data$attributes$Delivery)) == 1] <- 2
res_data$delivery[res_data$delivery_cat == 1] <- 1
res_data$delivery[res_data$delivery_cat == 0 | res_data$delivery_cat == 2] <- 0
res_data$delivery_miss[res_data$delivery_cat == 2] <- 1
res_data$delivery_miss[res_data$delivery_cat == 0 | res_data$delivery_cat == 1] <- 0
table(res_data$delivery_cat, res_data$delivery)
table(res_data$delivery_cat, res_data$delivery_miss)

##accepts credit cards
res_data$credit_cat <- as.numeric(res_data$attributes$`Accepts Credit Cards`)
res_data$credit_cat[as.numeric(is.na(res_data$attributes$`Accepts Credit Cards`)) == 1] <- 2
res_data$credit[res_data$credit_cat == 1] <- 1
res_data$credit[res_data$credit_cat == 2 | res_data$credit_cat == 0] <- 0
res_data$credit_miss[res_data$credit_cat == 2] <- 1
res_data$credit_miss[res_data$credit_cat == 1 | res_data$credit_cat == 0] <- 0
table(res_data$credit_cat, res_data$credit)
table(res_data$credit_cat, res_data$credit_miss)

##wheelchair accessible
res_data$wheelch_cat <- as.numeric(res_data$attributes$`Wheelchair Accessible`)
res_data$wheelch_cat[as.numeric(is.na(res_data$attributes$`Wheelchair Accessible`)) == 1] <- 2
res_data$wheelch[res_data$wheelch_cat == 1] <- 1
res_data$wheelch[res_data$wheelch_cat == 0 | res_data$wheelch_cat == 2] <- 0
res_data$wheelch_miss[res_data$wheelch_cat == 2] <- 1
res_data$wheelch_miss[res_data$wheelch_cat == 1 | res_data$wheelch_cat == 0] <- 0
table(res_data$wheelch_cat, res_data$wheelch)
table(res_data$wheelch_cat, res_data$wheelch_miss)

##has TV
res_data$TV_cat <- as.numeric(res_data$attributes$`Has TV`)
res_data$TV_cat[as.numeric(is.na(res_data$attributes$`Has TV`)) == 1] <- 2
res_data$TV[res_data$TV_cat == 1] <- 1
res_data$TV[res_data$TV_cat == 0 | res_data$TV_cat == 2] <- 0
res_data$TV_miss[res_data$TV_cat == 2] <- 1
res_data$TV_miss[res_data$TV_cat == 0 | res_data$TV_cat == 1] <- 0
table(res_data$TV_cat, res_data$TV)
table(res_data$TV_cat, res_data$TV_miss)

##noise level (convert character variable to numeric variable)
table(res_data$attributes$`Noise Level`)
res_data$noisier <- 'NA'
res_data$noisier[res_data$attributes$`Noise Level` == 'very_loud'] <- 4
res_data$noisier[res_data$attributes$`Noise Level` == 'loud'] <- 3
res_data$noisier[res_data$attributes$`Noise Level` == 'average'] <- 2
res_data$noisier[res_data$attributes$`Noise Level` == 'quiet'] <- 1
res_data$noisier <- as.numeric(res_data$noisier)
res_data$noisier[as.numeric(is.na(res_data$noisier)) == 1] <- 0
res_data$noisier_miss <- ifelse(res_data$noisier == 0, 1, 0)
table(res_data$noisier, res_data$attributes$`Noise Level`)
table(res_data$noisier, res_data$noisier_miss)

##good for kids
res_data$gfk_cat <- as.numeric(res_data$attributes$`Good for Kids`)
res_data$gfk_cat[as.numeric(is.na(res_data$attributes$`Good for Kids`)) == 1] <- 2
res_data$gfk[res_data$gfk_cat == 1] <- 1
res_data$gfk[res_data$gfk_cat == 0 | res_data$gfk_cat == 2] <- 0
res_data$gfk_miss[res_data$gfk_cat == 2] <- 1
res_data$gfk_miss[res_data$gfk_cat == 0 | res_data$gfk_cat == 1] <- 0
table(res_data$gfk_cat, res_data$gfk)
table(res_data$gfk_cat, res_data$gfk_miss)

##good for groups
res_data$gfg_cat <- as.numeric(res_data$attributes$`Good For Groups`)
res_data$gfg_cat[as.numeric(is.na(res_data$attributes$`Good For Groups`)) == 1] <- 2
res_data$gfg[res_data$gfg_cat == 1] <- 1
res_data$gfg[res_data$gfg_cat == 0 | res_data$gfg_cat == 2] <- 0
res_data$gfg_miss[res_data$gfg_cat == 2] <- 1
res_data$gfg_miss[res_data$gfg_cat == 1 | res_data$gfg_cat == 0] <- 0
table(res_data$gfg_cat, res_data$gfg)
table(res_data$gfg_cat, res_data$gfg_miss)

##price range
res_data$price <- as.numeric(res_data$attributes$`Price Range`)
res_data$price[as.numeric(is.na(res_data$attributes$`Price Range`)) == 1] <- 0
table(res_data$price, res_data$attributes$`Price Range`)

##create binary indicator of missingness for price range
res_data$price_miss <- ifelse(res_data$price == 0, 1, 0)
table(res_data$price, res_data$price_miss)

##attire (convert from character to numeric variable from 1 to 3, 
##where 1 = casual, 2 = dressy, 3 = formal)
res_data$attire[res_data$attributes$Attire == 'casual'] <- 1
res_data$attire[res_data$attributes$Attire == 'dressy'] <- 2
res_data$attire[res_data$attributes$Attire == 'formal'] <- 3
res_data$attire <- as.numeric(res_data$attire)
res_data$attire[as.numeric(is.na(res_data$attire)) == 1] <- 0
table(res_data$attire, res_data$attributes$Attire)

##create binary indicator for missingness on attire
res_data$attire_miss <- ifelse(res_data$attire == 0, 1, 0)
table(res_data$attire, res_data$attire_miss)

##      AMBIENCE

##romantic
res_data$romantic_cat <- as.numeric(res_data$attributes$Ambience$romantic)
res_data$romantic_cat[as.numeric(is.na(res_data$attributes$Ambience$romantic)) == 1] <- 2
res_data$romantic[res_data$romantic_cat == 1] <- 1
res_data$romantic[res_data$romantic_cat == 0 | res_data$romantic_cat == 2] <- 0
res_data$romantic_miss[res_data$romantic_cat == 2] <- 1
res_data$romantic_miss[res_data$romantic_cat == 0 | res_data$romantic_cat == 1] <- 0
table(res_data$romantic_cat, res_data$romantic)
table(res_data$romantic_cat, res_data$romantic_miss)

##intimate
res_data$intimate_cat <- as.numeric(res_data$attributes$Ambience$intimate)
res_data$intimate_cat[as.numeric(is.na(res_data$attributes$Ambience$intimate)) == 1] <- 2
res_data$intimate[res_data$intimate_cat == 1] <- 1
res_data$intimate[res_data$intimate_cat == 0 | res_data$intimate_cat == 2] <- 0
res_data$intimate_miss[res_data$intimate_cat == 2] <- 1
res_data$intimate_miss[res_data$intimate_cat == 1 | res_data$intimate_cat == 0] <- 0
table(res_data$intimate_cat, res_data$intimate)
table(res_data$intimate_cat, res_data$intimate_miss)

##classy
res_data$classy_cat <- as.numeric(res_data$attributes$Ambience$classy)
res_data$classy_cat[as.numeric(is.na(res_data$attributes$Ambience$classy)) == 1] <- 2
res_data$classy[res_data$classy_cat == 1] <- 1
res_data$classy[res_data$classy_cat == 0 | res_data$classy_cat == 2] <- 0
res_data$classy_miss[res_data$classy_cat == 2] <- 1
res_data$classy_miss[res_data$classy_cat == 0 | res_data$classy_cat == 1] <- 0
table(res_data$classy_cat, res_data$classy)
table(res_data$classy_cat, res_data$classy_miss)

##hipster
res_data$hipster_cat <- as.numeric(res_data$attributes$Ambience$hipster)
res_data$hipster_cat[as.numeric(is.na(res_data$attributes$Ambience$hipster)) == 1] <- 2
res_data$hipster[res_data$hipster_cat == 1] <- 1
res_data$hipster[res_data$hipster_cat == 0 | res_data$hipster_cat == 2] <- 0
res_data$hipster_miss[res_data$hipster_cat == 2] <- 1
res_data$hipster_miss[res_data$hipster_cat == 0 | res_data$hipster_cat == 1] <- 0
table(res_data$hipster_cat, res_data$hipster)
table(res_data$hipster_cat, res_data$hipster_miss)

##divey
res_data$divey_cat <- as.numeric(res_data$attributes$Ambience$divey)
res_data$divey_cat[as.numeric(is.na(res_data$attributes$Ambience$divey)) == 1] <- 2
res_data$divey[res_data$divey_cat == 1] <- 1
res_data$divey[res_data$divey_cat == 0 | res_data$divey_cat == 2] <- 0
res_data$divey_miss[res_data$divey_cat == 2] <- 1
res_data$divey_miss[res_data$divey_cat == 0 | res_data$divey_cat == 1] <- 0
table(res_data$divey_cat, res_data$divey)
table(res_data$divey_cat, res_data$divey_miss)

##touristy
res_data$touristy_cat <- as.numeric(res_data$attributes$Ambience$touristy)
res_data$touristy_cat[as.numeric(is.na(res_data$attributes$Ambience$touristy)) == 1] <- 2
res_data$touristy[res_data$touristy_cat == 1] <- 1
res_data$touristy[res_data$touristy_cat == 0 | res_data$touristy_cat == 2] <- 0
res_data$touristy_miss[res_data$touristy_cat == 2] <- 1
res_data$touristy_miss[res_data$touristy_cat == 0 | res_data$touristy_cat == 1] <- 0
table(res_data$touristy_cat, res_data$touristy)
table(res_data$touristy_cat, res_data$touristy_miss)

##trendy
res_data$trendy_cat <- as.numeric(res_data$attributes$Ambience$trendy)
res_data$trendy_cat[as.numeric(is.na(res_data$attributes$Ambience$trendy)) == 1] <- 2
res_data$trendy[res_data$trendy_cat == 1] <- 1
res_data$trendy[res_data$trendy_cat == 0 | res_data$trendy_cat == 2] <- 0
res_data$trendy_miss[res_data$trendy_cat == 2] <- 1
res_data$trendy_miss[res_data$trendy_cat == 0 | res_data$trendy_cat == 1] <- 0
table(res_data$trendy_cat, res_data$trendy)
table(res_data$trendy_cat, res_data$trendy_miss)

##upscale
res_data$upscale_cat <- as.numeric(res_data$attributes$Ambience$upscale)
res_data$upscale_cat[as.numeric(is.na(res_data$attributes$Ambience$upscale)) == 1] <- 2
res_data$upscale[res_data$upscale_cat == 1] <- 1
res_data$upscale[res_data$upscale_cat == 0 | res_data$upscale_cat == 2] <- 0
res_data$upscale_miss[res_data$upscale_cat == 2] <- 1
res_data$upscale_miss[res_data$upscale_cat == 0 | res_data$upscale_cat== 1] <- 0
table(res_data$upscale_cat, res_data$upscale)
table(res_data$upscale_cat, res_data$upscale_miss)

##      ALCOHOL

##create 3-category variable for alcohol: 1 = yes (beer and wine or full bar), 0 = no, 2 = missing
res_data$alcohol_cat[res_data$attributes$Alcohol == 'beer_and_wine' | res_data$attributes$Alcohol == 'full_bar'] <- 1
res_data$alcohol_cat[res_data$attributes$Alcohol == 'none'] <- 0
res_data$alcohol_cat[as.numeric(is.na(res_data$attributes$Alcohol)) == 1] <- 2
res_data$alcohol[res_data$alcohol_cat == 1] <- 1
res_data$alcohol[res_data$alcohol_cat == 0 | res_data$alcohol_cat == 2] <- 0
res_data$alcohol_miss[res_data$alcohol_cat == 2] <- 1
res_data$alcohol_miss[res_data$alcohol_cat == 0 | res_data$alcohol_cat == 1] <- 0
table(res_data$alcohol_cat, res_data$alcohol)
table(res_data$alcohol_cat, res_data$alcohol_miss)

##create 4-category variable for alcohol: 1 = beer_and_wine, 2 = full_bar, 3 = none, 4 = missing
res_data$alcohol_cat4[res_data$attributes$Alcohol == 'beer_and_wine'] <- 1
res_data$alcohol_cat4[res_data$attributes$Alcohol == 'full_bar'] <- 2
res_data$alcohol_cat4[res_data$attributes$Alcohol == 'none'] <- 3
res_data$alcohol_cat4[as.numeric(is.na(res_data$attributes$Alcohol)) == 1] <- 4
table(res_data$alcohol_cat4, res_data$attributes$Alcohol, exclude=NULL)

##represent alcohol with 3 dummy indicators where reference category is "none"
res_data$beer_and_wine <- ifelse(res_data$alcohol_cat4 == 1, 1, 0) 
table(res_data$beer_and_wine, res_data$alcohol_cat4, exclude=NULL)
res_data$fullbar <- ifelse(res_data$alcohol_cat4 == 2, 1, 0)
table(res_data$fullbar, res_data$alcohol_cat4)
res_data$alc_miss <- ifelse(res_data$alcohol_cat4 == 4,1,0) 
table(res_data$alc_miss, res_data$alcohol_cat4, exclude=NULL)

##      GOOD FOR WHICH MEAL
##breakfast
res_data$breakfast_cat <- as.numeric(res_data$attributes$`Good For`$breakfast)
res_data$breakfast_cat[as.numeric(is.na(res_data$attributes$`Good For`$breakfast)) == 1] <- 2
res_data$breakfast[res_data$breakfast_cat == 1] <- 1
res_data$breakfast[res_data$breakfast_cat == 0 | res_data$breakfast_cat == 2] <- 0
res_data$breakfast_miss[res_data$breakfast_cat == 2] <- 1
res_data$breakfast_miss[res_data$breakfast_cat == 0 | res_data$breakfast_cat == 1] <- 0
table(res_data$breakfast_cat, res_data$breakfast)
table(res_data$breakfast_cat, res_data$breakfast_miss)

##brunch
res_data$brunch_cat <- as.numeric(res_data$attributes$`Good For`$brunch)
res_data$brunch_cat[as.numeric(is.na(res_data$attributes$`Good For`$brunch)) == 1] <- 2
res_data$brunch[res_data$brunch_cat == 1] <- 1
res_data$brunch[res_data$brunch_cat == 0 | res_data$brunch_cat == 2] <- 0
res_data$brunch_miss[res_data$brunch_cat == 2] <- 1
res_data$brunch_miss[res_data$brunch_cat == 0 | res_data$brunch_cat == 1] <- 0
table(res_data$brunch_cat, res_data$brunch)
table(res_data$brunch_cat, res_data$brunch_miss)

##lunch
res_data$lunch_cat <- as.numeric(res_data$attributes$`Good For`$lunch)
res_data$lunch_cat[as.numeric(is.na(res_data$attributes$`Good For`$lunch)) == 1] <- 2
res_data$lunch[res_data$lunch_cat == 1] <- 1
res_data$lunch[res_data$lunch_cat == 0 | res_data$lunch_cat == 2] <- 0
res_data$lunch_miss[res_data$lunch_cat == 2] <- 1
res_data$lunch_miss[res_data$lunch_cat == 0 | res_data$lunch_cat == 1] <- 0
table(res_data$lunch_cat, res_data$lunch)
table(res_data$lunch_cat, res_data$lunch_miss)

##dinner
res_data$dinner_cat <- as.numeric(res_data$attributes$`Good For`$dinner)
res_data$dinner_cat[as.numeric(is.na(res_data$attributes$`Good For`$dinner)) == 1] <- 2
res_data$dinner[res_data$dinner_cat == 1] <- 1
res_data$dinner[res_data$dinner_cat == 0 | res_data$dinner_cat == 2] <- 0
res_data$dinner_miss[res_data$dinner_cat == 2] <- 1
res_data$dinner_miss[res_data$dinner_cat == 0 | res_data$dinner_cat == 1] <- 0
table(res_data$dinner_cat, res_data$dinner)
table(res_data$dinner_cat, res_data$dinner_miss)

##dessert
res_data$dessert_cat <- as.numeric(res_data$attributes$`Good For`$dessert)
res_data$dessert_cat[as.numeric(is.na(res_data$attributes$`Good For`$dessert)) == 1] <- 2
res_data$dessert[res_data$dessert_cat == 1] <- 1
res_data$dessert[res_data$dessert_cat == 0 | res_data$dessert_cat == 2] <- 0
res_data$dessert_miss[res_data$dessert_cat == 2] <- 1
res_data$dessert_miss[res_data$dessert_cat == 0 | res_data$dessert_cat == 1] <- 0
table(res_data$dessert_cat, res_data$dessert)
table(res_data$dessert_cat, res_data$dessert_miss)

##latenight
res_data$latenight_cat <- as.numeric(res_data$attributes$`Good For`$latenight)
res_data$latenight_cat[as.numeric(is.na(res_data$attributes$`Good For`$latenight)) == 1] <- 2
res_data$latenight[res_data$latenight_cat == 1] <- 1
res_data$latenight[res_data$latenight_cat == 0 | res_data$latenight_cat == 2] <- 0
res_data$latenight_miss[res_data$latenight_cat == 2] <- 1
res_data$latenight_miss[res_data$latenight_cat == 0 | res_data$latenight_cat == 1] <- 0
table(res_data$latenight_cat, res_data$latenight)
table(res_data$latenight_cat, res_data$latenight_miss)

## REGRESSION DIAGNOSTICS

##run basic regression model to identify problems and see if covariates behave in surprising ways

##run test model regressing ratings on all predictors including restaurant subcategories
totalmodel1 <- lm(stars ~ city + take_out + take_out_miss + wifi + wifi_miss + outdoor + outdoor_miss + 
                          garage + garage_miss + street + street_miss + validate + validate_miss + 
                          lot + lot_miss + valet + valet_miss + 
                          takesres + takesres_miss + waiter + waiter_miss + caters + caters_miss + 
                          delivery + delivery_miss + credit + credit_miss + wheelch + wheelch_miss + 
                          TV + TV_miss + noisier + noisier_miss + gfk + gfk_miss + gfg + gfg_miss + 
                          price + price_miss + attire + attire_miss + 
                          romantic + romantic_miss + intimate + intimate_miss + classy + classy_miss + 
                          hipster + hipster_miss + divey + divey_miss + touristy + touristy_miss + 
                          trendy + trendy_miss + upscale + upscale_miss + beer_and_wine + fullbar + 
                          alc_miss + breakfast + breakfast_miss + brunch + brunch_miss + lunch + lunch_miss + 
                          dinner + dinner_miss + dessert + dessert_miss + latenight + latenight_miss +
                          AmericanNew + AmericanTrad + ArtsEntertainment + AsianFusion + Bagels + Bakeries + 
                          Barbeque + Bars + BreakfastBrunch + British + Buffets + Burgers + Cafes + 
                          CajunCreole + CanadianNew + Caribbean + ChickenWings + Chinese + CocktailBars + 
                          CoffeeTea + ComfortFood + Delis + Desserts + Diners + Event + FastFood + FishChips + 
                          FoodTrucks + French + Gastropubs + German + GlutenFree + Greek + Hawaiian + HotDogs + 
                          IceCream + Indian + Italian + Japanese + JuiceBars + Korean + LatAmer + Lounges + 
                          Mediterranean + Mexican + MidEast + Nightlife + Pizza + Pubs + Salad + Sandwiches + 
                          Seafood + SoulFood + Soup + Southern + Specialty + SportsBars + Steakhouses + SushiBars + 
                          TapasBars + TapasSmall + TexMex + Thai + Vegan + Vegetarian + Vietnamese + WineBars, 
                          data=res_data)
summary(totalmodel1)

## in test model, 8 variables were identified as having singularities

## Referring back to computations of missingness, I noticed that the missingness indicators 
##for ambience type have nearly identical amounts of missingness and are likely redundant. 
## I examined cross-tabs for these indicators of missingness (romantic, intimate, classy, touristy, and trendy) 
## and found that these were completely redundant.
table(res_data$romantic_miss, res_data$intimate_miss)
table(res_data$romantic_miss, res_data$classy_miss)
table(res_data$romantic_miss, res_data$touristy_miss)
table(res_data$romantic_miss, res_data$trendy_miss)
table(res_data$romantic_miss, res_data$hipster_miss)
table(res_data$romantic_miss, res_data$divey_miss)
table(res_data$romantic_miss, res_data$upscale_miss)


## given redundancy of indicators of missingness, I will remove all but 1 from test model

## also noticed that indicators of missingness for type of meal that restaurant is good for 
##(e.g., lunch, dinner, and latenight) all have nearly identical amounts of missingness and are likely redundant
## examined cross-tabs of these missingness indicators and found that these were also completely redundant
## will remove all but one of them from test model
table(res_data$breakfast_miss, res_data$brunch_miss)
table(res_data$breakfast_miss, res_data$lunch_miss)
table(res_data$breakfast_miss, res_data$dinner_miss)
table(res_data$breakfast_miss, res_data$dessert_miss)
table(res_data$breakfast_miss, res_data$latenight_miss)

## I also noticed that indicators of missingness for parking are likely to be redundant
## I will remove all but one of these from the model
table(res_data$garage_miss, res_data$street_miss)
table(res_data$garage_miss, res_data$validate_miss)
table(res_data$garage_miss, res_data$lot_miss)
table(res_data$garage_miss, res_data$valet_miss)

## rerun test model with redundant missingness indicators removed
totalmodel2 <- lm(stars ~ city + take_out + take_out_miss + wifi + wifi_miss + outdoor + outdoor_miss + 
                          garage + street + validate + validate_miss + lot + valet +
                          takesres + takesres_miss + waiter + waiter_miss + caters + caters_miss + 
                          delivery + delivery_miss + credit + credit_miss + wheelch + wheelch_miss + 
                          TV + TV_miss + noisier + noisier_miss + gfk + gfk_miss + gfg + gfg_miss + 
                          price + price_miss + attire + attire_miss + 
                          romantic + intimate + classy + hipster + divey + divey_miss + 
                          touristy + trendy + upscale + 
                          beer_and_wine + fullbar + alc_miss + 
                          breakfast + brunch + lunch + dinner + dessert + dessert_miss + latenight +
                          AmericanNew + AmericanTrad + ArtsEntertainment + AsianFusion + 
                          Bagels + Bakeries + Barbeque + Bars + BreakfastBrunch + British + Buffets + Burgers +  
                          Cafes + CajunCreole + CanadianNew + Caribbean + ChickenWings + Chinese + CocktailBars + 
                          CoffeeTea + ComfortFood + Delis + Desserts + Diners + 
                          Event + FastFood + FishChips + FoodTrucks + French + Gastropubs + German + GlutenFree + 
                          Greek + Hawaiian + HotDogs + IceCream +  Indian + Italian + Japanese + JuiceBars + 
                          Korean + LatAmer + Lounges + Mediterranean + Mexican + MidEast + Nightlife + 
                          Pizza + Pubs + 
                          Salad + Sandwiches + Seafood + SoulFood + Soup + Southern + Specialty + SportsBars + 
                          Steakhouses + SushiBars + TapasBars + TapasSmall + TexMex + Thai + 
                          Vegan + Vegetarian + Vietnamese + WineBars, data=res_data)
summary(totalmodel2)

## examine residuals of totalmodel2 to determine whether they are normally distributed, 
##thereby satisfying a basic assumption of linear regression

hist(resid(totalmodel2))

## residuals do appear to approximate the normal distribution, indicating that linear regression may be tenable


## write output to a spreadsheet 
write.csv(as.data.frame(summary(totalmodel2)$coef), file="totalmodel2.csv")

## run multilevel model in which restaurants are nested within cities and there's a random effect for intercepts 
## in which they are allowed to vary across cities

totalmodel3 <- lmer(stars ~ take_out + take_out_miss + wifi + wifi_miss + outdoor + outdoor_miss + 
                            garage + street + validate + validate_miss + lot + valet +
                            takesres + takesres_miss + waiter + waiter_miss + caters + caters_miss + 
                            delivery + delivery_miss + credit + credit_miss + wheelch + wheelch_miss + 
                            TV + TV_miss + noisier + noisier_miss + gfk + gfk_miss + gfg + gfg_miss + 
                            price + price_miss + attire + attire_miss + 
                            romantic + intimate + classy + hipster + divey + divey_miss + 
                            touristy + trendy + upscale + 
                            beer_and_wine + fullbar + alc_miss + 
                            breakfast + brunch + lunch + dinner + dessert + dessert_miss + latenight +
                            AmericanNew + AmericanTrad + ArtsEntertainment + AsianFusion + 
                            Bagels + Bakeries + Barbeque + Bars + BreakfastBrunch + British + Buffets + Burgers +  
                            Cafes + CajunCreole + CanadianNew + Caribbean + ChickenWings + Chinese + CocktailBars + 
                            CoffeeTea + ComfortFood + Delis + Desserts + Diners + 
                            Event + FastFood + FishChips + FoodTrucks + French + Gastropubs + German + GlutenFree + 
                            Greek + Hawaiian + HotDogs + IceCream +  Indian + Italian + Japanese + JuiceBars + 
                            Korean + LatAmer + Lounges + Mediterranean + Mexican + MidEast + Nightlife + 
                            Pizza + Pubs + 
                            Salad + Sandwiches + Seafood + SoulFood + Soup + Southern + Specialty + SportsBars + 
                            Steakhouses + SushiBars + TapasBars + TapasSmall + TexMex + Thai + 
                            Vegan + Vegetarian + Vietnamese + WineBars + (1|city), data=res_data)
summary(totalmodel3)

## write output to a spreadsheet 
write.csv(as.data.frame(summary(totalmodel3)$coef), file="totalmodel3.csv")

##  examine model where restaurants are weighted by review_count (i.e., restaurants with more reviews have more influence on 
## parameter estimates)

totalmodel4 <- lm(stars ~ city + take_out + take_out_miss + wifi + wifi_miss + outdoor + outdoor_miss + 
                          garage + street + validate + validate_miss + lot + valet +
                          takesres + takesres_miss + waiter + waiter_miss + caters + caters_miss + 
                          delivery + delivery_miss + credit + credit_miss + wheelch + wheelch_miss + 
                          TV + TV_miss + noisier + noisier_miss + gfk + gfk_miss + gfg + gfg_miss + 
                          price + price_miss + attire + attire_miss + 
                          romantic + intimate + classy + hipster + divey + divey_miss + 
                          touristy + trendy + upscale + 
                          beer_and_wine + fullbar + alc_miss + 
                          breakfast + brunch + lunch + dinner + dessert + dessert_miss + latenight +
                          AmericanNew + AmericanTrad + ArtsEntertainment + AsianFusion + 
                          Bagels + Bakeries + Barbeque + Bars + BreakfastBrunch + British + Buffets + Burgers +  
                          Cafes + CajunCreole + CanadianNew + Caribbean + ChickenWings + Chinese + CocktailBars + 
                          CoffeeTea + ComfortFood + Delis + Desserts + Diners + 
                          Event + FastFood + FishChips + FoodTrucks + French + Gastropubs + German + GlutenFree + 
                          Greek + Hawaiian + HotDogs + IceCream +  Indian + Italian + Japanese + JuiceBars + 
                          Korean + LatAmer + Lounges + Mediterranean + Mexican + MidEast + Nightlife + 
                          Pizza + Pubs + 
                          Salad + Sandwiches + Seafood + SoulFood + Soup + Southern + Specialty + SportsBars + 
                          Steakhouses + SushiBars + TapasBars + TapasSmall + TexMex + Thai + 
                          Vegan + Vegetarian + Vietnamese + WineBars, data=res_data, weights=review_count)
summary(totalmodel4)

## write output to a spreadsheet 
write.csv(as.data.frame(summary(totalmodel4)$coef), file="totalmodel4.csv")

## add weights to multilevel model 
totalmodel5 <- lmer(stars ~ take_out + take_out_miss + wifi + wifi_miss + outdoor + outdoor_miss + 
                            garage + street + validate + validate_miss + lot + valet +
                            takesres + takesres_miss + waiter + waiter_miss + caters + caters_miss + 
                            delivery + delivery_miss + credit + credit_miss + wheelch + wheelch_miss + 
                            TV + TV_miss + noisier + noisier_miss + gfk + gfk_miss + gfg + gfg_miss + 
                            price + price_miss + attire + attire_miss + 
                            romantic + intimate + classy + hipster + divey + divey_miss + 
                            touristy + trendy + upscale + 
                            beer_and_wine + fullbar + alc_miss + 
                            breakfast + brunch + lunch + dinner + dessert + dessert_miss + latenight +
                            AmericanNew + AmericanTrad + ArtsEntertainment + AsianFusion + 
                            Bagels + Bakeries + Barbeque + Bars + BreakfastBrunch + British + Buffets + Burgers +  
                            Cafes + CajunCreole + CanadianNew + Caribbean + ChickenWings + Chinese + CocktailBars + 
                            CoffeeTea + ComfortFood + Delis + Desserts + Diners + 
                            Event + FastFood + FishChips + FoodTrucks + French + Gastropubs + German + GlutenFree + 
                            Greek + Hawaiian + HotDogs + IceCream +  Indian + Italian + Japanese + JuiceBars + 
                            Korean + LatAmer + Lounges + Mediterranean + Mexican + MidEast + Nightlife + 
                            Pizza + Pubs + 
                            Salad + Sandwiches + Seafood + SoulFood + Soup + Southern + Specialty + SportsBars + 
                            Steakhouses + SushiBars + TapasBars + TapasSmall + TexMex + Thai + 
                            Vegan + Vegetarian + Vietnamese + WineBars + (1|city), data=res_data, weights=review_count)
summary(totalmodel5)

print(totalmodel5, correlation=TRUE) 
## write output to a spreadsheet 
write.csv(as.data.frame(summary(totalmodel5)$coef), file="totalmodel5.csv")

##save data file that includes derived variables
saveRDS(res_data, "res_data.RDS")












































