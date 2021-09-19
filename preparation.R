setwd("C:/Users/Markus Cservenka/Desktop/Data Science/3 Healthy Diet") #set wd for functions

source("functions.r")
library("rjson")
library("tidyverse")

#
#get data
#
rawFood = getData("data/raw/Nutrition", "^food_logs")
rawBeverage = getData("data/raw/Nutrition", "^water_logs")
rawWeight = getData("data/raw/Weight", "^weight")
rawExercise = getData("data/raw/Physical Activity", "^exercise")


#
#transform lists to data frames
#
food = listsToDataFrame(rawFood, foodCols)
beverage = listsToDataFrame(rawBeverage, beverageCols)
weight = listsToDataFrame(rawWeight, weightCols)
exercise = listsToDataFrame(rawExercise, exerciseCols)

rm(rawFood, rawBeverage, rawExercise, rawWeight,
   foodCols, beverageCols, weightCols, exerciseCols)




#prepare data
str(beverage)
beverage = beverage %>%
  transmute(waterAmount = as.numeric(waterAmount),
            date = as.Date(date, format = "%m/%d/%y")) %>%
  rename(Date = date, Water_Amount = waterAmount)

beverageAgg = beverage %>%
  group_by(Date) %>%
  summarise(Water_Amount = sum(Water_Amount, na.rm = T))


str(food)

food = food %>%
  mutate_at(c("loggedFood.amount", "nutritionalValues.calories", "nutritionalValues.fiber",
              "nutritionalValues.fat","nutritionalValues.carbs", "nutritionalValues.sodium",
              "nutritionalValues.protein"), as.numeric) %>%
  mutate(date = as.Date(logDate, format = "%Y-%m-%d"),
         #convert liter to gram
         loggedFood.amount = case_when(loggedFood.unit.name == "liter" ~ loggedFood.amount * 1000,
                                       loggedFood.unit.name == "gram" ~ loggedFood.amount)) %>%
  rename(Food_Name = loggedFood.name, Food_Brand = loggedFood.brand, Food_Amount = loggedFood.amount, 
         Food_Unit = loggedFood.unit.name, Calories = nutritionalValues.calories, Fat = nutritionalValues.fat,
         Fiber = nutritionalValues.fiber, Carbs = nutritionalValues.carbs, Sodium = nutritionalValues.sodium,
         Protein = nutritionalValues.protein, LogDate = logDate, Date = date)


foodAgg = food %>%
  group_by(Date) %>%
  summarise_at(c("Calories", "Carbs", "Fat", "Protein", "Food_Amount"), sum)


str(weight)

weight = weight %>%
  transmute(Weight = as.numeric(weight),
            BMI = as.numeric(bmi),
            Date = as.Date(date, format = "%m/%d/%y"))
#no aggregation necessary because weight is daily


str(exercise)

exercise = exercise %>%
  mutate_at(c("averageHeartRate", "calories", "duration", "activeDuration", "steps",
              "originalDuration", "elevationGain", "distance", "speed", "pace"), as.numeric) %>%
  mutate(Date = as.Date(substr(lastModified, 1, 8), format = "%m/%d/%y"),
         Duration_Minutes = round(duration / 60000, 2)) %>%
  rename(Activity_Name = activityName, Heart_Rate = averageHeartRate, Calories_Burned = calories,
         Duration = duration, Active_Duration = activeDuration, Steps = steps, Elevation_Gain = elevationGain)


exerciseAgg1 = exercise %>% #summarise with mean
  group_by(Date) %>%
  summarise(Heart_Rate = mean(Heart_Rate, na.rm = TRUE))

exerciseAgg2 = exercise %>% #summarise with sum
  group_by(Date) %>%
  summarise_at(c("Calories_Burned", "Duration_Minutes", "Duration", "Active_Duration", "Steps"), sum, na.rm = TRUE)

exerciseAgg = inner_join(exerciseAgg1, exerciseAgg2)
rm(exerciseAgg1, exerciseAgg2)



#create base data frame
allDays = seq(as.Date("2019-11-09"), as.Date("2021-02-02"), by="days")
base = data.frame(matrix(data = NA, nrow = length(allDays), ncol = 1))
names(base) = "Date"
base$Date = allDays
rm(allDays)

#merge aggregated data frames
base = base %>% 
       left_join(foodAgg) %>%
       left_join(beverageAgg) %>%
       left_join(weight) %>%
       left_join(exerciseAgg)

rm(foodAgg, beverageAgg, exerciseAgg)

#create csv files
write.csv(base, "data/prepared/aggregated.csv", row.names = FALSE)
write.csv(food, "data/prepared/food.csv", row.names = FALSE)
write.csv(beverage, "data/prepared/beverage.csv", row.names = FALSE)
write.csv(weight, "data/prepared/weight.csv", row.names = FALSE)
write.csv(exercise, "data/prepared/exercise.csv", row.names = FALSE)
