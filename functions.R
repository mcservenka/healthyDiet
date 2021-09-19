#specify column names of required source data
foodCols = c("loggedFood.name", "loggedFood.brand", "loggedFood.amount", "loggedFood.unit.name", 
             "nutritionalValues.calories", "nutritionalValues.fat", "nutritionalValues.fiber", 
             "nutritionalValues.carbs", "nutritionalValues.sodium", "nutritionalValues.protein", 
             "logDate")
beverageCols = c("date", "waterAmount", "measurementUnit")
weightCols = c("weight", "bmi", "date")
exerciseCols = c("activityName", "averageHeartRate" , "calories", "duration",
                 "activeDuration", "steps", "lastModified", "startTime", "originalStartTime",
                 "originalDuration", "elevationGain", "hasActiveZoneMinutes", "distance", "distanceUnit",
                 "speed", "pace")


getData = function(folder, pattern) {
  files = list.files(folder, pattern = pattern, full.names = T) #get files
  mainList = list()
  for (index in 1:length(files)) {
    li = fromJSON(file = files[index])
    mainList[[index]] = li
  }
  mainList = unlist(mainList, recursive = F)
  return(mainList)
}

getColNames = function(baseList) {
  cols = c()
  for (index in 1:length(baseList)) {
    cols = c(cols,names(unlist(baseList[[index]])))
  }
  colsUnique = unique(cols)
  return(colsUnique)
}



listsToDataFrame = function(baseList, colNames) {
  if (is.null(colNames)) {
    colNames = getColNames(baseList = baseList)
  }
  
  df = as.data.frame(matrix(data = NA, nrow = length(baseList), ncol = length(colNames)))
  names(df) = colNames
  for (index in 1:length(baseList)) {
    df[index,] = unlist(baseList[[index]])[colNames]
  }
  return(df)
}
