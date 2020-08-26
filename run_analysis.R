#never use R without this amazing package ^_^
library(tidyverse)

#read data sets
X_train = as_tibble(read.table("X_train.txt"))
X_test = as_tibble(read.table("X_test.txt"))
y_train = as_tibble(read.table("y_train.txt"))
y_test = as_tibble(read.table("y_test.txt"))

#merge
merge.X <- rbind(X_train,X_test)
merge.y <- rbind(y_train,y_test)

#read columns names and store as a  lowercase vector, then process 
#the columns by removing '-' and any '()'. 
#Note the use of \\ to match a parenthesis
col_names = read.table("features.txt") %>% select(2)
col_names = tolower(col_names$V2)
col_names = str_replace_all(col_names, "-", "")
col_names = str_replace_all(col_names, "\\(\\)", "")


#set appropriate column names, then go through and get the columns
#associated with mean and standard deviation
colnames(merge.X) <- col_names
merge.X <- merge.X %>% select(contains(c("mean", "std")))

#thats X... now lets take care of y. First lets write a quick function that 
#makes the variables more clear
map_it <- function(x){
  ifelse(x == 1, "WALKING",
  ifelse(x == 2, "WALKING_UPSTAIRS",
  ifelse(x == 3, "WALKING_DOWNSTAIRS",
  ifelse(x == 4, "SITTING",
  ifelse(x == 5, "STANDING",
  ifelse(x == 6, "LAYING", x))))))
}
#set the column name
colnames(merge.y) <- "activity"

#replace 1,2,3,4,5,6 with mapped name. This makes the column a character vector
#but in reality it is best to make these factor variables
merge.y <- merge.y %>% mutate(activity = as.factor(map_it(activity)))

#finally combine the columns to make a clean data set
masterdf <- cbind(merge.X, merge.y)


#still not done. step 5 wants the average for each activity for the different
#subjects. Thus we want to group the activity and then calculate the mean.
#Here we will use R's awesome new across function which is an improvement over
#summarise_all, summarise_if etc...
grouped <- masterdf %>% group_by(activity) %>% 
                        summarise(across(.cols = everything(),.fns = mean)) %>% 
                        ungroup()
write.table(grouped, "tidy_data.txt", row.names = FALSE)