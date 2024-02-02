################################################################################
################################ Final Project #################################
################################################################################

############################ Importing the data set ############################
gun_violence = read.csv("/Users/cara-lifarrell/Desktop/Final Project/gun_violence.csv")
attach(gun_violence)

############################ Data Preprocessing 1 ##############################

### Dropping irrelevant columns ###
# Columns to drop
columns_to_drop = c("incident_id",
                    "city_or_county",
                    "address",
                    "incident_url", 
                    "source_url",
                    "incident_url_fields_missing",
                    "incident_characteristics",
                    "location_description",
                    "notes",
                    "participant_name",
                    "sources")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

### Creating a column for year ###
library(dplyr)
library(tidyr)

date_column = gun_violence$date

# Split name column into year and month
gun_violence = gun_violence %>% separate(date, c('year', 'month', 'day'))
attach(gun_violence)

# Drop the day column
columns_to_drop = c("day")
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

# Transforming the two new columns into numeric datatypes
gun_violence$year = as.numeric(as.character(gun_violence$year))
gun_violence$month = as.numeric(as.character(gun_violence$month))
attach(gun_violence)

### Checking datatypes ###
data_types = sapply(gun_violence, class)
print(data_types)

### Unique Values ###
# state
unique_states = unique(gun_violence$state)
print(unique_states)

# congressional_district
unique_congressional_district = unique(gun_violence$congressional_district)
print(unique_congressional_district)

# state_house_district
unique_state_house_district = unique(gun_violence$state_house_district)
print(unique_state_house_district)

# state_house_district
unique_state_senate_district = unique(gun_violence$state_senate_district)
print(unique_state_senate_district)

############################ Data Visualization 1 ##############################
library(ggplot2)

### year ###
# Count the frequency of each year
year_counts = gun_violence %>% group_by(year) %>% summarize(count = n())

# Create a bar chart
plot_year = ggplot(year_counts, aes(x = factor(year), y = count)) +
  geom_bar(stat = "identity", fill = "#003f5c") +
  labs(title = "Gun Violence Incidents by Year (2013-2018)",x = "Year",y = "Number of Incidents") +
  theme_minimal() + 
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

### month ###
# Count the frequency of each month
month_counts = gun_violence %>% group_by(month) %>% summarize(count = n())

# Create a bar chart
plot_month = ggplot(month_counts, aes(x = factor(month), y = count)) +
  geom_bar(stat = "identity", fill = "#003f5c") +
  labs(title = "Gun Violence Incidents by Month (2013-2018)",x = "Month",y = "Number of Incidents") +
  theme_minimal() + 
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

### Dropping 2018 ###
gun_violence = gun_violence[gun_violence$year != 2018, ]

### year (no 2018) ###
# Count the frequency of each year
year_counts2 = gun_violence %>% group_by(year) %>% summarize(count = n())

# Create a bar chart
plot_year_no2018 = ggplot(year_counts2, aes(x = factor(year), y = count)) +
  geom_bar(stat = "identity", fill = "#58508d") +
  labs(title = "Gun Violence Incidents by Year (2013-2017)",x = "Year",y = "Number of Incidents") +
  theme_minimal() + 
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

### month (no 2018) ###
# Count the frequency of each month
month_counts2 = gun_violence %>% group_by(month) %>% summarize(count = n())

# Create a bar chart
plot_month_no2018 = ggplot(month_counts2, aes(x = factor(month), y = count)) +
  geom_bar(stat = "identity", fill = "#58508d") +
  labs(title = "Gun Violence Incidents by Month (2013-2017)",x = "Month",y = "Number of Incidents") +
  theme_minimal() + 
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

### year and month ###
library(gridExtra)
library(grid)

top_row_title = textGrob("Including 2018 incidents")
bottom_row_title = textGrob("Without 2018 incidents")

# Arrange the plots in a 2x2 grid with custom row titles
grid.arrange(plot_year, plot_month, plot_year_no2018, plot_month_no2018, 
             ncol = 2, top = top_row_title, bottom = bottom_row_title)

### state ###
# Count the frequency of each state
state_counts = gun_violence %>% group_by(state) %>% summarize(count = n())
class(state_counts$count)

# Count the frequency of each state
state_counts = gun_violence %>% group_by(state) %>% summarize(count = n())

# Top 5 states
top_states = state_counts %>% top_n(10, wt = count)

plot_state = ggplot(top_states, aes(x = reorder(state, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#bc5090") +
  labs(title = "Top 10 States with the Highest Number of Gun Violence Incidents (2013-2017)",
       x = "State",
       y = "Number of Incidents") +
  theme_minimal() + 
  theme(
    text = element_text(color = "black"),
    plot.title = element_text(face = "bold")
  )

# Show the plot
print(plot_state)

########################### Adding 2018 values again ###########################
gun_violence = read.csv("/Users/cara-lifarrell/Desktop/Final Project/gun_violence.csv")
attach(gun_violence)

### Dropping irrelevant columns ###
# Columns to drop
columns_to_drop = c("incident_id",
                    "city_or_county",
                    "address",
                    "incident_url", 
                    "source_url",
                    "incident_url_fields_missing",
                    "incident_characteristics",
                    "location_description",
                    "notes",
                    "participant_name",
                    "sources")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

### Creating a column for year ###
library(dplyr)
library(tidyr)

date_column = gun_violence$date

# Split name column into year and month
gun_violence = gun_violence %>% separate(date, c('year', 'month', 'day'))
attach(gun_violence)

# Drop the day column
columns_to_drop = c("day")
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

# Transforming the two new columns into numeric datatypes
gun_violence$year = as.numeric(as.character(gun_violence$year))
gun_violence$month = as.numeric(as.character(gun_violence$month))
attach(gun_violence)

### Checking datatypes ###
data_types = sapply(gun_violence, class)
print(data_types)

############################ Gun Violence Location #############################
# Creating a separate dataframe for gun_violence_location
gun_violence_location = data.frame(congressional_district = gun_violence$congressional_district,
                                   latitude = gun_violence$latitude,
                                   longitude = gun_violence$longitude,
                                   state_house_district = gun_violence$state_house_district,
                                   state_senate_district = gun_violence$state_senate_district)
attach(gun_violence_location)

# Checking the NAs for each columns
na_counts = colSums(is.na(gun_violence_location))
print(na_counts)

# As a dataframe
na_counts_df = data.frame(Column = names(gun_violence_location), NA_Count = na_counts)
print(na_counts_df)

# Dropping NA rows 
gun_violence_location = na.omit(gun_violence_location) 
attach(gun_violence_location)

# Correlation Matrix
quantvars = gun_violence_location[, c(1,2,3,4,5)] 
corr_matrix = cor(quantvars) 
round(corr_matrix, 2) 
# Although there are no correlation coefficients >= 0.80, congressional_district, 
# state_house_district and state_senate_district should be dropped as they have 
# relatively higher coefficients of >= 0.23

# Dropping these columns in the main dataframe
# Columns to drop
columns_to_drop = c("congressional_district",
                    "state_house_district",
                    "state_senate_district")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

############################ Data Preprocessing 2 ##############################
### Dropping more columns ###
# Columns to drop
columns_to_drop = c("gun_type",
                    "participant_age",
                    "participant_status",
                    "participant_type")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

### Empty rows ###
# Replacing empty rows with NA
gun_violence[gun_violence == ""] = NA

### Examining NAs ### 
# Checking the NAs for each columns
na_counts = colSums(is.na(gun_violence))
print(na_counts)

# As a dataframe
na_counts_df = data.frame(Column = names(gun_violence), NA_Count = na_counts)
print(na_counts_df)

# Dropping NA rows 
gun_violence = na.omit(gun_violence)
attach(gun_violence)

########################## Gun Violence Participants ###########################
# Creating a separate dataframe for the characteristics of the participants
# in the gun violence cases
gun_violence_participants = data.frame(participant_age_group = gun_violence$participant_age_group,
                                       participant_gender = gun_violence$participant_gender,
                                       participant_relationship = gun_violence$participant_relationship)
attach(gun_violence_participants)

# Dropping columns that will be used in the gun_violence_participants dataframe
# Columns to drop
columns_to_drop = c("participant_age_group",
                    "participant_gender",
                    "participant_relationship")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

### participant_age_group ###
# Extract age groups from the "participant_age_group" column
gun_violence_participants$participant_age_group = gsub(".*::(\\S+).*", "\\1", gun_violence_participants$participant_age_group)

# From sorting the values based on this column, we can get the index rows which have
# more than one participant age group and will replace this with "Mixed"

# Replace "participant_age_group" with "Mixed" for rows 47 to 84 
gun_violence_participants$participant_age_group[58:84] = "Mixed"
attach(gun_violence_participants)

unique_participant_age_group2 = unique(gun_violence_participants$participant_age_group)
print(unique_participant_age_group2)

# Dummifying participant_age_group
dummies = model.matrix(~ participant_age_group - 1, data = gun_violence_participants) # Creating thr dummy variables
gun_violence_participants = cbind(gun_violence_participants, dummies) # Merging the datasets
attach(gun_violence_participants)

# Columns to drop
columns_to_drop = c("participant_age_group")

# Dropping columns
gun_violence_participants = gun_violence_participants[, !names(gun_violence_participants) %in% columns_to_drop]
attach(gun_violence_participants)

### participant_gender ###
# Iterate through each observation to check if there is a female involved
# Useful link: https://stackoverflow.com/questions/59812047/how-to-count-the-occurrences-of-each-word-in-a-column-in-r

gun_violence_participants$female_involved = 0 # new empty column

for (i in seq_along(gun_violence_participants$participant_gender)) {
  female_present = any(grepl("Female", strsplit(gun_violence_participants$participant_gender[i], "\\|\\|", fixed = TRUE)[[1]]))
  gun_violence_participants$female_involved[i] = as.integer(female_present)
}

# Columns to drop
columns_to_drop = c("participant_gender")

# Dropping columns
gun_violence_participants = gun_violence_participants[, !names(gun_violence_participants) %in% columns_to_drop]
attach(gun_violence_participants)

### participant_relationship ###
# Iterate through each observation to check if there is a closer_relationship
# "Closer Relationship" includes: "Acquaintance", "Co-worker", "Family", "Friends", "Neighbor", "Significant others"
gun_violence_participants$known = 0

# Relationship types to check for
relationship_types = c("Acquaintance", "Co-worker", "Family", "Friends", "Neighbor", "Significant others")

for (i in seq_along(gun_violence_participants$participant_relationship)) {
  word_present = any(sapply(relationship_types, function(word) grepl(word, strsplit(gun_violence_participants$participant_relationship[i], "\\|\\|", fixed = TRUE)[[1]])))
  gun_violence_participants$known[i] = as.integer(word_present)
}

# Columns to drop
columns_to_drop = c("participant_relationship")

# Dropping columns
gun_violence_participants = gun_violence_participants[, !names(gun_violence_participants) %in% columns_to_drop]
attach(gun_violence_participants)

# Merge with original dataset
gun_violence = cbind(gun_violence, gun_violence_participants)
attach(gun_violence)

###################### Gun Violence Gun Characteristics ########################

# Creating a separate dataframe for the characteristics of the incidents (guns and incident characteristics)
gun_violence_gun_characteristics = data.frame(gun_stolen = gun_violence$gun_stolen)
attach(gun_violence_gun_characteristics)

# Dropping columns that will be used in the gun_violence_participants dataframe
# Columns to drop
columns_to_drop = c("gun_stolen")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

### gun_stolen ###
# Iterate through each observation to check if there is a gun stolen (1: yes, 0: no)
gun_violence_gun_characteristics$gun_status = 0 # new empty column

for (i in seq_along(gun_violence_gun_characteristics$gun_stolen)) {
  stolen_gun_present = any(grepl("Stolen", strsplit(gun_violence_gun_characteristics$gun_stolen[i], "\\|\\|", fixed = TRUE)[[1]]))
  gun_violence_gun_characteristics$gun_status[i] = as.integer(stolen_gun_present)
}

# Merge with original dataset
gun_violence = cbind(gun_violence, gun_violence_gun_characteristics)

# Columns to drop
columns_to_drop = c("gun_stolen")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

############################ Data Visualization 2 ##############################

### n_killed ###
# Summary statistics
summary_stats_n_killed = summary(gun_violence$n_killed)
print(summary_stats_n_killed)

# Boxplot
boxplot(n_killed)

# Histogram
hist(n_killed, breaks = seq(min(n_killed)-0.5, max(n_killed)+0.5, by = 1), 
     col = "#ff6361", main = "Frequency of the Number of Killed", 
     xlab = "Number of Killed", ylab = "Number of Incidents")

### n_injured ### 
# Summary statistics
summary_stats_n_injured = summary(gun_violence$n_injured)
print(summary_stats_n_injured)

# Boxplot
boxplot(n_injured)

# Histogram
hist(n_injured, breaks = seq(min(n_injured)-0.5, max(n_injured)+0.5, by = 1), 
     col = "#ff6361", main = "Frequency of the Number of Injured", 
     xlab = "Number of Injured", ylab = "Number of incidents")

### n_guns_involved ###
# Summary statistics
summary_stats_n_guns_involved = summary(gun_violence$n_guns_involved)
print(summary_stats_n_guns_involved)

# Boxplot
boxplot(n_guns_involved)

# Histogram
hist(n_guns_involved, breaks = seq(min(n_guns_involved) - 0.5, max(n_guns_involved) + 0.5, by = 1),
     col = "#ff6361", main = "Frequency of the Number of Guns Involved",
     xlab = "Number of Guns Involved", ylab = "Number of Incidents")

# Together
par(mfrow = c(1, 2))

# Plot the first histogram
hist(n_killed, breaks = seq(min(n_killed) - 0.5, max(n_killed) + 0.5, by = 1),
     col = "#ff6361", main = "Frequency of the Number of Killed",
     xlab = "Number of Killed", ylab = "Number of Incidents")

# Plot the second histogram
hist(n_injured, breaks = seq(min(n_injured) - 0.5, max(n_injured) + 0.5, by = 1),
     col = "#ff6361", main = "Frequency of the Number of Injured",
     xlab = "Number of Injured", ylab = "Number of Incidents")

# To stop using the par(mfrow) setting for subsequent plots
dev.off()

# participant_age_group

# participant_age_groupAdult
# chosen colours
custom_colors = c("#ff6361", "#ffa600")

# data frame 
age_group_counts_Adult = table(gun_violence$participant_age_groupAdult)
age_group_df_Adult = as.data.frame(age_group_counts_Adult)
colnames(age_group_df_Adult) = c("Age_Group", "Count")

# Bar chart
chart_adult = ggplot(age_group_df_Adult, aes(x = Age_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Age Group Involved", 
                    labels = c("No Adults", "Adults")) + 
  labs(title = "Gun Violence Incidents Involving Adults",
       x = element_text("Age Group", face = "italic"), 
       y = element_text("Number of Incidents", face = "italic")) + 
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

# participant_age_groupChild
# data frame 
age_group_counts_Child = table(gun_violence$participant_age_groupChild)
age_group_df_Child = as.data.frame(age_group_counts_Child)
colnames(age_group_df_Child) = c("Age_Group", "Count")

# Bar chart
chart_child = ggplot(age_group_df_Child, aes(x = Age_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Age Group Involved", 
                    labels = c("No Children", "Children")) + 
  labs(title = "Gun Violence Incidents Involving Children",
       x = element_text("Age Group", face = "italic"), 
       y = element_text("Number of Incidents", face = "italic")) + 
  theme_minimal() +
  theme(legend.position = "top", 
        plot.title = element_text(face = "bold"))

# participant_age_groupMixed
# data frame 
age_group_counts_Mixed = table(gun_violence$participant_age_groupMixed)
age_group_df_Mixed = as.data.frame(age_group_counts_Mixed)
colnames(age_group_df_Mixed) = c("Age_Group", "Count")

# Bar chart
chart_mixed = ggplot(age_group_df_Mixed, aes(x = Age_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Age Group Involved", 
                    labels = c("Not Mixed", "Mixed")) + 
  labs(title = "Gun Violence Incidents Involving Mixed Age Groups",
       x = element_text("Age Group", face = "italic"),  
       y = element_text("Number of Incidents", face = "italic")) + 
  theme_minimal() +
  theme(legend.position = "top", 
        plot.title = element_text(face = "bold"))

# Loading a library to combine ggplots
library(patchwork)

# Combining the charts
combined_chart_patchwork = chart_adult + chart_child + chart_mixed
combined_chart_patchwork

# female_involved 
# data frame 
female_involved_counts = table(gun_violence$female_involved)
female_involved_df = as.data.frame(female_involved_counts)
colnames(female_involved_df) = c("Female_Involved", "Count")

# Bar chart
chart_female = ggplot(female_involved_df, aes(x = Female_Involved, y = Count, fill = Female_Involved)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Female Involved", 
                    labels = c("No Females", "Females")) + 
  labs(title = "Gun Violence Incidents Involving Females",
       x = element_text("Gender", face = "italic"), 
       y = element_text("Number of Incidents", face = "italic")) + 
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

# closer_relationship 
closer_relationship_counts = table(gun_violence$known)
closer_relationship_df = as.data.frame(closer_relationship_counts)
colnames(closer_relationship_df) = c("Closer_Relationship", "Count")

# Bar chart
chart_relationship = ggplot(closer_relationship_df, aes(x = Closer_Relationship, y = Count, fill = Closer_Relationship)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Relationship Status", 
                    labels = c("Unknown", "Known")) + 
  labs(title = "Gun Violence Incident Relationship Status",
       x = element_text("Relationship Status", face = "italic"), 
       y = element_text("Number of Incidents", face = "italic")) + 
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

# gun_status 
gun_status_counts = table(gun_violence$gun_status)
gun_status_df = as.data.frame(gun_status_counts)
colnames(gun_status_df) = c("Gun_Status", "Count")

# Bar chart
chart_gun_status = ggplot(gun_status_df, aes(x = Gun_Status, y = Count, fill = Gun_Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Gun Status", 
                    labels = c("Not Stolen", "Stolen")) + 
  labs(title = "Gun Violence Incident Gun(s) Involved Status",
       x = element_text("Gun Status", face = "italic"), 
       y = element_text("Number of Incidents", face = "italic")) + 
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

# Combining the charts
combined_chart_patchwork2 = chart_female + chart_relationship + chart_gun_status
combined_chart_patchwork2

############################ Data Preprocessing 3 ##############################

# Creating the y-variable
# Creating a new variable column
gun_violence$severity = NA

for (i in seq_along(gun_violence$severity)) {
  killed = gun_violence$n_killed[i]
  if (killed >= 3) {
    gun_violence$severity[i] = "high"
  } else if (killed >= 1 && killed < 3) {
    gun_violence$severity[i] = "medium"
  } else {
    gun_violence$severity[i] = "low"
  }
}

# Counts for severe and less-severe cases
table(gun_violence$severity)

# Moving the state and incident_characteristics at the end of the table
gun_violence = gun_violence[, c(setdiff(names(gun_violence), c("state")), "state")]

# Moving the severity_category to the beginning of the table
gun_violence = gun_violence[, c("severity", setdiff(names(gun_violence), "severity"))]

attach(gun_violence)

# Columns to drop
# Does not make sense to keep n_killed if mass_shooting is based on this
columns_to_drop = c("n_killed")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

# Correlation matrix
quantvars = gun_violence[, c(2:14)] 
corr_matrix = cor(quantvars) 
round(corr_matrix, 2) 
# Significantly highly correlated variables (>= |0.80|) seem to be:
# participant_age_groupTeen and participant_age_groupAdult

# Dropping highly correlated variables
columns_to_drop = c("participant_age_groupTeen")

# Dropping columns
gun_violence = gun_violence[, !names(gun_violence) %in% columns_to_drop]
attach(gun_violence)

# Correlation matrix
quantvars = gun_violence[, c(2:13)] 
corr_matrix = cor(quantvars) 
round(corr_matrix, 2) 

################################# Random Forest ################################ 

library(randomForest)

str(gun_violence$severity)
gun_violence$severity = as.factor(gun_violence$severity)
levels(gun_violence$severity)

myforest=randomForest(severity~year+month+n_injured+latitude+longitude+n_guns_involved+
                        participant_age_groupAdult+participant_age_groupChild+
                        participant_age_groupMixed+female_involved+known+gun_status, 
                      ntree=500, data=gun_violence, importance=TRUE, na.action=na.omit)
myforest

importance(myforest)
varImpPlot(myforest)

# Insignificant variables: 
# month, participant_age_groupChild, and participant_age_groupMix

##################################### QDA ###################################### 

# Loading libraries
library(MASS)
library(klaR)

myqda = qda(severity~year+n_injured+latitude+longitude+n_guns_involved+
              female_involved+known+gun_status)
myqda

# Example application
predict(myqda, data.frame(year=2023, month=6, n_injured=10, latitude=41.8781, 
                          longitude=87.6298, n_guns_involved=2, female_involved=1,
                          known=1, gun_status=0))

############################# Classification Tree ############################## 

# Loading libraries
library(tree)
library(rpart)
library(rpart.plot)

table(severity)

# Building a tree using cp=0.007
classifiedtree=rpart(severity~year+n_injured+latitude+longitude+n_guns_involved+
                       female_involved+known+gun_status, control=rpart.control(cp=0.007))

# Plotting the tree
rpart.plot(classifiedtree)

# Overfitted tree
# Building an overfitted tree
overfitted_tree=rpart(severity~year+n_injured+latitude+longitude+n_guns_involved+
                        participant_age_groupAdult+
                        female_involved+known+gun_status, control=rpart.control(cp=0.001))
rpart.plot(overfitted_tree)

# Checking out of sample performance
printcp(overfitted_tree)
plotcp(overfitted_tree)

opt_cp=overfitted_tree$cptable[which.min(overfitted_tree$cptable[,"xerror"]),"CP"]
opt_cp

# Optimal tree
mybesttree=rpart(severity~year+n_injured+latitude+longitude+n_guns_involved+
                   participant_age_groupAdult+
                   female_involved+known+gun_status, control=rpart.control(cp=opt_cp))
printcp(mybesttree)
rpart.plot(mybesttree)

summary(mybesttree)





