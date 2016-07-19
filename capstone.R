# In this project we combine various datasets that are relevant in 
# answering the big question that we want to answer - ``Who are the
# people in the United States that are in favor of Donald Trump
# running this nation ? " We hope to find out from our analysis as to
# what are the characteristics of these people such as their age,
# gender, race and ethnicity, their religious views, to which income 
# groups they belong, their occupations, their levels of education, 
# if they are veterans, if they have extreme fascist views (in 
# particular we want to analyze if the states that voted for Trump
# had or has a large presence of the ku klux klan !) etc. 

# We plan to expand on this list as we progress in the project. 
# Naturally this list consists of our variables.
# We should compare this with the data from the actual results of the
# primaries. This can be found in the kaggle site on the US Election
# 2016 - https://www.kaggle.com/benhamner/2016-us-election. 
# The datasets from this site include demographic information on the
# counties of all the 51 states. Information for just the state alone
# is also found. This is found in the dataset county_facts.csv .
# The dataset county_facts_dictionary.csv explains the variables of 
# previous dataset.

# We want to work with only the states and not the counties. We can 
# obtain exactly these by running the following code.
only_states <- filter(county_facts, V3 == "")
first_row <- slice(county_facts, 1)
# The dataset along with the variable abbreviations.
only_states_1 <- bind_rows(first_row, only_states)

# Joining the Ku Klux Klan dataset. Open the klaverns_all_sans_sources.csv.
kkk <- select(klaverns_all_sans_sources, state:klan_number)
kkk5 <- group_by(kkk, state) %>%
  + select(klan_number) %>%
+ summarise_each(funs(sum))
# Change column name state to area_name to match the main dataset.
kkk6 <- select(kkk5, area_name = state)
kkk7 <- bind_cols(kkk6, select(kkk5, klan_number))
# Delete the United States row in the only_states_1 dataframe.
only_states_2 <- only_states_1[-2,]
# Join the kkk7 dataframe with the only_states_2 dataframe.

# Turn the first row of the only_states_2 matrix into the column names 
# of the same.
mat <- as.matrix(only_states_1)
colnames(only_states_2) <- mat[1,]
only_states_3 <- only_states_2[-1,]

function(y){if(y == "DC"){print("District Of Columbia")} else print(y)}
kkk9 <- transmute(kkk7, area_name = lapply(as.character(kkk7$area_name), correcter))

add_hawaii = data.frame(area_name = "Hawaii", klan_number = 0, stringsAsFactors = FALSE)


kkk_test <- bind_rows(kkk7, add_hawaii)
kkk_test_1 <- filter(kkk_test, area_name != "Panama")
kkk_test_slice <- slice(kkk_test_1, 1:11)
kkk_test_slice_2 <- slice(kkk_test_1, 12:50)
kkk_test_2 <- bind_rows(kkk_test_slice, add_hawaii)
kkk_test_3 <- bind_rows(kkk_test_2, kkk_test_slice_2)
kkk_klan <- as.data.frame(kkk_test_3$klan_number)
kkk_test_5 <- bind_cols(kkk_test_4, kkk_klan) 
colnames(kkk_test_5) <- c("area_name", "Number of Klans")
kkk_test_7 <- as.data.frame(kkk_test_5)
data_states <- bind_cols(only_states_3, kkk_test_7[2])

# Adding statewise religion data into the main data set.
mat2 <- as.matrix(Religion)
colnames(Religion) <- mat2[1, ]
Religion_1 <- Religion[-1, ]
data_states_1 <- bind_cols(data_states, Religion_1)

# Adding statewise race data into the main data set.
race_data <- slice(raw_data, 4:56)
mat3 <- as.matrix(race_data)
colnames(race_data) <- mat3[1, ]
race_data_1 <- race_data[-1, ]
race_data_2 <- race_data_1[-1, ]
race_data_3 <- select(race_data_2, -Location)
race_data_4 <- select(race_data_3, -8)
race_data_5 <- select(race_data_4, -Total)
data_states_2 <- bind_cols(data_states_1, race_data_5)

# Adding county typology data into the main data set.
Typology <- slice(Typology.2015.Update.Table.1, -1)
mat4 <- as.matrix(Typology)
colnames(Typology) <- mat4[1, ]
Typology_1 <- Typology[-1, ]
Typology_2 <- select(Typology_1, -County_name)
Typology_3 <- select(Typology_2, -c(FIPStxt, Type_2015_Update))
Typology_4 <- select(Typology_3, -State)
f2n <- function(x){as.numeric(as.character(x))}
Typology_5 <- mutate_each(Typology_4, funs(f2n))
Typology_6 <- bind_cols(select(Typology_3, State), Typology_5)
Typology_7 <- as.data.frame(Typology_6)

Typology_8 <- Typology_7 %>%
  + group_by(State) %>%
  + summarise_each(funs(sum))
Typology_9 <- Typology_8[-1, ]
colnames(states) <- c("area_name", "State")
Typology_10 <- right_join(Typology_9, states, by ="State")

data_states_3 <- bind_cols(data_states_2, select(Typology_10, -State))
data_states_4 <- as.data.frame(data_states_3)

# We now join the results of the primaries into the main data set.
primary_trump <- filter(primary_results, candidate == "Donald Trump")
primary_trump_1 <- select(primary_trump, -c(county, fips, party))
# The primary_results.csv data file was not up to date as it had 
# primary results data for just 25 states. Using a more up to date
# set with data for 46 states.

primary_results_complete_trump <- filter(primary_results._complete, candidate == "Donald Trump")
primary_trump_1 <- primary_trump %>%
  + group_by(state) %>%
  + summarise_each(funs(sum))
colnames(primary_trump_1)[1] <- "area_name"

full_data_2 <- semi_join(data_states_4, primary_trump_todate)
full_data_3 <- bind_cols(full_data_2, select(primary_trump_todate, -area_name))

colnames(full_data_3)[2] <- "state"
full_data_4 <- select(full_data_3, -area_name)
full_data_5 <- select(full_data_4, -state_abbreviation)
# The resulting data set has data for only 46 states excluding, Colorado,
# DC, Maine, Minnesota and North Dakota.

# We have joined the KKK data and the statewise, religion, race and 
# economic type data to the main data set from the kaggle site on
# the US election 2016. We have a total of 330 variables now and 
# 46 observations corresponding to the 46 states.

# Preliminary analysis - 

# First we get the percentage of voting poulation. This is
# obtained by adding AGE295214 + AGE775214. However these are of 
# type factor and have to be converted to type numeric. We should
# use the paste function also.

full_data_6 <- mutate(full_data_5, voting_pop_percent = as.numeric(paste(AGE295214)) + as.numeric(paste(AGE775214)))
full_data_7 <- mutate(full_data_6, voting_population = round(as.numeric(paste(voting_pop_percent))*as.numeric(paste(PST045214))/100))
# This gives the voting population for the year 2014. In principal
# we should adjust this value by looking at a change in the population
# and use this adjusted value as our parameter. For now we assume that
# this chage is insignificant.

full_data_8 <- mutate(full_data_7, absolute_votes = 100*round(as.numeric(paste(votes))/as.numeric(paste(voting_population))))
# Plotting correlation of votes with Number of Klans.
plot(full_data_8$`Number of Klans`, full_data_8$absolute_votes, xlab = "Number of Klans", ylab = "Fraction of Votes", main = "Correlation with KKK")
# Labelling data points of the scatter plot.
text(full_data_8$`Number of Klans`, full_data_8$absolute_votes, labels = full_data_8$state, cex = 0.7)
 
# Regression tree between the percentage of votes and the economic
# indicators of the state.
tree1 = rpart(absolute_votes ~ Farming_2015_Update + Mining_2015_Update + Manufacturing_2015_Update + Government_2015_Update + Recreation_2015_Update + Nonspecialized_2015_Update, data=full_data_8)

# Regression tree between the percentage of votes and it's dependence
# on low education, employment and poverty in the state.
tree2 = rpart(absolute_votes ~ Low_Education_2015_Update + Low_Employment_Cnty_2008_2012_25_64 + Persistent_Poverty_2013, data = full_data_8)
# The tree plot.
prp(tree2, main = "Impact of Education, Poverty and Employment")

# Regression tree showing the Impact of Urbanization.
tree3 = rpart(absolute_votes ~ Metro_NonMetro, data = full_data_8)
# The tree plot.
prp(tree3, main = "Impact of Urbanization")

# Regression tree showing dependence on the number of KKKlans.
tree4 = rpart(absolute_votes ~ Klans, data = full_data_8)
# The tree plot.
prp(tree4, main = "Dependence on Number of Klans")

# Regression tree showing dependence on Mean household income.
tree5  = rpart(absolute_votes ~ as.numeric(paste(INC110213)), data = full_data_8)
# The tree plot.
prp(tree5, main = "Dependence on Mean Household Income")

# We encounter errors while performing regression with respect to the
# race variables as they are all of class factor. The race data that
# we attached separately have missing values for three of the races.
# So we decided to change the factor to numeric. However if
# we do this and make the rpart, the resulting tree plots look
# very clumsy. So we first create new variables where we change the
# factor classes to numeric and then call rpart.
full_data_9 <- mutate(full_data_8, White_new = as.numeric(paste(RHI125214)))
full_data_10 <- mutate(full_data_9, Black_new = as.numeric(paste(RHI225214)))
full_data_11 <- mutate(full_data_10, Native_new = as.numeric(paste(RHI325214)))
full_data_12 <- mutate(full_data_11, Asian_new = as.numeric(paste(RHI425214)))
full_data_13 <- mutate(full_data_12, Islander_new = as.numeric(paste(RHI525214)))
full_data_14 <- mutate(full_data_13, Mixed_new = as.numeric(paste(RHI625214)))
full_data_15 <- mutate(full_data_14, Hispanic_new = as.numeric(paste(RHI725214)))
# Race dependence.
tree8 = rpart(absolute_votes ~ White_new + Black_new + Native_new + Asian_new + Islander_new + Mixed_new + Hispanic_new, data = full_data_15, method ="anova")
# The tree plot.
rpart.plot(tree8)
# Saving to a pdf or ps file.
post(tree8, file="race.ps", title = "Race Dependence-Vote % on Nodes")

#This gives the vector with number of counties for the 46 states under
# consdieration.
count = c(67, 29, 15, 75, 58, 8, 3, 67, 159, 5, 44, 102, 92, 99, 105, 120, 64, 24, 14, 83, 82, 115, 56, 93, 17, 10, 21, 33, 62, 100, 88, 77, 36, 67, 5, 46, 66, 95, 254, 29, 14, 133, 39, 55, 72, 23)
# Convert this to a data frame.
count_df = data.frame(count)
data_EEP_1 <- bind_cols(data_EEP, count_df)
# The above data set only consists of the Education, Poverty and 
# Employment statistics from the Typology data set.
# This data set only includes the number of counties that have the
# specified deature like Low Education, Low Employment and Poverty.
# To make better predictions we need variables that do not depend
# on the number of counties. So we create another data set where
# the variables are absolute. 
data_EEP_1 <- bind_cols(data_EEP, count_df)
data_EEP_2 <- mutate(data_EEP_1, Educationp = round((Education/count)*100) )
data_EEP_3 <- mutate(data_EEP_2, Povertyp = round((Poverty/count)*100) )
data_EEP_4 <- mutate(data_EEP_3, Employmentp = round((Employment/count)*100) )
data_EEP_new <-select(data_EEP_4, -c(Education, Poverty, Employment, count))
colnames(data_EEP_new)[1:4] <- c("Votes", "Education", "Poverty", "Employment")

# Doing the same for the economic type data.
data_economy <- select(full_data_15, c(absolute_votes, Farming_2015_Update, Mining_2015_Update, Manufacturing_2015_Update, Government_2015_Update, Recreation_2015_Update, Nonspecialized_2015_Update))
colnames(data_economy)[1:7] <- c("Votes", "Farming", "Mining", "Manufacturing", "Government", "Recreation", "Nonspecialized")
data_economy_1 <- bind_cols(data_economy, count_df)
data_economy_2 <- mutate(data_economy_1, Farmingp = round(Farming/count*100))
data_economy_3 <- mutate(data_economy_2, Miningp = round(Mining/count*100))
data_economy_4 <- mutate(data_economy_3, Manufacturingp = round(Manufacturing/count*100))
data_economy_5 <- mutate(data_economy_4, Governmentp = round(Government/count*100))
data_economy_6 <- mutate(data_economy_5, Recreationp = round(Recreation/count*100))
data_economy_7 <- mutate(data_economy_6, Nonspecializedp = round(Nonspecialized/count*100))
data_economy_new <- select(data_economy_7, -c(Farming, Mining, Manufacturing, Government, Recreation, Nonspecialized, count))
colnames(data_economy_new)[1:7] <- c("Votes", "Farming", "Mining", "Manufacturing", "Government", "Recreation", "Nonspecialized")

# Same for urbanization.
data_urbanization <- select(full_data_15, c(absolute_votes, Metro_NonMetro))
colnames(data_urbanization)[1:2] <- c("Votes", "Urbanization")
data_urbanization_1 <- bind_cols(data_urbanization, count_df)
data_urbanization_2 <- mutate(data_urbanization_1, Urbanp = round(Urbanization/count*100))
data_urbanization_new <- select(data_urbanization_2, -c(Urbanization, count))
colnames(data_urbanization_new)[1:2] <- c("Votes", "Urbanization")


# Scatter plots from base R, unused.

plot(full_data$Klans, full_data$absolute_votes, xlab = "Number of Klans", ylab = "Percentage of Votes")
text(full_data$Klans, full_data$absolute_votes, labels = full_data$state, cex = 0.7)
abline(v = 224, col = "Blue")
abline(h = 7.8, col = "Red")
abline(v = 2018, col = "Green")
abline(h = 14, col = "Yellow")

plot(EEP1$Education, EEP1$Votes, xlab = "Percentge of High School Graduates and Higher Degrees", ylab = "Percentage of Votes")
text(EEP1$Education, EEP1$Votes, labels = EEP1$full_data_15.state, cex = 0.7)
abline(v = 4.5, col = "Blue")
abline(h = 11, col = "Red")

plot(urbanization1$Urbanization, urbanization1$Votes, xlab = "Percentage of Metros", ylab = "Percentage of Votes")
text(urbanization1$Urbanization, urbanization1$Votes, labels = urbanization1$full_data_15.state, cex = 0.7)
abline(v = 51, col = "Blue")























































































