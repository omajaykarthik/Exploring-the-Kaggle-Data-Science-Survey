#----------------------------------------------
#1. Welcome to the world of data science
#-----------------------------------------
  
 # Loading necessary packages
library(tidyverse)

# Loading the data
responses <- read_csv("kagglesurvey.csv")

# Printing the first 10 rows
head(responses,10)

#----------------------------------------------
#2. Using multiple tools
# Now that we've loaded in the survey results, we want to focus on
# the tools and languages thatthe survey respondents use at work.
#----------------------------------------------

# Printing the first respondents' tools and languages
print(responses$WorkToolsSelect[1])
print(responses$LanguageRecommendationSelect[1])

# Creating a new data frame called tools
tools <- data.frame(WorkToolsSelect = responses$WorkToolsSelect)

# Adding a new column to tools which splits the WorkToolsSelect column at the commas and unnests the new column
tools <- tools  %>% 
  mutate(work_tools = str_split(responses$WorkToolsSelect,pattern = ','))%>% 
  unnest(work_tools)

#Viewing the first 6 rows of tools
head(tools)

#----------------------------------------------
#3. Counting users of each tool
#----------------------------------------------  
# Creating a new data frame
tool_count <- data.frame(tools)

# Grouping the data by work_tools, calculate the number of responses in each group
# Sorting tool_count so that the most popular tools are at the top

tool_count <- tool_count  %>% 
  group_by(work_tools) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))

# Printing the first 6 results
head(tool_count)


#----------------------------------------------  
#4. Plotting the most popular tools
#----------------------------------------------  

# Creating a bar chart of the work_tools column. 
ggplot(tool_count, aes(x = reorder(work_tools, count), y = count)) + 

# Arranging the bars so that the tallest are on the far right
    geom_col() +

# Rotating the bar labels 90 degrees
    theme(axis.text.x=element_text(angle=90, hjust=1))


#----------------------------------------------  
#5. The R vs Python debate

#Within the field of data science, 
#there is a lot of debate among professionals about whether R 
#or Python should reign supreme. You can see from our last figure that 
#R and Python are the two most commonly used languages, but it's possible that many respondents use both R and Python. Let's take a look at how many people use R, Python, and both tools.
#----------------------------------------------  


# Creating a new data frame called debate_tools
debate_tools <- data.frame(responses)

# Creating a new column called language preference, based on the conditions specified in the Instructions
debate_tools <- debate_tools  %>% 
   mutate(language_preference = case_when(
     str_detect(WorkToolsSelect,"R")      & !str_detect(WorkToolsSelect,"Python")  ~ "R",
     str_detect(WorkToolsSelect,"Python") & !str_detect(WorkToolsSelect,"R")       ~ "Python",
     str_detect(WorkToolsSelect,"R")      & str_detect(WorkToolsSelect,"Python")   ~ "both",
    !str_detect(WorkToolsSelect,"R")      & !str_detect(WorkToolsSelect,"Python")  ~ "neither"))

# Printing the first 6 rows
head(debate_tools)

#----------------------------------------------  
#6. Plotting R vs Python users
#----------------------------------------------  
# Creating a new data frame
debate_plot <- data.frame(debate_tools)

# Grouping by language preference and calculate number of responses
debate_plot <- debate_plot  %>% 
   group_by(language_preference)  %>% 
   summarize(userscount = n()) %>%

# Removing the row for users of "neither"
    filter(!language_preference == "neither")

# Creating a bar chart
 ggplot(debate_plot, aes(x= reorder(language_preference ,userscount) , y = userscount)) + geom_bar(stat = "identity")

  #----------------------------------------------  
 #7. Language recommendations
 #----------------------------------------------  
 # Creating a new data frame
 recommendations <- data.frame(debate_tools)
 
 # Grouping by language_preference and then LanguageRecommendationSelect
 recommendations <- recommendations  %>%
   group_by(language_preference, LanguageRecommendationSelect) %>%
   summarize(langRecomm = n()) %>%
   
 
 # Removing empty responses and include the top recommendations
 filter(!is.na(LanguageRecommendationSelect ) , !is.na(language_preference ) )  %>%
   mutate( rowNum = row_number(langRecomm)) %>%
   filter(rowNum %in% 1:4)  
 
 #-------------------------------------------------
 #8. The most recommended language by the language used
 #------------------------------------------------
 ggplot(recommendations,aes(x=LanguageRecommendationSelect,y=langRecomm)) + 
   geom_bar(stat = "identity") + facet_wrap(~language_preference)
   
 