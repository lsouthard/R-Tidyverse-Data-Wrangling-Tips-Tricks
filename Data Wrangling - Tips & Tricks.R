#set your working directory to where your file is saved
setwd("C:/Users/lpantlin/Desktop/Other Projects/R Talk")


# Prerequisites -----------------------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(forcats)
library(magrittr)
library(ggplot2)

#load the data

survey=read.csv("Fake Survey Data.csv", header = T)
survey <- survey[-c(1),] 
#what are the problems?

#the first line of the aq dataset was survey question numbers and the second row was the actual numbers
survey=read.csv("Fake Survey Data.csv", header=T, sep=",", skip = 1, na = c("", "NA"), stringsAsFactors = F)

#first I don't want to deal with these terrible columns
survey <- survey %>% 
  setNames(c("state", "county", "gender", "gender.other", 
                             "age", "ethnicity", "highestedu", 
                             "hometown.state", "home.country", "hometown.setting", 
                             "relationship.status", "children", "political.views.shared", 
                             "income", "employment.status", "political.view.self", 
                             "religiosity", "practicing.relig", "often.relig"))
#did it work?
colnames(survey)
# Peak the data  -----------------------------------------------------------------

# view the data
head(survey)

#look at the structure
#approximately the same as the dropdown in your global enviornment
str(survey)

#how many variables have missing data
# https://sebastiansauer.github.io/sum-isna/
map(survey, ~sum(is.na(.)))

#Other ways to examine the dataframe
#class of an object
class(survey)

#dimensions of an object
dim(survey)


# Data formats of variables -----------------------------------------------


#what are the classes?
typeof(survey$gender)

#is that really what it is?
as.factor(survey$gender) %>% levels()
#there are no levels to suggest that this is the case

#change to character
survey$gender %<>% as.character()

#check to make sure it worked
#you can also look in the dropdown in your gloabl enviornment
typeof(survey$gender)


# Applied: Reorder factors -----------------------------------------------------------------

#What are the order of your levels?
as.factor(survey$gender) %>% 
  levels()

#why does it matter
survey %>% 
  filter(political.view.self %in% c("Democrat", "Republican", "Independent")) %>% 
  ggplot(aes(x = gender, y = religiosity, fill = political.view.self))+
  geom_col(position = position_dodge())  +
  scale_fill_manual(values = c("#241571", "gray", "#710C04"))+
  labs(x="Gender Identity", y = "Religiosity", 
       title = "Comparison of Religiosity by Gender and Political Alignment",
       fill = "Political Alignment")+
  scale_y_discrete(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.title.y = element_text(size =12), 
        axis.title.x = element_text(size=  12),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  size = 12),
        text=element_text(size = 10, family = "serif"),
        legend.position = "top")

#Reorganize so the output is: man, woman, other

#I'm going to save this to a new dataframe for the time being called reordered.gender
reordered.gender <- survey %>% 
  mutate(gender = as.factor(fct_relevel(gender, c("Man", "Woman"))))

#Did it work?
as.factor(reordered.gender$gender) %>% 
  levels()

#or just pipe it right into my figure if I only want to do it once:
survey %>% 
  filter(political.view.self %in% c("Democrat", "Republican", "Independent")) %>% 
  ggplot(aes(x = fct_relevel(gender, c("Man", "Woman")), y = fct_relevel(religiosity, "10", after = Inf), fill = political.view.self))+
  geom_col(position = position_dodge())  +
  scale_fill_manual(values = c("#241571", "gray", "#710C04"))+
  labs(x="Gender Identity", y = "Religiosity", 
       title = "Comparison of Religiosity by Gender and Political Alignment",
       fill = "Political Alignment")+
  scale_y_discrete(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.title.y = element_text(size =12), 
        axis.title.x = element_text(size=  12),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  size = 12),
        text=element_text(size = 10, family = "serif"),
        legend.position = "top")





# Creating variables ------------------------------------------------------

survey %>% 
  #creates an ID column by using the row index as an integar
  tibble::rowid_to_column("ID") %>% 
  #creates an aggregate column of the average total population
  mutate(avg = mean(as.numeric(political.view.self))) %>% 
  #since there is no grouping the above will show the average repeated on all rows
  #Let's group by state and get the average population by state
  group_by(gender) %>% 
  mutate(gender.avg = mean(as.numeric(political.view.self))) %>% 
  ungroup() %>% 
  #you can group by multiple items too and do the same thing
  group_by(gender, ethnicity) %>% 
  mutate(demo.avg = mean(as.numeric(political.view.self))) %>% 
  ungroup() %>% 
  select(c(ID, avg, gender, gender.avg, ethnicity, demo.avg))

#using groups
survey %>% 
  group_by(gender, ethnicity) %>% 
  summarise(avg.political = mean(political.view.self),
            sd.political = sd(political.view.self)) %>% 
  ungroup()



# Recode variables ---------------------------------------------------------

#recall the factor order. This is imporatnt
as.factor(reordered.gender$gender) %>% levels()

#I want to recode gender into numeric values
#same variable
recode <- survey %>% 
  mutate(gender = as.numeric(as.factor(gender))) %>% 
  select(gender)

#did it work?
table(recode)

#new variable
recode <- survey %>% 
  mutate(numeric.gender = as.numeric(as.factor(gender))) %>% 
  select(gender, numeric.gender)

#did it work?
table(recode)

#I don't want women as 3 I want them as 2
#this is a great application for reordering
reordered.gender <- survey %>% 
  mutate(gender = as.factor(fct_relevel(gender, c("Man", "Woman")))) 

#displays graph of gender, ethnicity, and religiosity
survey %>% 
  ggplot(aes(x = ethnicity, fct_relevel(religiosity, "10", after = Inf), fill =  fct_relevel(gender, c("Man", "Woman"))))+
  geom_col(position = position_dodge())  +
  scale_fill_manual(values = c("light blue", "gray", "light green"))+
  labs(x="Ethnicity", y = "Religiosity", 
       title = "Comparison of Religiosity by Gender and Political Alignment",
       fill = "Gender")+
  scale_y_discrete(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.title.y = element_text(size =12), 
        axis.title.x = element_text(size=  12),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  size = 12),
        text=element_text(size = 10, family = "serif"),
        legend.position = "top")+ coord_flip()

# I want to recode ethnicity as "Asian" or "other"

#what are the levels
as.factor(survey$ethnicity) %>% levels()
collapse <- survey %>% 
  mutate(ethnicity.new = if_else(as.character(ethnicity) == "Asian", "Asian", "Other"))%>% 
  select(ethnicity, ethnicity.new)

#compare new vs. old
table(collapse)

#or just run the levels
as.factor(collapse$ethnicity.new) %>% levels

#I want to include anyone who identifies as asian in my asian group
collapse <- survey %>% 
  mutate(ethnicity.new = if_else(grepl("Asian",
                                      as.character(ethnicity)), 
                                "Asian", "Other")) %>% 
  select(ethnicity, ethnicity.new)

#collapse asian group and leave everyone else the same
collapse <- survey %>%
  mutate(ethnicity.new = if_else(grepl("Asian", 
                                       as.character(ethnicity)), "Asian", 
                                 as.character(ethnicity))) %>%
  select(ethnicity, ethnicity.new)

#understanding if_else vs ifelse

#run the code
survey %>% 
  mutate(if_else.example = if_else(gender == "Woman", "Woman", 0))

#run the code
survey %>% 
  mutate(ifelse.example = ifelse(gender == "Woman", "Woman", 0)) %>% 
  select(gender, ifelse.example)

#fix the first one with matching datatypes
survey %>% 
  mutate(if_else.example = if_else(gender == "Woman", 1, 0)) %>% 
  select(gender, if_else.example)


#I'm tired of writing as.character() and I've made my point
#permantly change to as.character
survey$ethnicity%<>% as.character
#I want there groups not just two
collapse3 <- survey %>% 
  mutate(ethnicity.new = case_when(ethnicity == "Asian" ~ "Asian",
                                  ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
                                  T ~ "Other")) %>% 
  select(ethnicity, ethnicity.new)

#Anytime someone identifies in the his/lantix group put in the same group
# use grep + case_when
collapse3 <- survey %>% 
  mutate(ethnicity.new = case_when(grepl("Asian", ethnicity) ~ "Asian",
                                  grepl("Hispanic", ethnicity) ~ "Hispanic or Latino",
                                  T ~ "Other")) %>% 
  select(ethnicity, ethnicity.new)

#let's clean up ethnicity
survey %>% 
  mutate(ethnicity = as.character(ethnicity),
         ethnicity = case_when(grepl("Asian", ethnicity) ~ "Asian",
                               grepl("Hispanic", ethnicity) ~ "Hispanic or Latino", 
                               grepl("Hawaii", ethnicity) ~ "Native Hawaiian or Pacific Islander",
                               T ~ ethnicity)) %>% 
  ggplot(aes(x = ethnicity, fct_relevel(religiosity, "10", after = Inf), fill =  fct_relevel(gender, c("Man", "Woman"))))+
  geom_col(position = position_dodge())  +
  scale_fill_manual(values = c("light blue", "gray", "light green"))+
  labs(x="Ethnicity", y = "Religiosity", 
       title = "Comparison of Religiosity by Gender and Political Alignment",
       fill = "Gender")+
  scale_y_discrete(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.title.y = element_text(size =12), 
        axis.title.x = element_text(size=  12),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(hjust = 0.5,
                                  size = 12),
        text=element_text(size = 10, family = "serif"),
        legend.position = "top")+ coord_flip()

#all three options


recode.options <- survey %>% 
  mutate(option1.recode = recode(ethnicity, "Asian"  = "Asian",
                                 "Asian,Native Hawaiian or Pacific Islander,White or Caucasian" = "Asian",
                                 "Asian,Native Hawaiian or Pacific Islander,White or Caucasian,Hispanic or Latino"= "Asian"),
         option2.ifelse = if_else(grepl("Asian",
                                        as.character(ethnicity)), 
                                  "Asian", "Other"),
         option3a.casewhen = case_when(ethnicity == "Asian" ~ "Asian",
                                       ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
                                       T ~ ethnicity),
         option3b.casewhen.grepl = case_when(grepl("Asian", ethnicity) ~ "Asian",
                                             grepl("Hispanic", ethnicity) ~ "Hispanic or Latino",
                                             T ~ "Other"),
         option3b.casewhen.grepl.mix = case_when(grepl("Asian", ethnicity) ~ "Asian",
                                                 ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
                                                 T ~ ethnicity)) %>% 
  select(c(ethnicity, option1.recode, option2.ifelse, option3a.casewhen, option3b.casewhen.grepl, option3b.casewhen.grepl.mix))

levels(as.factor(recode.options$option1.recode))
levels(as.factor(recode.options$option2.ifelse))
levels(as.factor(recode.options$option3a.casewhen))
levels(as.factor(recode.options$option3b.casewhen.grepl))
levels(as.factor(recode.options$option3b.casewhen.grepl.mix))

# Joins -------------------------------------------------------------------

#load another dataset
covid <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

#make sure both are the same data type
survey1 <- survey %>% 
  #creates an ID column by using the row index as an integar
  tibble::rowid_to_column("ID")

#not the best
#this gives you a lot of duplicates because the identifers aren't great
leftj <- left_join(survey1, covid, by = c("state" = "state", "county" = "county"))

#could try an inner join
#this is a unique case that will tack on the information of each covid case to the end of 
#the survey dataset 
innerj <- inner_join(survey1, covid, by = c("state" = "state", "county" = "county"))

#sanity check
anti_join(survey, covid, by = c("state" = "state", "county" = "county"))
  

# Duplicates --------------------------------------------------------------
survey %>% 
  #this compares rows and makes sure there isn't a row that is 
  #completely the same across all columns
  distinct(.keep_all = T) 

survey1 <- survey1 %>% 
  #this returns 3 rows because it's creating a distinct observation for each gender
  #(which has 3 levels), thus yielding 3 responses. They are the first one for each level of 
  #gender
  distinct(gender, .keep_all = T) 

# Other cool tricks -------------------------------------------------------

library(janitor)
#remove empty columns at the end of the file
survey <- survey %>% remove_empty("cols")


