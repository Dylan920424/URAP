# Packages
library(tidyverse)
library(tidytext)
library(stringr)
library("readxl")
library(dplyr)
library(stringr)  
library(ggplot2)
library(dplyr)
library("viridis")
library(tidyr)

# Select folder
getwd()
setwd("C:/Users/dylan/OneDrive/Desktop/URAP/csv")



# READ PREPARED FINAL FILE *************
df_comb <- read.csv("all.csv")


######TEXT ANALYTICS####

# Add: PriKey (uniquely identify rows 1 to N, sorted from first to last review by date)
df_comb$rev.pk <- as.numeric(rownames(df_comb))


# Remove rows without date
df_comb <- df_comb[!(is.na(df_comb$date)),]
df_comb <- df_comb[!(is.na(df_comb$product)),]


#Count number of entries per firm (this still includes ALL FIRMS)
entries <- df_comb %>%
  group_by(product) %>%
  count


#Convert all text to lowercase (pros and cons)
df_comb$title <- tolower(df_comb$title) 
df_comb$review <- tolower(df_comb$review) 

# Transform rev.date into a date
df_comb$date <- df_comb$date %>% as.Date()


################ reviews analysis ##########

#####Count the number of IEQ-related words

#Dataset_for_negative
df_comb_neg <- df_comb
df_comb_pos <- df_comb

#### NEGATIVE ANALYSIS ####

#Visual negative
keywords_visual <- c("very dim","too dim", "really dim", "not bright","not enough light","too bright","would be good lights","not quite bright","bright but","overly bright")
df_comb_neg$temp <- gsub("but not bright enough|if they were too bright|which is too bright|too bright on it|not too bright|\"too bright\"|too bright in the bedroom|you can adjust to too bright|dim it if it????????s too bright|bulbs are too bright|from barely on to too bright|isn????????t too bright|mot too bright|without being too bright|if too bright|worried they would be too bright|worried they would be too bright", " ",df_comb_neg$review)
df_comb_neg$visual_neg <-as.numeric((str_count(df_comb_neg$temp, paste(sprintf("\\b%s\\b", keywords_visual), collapse = '|'))))
sum(df_comb_neg$visual_neg, na.rm=T)

#Thermal negative
keywords_thermal <- c("too hot","too cold","right temperature?","wrong temperature","doesn't give the right temperature","would not show the right temperature","no right temperature","unable to get to the right temperature","right temperature but it won't sustain it","right temperature keep changing","never truly maintain the right temperature", "extremely hot","freezing cold","were an still uncomfortable","always felt uncomfortable","uncomfortable times","completely uncomfortable","very uncomfrotable","uncomfortable level","this one was uncomfortable","you are uncomfortable","been uncomfortable","terribly uncomfortable","totally uncomfortable","sit through uncomfortable temperatures","the uncomfortable temperatures","uncomfortable temperature control","uncomfortable to use")
df_comb_neg$temp <- gsub("wasnt too hot|wasnt too cold|wasn\'t too hot|wasn\'t too cold|not too hot|not too cold|learns to set right temperature|keep your house at a right temperature|always the right temperature|get it to the right temperature|finally the right temperature|just the right temperature|very uncomfortable entering|when i'm not at the right temperature|at the right temperature|not freezing cold|not extremely hot|very uncomfortable entering", " ", df_comb_neg$review)
df_comb_neg$Thermal_neg <-as.numeric((str_count(df_comb_neg$temp, paste(sprintf("\\b%s\\b", keywords_thermal), collapse = '|'))))
sum(df_comb_neg$Thermal_neg, na.rm=T)

#Energy negative
keywords_energy <-c("increased our spending","increased spending","not worth energy savings","no energy savings","no savings","does not save energy","doesnt save energy","does not save money","doesnt save money","costly bill","expensive bill","my electric bill get 150% higher", "big bill","huge bill","bigger bill","larger bill","higher bill")
df_comb_neg$temp <- gsub("save money from higher bill|don't have big bill|instead of getting a super expensive bill", " ", df_comb_neg$review)
df_comb_neg$Energy_neg <-as.numeric((str_count(df_comb_neg$temp, paste(sprintf("\\b%s\\b", keywords_energy), collapse = '|'))))
sum(df_comb_neg$Energy_neg, na.rm=T)

#Control negative
keywords_control <- c("does not dim","only through the app","app is bad","app is slow","no remote access","unable to dim","cannot dim","will not work with multiple","app is hard to use","can control from app","does not have option to turn off automatically","could be more user friendly","only works on amazon controlled devices", "cannot dim","no timer setting","can not program","will not connect","i still cannot get them to work correctly","never could get this to work","only works with alexa","constantly lose connectivity","they lose connectivity","they lose connection","exclusively with alexa","it loses connection","connection loss")
df_comb_neg$temp <- gsub("app is awesome|from the phone|if they lose connection", " ", df_comb_neg$review)
df_comb_neg$Control_neg <-as.numeric((str_count(df_comb_neg$temp, paste(sprintf("\\b%s\\b", keywords_control), collapse = '|'))))
sum(df_comb_neg$Control_neg, na.rm=T)

#Ease of use negative
keywords_ease <- c("instructions are useless","so very hard to set up","does not want to connect","completely stopped working","could not get it to work","not very easy","not easy","difficult installation","difficult to install","time consuming","difficult to set up","difficult to program","hard to install","difficult to learn","hard to learn","difficult to use","hard to use","will not connect","can not connect")
df_comb_neg$temp <- gsub("some were difficult to set up|easy to install|not difficult to install|not difficult to set up|not difficult to use|not hard to use|not a difficult installation|not difficult to program|not hard to learn|not difficult to learn|not time consuming|for a light switch in the kitchen that's not easy to reach|not easy to determine|customer support was not easy to reach|the directions of course are not easy|but not easy for everyone", " ", df_comb_neg$review)
df_comb_neg$Ease_neg <-as.numeric((str_count(df_comb_neg$temp, paste(sprintf("\\b%s\\b", keywords_ease), collapse = '|'))))
sum(df_comb_neg$Ease_neg, na.rm=T)

df_comb_neg['cons_type'] <- NA

for(i in c(1:nrow(df_comb_neg))){
  temp = df_comb_neg[i,]
  category = ""
  if(temp$visual_neg > 0){
    category <- str_c(category,"visual_neg&")
  }
  if(temp$Thermal_neg > 0){
    category <- str_c(category,"thermal_neg&")
  }
  if(temp$Energy_neg > 0){
    category <- str_c(category, "energy_neg&")
  }
  if(temp$Control_neg > 0){
    category <- str_c(category,"control_neg&")
  }
  if(temp$Ease_neg > 0){
    category <- str_c(category, "ease_neg&")
  }
  if(str_length(category)==0){
    category = "no negative"
  }else{
    category <- substr(category,1, str_length(category)-1)
  }
  df_comb_neg[i,]$cons_type <- category
}
                                


################### POSITIVE ANALYSIS ################

#Dataset_for_positive
df_comb_pos <- df_comb

#Visual positive
keywords_visual <- c("beautiful light","good light","great light","nice light","not too bright","perfect light", "not blinding", "non blinding","is bright","light is very bright","lights are very bright","isn????????t too bright", "amazing light", "perfect brightness","well lit","lit up","just bright enough","quite bright","fantastically bright","optimal brightness","are bright", "incredibly bright", "relaxing brightness","decently bright","sufficiently bright","looks bright","bright light", "best light","colorful light","excellent light","great color","beautiful color","good color","vibrant color","nice warming light","nice warm light","nice warm white","nice warm glow","nice warm lighting","favorite light","really bright","are bright","nice and bright","extremely bright","absolutely bright","super bright","pretty bright","nice color","nice and warm light","light is nice and warm","light is warm")
df_comb_pos$temp <- gsub("too bright|too dark|beautiful lights on our beautiful tree|good light dies super fast|good light switch|would be good lights|good lightbulb|great light bulb|great light switch|nice light switch|nice light bulb|perfect light bulb|light is brighter|isnt well lit|i have a well lit space|some are brighter"," ", df_comb_pos$review)
df_comb_pos$Visual_pos <-as.numeric((str_count(df_comb_pos$temp, paste(sprintf("\\b%s\\b", keywords_visual), collapse = '|'))))
sum(df_comb_pos$Visual_pos, na.rm=T)

#Thermal positive
keywords_thermal <- c("excellent temperature","good temperature","great temperature","is not cold","not too hot","perfect temperature","right temperature","temperature is controlled","temperature is correct","temperature is perfect","temperature was just right","warm in the winter","comfortable temperature", "consistent temperature","nice warm","warms me","warms us","steady temperature","accurate temperature","temperature is accurate","temperature is right","temperature was right","temperature was correct","temperature is correct","doesn????????t get uncomfortable","very comfortable","keeps the house comfortable","comfortable when you get home","house is comfortable","max comfort","keep you comfortable","improve your comfort","improve my comfort","sheer comfort","comfortable summer","comfortable year round")
df_comb_pos$temp <- gsub("right temperature?|doesn't give the right temperature|would not show the right temperature|no right temperature|unable to get to the right temperature|right temperature but it won't sustain it|right temperature keep changing|bright temperature|doesn't show the right temperature|never truly maintain the right temperature|indicates the set temperature is correct|is warmed up|light is warm|when the weather is warmer so|the other sensor is warmer|the weather still is warm|this warm light|a bulb that is warm|his warm|that upstairs is warmer than downstairs|now that it is warming up|nice warming light|favors effieciency over comfort|prioritizes efficiency over comfort", " ", df_comb_pos$review)
df_comb_pos$Thermal_pos <-as.numeric((str_count(df_comb_pos$temp, paste(sprintf("\\b%s\\b", keywords_thermal), collapse = '|'))))
sum(df_comb_pos$Thermal_pos, na.rm=T)

#Energy positive
keywords_energy <-c("save on your heating bills","way cheaper","bills are lower","saving energy costs","bills went down","saving you money","save energy","save money on","save money and","save money by","save money with","save money in","save money while","save electricity","saves money","saves energy","lower energy bill","lower bill","lower energy cost","lower monthly cost","saves electricity","save on your bill","bill was lowered","saved money","saved electricity","reduced my bill","reduce your bill","reduced my energy bill","reduces my spending","reduced my spending","lessened my spending","lessened spending","lessens my spending","lessens spending","lessens my bill","lessens my energy bill","lessened energy spending","lessens the bill","lessen your bill","spent less","charged less")
df_comb_pos$temp <- gsub("without my permission to disable the thermostat to save energy|it does not regulate the heat equally through the home to save energy|give me \"pointers\" on how to save energy|basically just shuts your hvac system off to save energy|basically just shuts your hvac system off to save energy|not really what you want to do if you are looking to save energy|hope it will save energy|plan to use geo-fencing to save energy|hard to see how this unit could save energy|trying to make me save energy|i wanted it to save energy|i want to save energy|they were ?????~helping me?????T save energy|save energy costs for me, but don't do it at the cost of family comfort|does not save energy|is supposed to save energy?|was supposed to save energy|unsure on how it is going to help me save energy|it pushes the limits trying to save energy|does it save money on electricity and gas?|not save money on|save money on wiring|learned nothing that will help me save money on my electric bill|save money and buy the cheaper|save money and buy|trying to save money and|black friday picked up one to save money and|save money and do it yourself|save money and buy a regular one|i got this to save money and|save money and secure uour home against|software engineers to save money and|like a nice way to save money while", " ", df_comb_pos$review)
df_comb_pos$Energy_pos <-as.numeric((str_count(df_comb_pos$temp, paste(sprintf("\\b%s\\b", keywords_energy), collapse = '|'))))
sum(df_comb_pos$Energy_pos, na.rm=T)

#Control positive
keywords_control <-c("relaxing","app works great","app is easy to use","there is a setting","dimmable","makes controlling the temperature remotely simple","control your heat from anywhere","good temp control","increased my control","precise and controlled","high precision","can control temperature more precisely","allows you to control","allows for control","allows for easy control","great control","good control","can control","can easily control","very convenient in controlling","enables remote access","can remotely")
df_comb_pos$temp <-gsub("seizes control|could not see them|app won't allow me|cannot see or control it|only way to control|only controls|even though i can control|neither of my echo dots can control it|only one user in a household can control it", " ", df_comb_pos$review)
df_comb_pos$Control_pos <- as.numeric((str_count(df_comb_pos$temp, paste(sprintf("\\b%s\\b", keywords_control), collapse = '|'))))
sum(df_comb_pos$Control_pos, na.rm=T)

#Ease of use positive
keywords_ease <-c("very easy to install","easy to configure","breeze to install", "very easy to use", "very easy to set", "very easy to connect","very easy to program","very easy to operate","very easy to control","super easy to install","super easy to set","super easy to use","super easy to setup","super easy to connect", "super easy to control","was easy to install","was easy to set","was easy to connect","was easy to use","simple to install","simple to set","simple install","simple to use","intuitive to use","a child could do it","child could set it up","child can do it","child could use it","enough for a child","easy for my kids to use","simple to program","simple to connect","easy to program","setup was easy","setup was extremely easy","set up easily","really easy to set up","setup was really easy")
df_comb_pos$temp <-gsub("setup was easy until|setup was easy then|easy to use until|not very easy|not easy|claimed it was simple and easy to install|not intuitive|not simple", " ", df_comb_pos$review)
df_comb_pos$Ease_pos <- as.numeric((str_count(df_comb_pos$temp, paste(sprintf("\\b%s\\b", keywords_ease), collapse = '|'))))
sum(df_comb_pos$Ease_pos, na.rm=T)

df_comb_pos['pros_type'] <- NA

for(i in c(1:nrow(df_comb_pos))){
  temp = df_comb_pos[i,]
  category = ""
  if(temp$Visual_pos > 0){
    category <- str_c(category,"visual_pos&")
  }
  if(temp$Thermal_pos > 0){
    category <- str_c(category,"thermal_pos&")
  }
  if(temp$Energy_pos > 0){
    category <- str_c(category, "energy_pos&")
  }
  if(temp$Control_pos > 0){
    category <- str_c(category,"control_pos&")
  }
  if(temp$Ease_pos > 0){
    category <- str_c(category, "ease_pos&")
  }
  if(str_length(category)==0){
    category = "no positive"
  }else{
    category <- substr(category,1, str_length(category)-1)
  }
  df_comb_pos[i,]$pros_type <- category
}


################ ______ MERGE POSITIVE AND NEGATIVE DATASETS AND SAVE FILES ______ #######

#Save df_comb_pos 
write.csv(df_comb_pos, "...path...")

#Save df_comb_neg 
write.csv(df_comb_neg, "...path...")
