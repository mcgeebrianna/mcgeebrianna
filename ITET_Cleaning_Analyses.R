rm(list=ls()) #Clear Environment 
library(plyr) #set of packages
library(dplyr)
library(tidyr)
library(purrr)
library(nlme)
library(psych)
library(car)
library(ggplot2)
library(stringr)
library(rio)
library(data.table)
library(rstatix)
library(tidyverse)
library(ggpubr)

dir <- list.files(pattern =".csv")

#dir <- '107.csv'

Summary_Table <- data.frame(matrix(ncol = 16, nrow = 0))
x <- c("id", "OtherDuration", "TargetDuration", "OtherFixationCount", "TargetFixationCount", 
       "OtherLatency", "TargetLatency", "FV1_OtherDuration", "FV1_TargetDuration", "FV1_OtherFC", 
       "FV1_TargetFC", "FV1_OtherLatency", "FV1_TargetLatency", "OtherFirstLook", "TargetFirstLook",
       "FV1_FirstLook")
colnames(Summary_Table) <- x

for(i in 1:length(dir)) {
  dat2 <- read.csv(dir[i], header = TRUE, sep = ",")
  
  # Cleaning AOI label names
  dat2$IA_LABEL <- gsub("_", " ", dat2$IA_LABEL)
  dat2<- separate(dat2, col = IA_LABEL, into = c('trial_name', 'aoi'), extra = "drop", remove = TRUE, sep = ' ')
  dat2$target <- as.character(dat2$target)
  
  #DVs
  # Target & Other durations (IA Dwell time)
  # Target & Other fixation counts (IA fixation Count)
  # First look (compare IA first fixation time btwn target & other (earliest = first))
  


  for (row in 1:nrow(dat2)) {
    
    # Dwell Times/Durations
    
    dat2$OtherDuration <- ifelse((dat2$target == "Flower" & dat2$aoi == "sun"), dat2$IA_DWELL_TIME,
                                 ifelse(dat2$target == "Sun" & dat2$aoi == "flower", dat2$IA_DWELL_TIME, NA))

  
  
    dat2$TargetDuration <- ifelse((dat2$target == "Flower" & dat2$aoi == "flower"), dat2$IA_DWELL_TIME,
                                ifelse(dat2$target == "Sun" & dat2$aoi == "sun", dat2$IA_DWELL_TIME, NA))
  
  # # Fixation Counts

    dat2$OtherFixationCount <- ifelse((dat2$target == "Flower" & dat2$aoi == "sun"), dat2$IA_FIXATION_COUNT,
                                 ifelse(dat2$target == "Sun" & dat2$aoi == "flower", dat2$IA_FIXATION_COUNT, NA))
    
    dat2$TargetFixationCount <- ifelse((dat2$target == "Flower" & dat2$aoi == "flower"), dat2$IA_FIXATION_COUNT,
                                       ifelse(dat2$target == "Sun" & dat2$aoi == "sun", dat2$IA_FIXATION_COUNT, NA))
  
    # Latency
    
    dat2$OtherLatency <- ifelse((dat2$target == "Flower" & dat2$aoi == "sun"), (as.numeric(dat2$IA_FIRST_FIXATION_TIME) - dat2$IA_START_TIME),
                                ifelse(dat2$target == "Sun" & dat2$aoi == "flower", (as.numeric(dat2$IA_FIRST_FIXATION_TIME) - dat2$IA_START_TIME), NA))
  
    dat2$TargetLatency <- ifelse((dat2$target == "Flower" & dat2$aoi == "flower"), (as.numeric(dat2$IA_FIRST_FIXATION_TIME) - dat2$IA_START_TIME),
                                ifelse(dat2$target == "Sun" & dat2$aoi == "sun", (as.numeric(dat2$IA_FIRST_FIXATION_TIME) - dat2$IA_START_TIME), NA))
    
    
    #SummaryStats <- summary(dat2[c("OtherDuration", "TargetDuration", "OtherFixationCount", "TargetFixationCount", "OtherLatency", "TargetLatency")], na.rm = TRUE)

     
  }
  dat2[dat2 == 0] <- NA  
  dat2$FirstLook <- NA
  
  for (p in seq(from = 17, to = length(dat2$IA_FIRST_FIXATION_TIME), by = 2)) {
  
    #print(p)
    #print(dat2$IA_FIRST_FIXATION_TIME[p])
    #print(dat2$IA_FIRST_FIXATION_TIME[p+1])
    
    if(dat2$IA_FIRST_FIXATION_TIME[p] == "." & dat2$IA_FIRST_FIXATION_TIME[p+1] == ".") {
      
      #
      # both ARE periods
      #
      #dat2$FirstLook[p] <- 0
      #dat2$FirstLook[p+1] <- 0
      
    } else if(dat2$IA_FIRST_FIXATION_TIME[p] != "." & dat2$IA_FIRST_FIXATION_TIME[p+1] != ".") {

      #
      # both are NOT periods
      #
      linenumber <- ifelse(as.numeric(dat2$IA_FIRST_FIXATION_TIME[p]) < as.numeric(dat2$IA_FIRST_FIXATION_TIME[p+1]), p, p+1)
      dat2$FirstLook[linenumber] <- switch (paste0(dat2$target[linenumber], dat2$aoi[linenumber], sep = ""),
        "Flowersun" = "Other",
        "Sunflower" = "Other",
        "Flowerflower" = "Target",
        "Sunsun" = "Target",
        9
      )
      
    } else if (dat2$IA_FIRST_FIXATION_TIME[p] != ".") {
      
      #
      # use the FIRST line
      #
      dat2$FirstLook[p] <- switch (paste0(dat2$target[p], dat2$aoi[p], sep = ""),
         "Flowersun" = "Other",
         "Sunflower" = "Other",
         "Flowerflower" = "Target",
         "Sunsun" = "Target", 8
      )
      
    } else {

      #
      # use the SECOND line
      #
      dat2$FirstLook[p+1] <- switch (paste0(dat2$target[p+1], dat2$aoi[p+1], sep = ""),
         "Flowersun" = "Other",
         "Sunflower" = "Other",
         "Flowerflower" = "Target",
         "Sunsun" = "Target", 7
      )
      
    }
  }

  id <- dat2$RECORDING_SESSION_LABEL[1]
  #OtherDuration <- with(dat2, mean(OtherDuration[17:last(dat2$OtherDuration)]), na.rm = TRUE)
  OtherDuration <- with(dat2, mean(OtherDuration[17:33], na.rm = TRUE))
  TargetDuration <- with(dat2, mean(TargetDuration[17:33], na.rm = TRUE))
  OtherFixationCount = with(dat2, mean(OtherFixationCount[17:33], na.rm = TRUE))
  TargetFixationCount = with(dat2, mean(TargetFixationCount[17:33], na.rm = TRUE))
  OtherLatency = with(dat2, mean(OtherLatency[17:33], na.rm = TRUE))
  TargetLatency = with(dat2, mean(TargetLatency[17:33], na.rm = TRUE))
  OtherFirstLook = length(which(dat2$FirstLook == "Other"))
  TargetFirstLook = length(which(dat2$FirstLook == "Target"))
  
  FV1_FirstLook <- if(is.na(dat2$FirstLook[17])) {
    dat2$FirstLook[18]
  } else if (!is.na(dat2$FirstLook[17])) {
    dat2$FirstLook[17]
  } else { NA
  }

  # FV1_OtherFL <- if(is.na(dat2$FirstLook[17])) {
  #   dat2$FirstLook[18]
  # } else if (!is.na(dat2$FirstLook[17])) {
  #   dat2$FirstLook[17]
  # } else { NA
  # 
  # }
  
  FV1_OtherDuration <- if(is.na(dat2$OtherDuration[17])) {
    dat2$OtherDuration[18]
  } else {
    dat2$OtherDuration[17]
  }
  
  
  FV1_TargetDuration <- if(is.na(dat2$TargetDuration[17])) {
    dat2$TargetDuration[18]
  } else {
    dat2$TargetDuration[17]
  } 
  
  
  FV1_OtherFC <- if(is.na(dat2$OtherFixationCount[17])) {
    dat2$OtherFixationCount[18]
  } else {
    dat2$OtherFixationCount[17]
  }
  
  FV1_TargetFC <- if(is.na(dat2$TargetFixationCount[17])) {
    dat2$TargetFixationCount[18]
  } else {
    dat2$TargetFixationCount[17]
  }
  
  FV1_OtherLatency <- if(is.na(dat2$OtherLatency[17])) {
    dat2$OtherLatency[18]
  } else {
    dat2$OtherLatency[17]
  }
  
  FV1_TargetLatency <- if(is.na(dat2$TargetLatency[17])) {
    dat2$TargetLatency[18]
  } else {
    dat2$TargetLatency[17]
  } 
  
#  FV1_FirstLook <- 0
  
  df <- data.frame(id, OtherDuration, TargetDuration, OtherFixationCount, TargetFixationCount, OtherLatency, TargetLatency,
                   FV1_OtherDuration, FV1_TargetDuration, FV1_OtherFC, FV1_TargetFC, FV1_OtherLatency, FV1_TargetLatency, OtherFirstLook, TargetFirstLook, FV1_FirstLook)

  Summary_Table <- rbind.data.frame(Summary_Table, df)
  rbind(Summary_Table, df)
}

print(Summary_Table, digits = 4)

#Replacing NaN with 0

Summary_Table <- Summary_Table %>% mutate(OtherDuration = coalesce(OtherDuration, 0))
Summary_Table <- Summary_Table %>% mutate(TargetDuration = coalesce(TargetDuration, 0))
Summary_Table <- Summary_Table %>% mutate(OtherFixationCount = coalesce(OtherFixationCount, 0))
Summary_Table <- Summary_Table %>% mutate(TargetFixationCount = coalesce(TargetFixationCount, 0))
Summary_Table <- Summary_Table %>% mutate(OtherLatency = coalesce(OtherLatency, 0))
Summary_Table <- Summary_Table %>% mutate(TargetLatency = coalesce(TargetLatency, 0))

# Calculating age
for (row in 1:nrow(Summary_Table)) {
  
  Summary_Table$age <- ifelse((Summary_Table$id <= 99), 8,
                         ifelse(Summary_Table$id <= 199, 12, 18))
}

# Output

csv <- write.csv(x = Summary_Table, file = "ITETSummaryStats.csv")


# Clean copy for analyses, just in case

Analyses <- Summary_Table

# FV1 Analyses here (looking only at values for the first FV trial, not aggregated over all 8 FV trials)

# Dwell time for first trial only

Analyses_FV1Duration <- reshape(Analyses, 
                             varying = c("FV1_OtherDuration", "FV1_TargetDuration"), 
                             v.names = "FV1_Duration", 
                             timevar = "Object", 
                             times = c("Other", "Target"),
                             new.row.names = 1:80,
                             direction = "long")

FV1Duration_Anova <- aov(FV1_Duration ~ age + Object, data = Analyses_FV1Duration)
summary(FV1Duration_Anova)

# Fixation Count for first trial only

Analyses_FV1FixCount <- reshape(Analyses, 
                                varying = c("FV1_OtherFC", "FV1_TargetFC"), 
                                v.names = "FV1_FixCount", 
                                timevar = "Object", 
                                times = c("Other", "Target"),
                                new.row.names = 1:80,
                                direction = "long")

FV1FixCount_Anova <- aov(FV1_FixCount ~ age + Object, data = Analyses_FV1FixCount)
summary(FV1FixCount_Anova)

group_by(Analyses_FV1FixCount, Object,  age) %>% #can also group by age
  summarise(
    count = n(),
    mean = mean(FV1_FixCount, na.rm = TRUE),
    sd = sd(FV1_FixCount, na.rm = TRUE)
  )
FV1_FixCount_Plot <- ggboxplot(Analyses_FV1FixCount, x = "age", y = "FV1_FixCount", color = "Object",
                           palette = c("#00AFBB", "#E7B800"))
print(FV1_FixCount_Plot)

# Latency for first trial only 

Analyses_FV1Latency <- reshape(Analyses, 
                                varying = c("FV1_OtherLatency", "FV1_TargetLatency"), 
                                v.names = "FV1_Latency", 
                                timevar = "Object", 
                                times = c("Other", "Target"),
                                new.row.names = 1:80,
                                direction = "long")

FV1Latency_Anova <- aov(FV1_Latency ~ age + Object, data = Analyses_FV1Latency)
summary(FV1Latency_Anova)

group_by(Analyses_FV1Latency,  Object) %>% #can also group by age, but n.s.
  summarise(
    count = n(),
    mean = mean(FV1_Latency, na.rm = TRUE),
    sd = sd(FV1_Latency, na.rm = TRUE)
  )


# Aggregate Duration over 8 trials

Analyses_Duration <- reshape(Analyses, 
             varying = c("OtherDuration", "TargetDuration"), 
             v.names = "Duration", 
             timevar = "Object", 
             times = c("Other", "Target"),
             new.row.names = 1:80,
             direction = "long")

# DUR anova over 8 trials
Duration_Anova <- aov(Duration ~ age + Object, data = Analyses_Duration)
summary(Duration_Anova)

group_by(Analyses_Duration,  Object) %>% #can also group by age, but n.s.
  summarise(
    count = n(),
    mean = mean(Duration, na.rm = TRUE),
    sd = sd(Duration, na.rm = TRUE)
  )
Duration_Plot <- ggboxplot(Analyses_Duration, x = "age", y = "Duration", color = "Object",
                           palette = c("#00AFBB", "#E7B800"))
print(Duration_Plot)

Analyses_Count <- reshape(Analyses, 
                     varying = c("OtherFixationCount", "TargetFixationCount"), 
                     v.names = "FixationCount", 
                     timevar = "Object", 
                     times = c("Other", "Target"),
                     new.row.names = 1:80,
                     direction = "long")

# Count anova over 8 trials
Count_Anova <- aov(FixationCount ~ age + Object, data = Analyses_Count)
summary(Count_Anova)

group_by(Analyses_Count,  Object) %>% #can also group by age, but n.s.
  summarise(
    count = n(),
    mean = mean(FixationCount, na.rm = TRUE),
    sd = sd(FixationCount, na.rm = TRUE)
  )

Count_Plot <- ggboxplot(Analyses_Count, x = "age", y = "FixationCount", color = "FC_Object",
                        palette = c("#00AFBB", "#E7B800"))
print(Count_Plot)

# Latency over 8 trials
      
Analyses_Latency <- reshape(Analyses, 
                          varying = c("OtherLatency", "TargetLatency"), 
                          v.names = "Latency", 
                          timevar = "Object", 
                          times = c("Other", "Target"),
                          new.row.names = 1:80,
                          direction = "long")

Latency_Anova <- aov(Latency ~ age + Object, data = Analyses_Latency)
summary(Latency_Anova)


group_by(Analyses_Latency,  Latency_Object) %>% #can also group by age, but n.s.
  summarise(
    count = n(),
    mean = mean(Latency, na.rm = TRUE),
    sd = sd(Latency, na.rm = TRUE)
  )

Latency_Plot <- ggboxplot(Analyses_Latency, x = "age", y = "Latency", color = "Object",
                          palette = c("#00AFBB", "#E7B800"))
print(Latency_Plot)

#First Looks over 8 trials

Analyses_FirstLooks <- reshape(Analyses, 
                            varying = c("OtherFirstLook", "TargetFirstLook"), 
                            v.names = "FirstLook", 
                            timevar = "Object", 
                            times = c("Other", "Target"),
                            new.row.names = 1:80,
                            direction = "long")

FirstLooks_Anova <- aov(FirstLook ~ age + Object, data = Analyses_FirstLooks)
summary(FirstLooks_Anova)

group_by(Analyses_FirstLooks,  Object) %>% #can also group by age, but n.s.
  summarise(
    count = n(),
    mean = mean(FirstLook, na.rm = TRUE),
    sd = sd(FirstLook, na.rm = TRUE)
  )
FirstLooks_Plot <- ggboxplot(Analyses_FirstLooks, x = "age", y = "FirstLook", color = "Object",
                           palette = c("#00AFBB", "#E7B800"))

print(FirstLooks_Plot)



# Check for outliers, Hampel filter? (+/- 3 SD from the mean/median)

lower_bound_DUR <- mean(Analyses_Duration$Duration) - 3 * sd(Analyses_Duration$Duration, constant = 1)
upper_bound_DUR <- mean(Analyses_Duration$Duration) + 3 * sd(Analyses_Duration$Duration, constant = 1)
outlier_ind_DUR <- which(Analyses_Duration$Duration < lower_bound_DUR | Analyses_Duration$Duration > upper_bound_DUR) # row numbers of potential outliers
# rows: 4,49, 75 identified as potential outliers
# mean: 766.64

Analyses_Duration2 <- Analyses_Duration # a copy for outlier removal
Analyses_Duration2$Duration[4] <- 766.64
Analyses_Duration2$Duration[49] <- 766.64
Analyses_Duration2$Duration[75] <- 766.64


#DUR anova without outliers
Duration_Anova2 <- aov(Duration ~ age + Object, data = Analyses_Duration2)
summary(Duration_Anova2)

# Duration_Anova2 <- Analyses_Duration %>%
#   #group_by(Object) %>%
#   anova_test(dv = Duration, wid = id, within = Object) %>%
#   get_anova_table () %>%
#   adjust_pvalue(method = "bonferroni")


# With outliers removed

Duration_Plot2 <- ggboxplot(Analyses_Duration2, x = "age", y = "Duration", color = "Object",
                           palette = c("#00AFBB", "#E7B800"))
print(Duration_Plot2)

# check for outliers
lower_bound_Count <- mean(Analyses_Count$FixationCount) - 3 * sd(Analyses_Count$FixationCount)
upper_bound_Count <- mean(Analyses_Count$FixationCount) + 3 * sd(Analyses_Count$FixationCount)
outlier_ind_Count <- which(Analyses_Count$FixationCount < lower_bound_Count | Analyses_Count$FixationCount > upper_bound_Count)
# rows: 75
# mean: 2.31

Analyses_Count2 <- Analyses_Count
Analyses_Count2$FixationCount[75] <- 2.31

# Count anova with outliers removed

Count_Anova2 <- aov(FixationCount ~ age + Object, data = Analyses_Count2)
summary(Count_Anova2)

# Count_Anova2 <- Analyses_Count %>%
#   #group_by(Object) %>%
#   anova_test(dv = FixationCount, wid = id, within = FC_Object) %>%
#   get_anova_table () %>%
#   adjust_pvalue(method = "bonferroni")

########################## for random t-test
FC_Ttest_8 <- c(); # data.frame(matrix(ncol = 1, nrow = 0))
FC_Ttest_12 <- c(); # data.frame(matrix(ncol = 1, nrow = 0))
FC_Ttest_18 <- c(); # data.frame(matrix(ncol = 1, nrow = 0))

for (row in 1:nrow(Analyses)) {
  
  if (!is.na(Analyses$FV1_TargetFC[row])) {
    
    print(length(FC_Ttest_8))
    if (Analyses$age[row] == 8) {
      FC_Ttest_8[length(FC_Ttest_8)+1] <- Analyses$FV1_TargetFC[row]
    } else if (Analyses$age[row] == 12) {
      FC_Ttest_12[length(FC_Ttest_12)+1] <- Analyses$FV1_TargetFC[row]
    } else if (Analyses$age[row] == 18) {
      FC_Ttest_18[length(FC_Ttest_18)+1] <- Analyses$FV1_TargetFC[row]
    }
    
    
  }
}


