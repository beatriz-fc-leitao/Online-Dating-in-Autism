Install packages
```{r}
install.packages("tidyverse")
install.packages("magrittr")
install.packages("matrixStats")

```

Load packages
```{r}
library(tidyverse)
library(magrittr)
library(matrixStats)
library(broom)

```

Non-essential data
```{r}
non_essential <- online_dating_ASD_data %>%
  select(V1:V9, V138) %>%
  rename(progress = V1, duration = V2, participant_criteria = V3, consent = V4, female = V5, seeking = V6, age = V7, ethnicity = V8, in_UK = V9, attention_check = V138)
```

Getting rid of non-essential columns
```{r}
essential <- online_dating_ASD_data %>%
select( -V1, -V2, -V3, -V4, -V5, -V6, -V7, -V8, -V9, -V41, -V73, -V105, -V137, -V138, -V151, -V158, -V159, -V160, -V161, -V162, -V163, -V164)
```

Renaming condition and arranging by ascending order of condition
```{r}
essential <- essential %>%  
  rename(condition = V165) %>%
  arrange(condition) 

```

Getting rid of extra rows 
```{r}
essential <- essential %>%
  filter(condition == 1 | condition == 2 | condition == 3 | condition == 4)
```

Merging questions in 4 conditions
```{r}
essential <- essential %>%
  unite("social_1", c(V10, V42, V74, V106), remove = TRUE) %>%
  unite("social_2", c(V11, V43, V75, V107), remove = TRUE) %>%
  unite("social_3", c(V12, V44, V76, V108), remove = TRUE) %>%
  unite("social_4", c(V13, V45, V77, V109), remove = TRUE) %>%
  unite("social_5", c(V14, V46, V78, V110), remove = TRUE) %>%
  unite("social_6", c(V15, V47, V79, V111), remove = TRUE) %>%
  unite("social_7", c(V16, V48, V80, V112), remove = TRUE) %>%
  unite("social_8", c(V17, V49, V81, V113), remove = TRUE) %>%
  unite("social_9", c(V18, V50, V82, V114), remove = TRUE) %>%
  unite("social_10", c(V19, V51, V83, V115), remove = TRUE) %>%
  unite("social_11", c(V20, V52, V84, V116), remove = TRUE) %>%
  unite("social_12", c(V21, V53, V85, V117), remove = TRUE) %>%
  unite("physical_1", c(V22, V54, V86, V118), remove = TRUE) %>%
  unite("physical_2", c(V23, V55, V87, V119), remove = TRUE) %>%
  unite("physical_3", c(V24, V56, V88, V120), remove = TRUE) %>%
  unite("task_1", c(V25, V57, V89, V121), remove = TRUE) %>%
  unite("task_2", c(V26, V58, V90, V122), remove = TRUE) %>%
  unite("task_3", c(V27, V59, V91, V123), remove = TRUE) %>%
  unite("task_4", c(V28, V60, V92, V124), remove = TRUE) %>%
  unite("task_5", c(V29, V61, V93, V125), remove = TRUE) %>%
  unite("trustworthiness_1", c(V30, V62, V94, V126), remove = TRUE) %>%
  unite("trustworthiness_2", c(V31, V63, V95, V127), remove = TRUE) %>%
  unite("trustworthiness_3", c(V32, V64, V96, V128), remove = TRUE) %>%
  unite("trustworthiness_4", c(V33, V65, V97, V129), remove = TRUE) %>%
  unite("trustworthiness_5", c(V34, V66, V98, V130), remove = TRUE) %>%
  unite("trustworthiness_6", c(V35, V67, V99, V131), remove = TRUE) %>%
  unite("date_1", c(V36, V68, V100, V132), remove = TRUE) %>%
  unite("date_2", c(V37, V69, V101, V133), remove = TRUE) %>%
  unite("date_3", c(V38, V70, V102, V134), remove = TRUE) %>%
  unite("date_4", c(V39, V71, V103, V135), remove = TRUE) %>%
  unite("date_5", c(V40, V72, V104, V136), remove = TRUE)
```

Renaming experience columns
```{r}
essential <- essential %>%
rename(experience_3 = V139, experience_8 = V140, experience_2 = V141, experience_5 = V142, experience_12 = V143, experience_6 = V144, experience_1 = V145, experience_7 = V146, experience_9 = V147, experience_10 = V148, experience_4 = V149, experience_11 = V150)
```

Renaming stigma columns 
```{r}
essential <- essential %>%
  rename(stigma_1 = V152, stigma_2 = V153, stigma_3 = V154, stigma_4 = V155, stigma_5 = V156, stigma_6 = V157)
```

Making DV values numerical
```{r}
essential <- as.data.frame(lapply(essential, function(x) as.numeric(gsub(pattern = '[^(-?(\\d*\\.)?\\d+)]', replacement = '', x))))
```

Adding total stigma
```{r}
essential <- essential %>%
  mutate(stigma_total = (stigma_1 + stigma_2 + stigma_3 + stigma_4 + stigma_5 + stigma_6))

```

Calculating total experience
```{r}
experience <- essential %>%
  select("experience_1", "experience_2", "experience_3", "experience_4", "experience_5", "experience_6", "experience_7", "experience_8", "experience_9", "experience_10", "experience_11", "experience_12") %>%
  transmute(ex_1 = experience_1 * 1, 
            ex_2 = experience_2 * 2, 
            ex_3 = experience_3 * 3, 
            ex_4 = experience_4 * 4, 
            ex_5 = experience_5 * 5, 
            ex_6 = experience_6 * 6, 
            ex_7 = experience_7 * 7, 
            ex_8 = experience_8 * 8, 
            ex_9 = experience_9 * 9, 
            ex_10 = experience_10 * 10, 
            ex_11 = experience_11 * 11, 
            ex_12 = experience_12 * 12)

experience_matrix <- data.matrix(experience)

ex_total <- rowMaxs(experience_matrix, na.rm = TRUE)

exp_total <- as.data.frame(ex_total)
```

Adding total experience to essential
```{r}
essential$experience_total = exp_total$ex_total
```

Reordering columns
```{r}
col_order <- c("social_1", "social_2", "social_3", "social_4", "social_5", "social_6", "social_7", "social_8", "social_9", "social_10", "social_11", "social_12", "physical_1", "physical_2", "physical_3", "task_1", "task_2", "task_3", "task_4", "task_5", "trustworthiness_1", "trustworthiness_2", "trustworthiness_3", "trustworthiness_4", "trustworthiness_5", "trustworthiness_6", "date_1", "date_2", "date_3", "date_4", "date_5", "experience_1", "experience_2", "experience_3", "experience_4", "experience_5", "experience_6", "experience_7", "experience_8", "experience_9", "experience_10", "experience_11", "experience_12", "experience_total", "stigma_1", "stigma_2", "stigma_3", "stigma_4", "stigma_5", "stigma_6", "stigma_total", "condition")

essential <- essential[, col_order]
```

Making all values 1-5
```{r}
A <- function(x) x - 5

essential <- data.frame(apply(essential[1:26], 2, A), essential[27:52])

essential$date_3 = essential$date_3 - 17
```

Reverse scoring
```{r}
cols = c("social_3", "social_4", "social_5", "social_8", "social_11", "social_12", "physical_2", "physical_3", "task_1", "task_4", "task_5", "trustworthiness_2", "trustworthiness_5")

essential[, cols] <- lapply(cols, function(x) 6 - essential[, x])
```

Adding label column
```{r}
B <- function(x) if (x == 1 | x == 3) { 
  return("no label")
} else {
    return("yes label")
  }

essential$label = lapply(essential$condition, B)

```

Adding wording column
```{r}
C <- function(y) if (y == 1 | y == 2) {
  return("positive")
} else {
  return("negative")
}

essential$wording = lapply(essential$condition, C)
```

Revese coding stigma total and arranging condition in order
```{r}
essential$stigma_total = as.numeric(lapply(essential$stigma_total, function(z) 30 - z))


```

Adding DV mean columns
```{r}
essential$social_mean = rowMeans(essential[1:12])
essential$physical_mean = rowMeans(essential[13:15])
essential$task_mean = rowMeans(essential[16:20])
essential$trustworthiness_mean = rowMeans(essential[21:26])
essential$date_mean = rowMeans(essential[27:31])
```

Making final dataset to analyse
```{r}
final_data <- na.omit(essential[51:59])

```

Stigma median split 
```{r}
stigma_median <- median(final_data$stigma_total)

D <- function(x) if (x >= stigma_median) {
  return("high") 
} else {
  return("low")
}

final_data$stigmatiser = lapply(final_data$stigma_total, D)

```

Removing stigma total column form final data
```{r}
final_data <- final_data %>%
  select(-stigma_total)
```

Reordering final data columns
```{r}
reorder_columns <- c("condition", "label", "wording", "stigmatiser", "social_mean", "physical_mean", "task_mean", "trustworthiness_mean", "date_mean")

final_data <- final_data[, reorder_columns]
```

ANOVAs for all 5 DVs
```{r}

final_data$wording <- unlist(final_data$wording)
final_data$label <- unlist(final_data$label)
final_data$stigmatiser <- unlist(final_data$stigmatiser)

social_aov <- aov(social_mean ~ wording * label * stigmatiser, data = final_data)
physical_aov <- aov(physical_mean ~ wording * label * stigmatiser, data = final_data)
task_aov <- aov(task_mean ~ wording * label * stigmatiser, data = final_data)
trustworthiness_aov <- aov(trustworthiness_mean ~ wording * label * stigmatiser, data = final_data)
date_aov <- aov(date_mean ~ wording * label * stigmatiser, data = final_data)

tidy(social_aov)
tidy(physical_aov)
tidy(task_aov)
tidy(trustworthiness_aov)
tidy(date_aov)

```

