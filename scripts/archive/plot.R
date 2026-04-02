

##################   
#
# preprocessing
# 
################


# combine csvs
files <- list.files("../norming-exp/data", pattern = "task-pkec.*\\.csv$", recursive = TRUE, full.names = TRUE)
raw <- map_dfr(files, ~ read_csv(.x, col_types = cols(.default = "c")))

# check catch trials

data <- raw %>%
  rename(ID = 'Participant Private ID') %>%
  rename(stim = 'Spreadsheet: stim') %>%
  rename(block = 'Spreadsheet: randomized_trial')%>%
  filter(Display == "phonetic categorization") %>%
  filter(`Response Type` == "response") %>%
  mutate(
    catch = case_when(
      str_detect(stim, "left")  ~ `Spreadsheet: left`,
      str_detect(stim, "right") ~ `Spreadsheet: right`,
      TRUE ~ NA_character_
    ))%>%
  mutate(catch.corr = if_else(catch == Response, 1, 0))


catch.acc <- data %>% 
  filter(stim %in% c("pleft_ame.mp3", "pright_ame.mp3")) %>%
  group_by(ID) %>%
  summarise(acc = mean(catch.corr, na.rm = TRUE))



##################   
#
# plotting
# 
################




# prepare the data for plot
data.plot <- data %>%
  filter(stim != "pleft_ame.mp3" & stim != "pright_ame.mp3") %>%
  mutate(VOT = as.numeric(str_extract(stim, "(?<=_)[^.]+"))) %>%
  mutate(resp.num = ifelse(Response == "d", 0, 1))%>%
  mutate(word = str_extract(stim, "^[^_]+" )) %>%
  select(VOT, resp.num, ID, block, word, stim)

# plot the subject-level, raw data 
subj.acc <- data.plot %>% group_by(ID, VOT) %>%
  summarise(mean.resp = mean(resp.num), 
            se = sd(resp.num)/sqrt(n()), )

ggplot(subj.acc, aes(x = VOT, y = mean.resp, color= ID, group = ID)) +
  geom_line() +
  geom_point() +
  labs(x = "VOT",y = "probability of /t/")


# plot group-level data, with uncertainty around it

group.acc <- data.plot %>% group_by(VOT) %>%
  summarise(mean.resp = mean(resp.num), 
            se = sd(resp.num)/sqrt(n()), )

ggplot(group.acc, aes(x = VOT, y = mean.resp)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean.resp - se, ymax = mean.resp + se),
                width = 1) +
  labs(x = "VOT",y = "probability of /t/")



#group by each block
ggplot(data.plot, aes(x = VOT, y = resp.num, color = block, group = block)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  
  #theme_minimal() +
  labs(x = "VOT", y = "probability of /t/", title = "categorization curve") 



#group by each continua
ggplot(data.plot, aes(x = VOT, y = resp.num)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  facet_wrap(~ word) +
  labs(x = "VOT", y = "probability of /t/", title = "categorization curve")
