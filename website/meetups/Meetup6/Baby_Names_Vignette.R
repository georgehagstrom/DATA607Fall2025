library(tidyverse)
library(babynames)
library(ggplot2)

babynames


babynames |> 
  mutate(first_letter = str_sub(name,1,1)) |> 
  group_by(year,first_letter) |> 
  summarise(prop = sum(prop)) |> 
  filter(year %in% c(1880,2017)) |> 
  mutate(first_letter = as_factor(first_letter)) |> 
  ggplot(aes(x=prop,fct_reorder(first_letter,prop),color=as_factor(year))) +
  geom_point() +
  theme_minimal(base_size=16) +
  labs(title = "Change in popularity of Name First Letters",
       subtitle = "SSA Babynames data from 1880 to 2017",
       y = "First Letter",
       x= "Proportion")


babynames |> 
  mutate(last_letter = str_sub(name,-1,-1)) |> 
  group_by(year,last_letter) |> 
  summarise(prop = sum(prop)) |> 
  filter(year %in% c(1880,2017)) |> 
  mutate(first_letter = as_factor(last_letter)) |> 
  ggplot(aes(x=prop,fct_reorder(last_letter,prop),color=as_factor(year))) +
  geom_point() +
  theme_minimal(base_size=16) +
  labs(title = "Change in popularity of Name Last Letters",
       subtitle = "SSA Babynames data from 1880 to 2017",
       y = "Last Letter",
       x= "Proportion")

# Problem 2 How has the length of first names changed over time?

babynames = babynames |> 
  group_by(year,sex) |> 
  mutate(prop = n/sum(n)) |> 
  ungroup()
  
  
babynames |> 
  group_by(year) |> 
  summarise(mean_name_length = sum(str_length(name)*0.5*prop)) |> 
  ggplot(aes(x=year,y=mean_name_length)) +
  geom_point()


# How are unusual letters changing in popularity over time and why?

babynames |> 
  group_by(year) |> 
  summarise(prop_x = sum(0.5*prop*str_detect(name,"z|Z"))) |> 
  ggplot(aes(x=year,y=prop_x)) + 
  geom_point()

babynames |> filter(str_detect(name,"x|X")) |> 
  group_by(name) |> summarise(pop = sum(prop)) |> 
  arrange(desc(pop))

babynames |> 
  group_by(year) |> 
  summarise(prop_Alex = sum(0.5*prop*str_detect(name,"Alex"))) |> 
  ggplot(aes(x=year,y=prop_Alex)) +
  geom_point()


babynames |> filter(year == 2017,str_detect(name,"x|X"))  |> 
  arrange(desc(prop))


babynames |> filter(year == 1998,str_detect(name,"x|X"))  |> 
  arrange(desc(prop))


# Megan, Meghan, Megyn

Meg_match = regex("^(Ma|Me)(g|gh)(yn|in|on|an|en|n)$")

babynames |> filter(str_detect(name,Meg_match),sex == "F") |> 
  ggplot(aes(x=year,y=prop,color=name)) +
  geom_line()








babynames |> filter(name == "Meghyn",sex=="F") |> 
  ggplot(aes(x=year,y=prop)) + geom_point()


babynames |> 
  filter(str_detect(name,Meg_match)) |> 
  group_by(year) |> 
  summarise(Megan_prop = 0.5*sum(prop)) |> 
  ggplot(aes(x=year,y=Megan_prop)) +
  geom_point()












  