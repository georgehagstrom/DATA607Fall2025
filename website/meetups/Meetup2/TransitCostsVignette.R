library(tidyverse)
library(ggrepel)


transit_costs = read_csv("/home/georgehagstrom/work/Teaching/DATA607Fall2025/website/meetups/Meetup2/transit_cost.csv")


glimpse(transit_costs)

# making a graph with ggplot

ggplot(data = transit_costs, 
      mapping = aes(x=cost_km_millions))

ggplot(transit_costs,aes(x=length,y=cost_km_millions))

ggplot(data = transit_costs,
       mapping = aes(x=length,y=cost_km_millions))

ggplot(data = transit_costs,
       mapping = aes(x=length,y=cost_km_millions,color=country,))

# Can use the pipe to send data, can leave the mapping = blank:

  
transit_costs |>   ggplot(aes(x=cost_km_millions))


# Making our first visualization, adding a geom

transit_costs |> ggplot(aes(x=cost_km_millions)) +
  geom_histogram(binwidth = 500)


transit_costs |> ggplot(aes(x=cost_km_millions)) +
  geom_freqpoly(binwidth=50)


transit_costs |> ggplot(aes(x=cost_km_millions)) +
  geom_density(binwidth=50)

# Which are the cities with the highest transit costs?

transit_costs |> summarise(mean_cpm = mean(cost_km_millions,na.rm=TRUE))


transit_costs |> 
  group_by(city) |> 
  summarise(mean_cpm = mean(cost_km_millions)) |> 
  arrange(desc(mean_cpm)) |> 
  mutate(order = rank(-mean_cpm))



transit_costs |>
  group_by(city) |> 
  summarise(mean_cpm = mean(cost_km_millions)) |> 
  arrange(desc(mean_cpm)) |> 
  mutate(order = rank(-mean_cpm))

cities_label = c("New York","London","Barcelona","Milan","Paris","Mexico City","Beijing","Singapore","Istanbul")

transit_costs |>  
  group_by(city) |> 
  summarise(mean_cpm = mean(cost_km_millions)) |>  
  arrange(desc(mean_cpm)) |> 
  mutate(order = rank(-mean_cpm)) |> 
  mutate(is_nyc = (city == "New York")) |> 
  filter(mean_cpm > 100) |> 
  ggplot(aes(x=order,y=mean_cpm,color=is_nyc,label = city)) +
  geom_point(show.legend = FALSE) +
  geom_text_repel( data = .%>%   filter(city %in% cities_label),
                   aes(x=order,y=mean_cpm,label = city),
                   color="black",
                   size=6,
                   show.legend = FALSE) +
  theme_bw(base_size = 18) +
  labs(
    x = "Rank",
    y = "Mean Cost in Millions per KM",
    title = "Cities Ranked by Cost of Transit"  ) 
  
  
transit_costs |> ggplot(aes(x=cost_km_millions)) +
  geom_density()


# Comparing between countries

transit_costs |> 
  ggplot(aes(x=cost_km_millions)) +
  geom_density() +
  facet_wrap(~ country)

# Making it look better?

top_countries = transit_costs |> 
  count(country) |> 
  arrange(desc(n)) |> 
  head(9) |> 
  pull(country)

top_countries

transit_costs |> 
  filter(country %in% top_countries) |> 
  ggplot(aes(x=cost_km_millions)) +
  geom_density() +
  facet_wrap(~country,scales = "free_y") +
  theme_minimal() +
  scale_x_log10()


transit_costs |> 
  filter(country %in% top_countries) |> 
  ggplot(aes(x=cost_km_millions)) +
  geom_density() +
 scale_x_log10() +
  facet_wrap(~ country,scales="free_y") +
  theme_minimal()


## Ridge plots
library(ggridges)

transit_costs |> filter(country %in% top_countries) |> 
  ggplot(aes(x=cost_km_millions,y=country)) + 
  geom_density_ridges(alpha = 0.4) +
  theme_minimal() +
  scale_x_log10() +
  labs(title = "Transit Project Costs in the US are Exorbitant",
       subtitle = "Distribution of project costs in selected countries\nmeasured in millions of dollars per km.",
       x = "Millions of USD per KM (log scale)",
       y = "Country")


transit_costs |> 
  filter(country %in% top_countries) |> 
  ggplot(aes(x=cost_km_millions,y=country,)) +
  geom_density_ridges(alpha = 0.4) + 
  theme_minimal() +
  scale_x_log10() +
  labs(title = "Costs per KM of transit projects by country",
       subtitle = "Projects in the United States cost an order of magnitude more than other countries.\n Costs in Spains are surprisingly low.",
       y = "Country",
       x= "Millions of USD per KM (log scale)")

# Look at Year or project size

transit_costs |> 
  ggplot(aes(x=as.numeric(start_year),y=cost_km_millions)) +
  geom_point()
  


# Some random stuff

transit_costs |> 
  ggplot(aes(x=length,y=cost_km_millions)) +
  geom_point()

transit_costs |> 
  filter(length<300) |> 
  ggplot(aes(x=length,y=cost_km_millions)) +
  geom_point() +
  theme_bw()

transit_costs |> mutate(years_per_km = (as.numeric(end_year) - as.numeric(start_year))/length) |> 
  ggplot(aes(x=years_per_km,cost_km_millions)) +
  geom_point()  +
  theme_bw()

transit_costs |> arrange(desc(cost_km_millions)) |> 
  select(city,line)













