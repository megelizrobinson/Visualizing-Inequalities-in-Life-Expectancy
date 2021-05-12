# Sets plot images to nice size
options(repr.plot.width = 6, repr.plot.height = 6)

# Loading packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Loading data 
(life_expectancy <- read_csv("UNdata.csv"))

life_expectancy
#Rename `Country or Area` column
(life_expectancy <- life_expectancy %>% 
  rename(Country.or.Area = `Country or Area`))

#Subsetting and reshaping the life expectancy data 
(subdata <- life_expectancy %>% 
  filter(Year == "2000-2005") %>% 
  select(Country.or.Area, Subgroup, Value) %>% 
  spread(Subgroup, Value))

# Plot male and female life expectancy
ggplot(subdata, aes(Male, Female)) +
  geom_point()

# Adding an abline and changing the scale of axes of the previous plots
ggplot(subdata, aes(Male, Female)) +
  geom_point() +
  scale_x_continuous(limits = c(35,85)) +
  scale_y_continuous(limits = c(35,85)) +
  geom_abline(intercept = 0, slope = 1)

# Adding labels to previous plot
ggplot(subdata, aes(Male, Female)) +
  geom_point(color = "white", fill= "chartreuse3", shape = 21, alpha = .55,
             size = 5)+
  geom_abline(intercept = 0, slope = 1, linetype =2) +
  scale_x_continuous(limits = c(35,85)) +
  scale_y_continuous(limits = c(35,85)) +
  labs (title = "Life Expectancy at Birth by Country",
        subtitle = "Years. Period: 2000-2005. Average.",
        caption = "Source: United Nations Statistics Division",
        x = "Males",
        y = "Females")

# Draw attention to countries where the gap in life expectancy between men
# and women is significantly high

# Subsetting data to obtain countries of interest
(top_male <- subdata %>% 
  arrange(Male-Female) %>% 
  head (3))

(top_female <- subdata %>% 
  arrange(Female-Male) %>% 
  head(3))

# Adding text to the previous plot to label countries of interest 
ggplot(subdata, aes(Male, Female, label = Country.or.Area)) +
  geom_point(color = "white", fill= "chartreuse3", shape= 21, alpha = .55,
             size = 5)+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_x_continuous(limits = c(35,85)) +
  scale_y_continuous(limits = c(35,85)) +
  labs (title = "Life Expectancy at Birth by Country",
        subtitle = "Years. Period: 2000-2005. Average.",
        caption = "Source: United Nations Statistics Division",
        x = "Males",
        y = "Females") +
  geom_text(data = top_male, size = 3) +
  geom_text(data = top_female, size = 3) +
  theme_bw()

# How has life expectancy by gender evolved?
# Plot difference between men and women across countries between 
# 2000-2005 & 1985-1990

# Subseting, mutating, and reshaping life expectancy data
(subdata2 <- life_expectancy %>%
  filter(Year %in% c("1985-1990", "2000-2005")) %>% 
  mutate(Sub_Year = paste(Subgroup, Year, sep = "_")) %>% 
  mutate(Sub_Year= gsub("-","_", Sub_Year)) %>%
  select (-Subgroup, -Year) %>%
  spread(Sub_Year, Value) %>%
  mutate (diff_Female = Female_2000_2005 - Female_1985_1990, diff_Male =
             Male_2000_2005 - Male_1985_1990)
)

# Visualize average life expectancy differences between 1985-1990
# and 2000-2005 for men and women
ggplot(subdata2, aes(x = diff_Male, y = diff_Female, label = Country.or.Area)) +
  geom_point(color = "white", fill = "chartreuse3", shape = 21, alpha = .55,
             size = 5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25,25)) +
  labs (title = "Life Expectancy at Birth by Country in Years",
        subtitle = "Difference between 1895-1990 and 2000-2005. Average.",
        caption = "Source: United Nations Statistics Division",
        x = "Males",
        y = "Females") +
  theme_bw()


# Add reference lines to see differences between men and women more clearly
# Add an hline and vline to identify which countries people increased or 
# decreased their life expectancy
ggplot(subdata2, aes(x = diff_Male, y = diff_Female, label = Country.or.Area)) +
  geom_point(color = "white", fill = "chartreuse3", shape = 21, alpha = .55, 
             size = 5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_x_continuous(limits = c(-25,25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(title = "Life Expectancy at Birth by Country",
       subtitle = "Years. Difference between 1985-1990 and 2000-2005. Average.",
       caption = "Source: United Nations Statistics Division",
       x = "diff_Male",
       y = "diff_Female") +
  theme_bw()

# Highlight remarkable countries
# top 3 (countries where average life expectancy for men and women increased most)
# bottom 3 (where average life expectancy for men and women decreased most)

# Subseting data to obtain countries of interest
top <- subdata2 %>% 
  arrange(diff_Male + diff_Female) %>% 
  head (3)

bottom <- subdata2 %>% 
  arrange(-(diff_Male + diff_Female)) %>% 
  head(3)

# Adding text to previous plot to label countries of interest
ggplot(subdata2, aes(x = diff_Male, y = diff_Female, label = Country.or.Area),
       guide = FALSE) +
  geom_point(color = "white", fill = "chartreuse3", shape = 21, alpha = .55,
             size = 5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_x_continuous(limits = c(-25,25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(title = "Life Expectancy at Birth by Country",
       subtitle = "Years. Difference between 1985-1990 and 2000-2005. Average.",
       caption = "Source: United Nations Statistics Division",
       x = "Males",
       y = "Females")+
  geom_text(data = top, size = 3)+
  geom_text(data = bottom, size = 3) +
  theme_bw()
