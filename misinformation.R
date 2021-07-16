library(tidyverse)
library(httr)
library(extrafont)


df <- read_csv("https://api.gdeltproject.org/api/v2/tv/tv?format=csv&timespan=FULL&last24=yes&datanorm=raw&dateres=WEEK&query=misinformation%20(station:CNN%20OR%20station:FOXNEWS%20OR%20station:MSNBC%20)%20&mode=timelinevol&timezoom=yes")

colnames(df)<- c("week","station","value")

df1 <- df %>%
  #pivot_longer(cols = -week, names_to = "station", values_to = "seconds") %>%
  group_by(station,month = lubridate::floor_date(week,"month")) %>%
  summarise(value = sum(value))
  #mutate(month = as.Date(paste("1",month,sep = "/"), format = "%d/%m/%Y"))


ggplot(df, aes(x = week, y = value,color = station)) +
  #geom_bar(stat= "identity",position = "stack") + 
  #geom_point() +
  geom_path(size = 1.5, alpha = .75) +
  geom_point(data = filter(df, week == max(week)),size = 3) +
  
  scale_x_date(limits = c(as.Date("2019-06-01"),Sys.Date()), labels = scales::date_format("%b %y")) +
  ggsci::scale_color_lancet() +
  theme_minimal() +
  theme(
    text = element_text(size = 16, family = "Bahnschrift"),
    plot.caption = element_text(size = 8, color = "grey50")
  ) +
  labs(
    x = "Weekly Sum",
    y = "'Mentions'",
    fill = "Station",
    color = "Station",
    title = "Cable News Discussion of 'Misinformation'",
    caption = "Data Source: GDELT TV API"
  )

ggsave("misinformation.png", dpi = 640, width = 10, height = 5)
