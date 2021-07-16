library(tidyverse)
library(gdeltr2)
library(lubridate)
library(extrafont)
library(ggtext)
library(httr)
library(stringr)
library(readr)
library(jsonlite)
library(png)
library(grid)


cases<- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")))

state_date <- cases %>%
  select(UID,State=Province_State, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(date) %>%
  summarise(cases = sum(cases, na.rm = T)) %>%
  #filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  mutate(day = row_number(),type = "Cases")

loadfonts(device = "win")


corona_hour <- data.frame()

daystart <- Sys.time()
#i <- 1
for(i in 1:90) {
  print(i)
  if(i == 1) {
    enddate <- daystart
  }
  
  startdate <- enddate - days(7)
  
  
  enddate1 <- str_remove_all(str_remove_all(enddate,"[:punct:]"),"[:space:]")
  
  startdate1 <- str_remove_all(str_remove_all(startdate,"[:punct:]"),"[:space:]")
  
  
  corona_temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=",startdate1,"&enddatetime=",enddate1,
                                 "&query=%20%20%20ocr:%22coronavirus%20pandemic%22%20ocr:%22in%20the%20%united%20states%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour&last24=yes"))
  
  if("X1" %in% colnames(corona_temp) == TRUE) {
    print("if")
    corona_temp <- data.frame(date = startdate, occurances = 0)
  }
  
  colnames(corona_temp) <- c("date","occurances")
  
  
  tryCatch(
    corona_hour <- bind_rows(corona_hour,corona_temp),
    error = function(e) e
    
  )
  
  enddate <- daystart - days(i*7)
  
}

corona_week1 <- corona_hour %>%
  # mutate(day = as.Date(floor_date(date,"day"))) %>%
  # group_by(day) %>%
  # summarise(occurances = sum(occurances)) %>%
  mutate(week = as.Date(floor_date(date,"month"))) %>%
  group_by(week) %>%
  summarise(hours = sum(occurances, na.rm = TRUE)/(60*60))

corona_week1 <- corona_hour %>%
  # mutate(day = as.Date(floor_date(date,"day"))) %>%
  # group_by(day) %>%
  # summarise(occurances = sum(occurances)) %>%
  mutate(week = as.Date(floor_date(date,"month"))) %>%
  group_by(week) %>%
  summarise(hours = sum(occurances, na.rm = TRUE)/(60*60)) %>%
  mutate(hours = zoo::rollmean(hours, 1, fill = 0, align = "right"))


img <- readPNG("pandemic_img_crop.png")
g <- rasterGrob(img, interpolate=TRUE)

cases_overlay <- state_date %>%
  select(date, new_cases) %>%
  ungroup() %>%
  mutate(new_ra = zoo::rollmean(new_cases, 7, fill = 0,align = "right")) %>%
  mutate(new_scaled = new_ra * (max(corona_week1$hours) / max(new_ra)))


ggplot(corona_week1, aes(x = week, ymax = hours, y = hours)) +
  annotation_custom(g, xmin=as.Date("2021-03-01"), xmax=as.Date("2021-05-01"), ymin=max(corona_week1$hours)*.7, ymax=max(corona_week1$hours)) +
  geom_bar(stat = "identity",fill = "#591e7c",alpha = 0.85) +
  
  geom_line(inherit.aes = FALSE,data = cases_overlay, aes(x = date, y= new_scaled,ymin = 0), alpha = .85, fill = "red", color = "red", size = 2) +
  
  #geom_point(color = "#591e7c") +
  #geom_segment(aes(yend = 0, xend = week),color = "#591e7c") +
  
  #geom_line(aes(ymin = 0),alpha = .85,fill = "#591e7c",color = "#591e7c",size = 2) +
  # geom_vline(aes(xintercept = as.Date("2020-11-03")),linetype = "dashed") +
  # annotate("text", x = as.Date("2020-11-03"), y = max(corona_week1$hours), label = "Election Day",hjust = 1,vjust = 1,family = "Bahnschrift",size = 6,angle = 90) +
  geom_vline(aes(xintercept = as.Date("2021-01-20")),linetype = "dashed") +
  annotate("text", x = as.Date("2021-01-20"), y = max(corona_week1$hours), label = "Inauguration Day",hjust = 1,vjust = 1,family = "Bahnschrift",size = 6,angle = 90) +
  # geom_vline(aes(xintercept = as.Date("2020-05-28")),linetype = "dashed") +
  # annotate("text", x = as.Date("2020-05-28"), y = max(corona_week1$hours), label = "George Floyd Protests",hjust = 1,vjust = 1,family = "Bahnschrift",size = 6,angle = 90) +
  # geom_vline(aes(xintercept = as.Date("2021-05-13")),linetype = "dashed") +
  # annotate("text", x = as.Date("2021-05-13"), y = max(corona_week1$hours), label = "CDC No More Masks",hjust = 1,vjust = 1,family = "Bahnschrift",size = 6,angle = 90) +
  scale_x_date(limits = c(as.Date("2020-02-01"), Sys.Date()+lubridate::days(30)),
               breaks = scales::date_breaks(width = "1 month"), labels = scales::date_format(format = "%b-%y")) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . * (max(cases_overlay$new_ra) / max(corona_week1$hours)),
                                         labels = scales::comma_format(), breaks = scales::pretty_breaks(),
                                         name = "New Cases (7 day RA)")) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    legend.spacing.x = unit(.95, 'cm'),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 22,hjust = 0),
    plot.caption = element_text(color = "grey50",size = 8),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "#591e7c"),
    axis.text.y.left = element_text(color = "#591e7c"),
    axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1)
  ) +
  labs(
    title = "CNN 'Coronavirus Pandemic' Graphic",
    subtitle = "The graphic was omnipresent through 2020, but disappeared as cases peaked.",
    y = "Hours 'Coronavirus Pandemic' On Screen per month",
    caption = "GDELT TV AI API on screen text recognition"
  )

ggsave("gdeltplot1.png",width = 12, height = 8, dpi = 800)

#tes






corona_day <- corona_hour %>%
  # mutate(day = as.Date(floor_date(date,"day"))) %>%
  # group_by(day) %>%
  # summarise(occurances = sum(occurances)) %>%
  mutate(day  = as.Date(floor_date(date,"day"))) %>%
  group_by(day) %>%
  filter(day >= as.Date("2021-03-01")) %>%
  summarise(hours = sum(occurances)/(60*60))


ggplot(corona_day, aes(x = day, y = hours)) +
  annotation_custom(g, xmin=as.Date("2021-04-01"), xmax=as.Date("2021-04-26"),
                    ymin=max(corona_day$hours)*.6, ymax=max(corona_day$hours)) +
  geom_segment(aes(yend = 0, xend = day),color = "#591e7c",size = 0.75) +
  geom_point(color = "#591e7c",size = 2) +
  geom_vline(aes(xintercept = as.Date("2021-05-13")),linetype = "dashed") +
  geom_vline(aes(xintercept = Sys.Date()),linetype = "solid") +
  
  annotate("text", x = as.Date("2021-05-13"), y =1.5, label = "CDC No More Masks",vjust = 1,family = "Bahnschrift",size = 6,angle = 90) +
  annotate("text", x = Sys.Date(), y =1.5, label = "Today",vjust = 1,family = "Bahnschrift",size = 6,angle = 90) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    legend.spacing.x = unit(.95, 'cm'),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 22,hjust = 0),
    plot.caption = element_text(color = "grey50",size = 8),
  ) +
  labs(
    title = "CNN 'Coronavirus Pandemic' Graphic",
    subtitle = "The graphic has disappeared following updated CDC mask guidance.",
    y = "Hours 'Coronavirus Pandemic' On Screen per week",
    caption = "Data Source: GDELT TV AI API for on screen text recognition"
  )


ggsave("gdeltplot2.png",width = 12, height = 8, dpi = 800)

