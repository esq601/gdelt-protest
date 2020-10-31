library(tidyverse)
library(gdeltr2)
library(lubridate)
library(extrafont)
library(ggtext)
library(httr)
library(stringr)
library(readr)
library(jsonlite)

loadfonts(device = "win")

testvisual <- read_json("http://data.gdeltproject.org/blog/2019-ai-watches-a-week-of-television-news/image/CNNW_20190415_220000_Situation_Room_With_Wolf_Blitzer.tar.gz")


day1 <- as.POSIXct("2020-01-01 01:00:00")
day2 <- day1 + days(7)
str_remove_all(str_remove_all(day2,"[:punct:]"),"[:space:]")

daystart <- as.POSIXct("2020-09-05 01:00:00")

protest_hour <- data.frame()

for(i in 1:52) {
  print(i)
  if(i == 1) {
    enddate <- daystart
  }
  
  startdate <- enddate - days(7)
  
  
  enddate1 <- str_remove_all(str_remove_all(enddate,"[:punct:]"),"[:space:]")
  
  startdate1 <- str_remove_all(str_remove_all(startdate,"[:punct:]"),"[:space:]")
  
  
  protest_temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=",startdate1,"&enddatetime=",enddate1,
                                 "&query=%20%20%20asr:%22peaceful%20protest%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour"))
  colnames(protest_temp) <- c("date","occurances")
  
  protest_hour <- bind_rows(protest_hour,protest_temp)
  
  enddate <- daystart - days(i*7)
  
}


corona_hour <- data.frame()

for(i in 1:52) {
  print(i)
  if(i == 1) {
    enddate <- daystart
  }
  
  startdate <- enddate - days(7)
  
  
  enddate1 <- str_remove_all(str_remove_all(enddate,"[:punct:]"),"[:space:]")
  
  startdate1 <- str_remove_all(str_remove_all(startdate,"[:punct:]"),"[:space:]")
  
  
  corona_temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=",startdate1,"&enddatetime=",enddate1,
                                  "&query=%20%20%20ocr:%22coronavirus%20pandemic%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour"))
  colnames(corona_temp) <- c("date","occurances")
  
  corona_hour <- bind_rows(corona_hour,corona_temp)
  
  enddate <- daystart - days(i*7)
  
}

protest_hour1 <- protest_hour %>%
  mutate(query = "protest") %>%
  filter(occurances > 2)

corona_hour1 <- corona_hour %>%
  mutate(query = "coronavirus pandemic") %>%
  mutate(color = case_when(
    date %in% protest_hour1$date == TRUE ~ "blue",
    T ~ "red"
  )) %>%
  mutate(occ_min = occurances/60)


#combined_hour <- bind_rows(protest_hour1,corona_hour1)



ggplot(corona_hour1, aes(x = date, y = occ_min, color = color)) +
  geom_point(data = subset(corona_hour1,color =="red"),size = 1,alpha = .25) +
  geom_point(data = subset(corona_hour1,color =="blue"),size = 2.75) +
  scale_color_brewer(type = "qual", palette = 6) +
  theme_minimal() +
  scale_x_datetime(breaks = scales::date_breaks(width = "1 month"), labels = scales::date_format(format= "%b")) +
  theme(
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 20),
    plot.title = element_text(size = 22,hjust = 0),
    plot.caption = element_text(color = "grey50",size = 8),
  )  +
  labs(
    title = "CNN Coronavirus Death Tracker On-Screen Time",
    subtitle = "Hours with 'peaceful protests' discussion in red",
    y = "Minutes of each hour tracker on screen",
    caption = "Source: GDELT TV 2.0 AI API"
  ) 


#protest_occ <- read_csv("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=20200101000000&enddatetime=20200108000000&query=%20%20%20ocr:%22protest%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour")
#corona_occ <- read_csv("https://api.gdeltproject.org/api/v2/tvai/tvai?timespan=1y&query=%20%20%20ocr:%22coronavirus%20pandemic%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=minute")




trump_hour <- data.frame()

for(i in 1:52) {
  print(i)
  if(i == 1) {
    enddate <- daystart
  }
  
  startdate <- enddate - days(7)
  
  
  enddate1 <- str_remove_all(str_remove_all(enddate,"[:punct:]"),"[:space:]")
  
  startdate1 <- str_remove_all(str_remove_all(startdate,"[:punct:]"),"[:space:]")
  
  
  temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=",startdate1,"&enddatetime=",enddate1,
                                 "&query=%20%20%20asr:%22trump%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour"))
  colnames(temp) <- c("date","occurances")
  
  trump_hour <- bind_rows(trump_hour,temp)
  
  enddate <- daystart - days(i*7)
  
}

cuomo_hour <- data.frame()

for(i in 1:52) {
  print(i)
  if(i == 1) {
    enddate <- daystart
  }
  
  startdate <- enddate - days(7)
  
  
  enddate1 <- str_remove_all(str_remove_all(enddate,"[:punct:]"),"[:space:]")
  
  startdate1 <- str_remove_all(str_remove_all(startdate,"[:punct:]"),"[:space:]")
  
  
  temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=",startdate1,"&enddatetime=",enddate1,
                          "&query=%20%20%20asr:%22cuomo%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour"))
  colnames(temp) <- c("date","occurances")
  
  cuomo_hour <- bind_rows(cuomo_hour,temp)
  
  enddate <- daystart - days(i*7)
  
}


trump_rona_hour <- data.frame()

for(i in 1:52) {
  print(i)
  if(i == 1) {
    enddate <- daystart
  }
  
  startdate <- enddate - days(7)
  
  
  enddate1 <- str_remove_all(str_remove_all(enddate,"[:punct:]"),"[:space:]")
  
  startdate1 <- str_remove_all(str_remove_all(startdate,"[:punct:]"),"[:space:]")
  
  
  temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=",startdate1,"&enddatetime=",enddate1,
                          "&query=%20%20%20asr:%22trump%22%20ocr:%22coronavirus%20pandemic%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour"))
  colnames(temp) <- c("date","occurances")
  
  trump_rona_hour <- bind_rows(trump_rona_hour,temp)
  
  enddate <- daystart - days(i*7)
  
}

cuomo_rona_hour <- data.frame()

for(i in 1:52) {
  print(i)
  if(i == 1) {
    enddate <- daystart
  }
  
  startdate <- enddate - days(7)
  
  
  enddate1 <- str_remove_all(str_remove_all(enddate,"[:punct:]"),"[:space:]")
  
  startdate1 <- str_remove_all(str_remove_all(startdate,"[:punct:]"),"[:space:]")
  
  
  temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tvai/tvai?startdatetime=",startdate1,"&enddatetime=",enddate1,
                          "&query=%20%20%20asr:%22cuomo%22%20ocr:%22coronavirus%20pandemic%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=hour"))
  colnames(temp) <- c("date","occurances")
  
  cuomo_rona_hour <- bind_rows(cuomo_rona_hour,temp)
  
  enddate <- daystart - days(i*7)
  
}

cuomo_rona_hour <- cuomo_rona_hour %>%
  mutate(query = "cuomo rona")

trump_rona_hour <- trump_rona_hour %>%
  mutate(query = "trump rona")

cuomo_hour <- cuomo_hour %>%
  mutate(query = "cuomo")

trump_hour <- trump_hour %>%
  mutate(query = "trump")


trump_hour1 <- trump_hour %>%
  left_join(corona_hour, by = "date")

cuomo_hour1 <- cuomo_hour %>%
  left_join(corona_hour, by = "date")

rona_df <- full_join(cuomo_rona_hour,trump_rona_hour, by = "date")
nonrona_df <- bind_rows(cuomo_hour,trump_hour)


totaldf <- nonrona_df %>%
  left_join()

blm_occ <- data.frame()
for(i in c("FOXNEWS","CNN","MSNBC")){
  print(i)
  temp <- read_csv(paste0("https://api.gdeltproject.org/api/v2/tv/tv?timespan=FULL&query=%20%20%20trump%20%20station:",i,"%20&mode=TimelineVol&format=csv&dateres=month"))
  colnames(temp) <- c("date","station", "occ")
  
  blm_occ  <- bind_rows(blm_occ,temp) 
}

blm_occ1 <- blm_occ %>%
  mutate(date = as.Date(paste0("1/",date), format = "%d/%m/%Y"))
#blm_occ <- read_csv("https://api.gdeltproject.org/api/v2/tv/tv?timespan=FULL&query=%20%20%20%black%20lives%20matter%22%20%20station:CNN%20&mode=TimelineVol&format=csv&datanorm=raw&dateres=daily")

#blm_occ <- read_csv("https://api.gdeltproject.org/api/v2/tvai/tvai?timespan=FULL&query=%20%20%20ocr:%22blm%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=daily")

#colnames(blm_occ) <- c("date","station", "occ")
# blm_occ_wk <- blm_occ %>%
#   #mutate(week = floor_date(date,"week")) %>%
#   group_by(station,date) %>%
#   summarise(occ = sum(occ))

ggplot(subset(blm_occ1,date>as.Date("2015-01-01")), aes(x = date, y = occ,color = station)) + 
  #geom_path(size = 1) +
  # geom_point(data = subset(blm_occ_wk,occ>0&week > as.Date("2015-01-01")),color = "olivedrab",size = 1.25,shape = 16) +
  # geom_segment(aes(xend = week, yend =0),color = "olivedrab") +
  geom_path() +
  #geom_bar(stat = "identity",fill = "olivedrab") +
  # geom_vline(aes(xintercept = as.Date("2016-11-08")),color = "darkred",linetype = "dashed") +
  # geom_vline(aes(xintercept = as.Date("2020-11-03")),color = "darkred",linetype = "dashed") +
  # geom_text(aes(x = as.Date("2016-11-15"),y=5, label = "2016 Election"),color = "darkred",angle = 90, vjust = 1,size = 6,family = "Bahnschrift") +
  # geom_text(aes(x = as.Date("2020-11-10"),y=5, label = "2020 Election"),color = "darkred",angle = 90, vjust = 1,size = 6,family = "Bahnschrift") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 20),
    plot.title = element_text(size = 22,hjust = 0),
    plot.caption = element_text(color = "grey50",size = 8),
  ) +
  labs(
    title = "CNN On-Screen Time for 'Black Lives Matter'",
    y = "Minutes of On-Sceen Time in Week",
    caption = "Source: GDELT TV 1.0 API"
  ) +
  facet_wrap(~station)


climate_occ <- read_csv("https://api.gdeltproject.org/api/v2/tv/tv?timespan=FULL&query=%20%20%20%climate%20change%22%20%20station:CNN%20&mode=TimelineVol&format=csv&dateres=daily")

colnames(climate_occ) <- c("date","station", "occ")
climate_occ_wk <- climate_occ %>%
  mutate(week = floor_date(date,"week")) %>%
  group_by(week) %>%
  summarise(occ = sum(occ))

ggplot(subset(climate_occ_wk,week>as.Date("2014-01-01")), aes(x = week, y = occ)) + 
  #geom_path(size = 1) +
  geom_point(data = subset(climate_occ_wk,occ>0&week > as.Date("2014-01-01")),color = "darkblue",size = 1.25,shape = 16) +
  geom_segment(aes(xend = week, yend =0),color = "darkblue") +
  #geom_bar(stat = "identity",fill = "olivedrab") +
  geom_vline(aes(xintercept = as.Date("2016-11-08")),color = "darkred",linetype = "dashed") +
  geom_vline(aes(xintercept = as.Date("2020-11-03")),color = "darkred",linetype = "dashed") +
  # geom_text(aes(x = as.Date("2016-11-15"),y=50, label = "2016 Election"),color = "darkred",angle = 90, vjust = 1,size = 6,family = "Bahnschrift") +
  # geom_text(aes(x = as.Date("2020-11-10"),y=50, label = "2020 Election"),color = "darkred",angle = 90, vjust = 1,size = 6,family = "Bahnschrift") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 20),
    plot.title = element_text(size = 22,hjust = 0),
    plot.caption = element_text(color = "grey50",size = 8),
  ) +
  labs(
    title = "CNN On-Screen Time for 'Black Lives Matter'",
    y = "Minutes of On-Sceen Time in Week",
    caption = "Source: GDELT TV 2.0 AI API"
  )
