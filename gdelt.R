library(tidyverse)
library(gdeltr2)
library(lubridate)
library(extrafont)
library(ggtext)
library(httr)

library(readr)

loadfonts(device = "win")

gcam <- read_delim("~/GCAM-MASTER-CODEBOOK.TXT", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)

get_urls_gkgtv_most_recent_log()
aug_df <- c(rep(paste0("2020-06-",seq(1:30))),rep(paste0("2020-07-",seq(1:31))),rep(paste0("2020-08-",seq(1:31))))

dt2 <- gkg_tv_days(dates = aug_df)
#colnames(dt2)

dt2_ad <- gkg_tv_days(dates = "2020-08-29")

colnames(dt2_ad)

dt2_ad_minus <- dt2_ad %>%
  select(-isDocumentURL,-urlArchiveVideo)

aug30 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200830.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-30 19:00:00"))
aug31 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200831.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))
sep1 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200901.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))
sep2 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200902.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))
sep3 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200903.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))
sep4 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200904.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))
sep5 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200905.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))
sep6 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200906.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))
sep7 <- readr::read_tsv("http://data.gdeltproject.org/gdeltv2_iatelevision/20200907.gkg.csv.gz", col_names = FALSE) %>%
  mutate(dateTimeDocument = as.POSIXct("2020-08-31 19:00:00"))

head(dt2$dateTimeDocument)
aug_bind <- bind_rows(aug30,aug31,sep1,sep2) %>%
  select(idGKG = `X1`,nameSource = `X4`,locations = `X10`,counts = `X6`,tone = `X16`,gcam=`X18`,dateTimeDocument)
str(dt2$dateTimeDocument)

dttotal <- dt2 %>%
  select(idGKG,dateTimeDocument,nameSource,locations,counts,tone,gcam) %>%
  bind_rows(aug_bind)

dt2a <- dttotal %>%
  filter(str_detect(locations, "Portland") == TRUE)



dt2a_sent <- dt2a %>%
  parse_gkg_mentioned_article_tone()

dt2a_mentioned <- dt2a[1:10,] %>%
  parse_gkg_mentioned_gcams(return_wide = FALSE)

test <- dt2a_mentioned %>%
  left_join(gcam, by = c("idGCAM" = "Variable"))

dt2a_org <- dt2a %>%
  select(idGKG,nameSource, dateTimeDocument,counts) %>%
  left_join(dt2a_sent, by = "idGKG") %>%
  group_by(nameSource) %>%
  filter(nameSource %in% c("FOXNEWSW","CNNW","MSNBCW")) %>%
  mutate(nameSource = case_when(
    nameSource == "FOXNEWSW" ~ "Fox News",
    nameSource == "CNNW" ~ "CNN",
    nameSource == "MSNBCW" ~ "MSNBC",
    nameSource == "ALJAZ" ~ "Al Jazeera"
  )) %>%
  mutate(week = floor_date(dateTimeDocument, "week")) %>%
  group_by(nameSource, week) %>%
  summarise(num = n(), sentiment = mean(scoreTone))

unique(dt2a_org$nameSource)

p1 <- ggplot(dt2a_org, aes( x= week, y = num, fill = nameSource)) +
  geom_bar(stat="identity") +
  ggsci::scale_fill_nejm() +
  theme_minimal() +
  facet_wrap(~nameSource, ncol = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=3)) +
  scale_x_datetime(breaks = scales::breaks_width("1 month"), labels = scales::date_format(format = "%b-%Y")) +
  theme(
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 20),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "TV Media Coverage of Portland, OR",
    y = "Number of Segments in Week",
    x = "Week"
  ) +
  annotate("text", x = as.POSIXct("2020-06-15"), y = -Inf, label = "PROOF ONLY",
           hjust=1.1, vjust=-1.1, col="white", cex=6,
           fontface = "bold", alpha = 0.8)

p1
ggplot(dt2a_org, aes( x= dateTimeDocument, y=num,fill = nameSource)) +
  geom_density(alpha = .5) +
  ggsci::scale_fill_lancet() +
  labs(title = "TV Program Mentions of Portland, OR",
       subtitle = "GDELT GKG")


augnew <- c(rep(paste0("2020-08-",seq(1:31))))

gkg1 <- get_data_gkg_days_summary(augnew)

gkg2 <- gkg1 %>%
  #filter(str_detect(locations, "Portland, Oregon") == TRUE) %>%
 # filter(str_detect(themes, "PROTEST") == TRUE) %>%
  filter(str_detect(persons, "ted wheeler") == TRUE) %>%
  
  mutate(week = floor_date(dateEvent, "week")) %>%
  group_by(sources) %>%
  filter(n() > 10)


gkg2setn <- gkg2 %>%
  parse_gkg_mentioned_article_tone()



gkg2_join <- gkg2 %>%
  left_join(gkg2setn, by = "idGKG")# %>%
  group_by(sources) %>%
  summarise(num = n(), tone = median(scoreTone), meantone = mean(scoreTone))
  

unique(gkg2$sources)
ggplot(gkg2, aes(x=week, y = num, color = sources)) +
  geom_path(size = 1.5) +
  ggsci::scale_color_lancet()


ggplot(gkg2_join, aes( y = reorder(sources,scoreTone3,median), x = scoreTone3, color = scoreTone)) +
  geom_boxplot()



test <- ft_v2_api(terms = c("violence","riot"))

###  Testing stuff ####

# dt3 <- dt2 %>%
#   filter(str_detect(themes, c("PROTEST","RIOT"))== TRUE) %>%
#   parse_gkg_mentioned_article_tone()
# 
# dt_theme <- dt2 %>%
#   filter(str_detect(themes, c("PROTEST","RIOT"))== TRUE) %>%
#   parse_gkg_mentioned_event_counts()
# 
# unique(dt_theme$codeGKGTheme)
# 
# 
# dt4 <- dt2 %>%
#   filter(str_detect(themes, c("PROTEST","RIOT"))== TRUE) %>%
#   parse_gkg_mentioned_organizations()
# 
# dt_theme1 <- dt_theme %>%
#   filter(idCountry == "US" & codeGKGTheme == "PROTEST")
# head(dt2$themes)
# 
# df_gkg <- get_codes_gkg_themes()
