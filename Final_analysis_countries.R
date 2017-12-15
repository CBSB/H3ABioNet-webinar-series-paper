#############
# The objective of this script to show a map containing the following information:
# 1. Where the presenters are coming from
# 2. Some stats on their composition
###################


# Loading libraries -------------------------------------------------------
source("functions.R")

# Too  artistic! ----------------------------------------------------------
# uofk = geocode('University of Khartoum')
# mymap <- get_map(uofk,maptype = 'watercolor', source="stamen", 
#                  crop=FALSE)
# ggmap(mymap)

##########################



sheet_url <- gs_url("https://docs.google.com/spreadsheets/d/1gdO5fkJVSi5Pp0mC4LEVUfpifgJHMNXu2V4f-qdg8ek/edit#gid=1094396498
",  visibility = "private")

data15 <- gs_read(sheet_url, ws = "2015_seminar_schedule_dates")
data16 <- gs_read(sheet_url, ws = "2016_seminar_schedule_dates")
data17 <- gs_read(sheet_url, ws = "2017_seminar_schedule_dates")

data <- bind_rows(data15, data16, data17)
names(data)

presenter_city_data <- data %>%
  select(Country) %>%
  filter(!is.na(Country)) %>%
  filter(str_detect(Country, "[A-Za-z]")) %>%
  mutate(Country = str_replace(Country, "United Kingdom", "UK")) %>%
  mutate(Country = str_replace(Country, "United States of America", "USA")) %>%
  # mutate(Country = str_replace(Country, "United Arab Emirates", "UAE")) %>%
  mutate(Country = str_trim(Country))  %>%
  arrange(Country)  #%>% filter(!duplicated(Country))

presenter_city <- geocode(presenter_city_data[[1]]) %>%
  mutate(Type = 'Presenter')

presenter_city <- bind_cols(presenter_city_data, presenter_city)


# Audience data -----------------------------------------------------------
# 
# wb <- loadWorkbook("../manuscript stuff/data/H3ABionet Post Webinars' evaluation form (Responses).xlsx")
# getSheets(wb)
# audience_city_data <- readWorksheet(wb, sheet = "Form responses 1")
# 
# audience_city_data <- audience_city_data[-1,] %>%
#   mutate(Country = ifelse (is.na(Country), 
#                          country_from_instit(`Institution.name`), 
#                          Country)) %>%
#   select(Country)
# 
# audience_city <- geocode(audience_city_data[[1]]) %>%
#   mutate(Type = 'Audience') 
# 
# audience_city <- bind_cols(audience_city_data, audience_city)


# Complete data -----------------------------------------------------------

all <- 
  bind_rows(presenter_city) %>%
  group_by(Type, lon, lat, Country) %>%
  summarise(count = n())



# Plotting the map: -------------------------------------------------------

# ### Default approach:
# maps::map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
# points(all$lon,all$lat, col=as.factor(all$Type), pch=16)

### using ggplot:
mp <- NULL
mapWorld <- map_data('world') %>%
  mutate(Countries = 'world') %>%
  filter(!str_detect(region, "Antarctica"))
bordersWorld <- borders('world', colour="gray50") # create a layer of borders

consort <- c('Morocco', 'Tunisia','Egypt', 'Sudan','Mali', 'Niger', 'Nigeria', 'Ghana',
             'Uganda', 'Kenya', 'Tanzania', 'Botswana',
             'South Africa')
mapConsortia <- map_data("world", regions = consort) %>%
  mutate(Countries = 'H3ABioNet Consortium')

pres <- all %>% filter(Type == "Presenter") %>% select(Country)
  
mapPresenters <- map_data("world", regions = c(pres[[4]])) %>%
  mutate(Countries = 'Presenters') 

international <- setdiff(pres[[4]], consort)
mapInter <- map_data("world", regions = international) %>%
  mutate(Countries = "International collaborators") 

ggplot() + 
  geom_polygon(data = mapWorld, aes(x=long, y=lat, group=group), fill = 'seashell1', 
               color = 'gray50') +
  geom_polygon(data = mapConsortia, aes(x=long, y=lat, group=group, fill = Countries),  
               alpha = 1, color = 'gray50') +
  # geom_polygon(data = mapPresenters,
  #            aes(x=long, y=lat, group=group, fill = region), alpha=0.5) +
  geom_polygon(data = mapInter,
               aes(x=long, y=lat, group=group, fill = Countries), color = 'gray50') +
  # bordersWorld +
  # scale_fill_manual(name="Countries", values = c("Presenters" = 'blue', 
  #                                                "H3ABioNet Consortium" = 'red4', 
  #                                                "International collaborators" = 'green1')) +
  geom_point(data = all %>% filter(Type == "Presenter"),
             aes(x=lon, y=lat, size = count)) +
  geom_point(data = all %>% filter(Type == "Presenter"),
             aes(x=lon, y=lat, size = (count + 7)), shape = 1) +
  geom_label_repel(data = all %>% filter(Type == "Presenter"),
             aes(x=lon, y=lat, label = paste(Country))) +
  scale_size("Number of Presenters", breaks = c(5,10), labels = c("1-5", "6-10")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), panel.border = element_blank())  + 
  labs(x=" ", y=" ")


# Adding demographic annotations: -----------------------------------------


status <- data %>%
  select(`Career status` = `Status (graduate student, post-doc, PI)`) %>%
  filter(!is.na(`Career status`)) %>%
  group_by(`Career status`) %>%
  summarise(n = n()) %>%
  mutate(`Percent (%)` = round(n*100/sum(n), 0))
status

sex <- data %>%
  select(`Sex`) %>%
  filter(!is.na(`Sex`)) %>%
  group_by(`Sex`) %>%
  summarise(n = n())  %>%
  mutate(`Percent (%)` = round(n*100/sum(n), 0) )
sex

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = .75)),
  colhead = list(fg_params=list(cex = 0.75)),
  rowhead = list(fg_params=list(cex = 0.75)))

jpeg("Demographics of Webinars presenters.jpeg", width = 850, height = 550, quality = 100)

ggplot() + 
  geom_polygon(data = mapWorld, aes(x=long, y=lat, group=group), fill = 'seashell1', 
               color = 'gray50') +
  geom_polygon(data = mapConsortia, aes(x=long, y=lat, group=group, fill = Countries),  
               alpha = 1, color = 'gray50') +
  # geom_polygon(data = mapPresenters,
  #            aes(x=long, y=lat, group=group, fill = region), alpha=0.5) +
  geom_polygon(data = mapInter,
               aes(x=long, y=lat, group=group, fill = Countries), color = 'gray50') +
  # bordersWorld +
  # scale_fill_manual(name="Countries", values = c("Presenters" = 'blue', 
  #                                                "H3ABioNet Consortium" = 'red4', 
  #                                                "International collaborators" = 'green1')) +
  geom_point(data = all %>% filter(Type == "Presenter"),
             aes(x=lon, y=lat, size = count)) +
  geom_point(data = all %>% filter(Type == "Presenter"),
             aes(x=lon, y=lat, size = (count + 7)), shape = 1) +
  geom_label_repel(data = all %>% filter(Type == "Presenter"),
                   aes(x=lon, y=lat, label = paste(Country))) +
  scale_size("Number of Presenters", breaks = c(5,10), labels = c("1-5", "6-10")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), panel.border = element_blank())  + 
  labs(x=" ", y=" ") +
  annotation_custom(tableGrob(sex, theme = mytheme), xmin=-165, xmax=-150, ymin=-30, ymax=20) +
  annotation_custom(tableGrob(status, theme = mytheme), xmin=-175, xmax=-100, ymin=-60, ymax=0)

dev.off()
