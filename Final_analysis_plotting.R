# library definitions -----------------------------------------------------

source("H3ABioNet-webinar-series-paper/functions.R")

sheet_url <- gs_url("https://docs.google.com/spreadsheets/d/1nJpjrBROph-yOaPDJBk0jwWpf6HpSDt64iYmaZEnwU4/edit?usp=sharing",
                    visibility = "private")
rawdata <- gs_read(sheet_url) 

# rawdata1 = read_excel('../manuscript stuff/data/H3ABionet Post Webinars\' evaluation form (Responses).xlsx')


# Data cleaning -----------------------------------------------------------
data <- rawdata %>%
    filter( `Full name` != "ghjg") %>%
    unite(`Email address`,`Email address_1`, `Email address2`, 
          `Email address`, remove = TRUE) %>%
    mutate(`Email address` = gsub('NA_NA', '',`Email address`)) %>%
    mutate(`Email address` = gsub('_', '',`Email address`)) %>%
    mutate(Country = ifelse (is.na(Country), 
                             country_from_instit(`Institution name`), 
                             Country)) %>%
    select(-`Timestamp`, -`Full name`, -`Email address`, -`Institution name`, 
           -`Current title`)
  
media <-  data %>%
  mutate(`How did you learn about this event?` =
           str_replace(`How did you learn about this event?`,
                       ".*Facebook/ twitter.*$", "Facebook/ Twitter" )) %>%
  mutate(`How did you learn about this event?` =
           str_replace(`How did you learn about this event?`,
                       "^.*mailing list.*$|^.*organize.*$|^.*cooridnating.*$|^.*advisor.*$|^.*WG*$", 
                       "H3ABioNet mailing list" ))
  
academics <-  data %>%
    mutate(`Academic status` = str_replace(`Academic status`, '.*PhD|P.HD|Ph.D.|PI|Post-doc|Habilitation.*$', 
                                           'Phd (or equivalent)')) %>%
    mutate(`Academic status` = str_replace(`Academic status`, '.*M.Sc |MSc|Master|Msc.*$', 
                                           'MSc (or equivalent)')) %>%
    mutate(`Academic status` = str_replace(`Academic status`, '.*BIT|BCs.*$', 
                                           'BSc (or equivalent)')) %>%
    mutate(`Academic status` = str_replace(`Academic status`, 'Bioinformatics|Graduated|Researcher', 
                                           'Unknown degree')) %>%
    mutate(`Academic status` = str_replace(`Academic status`, '\\).*$', 
                                           '\\)')) %>%
    mutate(`Academic status` = str_replace_na(`Academic status`, 'Unknown degree'))  %>%
    select(`Academic status`)
  
  
# Extracting data ------------------------------------------------------

names(data)
logistics <-  theme_extract(data, "Logistics") %>% 
  set_names("A. Webinar timing", "B. Webinar platform, Mconf", "Webinar duration") %>% map_df(releveling)

presentation <- poll(data, "The presentation")

Content <- poll(data, "Content") %>%
  set_names("C. The relevance of the topic", "D. Appropriateness of level of details",
            "Applicability of information to my work", 
            "Enthusiasm of prsenter", "Othe topics of interest to you") %>% map_df(releveling)

# Plotting ----------------------------------------------------------------

media %>% plot_bars("How did you learn about this event?")

plots_logist <- bind_cols(academics, logistics ) %>% na.omit() %>%
  theme_plot(fill_by = "Academic status")

plots_content <- bind_cols(academics, Content ) %>% na.omit() %>%
  theme_plot(fill_by = "Academic status")


plots_logist[[1]] <- plots_logist[[1]] +  theme(legend.position="bottom")  
  # guides(fill=guide_legend(title="Number of attended webinars"))
mylegend<-g_legend(plots_logist[[1]])


relevant_plots <- grid.arrange(arrangeGrob( plots_logist[[1]] + theme(legend.position="none"),
                                            plots_logist[[2]] + theme(legend.position="none"),
                                            plots_content[[1]] + theme(legend.position="none"),
                                            plots_content[[2]] + theme(legend.position="none"),
                                            nrow=2),
                               mylegend, nrow=2,heights=c(10, 1))

# plots <- presentation%>%
#   map_df(releveling) %>%
#   bind_cols(academics) %>%
#   na.omit() %>%
#   theme_plot(fill = "Academic status")
# plots
# 
# p3 <- grid.arrange(arrangeGrob(plots[[1]] + theme(legend.position="none"),
#                                plots[[2]] + theme(legend.position="none"),
#                                plots[[3]] + theme(legend.position="none"),
#                                plots[[4]] + theme(legend.position="none"),
#                                plots[[5]] + theme(legend.position="none"),
#                                nrow=2),
#                    nrow=2,heights=c(10, 1))
# 
