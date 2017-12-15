# Loading libraries -------------------------------------------------------
if (!require(XLConnect)) {
  install.packages("XLConnect")
  library(XLConnect)
}

if (!require(ggrepel)){
  install.packages("ggrepel")
  library(ggrepel)
}

if (!require(gridExtra)){
  install.packages("gridExtra")
  library(gridExtra)
}

if (!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(stringi)){
  install.packages("stringi")
  library(stringi)
}

if (!require(ggmap)){
  install.packages("ggmap")
  library(ggmap)
}

if (!require(maps)){
  install.packages("maps")
  library(maps)
}

if (!require(scales)){
  install.packages("scales")
  library(scales)
}

if (!require(forcats)){
  install.packages("forcats")
  library(forcats)
}

if (!require(googlesheets)){
  install.packages("googlesheets")
  library(googlesheets)
}

# Defining functions ------------------------------------------------------

# Extract a theme's questions from a dataframe --------------------------------------------
## Questions around a central themem usually share the theme's name, and then the question 
## itself is placed between square parenthesis. This function takes the theme's name, 
## and removes it from the column names of the input dataframe
theme_extract <- function(dataframe, theme){
  x <- dataframe %>%
    select(contains(theme))
  names(x) <- names(x) %>%
    str_replace(".* \\[", "") %>%
    str_replace("\\]", "")
  x
}


# Plot a theme ------------------------------------------------------------
## This function plots the columns corresponding to a certain theme, typically extracted 
## using  the theme_extract function, and shows them as bar graph
theme_plot <- function(dataframe, fill_by = NULL){
  if (is.null(fill_by)) {
    g <- vector("list", length(dataframe))
    for (i in seq_along(dataframe)){
      g[[i]] <- dataframe %>% plot_bars( col = names(.)[i])
    }
  } else {
    g <- vector("list", length(dataframe)-1)
    for (i in seq(1, length(dataframe)-1)){ 
      # g[[i]] <- dataframe %>% plot_bars( col = names(.)[i+1], fill = fill_by)
      g[[i]] <- dataframe %>% plot_dot_plots( col = names(.)[i+1], fill = fill_by)
    }
  }
    
  g
}

# Plot a dot plot of a dataframe's column --------------------------------------------------
## The dot plot will have shapes according to how many variables are present. Caution not to plot 
## more than 6 at once
plot_dot_plots <- function(df, col, fill = NULL) {
  
  df %>%
    group_by()
  
  col2 <- str_replace_all(col, " ", "_")
  names(df)[names(df)==col] <- col2
  
  if (is.null(fill))
    plot <- ggplot(df, aes(x= reorder_size(get(col2)))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_y_continuous(labels=percent) + ylab("Percent") +
    xlab(col)
  else {
    fill2 <- str_replace_all(fill, " ", "_")
    names(df)[names(df)==fill] <- fill2
    plot <- ggplot( df, aes(x= reorder_size(get(col2)), fill = get(fill2))    ) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      scale_y_continuous(labels=percent) + ylab("Percent") +
      xlab(col) + guides(fill=guide_legend(title=str_replace_all(fill, " ", "\n")))
  }
  plot
}


# Plot a bar plot of a dataframe's column --------------------------------------------------
## The bar plot can also be filled according to another column as thought convenient
plot_bars <- function(df, col, fill = NULL) {

  col2 <- str_replace_all(col, " ", "_")
  names(df)[names(df)==col] <- col2

  if (is.null(fill))
    plot <- ggplot(df, aes(x= reorder_size(get(col2)))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_y_continuous(labels=percent) + ylab("Percent") +
    xlab(col)
  else {
    fill2 <- str_replace_all(fill, " ", "_")
    names(df)[names(df)==fill] <- fill2
    plot <- ggplot( df, aes(x= reorder_size(get(col2)), fill = get(fill2))    ) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      scale_y_continuous(labels=percent) + ylab("Percent") +
      xlab(col) + guides(fill=guide_legend(title=str_replace_all(fill, " ", "\n")))
  }
  plot
}


country_from_instit <-  function (instit.name) {
  country = ifelse(instit.name == "Institut Pasteur de Tunis", 'Tunisia',
                   ifelse(instit.name == "University of Cape Town", 'South Africa',
                          ifelse(instit.name == "University of Mauritius", 'Mauritius',
                                 ifelse(instit.name== "University of Khartoum", 'Sudan',
                                        ifelse(instit.name  == "Covenant University", 'Nigeria',
                                               ifelse(instit.name  == "University of the Western Cape", 'South Africa',
                                                      ifelse( instit.name  == "Uganda Virus Research Institute", 'Uganda',
                                                              ifelse(instit.name  == "Noguchi Memorial Institute for Medical Research, University of Ghana",
                                                                     'Ghana',
                                                                     ifelse(instit.name  == "University of the Free State", 'South Africa',
                                                                            ifelse(instit.name== "The Egyptian Center of Bioinformatics and Genomics ", 'Egypt',
                                                                                   'NA'))))))))))
  # country = data.frame(Country = country)
  # return(country)
}

# To change plot order of bars, change levels in underlying factor
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

# plot_bars <- function(df, col) {
#   col2 <- str_replace_all(col, " ", "_")
#   names(df)[names(df)==col] <- col2
#   plot <- ggplot(df, aes(x= reorder_size(get(col2)))) +
#     geom_bar(aes(y = (..count..)/sum(..count..))) +         
#     scale_y_continuous(labels=percent) + ylab("Percent") +
#     xlab(col)
#   plot
# }


poll <- function(data, string){
  poll <- data %>%
    select(contains(string))
  names(poll) <- names(poll) %>%
    str_replace(".* \\[", "") %>%
    str_replace("\\]", "")
  poll
}