setwd("C:/Users/carol/OneDrive - UBC/Documents/UBC/Master in Science/Master research/Chapter 1")

##Install packages##

packages <- c("dplyr", "geobr", "tidyverse", "sf", "spData", "ddrepel", 
              "maps", "mapdata","stringr", "viridis", "dplyr", "tidyr",
              "ggplot2","osmdata","units", "mapview", "ggmap","ggspatial",
              "tmap","Hmisc","stringi","stringr","rnaturalearth","raster",
              "rnaturalearthdata","readxl","openxlsx","leaflet","ggmap", "ggpattern")
newPackages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages)

##Run libraries##
library(dplyr)
library(geobr)
library(tidyverse)
library(sf)
library(spData)
library(ggrepel)
library(maps)
library(mapdata)
library(stringr)
library(viridis)
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(sf) # simple features - geospatial geometries
library(osmdata) # obtaining OpenStreetMap vector data
library(units) # working with units
library(mapview) # interactive geometry viewing
library(ggmap) # downloading raster maps from a variety of sources
library(ggspatial) # map backgrounds and annotations for ggplot
library(tmap) # static/interactive map library with ggplot-like syntax
library(Hmisc)##to open database .mdb
library(stringi) ##to remove special characters like accents in departments#
library(stringr)##to change strings in data e.g. row names
library(rnaturalearth) ##To obtain world map base layer for mapping
library(raster)
library(rnaturalearthdata)
library(readxl)
library(openxlsx)
library(leaflet)
library(ggmap)
library(htmlwidgets)
library(webshot)
library(ggpattern)


##Load data for map##
df <- read_excel("Review_Adoption_Silvopastoral_Systems_DB_Official.xlsx", sheet= 1) %>% filter (Include == "y")##this is the one I will be using for the analysis


##Veriables total and per paper##
unique(df$Variable_reported)



##Select variables##
df <- df %>% 
  dplyr::select(Article, ID_No, `Qualitate/Quantitative`, Country, Latitud, Longitud,
                Elevation_masl, `Avg_temp_(C)`, Precipitation_mm_anuales, Date_of_study,
                Name_of_program, Method, Sample_size, Spatial_Scale, 
                Factor_category, Subcategory, Effect_on_adoption_of_SPS, Type_of_cattle_ranching,
                Type_of_silvopasture_adopted, Adoption_clean)

##Set correct data types to variables##
df<- df%>% mutate(across(where(is.character), as.factor))%>%
  drop_na(Factor_category)

df$Effect_on_adoption_of_SPS <- factor(df$Effect_on_adoption_of_SPS, levels=c('null', 'negative', 'positive'))



##Explore relationships of SPS with the scaled data##
cat_analysis <- (Filter(is.factor, df))
num_analysis <- (Filter(is.numeric, df))

##Plot variables##
##Explore variable distribution##
cat_analysis %>%
  group_by("ID_No")%>%
  gather() %>% 
  filter(!is.na (value))%>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(stat= "count")

num_analysis %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


###Start with mapping##
##Figure 1a#
##Create count map by country##
count_id_country <- df%>% 
  distinct(ID_No, .keep_all = TRUE)%>%group_by(Country)%>% count()


# Merge the country_map dataframe with the world map data
##Map with white background##
world_map <- map_data("world")

# Filter the world map data to include only countries in Latin America
latin_america <- c("Mexico", "Guatemala", "Belize", "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panama",
                   "Colombia", "Venezuela", "Guyana", "Suriname", "French Guiana", "Ecuador", "Peru", "Bolivia",
                   "Brazil", "Paraguay", "Uruguay", "Argentina", "Chile")

latin_america_map <- subset(world_map, region %in% latin_america)

# Create a named vector or list with the number of papers for each country
papers_data <- c(Argentina = 2, Belize = 1, Brazil = 3, Colombia = 13, 
                 `Costa Rica` = 10, Ecuador = 1, Guatemala = 1, Mexico = 11, 
                 Nicaragua = 4, Panama = 2, Peru = 1, Uruguay = 1)

# Map the number of papers to the base map for Latin America
latin_america_map$Papers <- papers_data[latin_america_map$region]



fig1a<-ggplot() + 
  geom_polygon(data = latin_america_map, aes(x = long, y = lat, group = group, fill= Papers), color = "black") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradient(low = "white", high = "black", limits = c(0, 15), breaks = seq(0, 16, by = 2)) +
  labs(title = "",
       fill= "Number of\n documents") +
  theme_minimal()+
  annotation_north_arrow(location = "tr") + # ggspatial arrow on top right
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.box.background = element_rect(fill = "white", colour = "gray"),
        legend.margin = margin(5, 5, 5, 5))


ggsave("Figures_discover/Fig1a.png", fig1a, width = 5, height = 5, dpi = 1500)




##Figure 1b##
##Background map##
world_map <- map("world", fill = TRUE, col = "transparent", plot = FALSE)

library(leaflet)
pal <- colorNumeric(palette = "RdPu", domain = df$Date_of_study)


fig1b<- leaflet() %>% 
  addTiles() %>% 
  setView( lng = -68, lat = -20, zoom = 2.4) %>% 
  addProviderTiles("Esri.WorldImagery")%>%
  addPolygons(data = world_map, fillOpacity = 0, color = "black", weight = 1.1)%>%
  addCircleMarkers(data = df, lng = ~Longitud, lat = ~Latitud, weight=0.3,
                   color= "black", stroke = 1, 
                   radius = 3, opacity = 1)%>%
  addScaleBar(position = "topright")


saveWidget(fig1b, file = "fig1b.html", selfcontained = TRUE)
webshot("fig1b.html", file = "Figures/fig1b.png")




##Load data for analysis##

##Set positions for bar graph##
positions <- c("Market",  "Ecosystem service/ disservice", 
               "Farm characteristics",  
               "Farmer characteristics",
               "Management","Information transfer","Economic incentives" )


##Figure 2##
count_factor_total <- df%>% 
  filter(Factor_category!="Other")%>%
  group_by(Factor_category)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count()

fig_2<- ggplot(count_factor_total, aes(y = Factor_category, x = n)) +
  geom_bar(stat = "identity", color = "black", fill= "gray") +
  labs(x = "Number of documents", y = "", size=16, colour= "black") +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))+
  scale_y_discrete(limits = positions, labels= c("Market conditions","Ecosystem service/ disservice",
                                                 "Farm characteristics", "Farmer characteristics",
                                                  "Management","Information transfer",
                                                 "Economic incentives"))

ggsave("Figures_discover/Fig2.png", fig_2, width =8, height = 4, dpi = 1500)



##Figure 3##
# Calculate the count of unique ID numbers for each factor category
count_factor <- df%>% 
  filter(Factor_category!="Other")%>%
  group_by(Factor_category, Effect_on_adoption_of_SPS)%>% 
  distinct(ID_No, .keep_all = FALSE) %>%count() 

##Same but with proportions##
positions_fig3 <- c("Economic incentives", "Information transfer", "Ecosystem service/ disservice", 
                    "Farm characteristics",  "Farmer characteristics", "Management", "Market")
positions_proportions <- rev(positions_fig3)

df2<- count_factor %>%
  group_by(Factor_category) %>%
  summarise(n= sum(n))%>%
  mutate(percent = (n / sum(n)), cumsum = cumsum(percent), label=ifelse(paste0("N=", sum(n)),""))


fig_3= ggplot(count_factor, aes(y = Factor_category, x = n, fill = Effect_on_adoption_of_SPS, pattern= Effect_on_adoption_of_SPS)) +
  geom_col(position = "fill", color = "black") +
  scale_fill_manual(values = c("positive" = "white", "negative" = "black", "null" = "gray"), 
                    labels = c("Positive", "Negative", "Null"),
                    limits = c("positive", "negative", "null"))  +# Define colors for the bars
  labs(x = "", y = "", 
       fill="",
       title = "Effect on adoption") +
  theme_classic() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12),
        plot.title = element_text(size= 10, vjust = 0.1, hjust = 0.5),
        legend.position = "top", 
        legend.text.align =0, 
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_text(color = "black", size = 12, hjust = 0.5),
        legend.text = element_text(color = "gray3", size = 10))+
  scale_y_discrete(limits = positions_proportions, labels= c("Market conditions","Management","Farmer characteristics", "Farm characteristics", "Ecosystem service/ disservice",  "Information transfer",  "Economic incentives"))+
  scale_x_continuous(labels = scales::percent, expand= expansion(mult= 0.01, add= c(0, 0.02))) 


ggsave("Figures_discover/Fig3.png", fig_3, width =8, height = 4.5, dpi = 1500)


##Prepare data for figure 4##
# Calculate the count of unique ID numbers for each factor category
count_factor_and_sub <- df%>% 
  filter(Factor_category!="Other") %>% 
  #`Qualitate/Quantitative` != "Quanlitative")%>%
  group_by(Factor_category, Subcategory, Effect_on_adoption_of_SPS)%>% 
  distinct(ID_No, .keep_all = FALSE) %>%count() %>%
  arrange(desc(n))

# Select the top three Subcategories for each Factor_category
count_factor_and_sub_total <- df%>% 
  filter(Factor_category!="Other") %>% 
  group_by(Factor_category, Subcategory)%>% 
  distinct(ID_No, .keep_all = FALSE) %>%count() %>%
  arrange(desc(n))

##Count 3 subcategories most mentioned according to the number of appeareances in documents
df_top_three <- count_factor_and_sub_total %>%
  group_by(Factor_category) %>%
  slice(1:3)

##Replace to not habe others as a row##
df_top_three$Subcategory[df_top_three$Subcategory == "Other management"] <- "Time for investment recovery"
list_top_sub <- unique(df_top_three$Subcategory)


count_filtered_2 <- count_factor_and_sub %>% 
  filter (Subcategory %in% list_top_sub)%>%
  arrange(desc(n))



##Change Faced grid labels##
count_filtered_2$Factor_category <- factor(count_filtered_2$Factor_category, levels = c("Economic incentives", "Information transfer", "Ecosystem service/ disservice", 
                                                                                        "Farm characteristics",  "Farmer characteristics", "Management", "Market"),
                                           labels = c("Economic\n incentives", "Information \ntransfer", "Ecosystem \nservice/ \ndisservice", 
                                                      "Farm \ncharacteristics",  "Farmer \ncharacteristics", "Management", "Market \nconditions"))

count_filtered_2$Subcategory <- factor(count_filtered_2$Subcategory, levels = c("Lack of market for sustainable products", 
                                                                                "Production costs", "Price stability", 
                                                                                "Time for investment recovery", "Management complexity/cost",
                                                                                "Main product",
                                                                                "Demographics", "Wealth","Preferences and values",
                                                                                "Vulnerability to climate change","Forest", "Farm size",
                                                                                "Supporting services", "Provision services", "Regulating services",
                                                                                "Social networks", "Knowledge/Awareness on SPS", "Presence of technical assistance",
                                                                                "Economic incentives different to PES", "PES", "Income increase"), 
                                       labels = c("Lack of market for sustainable products", 
                                                  "Production costs", "Price stability", 
                                                  "Time for investment recovery", "Management complexity/cost",
                                                  "Main product",
                                                  "Demographics", "Wealth","Preferences and values",
                                                  "Vulnerability to climate change","Forest", "Farm size",
                                                  "Supporting services", "Provisioning services", "Regulating services",
                                                  "Social networks", "Knowledge/Awareness of SPS", "Presence of technical assistance",
                                                  "Economic incentives different to PES", "PES", "Income increase"))
##Figure 4##                  
# Plotting
# Reorder the factor levels

fig4= ggplot(count_filtered_2, aes(y = Subcategory, x = n, fill = Effect_on_adoption_of_SPS, pattern= Effect_on_adoption_of_SPS)) +
  geom_col(position = "fill", color = "black") +
  scale_fill_manual(values = c("positive" = "white", "negative" = "black", "null" = "gray"), 
                    labels = c("Positive", "Negative", "Null"),
                    limits = c("positive", "negative", "null"))  +# Define colors for the bars
  labs(x = "", y = "", 
       fill = "", 
       title = "Effect on adoption") +
  theme_classic()+
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray", linetype = 1),
        axis.text.y = element_text(color = "black", size = 10, hjust = 1, vjust = 0),
        axis.text.x = element_text(color = "black", size = 10, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 12), 
        plot.title = element_text(size= 10, vjust = 0.1, hjust = 0.5),
        strip.placement = "outside", 
        strip.text = element_text(color = "black", size = 10, hjust = 0.5, vjust = 0),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "top",  
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "gray3", size = 9.5)) +
  scale_x_continuous(labels = scales::percent, expand= expansion(mult= 0.01, add= c(0, 0.02)))+
  facet_wrap(~Factor_category, scales="free_y", ncol = 1, strip.position = "left")+
  theme(axis.text.x = element_text(angle = 90),
        axis.text.x.bottom = element_text(vjust = 0.5))



ggsave("Figures_discover/Fig4_bnw.png", fig4, width = 8, height = 10, dpi = 1500)



###Suplementary figures##

##Figure S1###
##Plot Years of studies##
count_date <- df%>% 
  distinct(ID_No, .keep_all = TRUE)%>%group_by(Date_of_study)%>% count()

fig_s1<-ggplot(count_date, aes(Date_of_study, n))+geom_line()



##Figure S2##
date_graph <- df%>% 
  group_by(Date_of_study)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 

##Plot##
fig_s2<- ggplot(date_graph, aes(Date_of_study, n))+ geom_line(colour= "black")+
  labs(x = "Date of study", y = "Number of documents") +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))

ggsave("Figures_discover/fig_s2.png", fig_s2, width = 8, height = 4, dpi = 1500)

##Figure S3- S7##
##Create dataframes to plot##
elev_graph <- df%>% 
  group_by(Elevation_masl)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 

temperature_graph <- df%>% 
  group_by(`Avg_temp_(C)`)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 

temperature_graph$`Avg_temp_(C)`<- round(temperature_graph$`Avg_temp_(C)`, 0)

prec_graph <- df%>% 
  group_by(Precipitation_mm_anuales)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 

sample_graph <- df%>% 
  group_by(Sample_size)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 


##Filter data for summary statistics##
filter_elevation <- elev_graph %>% filter (Elevation_masl < 1000)
sum(filter_elevation$n)

filter_temperature <- temperature_graph %>% filter (between(`Avg_temp_(C)`, 20, 30))
sum(filter_temperature$n)


filter_precipitation <- prec_graph %>% filter (between(Precipitation_mm_anuales, 1000, 2000))
sum(filter_precipitation$n)

filter_sample <- sample_graph %>% filter (Sample_size < 100)
sum(filter_sample$n)

##Plots S3-S6##
hist(elev_graph$Elevation_masl, xlab="Elevation (m.a.s.l)", ylab = "Number of documents", main= "", breaks=seq(0,3000,l=15))
hist(temperature_graph$`Avg_temp_(C)`, xlab = "Mean annual temperature (C°)", ylab = "Number of documents" , main= "", breaks=seq(9,36,l=10))
hist(prec_graph$Precipitation_mm_anuales,xlab = "Mean annual precipitation (mm)", ylab = "Number of documents" , main= "")
hist(sample_graph$Sample_size, xlab = "Sample size", ylab = "Number of documents" , main= "", breaks=seq(0,1605,l=50))



ggplot(elev_graph,aes(Elevation_masl))+geom_histogram(binwidth = 400,fill= "gray", color="black")+
  labs(x = "Elevation (m.a.s.l)", y = "Number of documents") +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))+
  scale_x_continuous(limits = c(0, 2600))

##Plot b##
ggplot(temperature_graph, aes(`Avg_temp_(C)`))+ geom_histogram(fill= "gray", color="black")+
  labs(x = "Mean annual temperature (C°)", y = "Number of documents") +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))+
  scale_y_continuous(limits= c(0,3.5),expand = c(0, 0)) 

#PLot c##
ggplot(prec_graph, aes(Precipitation_mm_anuales))+ geom_histogram(fill= "gray", color="black")+
  labs(x = "Mean annual precipitation (mm)", y = "Number of documents") +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12)) 

#PLot c##
ggplot(sample_graph, aes(Sample_size))+ geom_histogram(fill= "gray", color="black")+
  labs(x = "Sample size", y = "Number of documents") +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))



##Figure S7##
method_data <- df%>% 
  group_by(`Qualitate/Quantitative`)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 


figs6<- ggplot(method_data, aes(`Qualitate/Quantitative`, n))+ geom_bar(stat = "identity",fill= "lightgray",color="black")+
  labs(x = "", y = "Number of documents")+
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))+
  scale_y_continuous(limits= c(0,26),expand = c(0, 0)) 

ggsave("Figures_discover/fig_s7.png", figs6, width = 8, height = 4, dpi = 1500)

##Figure S8##
sps_type <- df%>% 
  group_by(Type_of_silvopasture_adopted)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 

# Define categories
categories <- list(
  "SPSi" = "SPSi",
  "SPS (unspecified)" = c("SPS", "not especified", "remanent trees", "Remnant trees"),
  "Dispersed trees" = "Dispersed trees",
  "Live fences" = "Live fences",
  "Fodder banks" = c("fodder bank", "improved pasture", "protein banks", "fodder banks"),
  "Other" = c("forestry", "forest conservation", "forest protection", "forest coservation", 
              "natural regeneration", "dispersed shrubs", "agroforestry",  "rotational grazing",
              "rotative grazing", "paddock rotation", "paddock division", "wind breaks", "livestock")
)

# Split the Type_of_silvopasture_adopted column into multiple rows
sps_type <- sps_type %>% 
  separate_rows(Type_of_silvopasture_adopted, sep = ", ")

# Function to assign categories
assign_category <- function(type) {
  for (cat in names(categories)) {
    if (any(sapply(categories[[cat]], function(keyword) grepl(keyword, type, ignore.case = TRUE)))) {
      return(cat)
    }
  }
  return(NA)
}

# Apply categorization
sps_type$Category <- sapply(sps_type$Type_of_silvopasture_adopted, assign_category)

# Aggregate data by category
cleaned_data <- sps_type %>% 
  group_by(Category) %>% 
  summarise(n = sum(n, na.rm = TRUE))

# Print cleaned dataset
print(cleaned_data)




##PLot by type os sps studied
fig_8<- ggplot(cleaned_data, aes(Category, n))+ geom_bar(stat = "identity",fill= "lightgray",color="black")+
  labs(x = "", y = "Number of documents")+
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))+
  scale_x_discrete(limits = c("Fodder banks", "Live fences", "Dispersed trees", 
                              "SPS (unspecified)", "SPSi", "Other"), 
                   labels = c("Fodder banks" = "Fodder\n banks",
                              "Live fences" = "Live\n fences",
                              "Dispersed trees" = "Dispersed\n trees",
                              "SPS (unspecified)" = "SPS\n (unspecified)",
                              "SPSi" = "SPSi",
                              "Other" = "Other"))
#scale_y_continuous(limits= c(0,50),expand = c(0, 0)) 

ggsave("Figures_discover/fig_s8.png", fig_8, width = 8, height = 4, dpi = 1500)


##Figure S9##
adoption_measure <- df%>% 
  group_by(Adoption_clean)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 

##Clean categories##
# Update counts
adoption_measure$n[adoption_measure$Adoption_clean == "Perceptions on SPS"] <- 
  adoption_measure$n[adoption_measure$Adoption_clean == "Perceptions on SPS"] + 
  adoption_measure$n[adoption_measure$Adoption_clean == "Presence of SPS in farm and perceptions"]

adoption_measure$n[adoption_measure$Adoption_clean == "Presence of SPS in farm"] <- 
  adoption_measure$n[adoption_measure$Adoption_clean == "Presence of SPS in farm"] + 
  adoption_measure$n[adoption_measure$Adoption_clean == "Presence of SPS in farm and perceptions"]

# Remove the combined category
adoption_measure <- adoption_measure[adoption_measure$Adoption_clean != "Presence of SPS in farm and perceptions", ]

# Print cleaned dataset
print(adoption_measure)



figs9<- ggplot(adoption_measure, aes(Adoption_clean, n))+ geom_bar(stat = "identity",fill= "lightgray",color="black")+
  labs(x = "", y = "Number of documents")+
  theme_minimal() +
  theme(axis.ticks = element_line(),
        axis.line = element_line(),
        panel.grid.major = element_line(color = "lightgray",linetype = 1),
        axis.text.y = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.text.x = element_text(color = "black",size = 12,hjust = 0.5, vjust = 0),
        axis.title=element_text(size=12))+
  scale_y_continuous(limits= c(0,26),expand = c(0, 0)) 

ggsave("Figures_discover/fig_s9.png", figs7, width = 8, height = 4, dpi = 1500)




##ID List##
ids_count <- df%>% 
  group_by(ID_No)%>% 
  distinct(ID_No, .keep_all = TRUE) %>%count() 
