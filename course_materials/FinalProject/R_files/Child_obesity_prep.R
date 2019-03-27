library(raster)
# Data for final assessment
# Childhood obesity in the USA
#https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-women-infant-and-child-dfe5d
ob_data <- read.csv("/Users/hughsturrock/Downloads/Nutrition__Physical_Activity__and_Obesity_-_Women__Infant__and_Child.csv")

ob_data <- subset(ob_data, ob_data$StratificationID1=="OVERALL")
ob_data_2014  <- subset(ob_data, ob_data$YearStart==2014)
ob_data_2014 <- subset(ob_data_2014, ob_data_2014$Question=="Percent of WIC children aged 2 to 4 years who have obesity")

# Link to spatial dataset
USA_adm <- raster::getData("GADM", country="USA", level = 1)
USA_adm$LocationAbbr <- substr(USA_adm$HASC_1,4,5)

# Merge
USA_adm <- merge(USA_adm, ob_data_2014, by="LocationAbbr")

# Convert proportion to number
USA_adm$NumberObese <- round((USA_adm$Data_Value/100) * USA_adm$Sample_Size, 0)

# leaflet plot
pal <- colorQuantile(tim.colors(5), USA_adm$Data_Value, n=5)
leaflet() %>% addTiles() %>%
  addPolygons(data = USA_adm, color = pal(USA_adm$Data_Value))

# Simplify data
USA_adm <- USA_adm[,c("LocationAbbr", "NAME_1", "NumberObese", "Sample_Size")]

USA_adm_ob <- USA_adm
save(USA_adm_ob, file= "/Users/hughsturrock/Documents/Work/MEI/DiSARM/GitRepos/spatial-epi-course/course_materials/FinalProject/Data/USA_adm_ob.RData")



