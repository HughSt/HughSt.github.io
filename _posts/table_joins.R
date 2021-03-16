
## Table joins

# Prevalence of low birth weight among new-borns
# Originally sourced from "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,NUTRITION,1.0/.NT_BW_LBW......?format=csv"
LBW <-read.csv("https://github.com/HughSt/HughSt.github.io/raw/master/course_materials/week2/LBW_global.csv")

# Filter for 2015
LBW_2015 <- subset(LBW, TIME_PERIOD==2015)

# Get countries spatial data
library(rworldmap)
countries <- rworldmap::countriesCoarse

# To join data, you need a common field. In this case, we are going to use country name. 
# Its always preferable to use codes instead of names/characters as they are less ambiguous.
# But in this case, we are forced to used names.
# To see whether the country names in your data exist in your spatial
# data, you can use the %in% commant
LBW_2015$Country %in% countries$NAME  

# To see which are not
LBW_2015$Country[!LBW_2015$Country %in% countries$NAME]  

# You might have to manually change these to match as the reason
# they are not matching is likely a minor spelling issue. For example
countries$NAME[27]
LBW_2015$Country[27]

# In this case, we actually have the 3 letter ISO country codes in both
# datasets
head(LBW_2015$REF_AREA)
head(countries$ISO_A3)
LBW_2015$REF_AREA %in% countries$ISO_A3  

# If the matching field in both objects has the same name,
# you can just use the `by` argument. If they are different
# use `by.x` and `by.y` 
countries_merged <- merge(countries, 
                   LBW_2015,
                   by.x = "ISO_A3",
                   by.y = "REF_AREA",
                   all.x = T)

dim(countries)
dim(countries_merged)
head(countries_merged)

# And map
col_pal <- colorNumeric("Reds",
                        countries_merged$OBS_VALUE)
leaflet() %>% addProviderTiles("CartoDB.DarkMatter") %>% addPolygons(data = countries_merged,
                                         col = col_pal(countries_merged$OBS_VALUE),
                                         fillOpacity = 0.8, weight=2) %>%
  addLegend(pal = col_pal, values = countries_merged$OBS_VALUE,
            title = "Prevalence of LBW")


# There are some useful functions to tweak character strings 
# to make them match. For example, let's imagine our LBW_2015
# dataset doesn't have the `REF_AREA` field and instead has 
# something that looks like this
LBW_2015$REF_AREA_2 <- paste0(tolower(LBW_2015$REF_AREA), ".")
head(LBW_2015$REF_AREA_2)

# In this case, we need to remove the "." and make
# the characters uppercase in order to match with 
# the ISO3 field in the `countries` object
# First remove the ".". In this case, as each 
# entry is 3 characters, we can just select the
# first 3 
LBW_2015$REF_AREA_2_custom <- substr(LBW_2015$REF_AREA_2, 1, 3)
head(LBW_2015$REF_AREA_2_custom )

# Now we need to make it uppercase
LBW_2015$REF_AREA_2_custom <- toupper(LBW_2015$REF_AREA_2_custom )
head(LBW_2015$REF_AREA_2_custom )

# Example of using gsub to substitute characters with other characters (or whitespace)
# replace all instances of "a" with "-"
gsub("a", "-", LBW_2015$REF_AREA_2)

# IF you are working with "." this is a special character and 
# you need to wrap it
gsub("\\Q.\\E", "-", LBW_2015$REF_AREA_2)

# The grep function is also useful for
# doing more of a fuzzy match. e.g. find 
# which elements of LBW_2015$REF_AREA_2 
# have an "a" in them
grep("a", LBW_2015$REF_AREA_2)
