#install.packages("acs")
#library(acs)
??acs.lookup

library(plyr)
library(dplyr)
list=c("plyr","dplyr","ggplot2","caret","magrittr","babynames","acs","choroplethr","choroplethrMaps")

# choroplethr package contains most map functions, ggmap,maps,mapproj,Rgooglemaps etc
#install.packages(list, dependencies = TRUE)
# install.packages("choroplethrMaps" )
sapply(list, require, character.only = TRUE)

acs.lookup(keyword = "Japanese", endyear = 2013)

acs.lookup(keyword = "Chinese", endyear = 2013)


acs.lookup(keyword = "African", endyear = 2013)

subsaharanAfrican=get_acs_data("B04001", "county", column_idx=9)[[1]]

head(subsaharanAfrican)

# install API keys
api.key.install('3229c893e71bdeb9c632143e2d8095976c14ab27');

# state_choropleth_acs("B01003", endyear=2012, span=5)


api.key.install()

l = get_acs_data("B02006", "county", column_idx=9)

ls = get_acs_data("B02006", "state", column_idx=9)[[1]]



# regions as FIPS County Codes.

str(l)

df = l[[1]]

head(df)

head(ls)
boxplot(df$value)

#two conclusions from this chart: 1) the median is very low and 2) there are two very large outliers.


data(county.regions)

head(county.regions)

df2 = merge(df, county.regions)
df2 = df2[order(-df2$value), ]
head(df2)


library(choroplethrMaps)


# county japanese population
county_choropleth(df, title = "2012 County Estimates:\nNumber of Japanese per County")


# statejapanese population
state_choropleth(ls, title = "2012 state Estimates:\nNumber of Japanese per state")

#According to this map, by living on the west coast I am already in a part of the country
#with a high concentration of Japanese people.

state_choropleth_acs("B01003", endyear=2012, span=5)


library(choroplethr)
data(df_pop_state)
state_choropleth(df_pop_state)



data(df_pop_county)
county_choropleth(df_pop_county, state_zoom="california", reference_map=TRUE)

library(devtools)
install_github("dkahle/ggmap")


# restart R

library(choroplethr)
data(df_pop_county)
county_choropleth(df_pop_county, state_zoom="california", reference_map=TRUE)


# world map
install.packages("choroplethrAdmin1")
packageVersion("choroplethrAdmin1")
library(choroplethrAdmin1)
library(ggplot2)

data(admin1.map)

ggplot(admin1.map, aes(long, lat, group=group)) + 
  geom_polygon() 



library(choroplethr)
library(choroplethrAdmin1)

?df_japan_census
data(df_japan_census)
df_japan_census$value = df_japan_census$pop_density_km2_2010

# prefecture names for kansai region in japan
kansai = c("mie", "nara", "wakayama", "kyoto", "osaka", "hyogo", "shiga")
admin1_choropleth(country.name = "japan", 
                  df           = df_japan_census, 
                  num_colors   = 4, 
                  zoom         = kansai)


admin1_choropleth(country.name = "japan", 
                  df           = df_japan_census, 
                  title        = "2010 Population DensitynKansai Region, Japan",
                  legend       = "People / km^2",
                  num_colors   = 4, 
                  zoom         = kansai,
                  reference_map = TRUE)



#MAP	        FUNCTION	             PACKAGE
#US States	state_choropleth	choroplethr
#US Counties	county_choropleth	choroplethr
#US ZIP Codes	zip_choropleth	choroplethrZip
#California Census Tracts	ca_tract_choropleth	choroplethrCaCensusTract

# install.packages("devtools")
library(devtools)
install_github('arilamstein/choroplethrZip@v1.4.0')
install_github("arilamstein/choroplethrCaCensusTract@v1.1.0")



library(choroplethrCaCensusTract)
data(df_ca_tract_demographics)
df_ca_tract_demographics$value = df_ca_tract_demographics$per_capita

ca_tract_choropleth(df_ca_tract_demographics,
                    title       = "2013 Los Angeles Census Tractn Per Capita Income",
                    legend      = "Income",
                    num_colors  = 4,
                    county_zoom = 6037)

ca_tract_choropleth(df_ca_tract_demographics,
                    title         = "2013 Los Angeles Census Tractn Per Capita Income",
                    legend        = "Income",
                    num_colors    = 4,
                    county_zoom   = 6037,
                    reference_map = TRUE)



library(choroplethrZip)
data(df_zip_demographics)
df_zip_demographics$value = df_zip_demographics$per_capita_income

zip_choropleth(df_zip_demographics,
               title       = "2013 Manhattan ZIP Code Income Estimates",
               legend      = "Per Capita Income",
               county_zoom = 36061)

zip_choropleth(df_zip_demographics,
               title         = "2013 Manhattan ZIP Code Income Estimates",
               legend        = "Per Capita Income",
               county_zoom   = 36061,
               reference_map = TRUE)


library(choroplethr)
data(df_pop_county)
county_choropleth(df_pop_county,
                  title      = "2012 California County Population Estimates",
                  legend     = "Population",
                  state_zoom = "california")

county_choropleth(df_pop_county,
                  title         = "2012 California County Population Estimates",
                  legend        = "Population",
                  state_zoom    = "california",
                  reference_map = TRUE)


library(choroplethr)
data(df_pop_state)
data(continental_us_states)

state_choropleth(df_pop_state,
                 title  = "2012 State Population Estimates",
                 legend = "Population",
                 zoom   = continental_us_states)

state_choropleth(df_pop_state,
                 title         = "2012 State Population Estimates",
                 legend        = "Population",
                 zoom          = continental_us_states,
                 reference_map = TRUE)



library(RgoogleMaps)
2
lat <- c(48,64) #define our map's ylim
3
lon <- c(-140,-110) #define our map's xlim
4
center = c(mean(lat), mean(lon))  #tell what point to center on
5
zoom <- 5  #zoom: 1 = furthest out (entire globe), larger numbers = closer in
6
terrmap <- GetMap(center=center, zoom=zoom, maptype= "terrain", destfile = "terrain.png") 
#lots of visual options, just like google maps: maptype =
#c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")





#Step 1: Learn About Choroplethr’s Object Oriented System
#There are several ways to change the reference map in choroplethr. 
#In my opinion the easiest method involves using choroplethr’s object oriented 
#system. This system is described in my article Object Oriented Choropleths.

#Step 2: Identify the Function to Override
#Object oriented programming allows us to selectively override portions of an object. 
#In this case we want to override the function get_reference_map():


get_reference_map = function()
{
  # note: center is (long, lat) but MaxZoom is (lat, long)
  
  center = c(mean(self$choropleth.df$long),
             mean(self$choropleth.df$lat))
  
  max_zoom = MaxZoom(range(self$choropleth.df$lat),
                     range(self$choropleth.df$long))
  
  get_map(location = center,
          zoom     = max_zoom,
          color    = "bw")
}



#Step 3: Learn ggmap
#The part of get_reference_map() that we want to change is get_map(), 
#which is part of the ggmap package. The best introductions to ggmap that
#I have seen are here and here. To create a color satellite reference map we
#need get_map to look like this


get_map(location = center,
        zoom     = max_zoom,
        maptype  = "satellite",
        color    = "color")






#Step 4: Create Your Own Class
#Combining the above information, if you want to create a ZIP Choropleth 
#object that uses a color satellite reference map, type the following:
library(choroplethr)
library(choroplethrZip)
library(R6)
library(RgoogleMaps)
library(ggmap)

ZipChoroplethSatellite = R6Class("ZipChoroplethSatellite", inherit = ZipChoropleth,
                                 public = list(
                                   
                                   get_reference_map = function()
                                   {
                                     # note: center is (long, lat) but MaxZoom is (lat, long)
                                     
                                     center = c(mean(self$choropleth.df$long),
                                                mean(self$choropleth.df$lat))
                                     
                                     max_zoom = MaxZoom(range(self$choropleth.df$lat),
                                                        range(self$choropleth.df$long))
                                     
                                     get_map(location = center,
                                             zoom    = max_zoom,
                                             maptype = "satellite",
                                             color   = "color")
                                   }
                                 )
)




#Step 5: Run
#To use ZipChoroplethSatellite type the following:
data(df_zip_demographics)
df_zip_demographics$value = df_zip_demographics$per_capita_income
nyc_fips = c(36005, 36047, 36061, 36081, 36085)

c = ZipChoroplethSatellite$new(df_zip_demographics)
c$set_zoom_zip(state_zoom=NULL, county_zoom = nyc_fips, zip_zoom=NULL, msa_zoom=NULL)
c$set_num_colors(4)
c$title  = "2013 New York City ZIP Code Tabulated Areas"
c$legend = "Per Capita Income"
c$render_with_reference_map()