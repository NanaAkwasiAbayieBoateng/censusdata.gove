

myapikey ="MyH007YFsRqBxCSrFcFP7A1olxzHD6Hjd1sxEibh"

# Set working directory 
setwd('~/DataApiR') # plug in the working directory on your machine

# Load package
install.packages("httr")
 library(httr)


# plug in your API key
 myapikey <- "YOUR API KEY"

# the url path to the service
 
URL <- "https://api.data.gov/ed/collegescorecard/v1/schools?"

# GET(): download all available data for Emory University
get.data <- GET(URL, query=list(api_key=myapikey,
                                  school.name="Emory University"))

# content(): extract the content from the query
 emory.data <- content(get.data) 
class(emory.data) # it's a list object


# what's in emory.data
names(emory.data) # contains two components: metadata, results



# what's inside the results component
names(emory.data$results[[1]])


# see available dev-categories for 2013 data
names(emory.data$results[[1]]$`2013`)



# available variables under the cost category for 2013 data
names(emory.data$results[[1]]$`2013`$cost)



# elements of the tuition variable
 names(emory.data$results[[1]]$`2013`$cost$tuition)
 
 
 # load package
  library(magrittr)
 
 # subset list for annual data only
  emory.ann <- emory.data$results[[1]][c(as.character(1996:2013))]
  names(emory.ann)
 
 ##  [1] "1996" "1997" "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005"
 ## [11] "2006" "2007" "2008" "2009" "2010" "2011" "2012" "2013"
 
 # extract enrollment of undergraduate degree-seeking students for each year
  s.size <- emory.ann %>%
   sapply(function(x) x$student$size) %>% 
   unlist()
 
 # extract percentage of first-generation students for each year
  s.fg <- emory.ann %>%
   sapply(function(x) x$student$share_firstgeneration) %>% 
   unlist()
 
 # combine the two variables into a data frame
  emory.s <- data.frame(s.size, s.fg)
 
 # see the first few rows of the data frame
 head(emory.s) 
 
 
 # create a variable of year from the row number
 emory.s$year <- rownames(emory.s)
 
 # create a variable s.fg.n: number of first-generation students
 emory.s$s.fg.n <- round(emory.s$s.size*emory.s$s.fg)
 
 # save the data as a .csv file
 write.csv(emory.s, file="emory.s.csv", row.names = F)
 
 
 
 # load package
 library(ggplot2)
 
 # Line graph of total enrollment and first generation student number
  ggplot(emory.s, aes(year)) + 
   geom_line(aes(y = s.size, colour = "s.size", group=1)) + 
   geom_line(aes(y = s.fg.n, colour = "s.fg.n", group=1)) +
   xlab("Year") + ylab("Number") + # Set axis labels
   ggtitle("Enrollment: Emory University") + # set title      
   scale_colour_discrete(name="Enrollment",  # modify legend
                         labels=c("First Generation", "Total")) +
   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) # adjust x-axis text position
  
  
  
 library(rscorecard)
sc_key('YOUR API KEY')
  
  # extract Virginia institutions' 2013 data with three variables
df <- sc_init() %>% 
    sc_filter(stabbr == 'VA') %>% 
    sc_select(unitid, instnm, stabbr) %>% 
    sc_year(2013) %>% 
    sc_get()
  
  # see first few cases
  head(df)
  
  # load packages
  library(httr)
  library(magrittr)
  library(ggplot2)
  
  # Get all stations data in Virginia, remember to plug in your own API key
   get.afs <- GET("http://api.data.gov/nrel/alt-fuel-stations/v1.json?api_key=[YOUR API KEY]&state=VA") 
  
  # extract content from the query
   afs <- content(get.afs)
  
  # see what's available in the downloaded data
   names(afs)
  
  ## [1] "station_locator_url" "total_results"       "station_counts"     
  ## [4] "fuel_stations"
  
  # how many stations in the downloaded data
  afs$total_results
  
  ## [1] 526
  
  # see variables/fields under fuel_stations
  names(afs$fuel_stations[[1]])
  
  
  # extract vars: station_name, fuel_type_code
fsname <- afs$fuel_stations %>%
    sapply(function(x) x$station_name) %>%
    unlist()
ftcode <- afs$fuel_stations %>%
    sapply(function(x) x$fuel_type_code) %>%
    unlist()
  
  # combine the two vars in a data frame
afsdf <- data.frame(fsname, ftcode)
  
  # see the first few rows
 head(afsdf)
 
 
 install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
 library(jsonlite)
 all.equal(mtcars, fromJSON(toJSON(mtcars)))