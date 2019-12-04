# Gun-Violence-Bay-Area

Project Summary
------
The goal of this project is to find what caused gun violence to decrease in the Bay Area while it was increasing in many other US cities for the past decade. Other [analysts](https://www.theguardian.com/us-news/ng-interactive/2019/jun/03/gun-violence-bay-area-drop-30-percent-why-investigation) have suggested that this decline is related to criminal justice reforms, tough gun laws, and investment in local communities. We will be comparing how different cities in the United States interact with these factors using data from individual cities and comparing the differences. 

Repo Guide
------
+ /Gun_Violence_Bay_Area : Shiny App  
+ .gitignore 
+ Prison_data.Rmd : code used to create visualization of imprisonment data
+ README.md  
+ about.Rmd : initial draft of the about page
+ latlon_script.R : Modified version of [this](http://www.storybench.org/geocode-csv-addresses-r/) script to get latitude and longitude coordinates from street addresses using Google Maps API
+ laws.Rmd : code used to create visualization of gun control laws data
+ plotly.Rmd : code used to create visualization of violent crimes data
+ prep_shiny.R : Prep script to move files around
+ san_francisco_data.Rmd : code used to create visualization of aggravated assault with gun in San Francisco data
+ trace_data.Rmd : code used to create visualization of gun violence with gun in Oakland data from [The Trace](https://www.thetrace.org/violent-crime-data/)