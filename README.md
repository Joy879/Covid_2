# Covid WHO dashboard 


A beginner's attempt to recreate the [WHO Covid dashboard](https://covid19.who.int/) using simple R and shinydashboard code. 


### TODO  - DESIGN CONSIDERATIONS 
* Add logic to make the map more zoomed in using coord_fixed ratio
* Fix plotly hover info to only work with custom text
* Change font sizes of titles and ticks. Also look into themes
* Figure out how to get data directly from website
* Bring logic for different colors if cases/deaths/vaccinations
* Add vaccination data and add map visualization and color logic for that as well
* Look into proper mapping using leaflet/folium/choropleth

### TODO - DATA MANIPULATION CONSIDERATIONS
* Add logic to update the regions in descending order and rename regions to official names.
* Filter data to focus on vaccination ratio/infection ratio
