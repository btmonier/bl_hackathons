
library(devtools)
library(EnvRtype)


data("G2F_2014_17")
data = G2F_2014_17

env   = data$env
lon   = data$lon
lat   = data$lat
env   = data$env
start = data$start
end = data$end
country = rep('USA1',length(lon))

env.data <- EnvRtype::get_weather(
    env.id = env,
    lat = lat,
    lon = lon,
    start.day = start,
    end.day = end,
    country = country,
    parallel = TRUE
)



# env.i = c("1_AN","1_PI","2_AN","2_PI") # environment ID
# lat = c(-22.875,-22.705,-22.875,-22.705) # latitude coordinates
# lon = c(-47.997,-47.637,-47.997,-47.637) # longitude coordinates
# plant.date = c("2016-01-26","2016-01-21","2017-01-12","2017-01-10") # year-month-day
# harv.date = c('2016-08-01',"2016-07-14","2017-07-25","2017-07-15")
# collecting weather data
# df.clim = get_weather(env.id = env.i,lat = lat,lon = lon,start.day = plant.date,end.day = harv.date,country = 'BRA')



## Get soil data example ----
dir <- "../../Downloads/SoilGrid/"
(soil_grid = list.files(path = dir,pattern = 'tif'))
(soil_name = gsub(soil_grid,pattern = '.tif',replacement = ''))

require(raster)
env.data2 = data.frame(env = env.i,LAT = lat, LON = lon)
soil_data = c()
for(i in 1:length(soil_grid)) {
    soil_data = rbind(
        soil_data,
        data.frame(
            Feature = soil_name[i],
            extract_GIS(
                covraster = raster(paste0(dir,'/',soil_grid[i])),
                name.out = 'Soil_Grid', env.data = env.data2)
        )
    )
}

head(soil_data)


