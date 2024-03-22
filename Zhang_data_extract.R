



### data extract - Zhang global methane emissions

library(ncdf4)
library(raster)


### select pathway (data > raw_methane_data_zhang)

nc_data <- nc_open(file.choose())

#### run the following block for each pathway -- takes about 30 minutes for each
{
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

flux.array <- ncvar_get(nc_data, "ch4e") # store the data in a 3-dimensional array
dim(flux.array) ### [Long, Lat, Time-slice]; Time slices are in months beginning 1961 - 2099 (139 years * 12 months = 1668 slices)

nc_close(nc_data)




## Loop to extract all months and generate totals for each year

start.list <- seq(1, 1668, 12)


for (j in 1:length(start.list)) {
  start <- start.list[j]
  end <- start.list[j] + 11
  index <- start.list[j]:end
  k <- 1
  month <- rep(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  year <- 1961 + (start - 1)/12
  yr <- rep(year, each=12)
  
  for (i in index) {
    flux.slice <- flux.array[,,i]
    r <- raster(t(flux.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    nam <- paste("data/temp/ch4_flux","monthly",yr[k], month[k], ".tif",sep="_")
    writeRaster(r,nam, format="GTiff", overwrite=TRUE)
    
    k <- k+1
    print(i)
    
  }}

# units in g/m2/mmonth


### sum all monthly rasters by year

yr.list <- seq(1961, 2099, 1)


k <- 1

for(i in 1:length(yr.list)) {
  yr <- yr.list[i]
  j <- k + 11
  f <- list.files("data/temp")[k:j]
  f <- paste("data/temp/", f, sep = "")
  ras <- lapply(f, raster)
  STACK1 <- stack(ras)
  yr.sum <- calc(STACK1, fun = sum, na.rm = TRUE)
  writeRaster(x = yr.sum, filename = paste("data/temp2/ch4_flux_annual_sum_", yr, ".tif", sep = ""), driver = "GeoTiff", overwrite=TRUE)
  k <- k + 12
  
  print(i)
}

# units in g/m2/yr




### sum global methane emission for each year

#f <- list.files(getwd())
f <- list.files("data/temp2")
f <- paste("data/temp2/", f, sep = "")




### write a function to sum all pixels for each raster in list
### this function also converts untis from g/m2/yr to Tg/yr

annual_sum <- function(r){
  output <- raster(r)
  a <- area(output)
  output <- overlay(output, a,
                    fun=function(r1, r2){return(r2*r1*10^6)})
  return(sum(values(output))/10^12)
}

### apply the function over all rasters in the directory
CH4_Tg_yr <- lapply(f, annual_sum)

}


### run for the corresponding pathway
# CH4_Tg_yr_2.6 <- unlist(CH4_Tg_yr)
# CH4_Tg_yr_4.5 <- unlist(CH4_Tg_yr)
# CH4_Tg_yr_6.0 <- unlist(CH4_Tg_yr)
CH4_Tg_yr_8.5 <- unlist(CH4_Tg_yr)




##### run this at the end
year <- yr.list
df <- data.frame(year)

df$CH4_Tg_yr_rcp26 <- CH4_Tg_yr_2.6
df$CH4_Tg_yr_rcp45 <- CH4_Tg_yr_4.5
df$CH4_Tg_yr_rcp60 <- CH4_Tg_yr_6.0
df$CH4_Tg_yr_rcp85 <- CH4_Tg_yr_8.5



write.csv(df, "data/Global_methane_projections.csv", row.names = FALSE)



