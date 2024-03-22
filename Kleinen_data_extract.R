

### data extract - Kleinen global methane emissions

#' cite data as
#' Kleinen, Thomas; Gromov, Sergey; Steil, Benedikt; Brovkin, Victor (2021).
#' Natural methane emissions 1850-3009. DOKU at DKRZ. 
#' https://hdl.handle.net/21.14106/edf716626b6105380067d7eeece2e4020464c793
#' 
#' ## UNITS FOR CH4 flux are in Kg/m2

library(ncdf4)
library(raster)


################ Uncomment here to run for each scenario --- match to line 46-50!!!!!!!!!!
### SSP 1.9
nc_data <- nc_open("data/raw_methane_data_kleinen/SSP1-1.9_annual_1850-3009.nc")
### SSP 2.6
nc_data <- nc_open("data/raw_methane_data_kleinen/SSP1-2.6_annual_1850-3009.nc")
### SSP 4.5
nc_data <- nc_open("data/raw_methane_data_kleinen/SSP2-4.5_annual_1850-3009.nc")
### SSP 7.0
nc_data <- nc_open("data/raw_methane_data_kleinen/SSP3-7.0_annual_1850-3009.nc")
### SSP 8.5
nc_data <- nc_open("data/raw_methane_data_kleinen/SSP5-8.5_annual_1850-3009.nc")


lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
flux.array <- ncvar_get(nc_data, "ch4_flux_wetland") # store the data in a 3-dimensional array
dim(flux.array) ### [Long, Lat, Time-slice]; Time slices are in months beginning 1961 - 2099 (139 years * 12 months = 1668 slices)
nc_close(nc_data)

#identify time slices for saving, 1961 (match zhang) - 2099. #1 is 1850
start <- 111
end <- 249
index <- start:end

#set up loop
for (i in index) {
  flux.slice <- flux.array[,,i]
  r <- raster(t(flux.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  #nam <- paste("data/raw_methane_data_kleinen/ssp1.9_1961_2099/ch4_flux","19",i-150+2000,sep="_")
  #nam <- paste("data/raw_methane_data_kleinen/ssp2.6_1961_2099/ch4_flux","19",i-150+2000,sep="_")
  #nam <- paste("data/raw_methane_data_kleinen/ssp4.5_1961_2099/ch4_flux","19",i-150+2000,sep="_")
  #nam <- paste("data/raw_methane_data_kleinen/ssp7.0_1961_2099/ch4_flux","19",i-150+2000,sep="_")
  nam <- paste("data/raw_methane_data_kleinen/ssp8.5_1961_2099/ch4_flux","19",i-150+2000,sep="_")
  
  writeRaster(r,nam, format="GTiff", overwrite=T)
    print(i)
}


#### open and write out all rasters above before proceeding


### write a function to sum all pixels for each raster in list
### also converts from kg/m2 into Tg/yr
annual_sum <- function(r){
  output <- raster(r)
  a <- area(output)
  output <- overlay(output, a,
                    fun=function(r1, r2){return(r2*r1*10^9)})
  return(sum(values(output))/10^12)
}


### apply function  

## ssp 1.9
f <- list.files("data/raw_methane_data_kleinen/ssp1.9_1961_2099/")
f <- paste("data/raw_methane_data_kleinen/ssp1.9_1961_2099/", f, sep = "")
CH4_Tg_yr <- lapply(f, annual_sum)
CH4_Tg_yr_1.9 <- unlist(CH4_Tg_yr)

## ssp 2.6
f <- list.files("data/raw_methane_data_kleinen/ssp2.6_1961_2099/")
f <- paste("data/raw_methane_data_kleinen/ssp2.6_1961_2099/", f, sep = "")
CH4_Tg_yr <- lapply(f, annual_sum)
CH4_Tg_yr_2.6 <- unlist(CH4_Tg_yr)

## ssp 4.5
f <- list.files("data/raw_methane_data_kleinen/ssp4.5_1961_2099/")
f <- paste("data/raw_methane_data_kleinen/ssp4.5_1961_2099/", f, sep = "")
CH4_Tg_yr <- lapply(f, annual_sum)
CH4_Tg_yr_4.5 <- unlist(CH4_Tg_yr)

## ssp 7.0
f <- list.files("data/raw_methane_data_kleinen/ssp7.0_1961_2099/")
f <- paste("data/raw_methane_data_kleinen/ssp7.0_1961_2099/", f, sep = "")
CH4_Tg_yr <- lapply(f, annual_sum)
CH4_Tg_yr_7.0 <- unlist(CH4_Tg_yr)

## ssp 8.5
f <- list.files("data/raw_methane_data_kleinen/ssp8.5_1961_2099/")
f <- paste("data/raw_methane_data_kleinen/ssp8.5_1961_2099/", f, sep = "")
CH4_Tg_yr <- lapply(f, annual_sum)
CH4_Tg_yr_8.5 <- unlist(CH4_Tg_yr)



##### run this at the end
year <- seq(1961,2099,1)
df <- data.frame(year)

df$CH4_Tg_yr_ssp19 <- CH4_Tg_yr_1.9
df$CH4_Tg_yr_ssp26 <- CH4_Tg_yr_2.6
df$CH4_Tg_yr_ssp45 <- CH4_Tg_yr_4.5
df$CH4_Tg_yr_ssp70 <- CH4_Tg_yr_7.0
df$CH4_Tg_yr_ssp85 <- CH4_Tg_yr_8.5

 
 
write.csv(df, "data/Global_methane_projections_kleinen.csv", row.names = FALSE)
 
 
 


