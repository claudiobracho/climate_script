## VARIABLES CLIMÁTICAS ACTUALES
# BIO1 a BIO19
climate_keys <- read_excel("data_sources/environmental_variables/climate/wcs_bio_def/climate_keys.xlsx")

# Definir proyección
euro_albers = CRS("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

for(i in 1:nrow(climate_keys)) {  
  # Abrir BIO[i]
  bio <- raster(paste0("data_sources/environmental_variables/climate/wc2.1_30s_bio/wc2.1_30s_bio_", climate_keys[i,2], ".tif",sep = ""))
  
  #Raster con la extensión del área de estudio (en nuestro caso en WGS84)
  base_raster_1 <- raster("data_sources/environmental_variables/base_raster1km.tiff")
  
  # Resample basándonos en un ráster base de 1x1km
  bio <- raster::resample(bio,
                          base_raster_1,
                          method='bilinear')
  
  # Cambiar proyección del raster
  bio<-projectRaster(bio, crs=euro_albers)
  
  # Resample de nuevo, pero en este caso basándonos en un ráster con diferente resolución espacial
  # Ej. 20x20km
  bio <- raster::resample(bio,
                          base_raster_20,
                          method='bilinear')
  
  #Crop-recorte a la misma extensión del raster del área de estudio
  bio <- mask(bio, base_raster_20)
  plot(bio)
  
  #  Exportar raster
  writeRaster(bio,  filename = file.path(paste0("data_sources/environmental_variables/bio20km/bio_",climate_keys[i,2], ".tif")), filetype = "GTiff", overwrite = TRUE)
}


## VARIABLES CLIMÁTICAS FUTURAS
future_climate_keys <- read_excel("data_sources/environmental_variables/future_climate/WORLDCLIM/future_climate_keys.xlsx")

for(i in 1:nrow(future_climate_keys)) {  
  # Open BIO[i]
  bio <- raster(paste0("data_sources/environmental_variables/future_climate/WORLDCLIM/2081-2100/ukesm1_0_ll/ssp126/",future_climate_keys[i,1],".tif",sep = ""))
  #Open again the first height (note in WGS84)
  height <- raster("data_sources/environmental_variables/mdt/mdt_study_area_wc2.tiff")
  
  #Focus to study area using first raster (also in WGS84)
  # Resample our bio1 raster based on our height raster (remember, both in WGS84, not our definitive projection)
  bio <- raster::resample(bio,
                          height,
                          method='bilinear')
  # Change raster projection
  bio<-projectRaster(bio, crs=euro_albers)
  # Resample again, but based on our height_def raster (10x10km)
  bio <- raster::resample(bio,
                          height_def,
                          method='bilinear')
  #Crop to same extent
  bio <- mask(bio, height_def)
  
  #  Export raster
  writeRaster(bio,  filename = file.path(paste0("data_sources/environmental_variables/future_climate/WORLDCLIM/2081-2100/ukesm1_0_ll/ssp126/",future_climate_keys[i,1], ".tif")), filetype = "GTiff", overwrite = TRUE)
}


## MEDIA Y DESVIACIÓN DE LOS FUTUROS CLIMÁTICOS
for(i in 1:nrow(future_climate_keys)) {  
  setwd(paste0("data_sources/environmental_variables/future_climate/WORLDCLIM/2081-2100/STACKS/ssp126/",future_climate_keys[i,1]))
  archivos <- list.files()
  archivos <- archivos[str_ends(archivos,".tif")]
  bio_future <- stack(archivos[1:3])
  mean <- calc(bio_future, base::mean)
  sd <- calc(bio_future, fun = sd)
  plot(mean)
  plot(sd)
  
  #  Export raster
  writeRaster(mean,  filename = file.path(paste0("data_sources/environmental_variables/future_climate/WORLDCLIM/2081-2100/STACKS/ssp126/",future_climate_keys[i,1],"/mean", ".tif")), filetype = "GTiff", overwrite = TRUE)
  writeRaster(sd,  filename = file.path(paste0("data_sources/environmental_variables/future_climate/WORLDCLIM/2081-2100/STACKS/ssp126/",future_climate_keys[i,1],"/sd", ".tif")), filetype = "GTiff", overwrite = TRUE)
  remove(sd)
}


## COMPARATIVA ENTRE EL CLIMA ACTUAL Y FUTURO
for(i in 1:nrow(future_climate_keys)) {
  mean_bio_future <- raster(paste0("data_sources/environmental_variables/future_climate/WORLDCLIM/2081-2100/STACKS/ssp585/",future_climate_keys[i,1],"/mean.tif",sep = ""))
  bio_current <- raster(paste0("data_sources/environmental_variables/selection_10km/",future_climate_keys[i,2],".tif",sep = ""))
  outputRaster <- overlay(mean_bio_future, 
                          bio_current, 
                          fun=function(r1, r2){return(r1-r2)})
  
  plot(outputRaster,
       main="Future climate - Current Climate\n Study Area")
  
  #Export object to new GeotIFF
  writeRaster(outputRaster, paste0("data_sources/environmental_variables/future_climate/bio_evolution",future_climate_keys[i,1],".tiff"),
              format="GTiff",  # specify output format - GeoTIFF
              overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
              NAflag=-9999) # set no data value to -9999
}


## TRANSFORMAR EL RASTER A DATAFRAME Y VISUALIZACIÓN
# convert to a df for plotting in two steps, example with bio_evol_8
# First, to a SpatialPointsDataFrame
bio_evol_8_pts <- rasterToPoints(bio_evol_8, spatial = TRUE)
# Then to a 'conventional' dataframe
bio_evol_8_df  <- data.frame(bio_evol_8_pts)
rm(bio_evol_8_pts, bio_evol_8)
bio_evol_8_df$Magnitude <- bio_evol_8_df$bio_evolutionbio8
ggplot() +
  theme_classic()+
  geom_raster(data = bio_evol_8_df , aes(x = x, y = y, fill = Magnitude)) + 
  ggtitle("Changes in the mean temperature of wettest quarter (°C*10)")+
  scale_fill_continuous(low = "grey", high = "red",limits = c(-10, 10))

# Opcional - 3D raster visualization
library(rasterVis)
library(rgl)
plot3D(bio_evol_8)
plot3D(bio_evol_8, zfac=1 #to set the relative elevation
       ,col= heat.colors, rev = TRUE)
plot3D(bio_evol_8, zfac=1 #to set the relative elevation
       ,col= heat.colors, rev = TRUE)
decorate3d(xlim = NULL, ylim = NULL, zlim = NULL, 
           xlab = "coords.x", ylab = "coords.y", zlab = "Change in bio8", 
           box = TRUE, axes = TRUE, 
           main = "bio8 changes in Europe by 2030 & ssp126", sub = NULL, 
           top = TRUE, aspect = FALSE, expand = 1.03, 
           tag = material3d("tag"))