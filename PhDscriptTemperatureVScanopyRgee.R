library(rgee)
## library sf for "features" e.g. vectors
library(sf)
## library terra for raster processing in R
library(terra)
## library ggplot2 for plotting
library(ggplot2)

rgee::ee_Initialize() 

# -------------------------------------------------------
# Load datasets
# -------------------------------------------------------

# Canopy height
canopy_height <- ee$Image("users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1")

# Landsat 9 collection
ls9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")

# Study areas
studyAreas <- ee$FeatureCollection("users/cirgeo/agritech/comuni")

# Filter Padova
padova <- studyAreas$filter(
  ee$Filter$eq("COMUNE", "Padova")
)

# Convert to geometry if needed
padova_geom <- padova$geometry()

# -------------------------------------------------------
# Landsat processing
# -------------------------------------------------------

ls9f <- ls9$
  filterDate('2020-01-01', '2022-12-30')$
  filterBounds(padova_geom)$
  filter(ee$Filter$lt("CLOUD_COVER", 10))$
  filter(ee$Filter$dayOfYear(180, 320))$
  select("ST_B10")$
  max()$
  multiply(0.00341802)$
  add(149)$
  subtract(273.15)$
  clip(padova_geom)

# -------------------------------------------------------
# Stack temperature + canopy height
# -------------------------------------------------------

stack <- ls9f$
  addBands(canopy_height)$
  rename(c("Temp", "CH"))$
  mask(canopy_height$gt(5))$
  clip(padova_geom) 

# -------------------------------------------------------
# Sample pixels
# -------------------------------------------------------

samples <- stack$sample(
  region = padova_geom,
  scale = 50,
  numPixels = 2000,
  geometries = TRUE
)

# Inspect
# samplesClient <- samples$getInfo()

## download sample points to r object
samplesClient.sf <- ee_as_sf(samples)
# plot(samplesClient.sf)
## write sample points to geopackage format, for loading to QGIS
sf::write_sf(samplesClient.sf, "samplesOutput.gpkg")
# if you want to load a geopackage file:
# sf::read_sf("samplesOutput.gpkg")

## if you want to export the stack to a geotif image in drive
task_img <- ee_image_to_drive(
  description = "stack",
  image = stack,
  fileFormat = "GEO_TIFF",
  region = padova$geometry(),
  fileNamePrefix = "stack",
  scale = 30
) $start()
 
## (ee_as_rast is a shortcut to downloading in drive and then 
## in local space, but needs googledrive library and access 
## credentials )
## stack.rast <- ee_as_rast(stack)
#  plot(stack.raster)

ggplot(samplesClient.sf, aes(x=CH, y=Temp) ) + geom_point()

linear.model <- lm(data = samplesClient.sf,   Temp ~ CH )
plot(linear.model)
summary(linear.model)
