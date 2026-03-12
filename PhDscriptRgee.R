library(rgee)
ee_Initialize()

## canopy height from Lang et al. 
canopy_height = ee$Image("users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1");
print( canopy_height$getInfo() )

## adding landsat 9 temperature band reduced to max value
ls9 = ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")
ls9f <- ls9$
  filterBounds(padova)$
  filter(ee$Filter$lt("CLOUD_COVER", 2))$
  filter(ee$Filter$dayOfYear(150, 280))$
  select("ST_B10")$
  max()$
  multiply(0.00341802)$
  add(149)$
  subtract(273.15)

## study area from Prof. Pirotti (polygons of 4 cities)
studyArea = ee$FeatureCollection("projects/spatial-logic-417507/assets/studyAreas")
## select only padova
padova = studyArea$filter( ee$Filter$eq("COMUNE", "Padova") );

## clip canopy heights
canopy_height_clipped = canopy_height$clip(padova)
## clip temperature
ls9fTemp = ls9f$clip(padova)
## export canopy
task_img <- ee_image_to_drive(
  description = "canopy",
  image = canopy_height_clipped,
  fileFormat = "GEO_TIFF",
  region = padova$geometry(),
  fileNamePrefix = "canopy",
  scale = 10
)$start()

## export temperature
task_img <- ee_image_to_drive(
  description = "temperature",
  image = ls9fTemp,
  fileFormat = "GEO_TIFF",
  region = padova$geometry(),
  fileNamePrefix = "temperature",
  scale = 30
) $start()


