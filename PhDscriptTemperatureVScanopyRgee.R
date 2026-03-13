library(rgee)
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
  mask(canopy_height$gt(2))$
  clip(padova_geom)$
  unmask()

# -------------------------------------------------------
# Sample pixels
# -------------------------------------------------------

samples <- stack$sample(
  region = padova_geom,
  scale = 50,
  numPixels = 1000,
  geometries = TRUE
)

# Inspect
print(samples)