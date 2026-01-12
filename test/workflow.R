# workflow.R
# Script based on reddPrec README.md workflow

# -------------------------------------------------------------------------
# 1. Preparation of data
# -------------------------------------------------------------------------

# Ensure we are working in the 'test' directory to save all outputs there
if (dir.exists("test")) {
  setwd("test")
}

library(climaemet)
library(sf)
library(giscoR)
library(reshape)

# Note: Ensure your AEMET API key is set.
# aemet_api_key("MY_API_KEY", install=TRUE)

# Download daily precipitation observations
# Using the function aemet_daily_clim as requested
data_daily <- aemet_daily_clim(station = "all",
                               start = "2025-03-01",
                               end = "2025-03-31",
                               return_sf = TRUE)

# Crop to Iberian Peninsula and Balearic Islands
spain <- gisco_get_nuts(country = "Spain", nuts_level = "2")
spain <- spain[-which(spain$NAME_LATN == 'Canarias'), ]
data_daily <- st_crop(data_daily, spain)

# Organize daily data in a single matrix (stations x days)
dd <- cbind(as.data.frame(data_daily), st_coordinates(data_daily))
obs_pr <- cast(dd[, c('fecha', 'indicativo', 'prec')], fecha ~ indicativo)

# Handle "Ip" (Trace) values and convert to numeric
obs_pr <- apply(obs_pr, 2, function(x){
  x <- gsub(',', '.', x)
  x[x == 'Ip'] <- NA
  as.numeric(x)
})

# Save Raw Data
dates_vec <- seq.Date(as.Date('2025-03-01'), as.Date('2025-03-31'), by = 'day')
raw_out <- data.frame(Date = dates_vec, as.data.frame(obs_pr))
write.csv(raw_out, "prec_raw.csv", row.names = FALSE)

# Create stations data.frame
stations <- data.frame(ID = dd$indicativo, alt = dd$altitud, lon = dd$X, lat = dd$Y)
stations <- stations[-which(duplicated(stations$ID)), ]

# -------------------------------------------------------------------------
# 2. Creation of geospatial (raster) data
# -------------------------------------------------------------------------

library(elevatr)
library(terra)

# Get elevation raster
dem <- get_elev_raster(data_daily, z = 5)
dem <- rast(dem)
dem <- crop(dem, spain)
dem <- mask(dem, spain)
dem[dem < 0] <- 0

# Calculate topographic variables
asp <- terra::terrain(dem, v = 'aspect')
aspectcosine <- cos(asp)
aspectsine <- sin(asp)
dist2coast <- costDist(dem)
lon <- rast(cbind(crds(dem), crds(dem)[, 1]), type = 'xyz', crs = 'EPSG:4326')
lat <- rast(cbind(crds(dem), crds(dem)[, 2]), type = 'xyz', crs = 'EPSG:4326')
roughness <- terra::terrain(dem, v = 'roughness')
slope <- terra::terrain(dem, v = 'slope')
tpi <- terra::terrain(dem, v = 'TPI')
tri <- terra::terrain(dem, v = 'TRI')

covs <- c(aspectcosine, aspectsine, dist2coast, dem, lon, lat, roughness, slope, tpi, tri)
names(covs) <- c('aspectcosine', 'aspectsine', 'dist2coast',
                 'elevation', 'longitude', 'latitude', 'roughness',
                 'slope', 'tpi', 'tri')

# PCA of topo variables
pca_cvs <- terra::princomp(covs[[-c(4:6)]])
pca_cvs <- terra::predict(covs[[-c(4:6)]], pca_cvs, index = 1:4)
names(pca_cvs) <- c("pc1", "pc2", "pc3", "pc4")
pdf("pca_cvs.pdf")
plot(pca_cvs)
dev.off()

# Extract PCs values to stations
stations <- vect(stations, geom = c('lon', 'lat'), crs = 'EPSG:4326', keepgeom = TRUE)
e <- terra::extract(pca_cvs, stations, ID = F)
stations <- cbind(stations, e)
stations <- as.data.frame(stations)

# Remove stations with no overlapping raster data
stations <- stations[complete.cases(stations), ]
obs_pr <- obs_pr[, match(stations$ID, colnames(obs_pr))]

# Visualization (Optional)
library(ggplot2)
st <- vect(stations, geom = c('lon', 'lat'), crs = 'EPSG:4326', keepgeom = TRUE)
st$ndata <- colSums(!is.na(obs_pr)) * 100 / nrow(obs_pr)
dem_df <- as.data.frame(dem, xy = TRUE)
colnames(dem_df)[3] <- "elevation"
st_sf <- st_as_sf(st)
st_sf$prec_acum <- colSums(obs_pr, na.rm = TRUE)

p_map <- ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(name = "Elevation (m)", low = "lightgreen", high = "darkgreen") +
  geom_sf(data = st_transform(st_sf, crs = st_crs(spain)), aes(size = prec_acum), color = "black", alpha = 0.7) +
  scale_size_continuous(name = "Cumulated PCP (mm)") +
  geom_sf(data = spain, fill = NA, color = "black") +
  coord_sf(crs = st_crs(spain)) +
  theme_minimal() +
  theme(legend.position = "right")
ggsave("stations_map.pdf", p_map)

# Environmental predictors
env <- c(dem, lon, lat, pca_cvs)
names(env)[1:3] <- c("alt", "lon", "lat")
pdf("env_predictors.pdf")
plot(env)
dev.off()

# -------------------------------------------------------------------------
# 3. Quality control
# -------------------------------------------------------------------------

library(reddPrec)
library(doParallel)
library(foreach)

# Robustly determine the path to the 'R' directory
# This handles execution from project root or 'test' directory
r_dir <- "R"
if (!dir.exists(r_dir) && dir.exists(file.path("..", r_dir))) {
  r_dir <- file.path("..", r_dir)
}

#source(file.path(r_dir, "learner_glm.R"))
#source(file.path(r_dir, "learner_nn.R"))
#source(file.path(r_dir, "learner_svm.R"))
#source(file.path(r_dir, "learner_xgboost.R"))
#source(file.path(r_dir, "predpoint.R"))
#source(file.path(r_dir, "predday.R"))
#source(file.path(r_dir, "gridPcp.R"))
#source(file.path(r_dir, "qcFirst.R"))
#source(file.path(r_dir, "qcLast.R"))
#source(file.path(r_dir, "qcPrec.R"))
#source(file.path(r_dir, "fillData.R"))
#source(file.path(r_dir, "gapFilling.R"))
#source(file.path(r_dir, "standardization.R"))
#source(file.path(r_dir, "stand_qq.R"))
#source(file.path(r_dir, "stand_ratio.R"))

# Run Quality Control
qcdata <- qcPrec(prec = obs_pr,
                 sts = stations,
                 model_fun = learner_xgboost,
                 crs = 'EPSG:4326',
                 coords = c('lon', 'lat'),
                 coords_as_preds = TRUE,
                 neibs = 15,
                 thres = NA,
                 qc = 'all',
                 qc3 = 10,
                 qc4 = c(0.99, 5),
                 qc5 = c(0.01, 0.1, 5),
                 ncpu = 22) # Adjust ncpu as needed

# Save QC Data
qc_out <- data.frame(Date = dates_vec, as.data.frame(qcdata$cleaned))
write.csv(qc_out, "prec_qc.csv", row.names = FALSE)

# Check flagged values
allcodes <- as.numeric(as.matrix(qcdata$codes))
flagged <- round(table(allcodes) * 100 / length(allcodes), 2)
print(flagged)

# -------------------------------------------------------------------------
# 4. Gap filling
# -------------------------------------------------------------------------

# Run Gap Filling
gf_res <- gapFilling(prec = qcdata$cleaned,
                     sts = stations,
                     model_fun = learner_xgboost,
                     dates = seq.Date(as.Date('2025-03-01'),
                                      as.Date('2025-03-31'),
                                      by = 'day'),
                     stmethod = 'ratio',
                     ncpu = 22, # Adjust ncpu as needed
                     thres = NA,
                     neibs = 15,
                     coords = c('lon', 'lat'),
                     crs = 'EPSG:4326',
                     coords_as_preds = TRUE,
                     window = 11)

# Compare results (Visualization)
library(patchwork)
library(scales)
library(hexbin)

# Daily and monthly data
pearson_daily <- round(cor(gf_res$obs, gf_res$st_pred, use = "pairwise.complete.obs"), 2)
obs <- gf_res[complete.cases(gf_res), ]
o <- aggregate(obs$obs, by = list(obs$ID), FUN = sum)
p <- aggregate(obs$st_pred, by = list(obs$ID), FUN = sum)
pearson_monthly <- round(cor(o[, 2], p[, 2], use = "pairwise.complete.obs"), 2)

daily_df <- gf_res
monthly_df <- data.frame(obs_sum = o[, 2], pred_sum = p[, 2])

# Axes & legend
all_values <- c(daily_df$obs, daily_df$st_pred, monthly_df$obs_sum, monthly_df$pred_sum)
axis_min <- floor(min(all_values, na.rm = TRUE))
axis_max <- ceiling(max(all_values, na.rm = TRUE))
hex_daily <- hexbin(daily_df$obs, daily_df$st_pred, xbins = 60)
hex_monthly <- hexbin(monthly_df$obs_sum, monthly_df$pred_sum, xbins = 60)
max_count <- max(c(hex_daily@count, hex_monthly@count))

# Plotting logic is extensive, refer to README for the full ggplot code if needed.
# Included minimal plotting setup above.

p1 <- ggplot(daily_df, aes(x=obs, y=st_pred)) +
  geom_hex(bins=60) +
  geom_abline(intercept=0, slope=1, col="red") +
  labs(title="Daily: Obs vs Pred", x="Observed", y="Predicted") +
  theme_minimal()

p2 <- ggplot(monthly_df, aes(x=obs_sum, y=pred_sum)) +
  geom_hex(bins=60) +
  geom_abline(intercept=0, slope=1, col="red") +
  labs(title="Monthly: Obs vs Pred", x="Observed", y="Predicted") +
  theme_minimal()

ggsave("gap_filling_daily.pdf", p1)
ggsave("gap_filling_monthly.pdf", p2)

# -------------------------------------------------------------------------
# 5. Gridding
# -------------------------------------------------------------------------

# Prepare data for gridding
recs <- gf_res$obs
recs[is.na(recs)] <- gf_res$st_pred[is.na(recs)]
rec <- data.frame(date = gf_res$date, ID = gf_res$ID, pred = recs)
rec <- cast(rec, date ~ ID)

# Save Filled Data
write.csv(rec, "prec_filled.csv", row.names = FALSE)

rec <- rec[, -1]

day21 <- as.numeric(rec[21, ])

# Run Gridding for a specific day
gridPcp(prec = day21,
        grid = env,
        dyncovars = NULL,
        sts = stations[, 1:4],
        model_fun = learner_xgboost,
        dates = as.Date('2025-03-21'),
        ncpu = 22, # Adjust ncpu as needed
        thres = NA,
        neibs = 15,
        coords = c('lon', 'lat'),
        crs = 'EPSG:4326',
        coords_as_preds = TRUE,
        dir_name = "grid_test")

# Plot the result
if (file.exists('./pred_grid_test/20250321.tif')) {
  pre <- terra::rast('./pred_grid_test/20250321.tif')
  title <- "Daily Precipitation 2025-03-21"
  vals <- c(-1, 0, 1, 2, 3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 1000)
  legnd <- paste(vals[1:21], vals[2:22], sep = '-')
  legnd[1] <- '0'
  legnd[21] <- '>110'

  cols <- c('#ffffff', '#ffd576', '#ffe685', '#fff0b0',
            '#fffecf', '#b2fdad', '#a2eda6', '#61cf87',
            '#00c56a', '#b2f1fe', '#72f0fe', '#1cdefe',
            '#31c3fe', '#6e90fe', '#b77dff', '#ca8eff',
            '#dc9ae7', '#f4a1f3', '#fec1ff', '#ffdcfe', '#e7e7e7')

  m <- cbind(vals[1:21], vals[2:22], 1:21)
  d <- classify(pre, m)
  fadd <- function() plot(vect(spain), add = T)
  pdf("grid_20250321.pdf")
  plot(d, type = "interval", breaks = 1:22, col = cols, plg = list(legend = legnd),
       main = title, fun = fadd)
  dev.off()
}
