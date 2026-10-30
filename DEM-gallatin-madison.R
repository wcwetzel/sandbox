library(sf)
library(terra)
library(elevatr)
library(tigris)
library(ggplot2)
library(patchwork)

options(tigris_use_cache = TRUE)

# =====================================================
# Study area
# =====================================================

xmin <- -111.354679
xmax <- -110.920550
ymin <- 45.268677
ymax <- 45.530943

bbox <- st_as_sf(
  st_as_sfc(
    st_bbox(
      c(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      crs = 4326
    )
  )
)

# =====================================================
# Download DEM
# =====================================================

dem <- get_elev_raster(
  locations = bbox,
  z = 11,
  clip = "locations"
)

dem <- rast(dem)

# force exact crop
dem <- crop(
  dem,
  ext(xmin, xmax, ymin, ymax)
)

# =====================================================
# Hillshade
# =====================================================

slope <- terrain(
  dem,
  v = "slope",
  unit = "radians"
)

aspect <- terrain(
  dem,
  v = "aspect",
  unit = "radians"
)

hill <- shade(
  slope,
  aspect,
  angle = 45,
  direction = 315
)

hill_df <- as.data.frame(
  hill,
  xy = TRUE,
  na.rm = TRUE
)

dem_df <- as.data.frame(
  dem,
  xy = TRUE,
  na.rm = TRUE
)

names(hill_df)[3] <- "hillshade"
names(dem_df)[3] <- "elevation"

# =====================================================
# Montana inset
# =====================================================

mt <- states(cb = TRUE)

mt <- mt[mt$STUSPS == "MT", ]

p1 <- ggplot() +
  geom_sf(
    data = mt,
    fill = "grey95",
    color = "grey40"
  ) +
  geom_sf(
    data = bbox,
    fill = NA,
    color = "red",
    linewidth = 1.2
  ) +
  theme_void() +
  labs(title = "Study area in Montana")

# =====================================================
# Elevation panel
# =====================================================

p2 <- ggplot() +

  geom_raster(
    data = hill_df,
    aes(x, y, fill = hillshade)
  ) +

  scale_fill_gradient(
    low = "black",
    high = "white",
    guide = "none"
  ) +

  ggnewscale::new_scale_fill() +

  geom_raster(
    data = dem_df,
    aes(x, y, fill = elevation),
    alpha = 0.65
  ) +

  scale_fill_gradientn(
    colours = c(
      "#f7f4f9",
      "#dfc27d",
      "#a6d96a",
      "#1a9850"
    ),
    name = "Elevation (m)"
  ) +

  coord_equal() +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold")
  ) +

  labs(title = "Southwest Montana study area")

# =====================================================
# Combine
# =====================================================

fig <- p1 + p2 +
  plot_layout(widths = c(1, 2.2))

ggsave(
  "study_area_map.png",
  fig,
  width = 10,
  height = 5,
  dpi = 600,
  bg = "white"
)

fig
