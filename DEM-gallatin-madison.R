library(sf)
library(terra)
library(elevatr)
#library(tigris)
library(hydrogeofetch)
library(ggplot2)
library(ggnewscale)
library(patchwork)
library(cowplot)

options(tigris_use_cache = TRUE)

# =====================================================
# Study area
# =====================================================

xmin <- -111.300000
xmax <- -110.930000
ymin <- 45.300000
ymax <- 45.500000

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
  z = 12,
  clip = "locations"
)

dem <- rast(dem)

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
    fill = 'black',
    color = "black"#,
    #linewidth = 0.8
  ) +
  theme_void()

# =====================================================
# Gallatin River
# =====================================================

rivers <- get_nhdplus(AOI = bbox)

major_rivers <- subset(rivers, streamorde >= 5)
# >= 5 captures just the Gallatin River
# Lower that number if I want to capture smaller rivers

# gallatin <- subset(
#   rivers,
#   gnis_name == "Gallatin River"
# )

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
    colours = rev(c(
      "#f7f4f9",
      "#dfc27d",
      "#a6d96a",
      "#1a9850"
    )),
    name = "Elevation (m)"
  ) +

  geom_sf(
    data = major_rivers,
    inherit.aes = FALSE,
    color = "#4a90e2",
    linewidth = 0.5,
    alpha=0.7,
    show.legend=FALSE
  ) +
  scale_linewidth(range=c(1, 2)) +

  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold")
  )

# =====================================================
# Combine
# =====================================================

fig <- p2 +
  inset_element(
    p1,
    left = 0.85,
    bottom = 0.65,
    right = 1.24,
    top = 1.05
  )

fig

ggsave(
  "study_area_map.png",
  fig,
  width = 10,
  height = 5,
  dpi = 600,
  bg = "white"
)
