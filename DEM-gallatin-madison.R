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

xmin <- -111.350000
xmax <- -111.050000
ymin <- 45.400000
ymax <- 45.4750000

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

d = read.csv('2026-FieldSite_LatLon.csv')

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
# Rivers and lakes
# =====================================================

rivers <- get_nhdplus(AOI = bbox)
rivers <- st_intersection(rivers, bbox)

major_rivers <- subset(rivers, streamorde >= 3)
# >= 5 captures just the Gallatin River
# Lower that number if I want to capture smaller rivers

lakes <- get_waterbodies(AOI = bbox)
lakes <- st_intersection(lakes, bbox)


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
  ) +

  geom_sf(
    data = lakes,
    inherit.aes = FALSE,
    fill = "#6baed6",
    color = "#2171b5",
    linewidth = 0.2,
    show.legend = FALSE
  ) +

  geom_point(data=d, aes(x=lon, y=lat))

# =====================================================
# Combine
# =====================================================

fig <- p2 +
  inset_element(
    p1,
    left = 0.7,
    bottom = 0.0,
    right = 1.0,
    top = 0.4
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
