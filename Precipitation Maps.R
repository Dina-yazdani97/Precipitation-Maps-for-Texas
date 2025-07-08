#1 Loading Packages --------------------------------------------------------
pacman::p_load(
  giscoR, terra, tidyverse, rayshader, sf, classInt, raster, 
  lattice, extrafont, rasterVis
)

# 2 Loading ShpFile -------------------------------------------------------

texas_boundary <- st_read("/home/user/shpfile/America.shp") %>% 
  st_transform(4326)
texas_boundary_buffered <- st_buffer(texas_boundary, dist = 0.1)


# 3 Loading Digital Elevation Model ---------------------------------------

Elevation <- rast("/home/user/Elavation.nc")


# 4 Loading Tif Files ----------------------------------------------------

rain_files <- list.files("/home/user/TifFiles/", pattern = "\\.tif$", full.names = TRUE)
rain_stack <- rast(rain_files)
texas_bbox <- st_bbox(texas_boundary_buffered)
Elevation_cropped <- crop(Elevation, texas_bbox)
rain_cropped <- crop(rain_stack, texas_bbox)


# 5 color Table -----------------------------------------------------------

Elevation_colors <- colorRampPalette(c("#006400", "#CDB79E", "#8B4500"))(100)
ylgnbu_colors <- c("#1E90FF", "#ADD8E6", "#FFFFFF", "#FFC1C1", "#F08080", "#EE3B3B", "#8B1A1A")
rain_palette <- colorRampPalette(ylgnbu_colors)(35)


# 6 Legend Settings -------------------------------------------------------

min_value <- max(min(values(rain_cropped), na.rm = TRUE), 20)
max_value <- max(values(rain_cropped), na.rm = TRUE)
guide_values <- seq(min_value, max_value, length.out = 35)



# 7 Stack -----------------------------------------------------------------

rain_raster <- stack(rain_cropped)
Elevation_raster <- stack(Elevation_cropped)


#x11(width = 30, height = 20)


# 8 Export Plot -----------------------------------------------------------

png("~/plot.png", 
    width = 30, height = 20, units = "cm", res = 300, 
    type = "cairo", family = "Times New Roman")

plot <- levelplot(rain_raster, 
                  layers = 1:nlayers(rain_raster),
                  margin = FALSE,
                  col.regions = rain_palette,
                  at = guide_values,
                  names.attr = names(rain_cropped),
                  layout = c(3, 2),
                  main = list(label = "Precipitation Maps for Texas", 
                              cex = 1.5, 
                              fontfamily = "Times New Roman", 
                              fontface = "bold"),
                  scales = list(
                    x = list(
                      draw = TRUE, 
                      rot = 0,      
                      fontfamily = "Times New Roman",
                      alternating = c(1, 1),
                      tck = c(0.5, 0),
                      cex = 0.8 
                    ),
                    y = list(
                      draw = TRUE, 
                      rot = 0,      
                      fontfamily = "Times New Roman",
                      alternating = c(1, 1),
                      tck = c(0.5, 0),
                      cex = 0.8  
                    )
                  ),
                  xlab = NULL,  
                  ylab = NULL, 
                  par.settings = list(
                    strip.background = list(col = "white"),
                    strip.border = list(col = "black"),
                    axis.line = list(col = "black"),
                    par.main.text = list(fontfamily = "Times New Roman", cex = 1.5, fontface = "bold"),
                    par.sub.text = list(fontfamily = "Times New Roman", fontface = "bold"),
                    par.xlab.text = list(fontfamily = "Times New Roman", fontface = "bold"),
                    par.ylab.text = list(fontfamily = "Times New Roman", fontface = "bold"),
                    add.text = list(fontfamily = "Times New Roman", fontface = "bold")
                  )) +
  latticeExtra::layer(sp.polygons(as(texas_boundary, "Spatial"), 
                                  col = "black", 
                                  lwd = 0.8)) +
  latticeExtra::as.layer(
    levelplot(Elevation_raster, 
              col.regions = Elevation_colors, 
              alpha.regions = 0.3),
    under = TRUE
  )

print(plot)
dev.off() 
