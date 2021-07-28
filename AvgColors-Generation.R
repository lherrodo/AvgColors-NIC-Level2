#install the necessary packages. only need to run this once
install.packages(c('sf', 'magick', 'dplyr'))

#load in the packages. you need to do this every time you open a fresh R session
library(magick)
library(sf)
library(dplyr)

#==============================================================================#
# Get your shapefile loaded and prepped:
    
    

#set your working directory, the folder where all your files will live
setwd("C:/Users/admin/Documents/R/Github-Rep/AvgColors-NIC-Level2/data/")




#create the subfolders that you'll need
dir.create(file.path(getwd(), "satellite"), showWarnings = FALSE)
dir.create(file.path(getwd(), "shapes"), showWarnings = FALSE)

#load in your shapefile. I'm using the US states one from here:
#https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
shapes <- read_sf("Deptos-Colors.shp")

#we need to separate out the shapefile into individual files, one per shape
for (i in 1:nrow(shapes)) {
    shape <- shapes[i, ]
    write_sf(shape, paste0("./shapes/", shape$nv2_nbre, ".shp"), delete_datasource = TRUE)
}

#==============================================================================#
#Download the satellite imagery


#load in your shapefile. I'm using the US states one from here:
#https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
shapes <- read_sf("Deptos-Colors.shp")

#for each shape in the shapefile, download the covering satellite imagery
for (i in 1:nrow(shapes)) {
    shape <- shapes[i, ] %>% st_transform(4326)
    box <- st_bbox(shape)
    
    url <- paste0("https://tiles.maps.eox.at/wms?service=wms&request=getmap&version=1.1.1&layers=s2cloudless-2019&",
                  "bbox=", box$xmin-.05, ",", box$ymin-.05, ",", box$xmax+.05, ",", box$ymax+.05,
                  "&width=4096&height=4096&srs=epsg:4326")
    
    #note: this assumes your shapefile has a column called NAME. you'll need to change this to a 
    #different col if it doesn't
    download.file(url, paste0("./satellite/", gsub("/", "", shape$nv2_nbre), ".jpg"), mode = "wb")
}



#==============================================================================#
#time to create the GDAL script
if (file.exists("script.txt")) { file.remove("script.txt")}

#first, georeference the images - convert them from flat jpgs to tifs that know where on the #globe the image is
for (i in 1:nrow(shapes)) {
    shape <- shapes[i, ]
    box <- st_bbox(shape)
    
    file_origin <- paste0(getwd(), '/satellite/', shape$nv2_nbre, '.jpg')
    file_temp <- paste0(tempdir(), "/", shape$nv2_nbre, '.jpg')
    file_final <- paste0(tempdir(), "/", shape$nv2_nbre, '_ref.tif')
    
    cmd1 <- paste0("gdal_translate -of GTiff",
                   " -gcp ", 0, " ", 0, " ", box$xmin, " ", box$ymax,
                   " -gcp ", 4096, " ", 0, " ", box$xmax, " ", box$ymax,
                   " -gcp ", 0, " ", 4096, " ", box$xmin, " ", box$ymin,
                   " -gcp ", 4096, " ", 4096, " ", box$xmax, " ", box$ymin,
                   ' "', file_origin, '"',
                   ' "', file_temp, '"')
    
    cmd2 <- paste0("gdalwarp -r near -tps -co COMPRESS=NONE  -t_srs EPSG:4326",
                   ' "', file_temp, '"',
                   ' "', file_final, '"')
    
    write(cmd1,file="script.txt",append=TRUE)
    write(cmd2,file="script.txt",append=TRUE)
}

#next, crop the georeferenced tifs to the outlines in the individual shapefiles
for (i in  1:nrow(shapes)) {
    shape <- shapes[i, ]
    file_shp <- paste0(getwd(), '/shapes/', shape$nv2_nbre, '.shp')
    file_orig <-  paste0(tempdir(), "/", shape$nv2_nbre, '_ref.tif')
    file_crop <-  paste0(tempdir(), "/", shape$nv2_nbre, '_crop.tif')
    
    cmd <- paste0("gdalwarp -cutline",
                  ' "', file_shp, '"',
                  " -crop_to_cutline -of GTiff  -dstnodata 255",
                  ' "', file_orig, '"',
                  ' "', file_crop, '"',
                  " -co COMPRESS=LZW -co TILED=YES --config GDAL_CACHEMAX 2048 -multi")
    
    write(cmd,file="script.txt",append=TRUE)
}

#reproject the shapes to a reasonable projection, so they're not as warped and stretched
for (i in  1:nrow(shapes)) {
    shape <- shapes[i, ]
    
    center <- st_centroid(shape) %>% st_coordinates
    utm <- as.character(floor((center[1] + 180) / 6) + 1)
    if (nchar(utm) == 1) {utm <- paste0("0", utm)}
    if (center[2] >=0) {
        epsg <- paste0("326", utm)
    } else {
        epsg <- paste0("327", utm)
    }
    
    file_orig <- paste0(tempdir(), "/", shape$nv2_nbre, '_crop.tif')
    file_mod <- paste0(tempdir(), "/", shape$nv2_nbre, '_reproj.tif')
    
    cmd <- paste0("gdalwarp -t_srs EPSG:", epsg,
                  ' "', file_orig, '"',
                  ' "', file_mod, '"')
    
    write(cmd,file="script.txt",append=TRUE)
}


#==============================================================================#
#RUN GDAL SCRIPT


#==============================================================================#
#Move the files we just created back into the working directory

#move the files from temp back to the wd
files <- list.files(tempdir(), pattern = "_reproj.tif")
for (i in 1:length(files)) {
    file.rename(from=paste0(tempdir(), "/", files[i]),to= paste0("C:/Users/admin/Documents/R/Github-Rep/AvgColors-NIC-Level2/data/reproj/", files[i]))
}
#MUST CREATE reproj folder!!!


#==============================================================================#
#get the average color of each image

#get the average color of each file
files <- list.files("./reproj/")
colors <- NULL

for (i in 1:length(files)) {
    #remove the white background of the image
    img <- image_read(paste0("./reproj/", files[i]))
    img <- image_transparent(img, "#ffffff", fuzz = 0)
    
    #convert to a 1x1 pixel as a trick to finding the avg color. ImageMagick will average the 
    #colors out to just 1.
    img <- image_scale(img,"1x1!")
    
    #get the color of the pixel
    avgcolor <- image_data(img, channels = "rgba")[1:3] %>% paste0(collapse="")
    
    #save it in a running list
    colors <- bind_rows(colors, data.frame(name = gsub("_reproj.tif", "", files[i]), color = paste0("#", avgcolor)))
}

#save the color file if you need it later
write.csv(colors, "colors.csv", row.names = F)


#============================================================================
#plot it all pretty

#install the necessary packages. only need to run this once
install.packages(c( 'ggplot2', 'ggthemes'))

#load in the packages. you need to do this every time you open a fresh R session
library(sf)
library(ggplot2)
library(ggthemes)
library(dplyr)

#set your wd
setwd("C:/Users/admin/Documents/R/Github-Rep/AvgColors-NIC-Level2/data")

#load the data
shapes <- read_sf("Deptos-Colors.shp")
colors <- read.csv("colors.csv")

#you'll need to change the join column names here if they're different than mine
shapes <- inner_join(shapes, colors, by = c("nv2_nbre" = "name")) 

#transform the shapefile to a reasonable projection
#if you're unsure here, google "epsg code" + "your region name" to find a good code to use
shapes <- st_transform(shapes, 4326)

#for example purposes only, I'm removing Alaska, Hawaii, and PR
shapes <- subset(shapes, !(STUSPS %in% c("AK", "HI", "PR")))

#plot it
ggplot() + theme_map() +
    geom_sf(data = shapes, aes(fill = I(color)), color = alpha("#fffbf2", .1)) +
    theme(plot.background = element_rect(fill = '#fffbf2', colour = '#fffbf2'))

#save it
ggsave("avgcolors.png", width = 7, height = 9, units = "in", dpi = 500)