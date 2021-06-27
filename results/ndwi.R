Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, leaflet, sp, raster, rgdal, ggmap, tmaptools)
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, ggplot2)

ndwi2000 <- read.csv("results/data//ndwi_2000_new.csv")
ndwi2010 <- read.csv("results/data//ndwi_2010_new.csv")

colnames(ndwi2000) <- paste0("Col", 1:ncol(ndwi2000))
colnames(ndwi2010) <- paste0("Col", 1:ncol(ndwi2010))

?paste0

ndwi2000_2010 <- rbind(ndwi2000, ndwi2010)

write.csv(ndwi2000_2010, "results/data//ndwi.csv")



ndwi <- read.csv("results/data//ndwi.csv")
ndwi$Col1 <- as.Date(ndwi$Col1, "%Y")

lm_ndwi <- lm(ndwi$Col1 ~ ndwi$Col2)
summary(lm_ndwi)
plot(lm_ndwi)
#plot
pndwi <- ggplot(ndwi, aes(x=Col1, y=Col2)) +
  geom_point(size = 0.6) +
  theme_light() +
  geom_smooth(method=lm, se=TRUE, fullrange = TRUE, aes(color ="red") )  +
  xlab("Year") +
  ylab("NDWI") +
  ggtitle("NDWI values over time in the Aral Lake Basin") +
  theme( axis.text=element_text(size=12),
         axis.title=element_text(size=13),
         plot.title = element_text(size=16),
         legend.position = c(0.85, 0.9),
         legend.direction = "horizontal") +
  scale_color_identity(name = "",
                       labels = c("regression line"),
                       guide = "legend") +
  annotate(geom="text",x=2005, y=0.8,label="R² = 0.007") +
  annotate(geom="text",x=2005,y=0.75,label="p-value << 0.001")

print(pndwi)

ggsave("C:/Users/Dell/Desktop/UniMaterial/MOD3project/report/results/data//ndwi.png",
       plot = last_plot(),
       dpi = 900,
       width = 7,
       height = 4)















