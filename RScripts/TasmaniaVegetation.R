#Created by Justin Mathias, 2/23/23

library("easypackages")
libraries(c("openxlsx", "tidyverse"))

tco <- read.xlsx("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/Tassie_2023_TEC_Inventory.xlsx")
tco
unique(tco$Species)




library("measurements")
conv_unit(860, "metric_ton", "Mg")
