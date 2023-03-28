#Created by Justin Mathias, 2/23/23

library("easypackages")
libraries(c("openxlsx", "tidyverse"))

#Read in tec data
tec <- read.xlsx("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/Tassie_2023_TEC_Inventory.xlsx", sheet = "final_data")
unique(tec$FieldID)
