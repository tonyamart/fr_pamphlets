library(tidyverse)
#install.packages("pdftools")
library(pdftools)

setwd("~/Documents/ds/PaPa/large_files/galerie_universelle/segments/")

# prepare table

meta <- read.csv("galerie_universelle - 1787_8_vol.csv")
glimpse(meta)

files <- meta %>% 
  filter(pages_pdf != "") %>% 
  # extract start and end pages
  mutate(page_start = str_extract(pages_pdf, "^\\d+"),
         page_end = str_extract(pages_pdf, "\\d+$"),
         folder = paste0("vol_", volume, "//"),
         pdf = paste0(folder, "V", volume, ".pdf")) %>% 
  
  # add segment id
  group_by(volume) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  
  # write path for segments
  mutate(outfile = paste0(folder, "V", volume, "_seg_", id, "_", pages_pdf, ".pdf"),
         page_start = as.numeric(page_start),
         page_end = as.numeric(page_end)) %>% 
  select(folder, pdf, pages_pdf, page_start, page_end, id, outfile)

head(files)

# main loop

t <- NULL
fh <- NULL

for (i in 1:length(unique(files$pdf))) {
  
  infile <- unique(files$pdf)[i]
  print(infile)
  
  print(pdf_length(infile))
  
  t <- files %>% filter(pdf == infile)
  
  st <- t %>% pull(page_start)
  end <- t %>% pull(page_end)
  
  fh <- t %>% pull(outfile)
  
  for (j in 1:length(fh)) {
    
    if (length(st) == length(end) & length(fh) == length(st)) {
      
      pdf_subset(infile, pages = st[j]:end[j], out = fh[j])
      
    }
  }
}


#dir()


# outfile <- paste0(prefix, infile)
# pdf_subset(infile, pages = st:end, out = outfile)

# vol 1
# infile <- "vol_4/V4.pdf"
# prefix <- "V1_seg_"
# 
# num <- pdf_length(infile)
# st <- c(19, 83, 105, 164, 230, 288, 354, 428, 486, 555, 559)
# end <- c(82, 102, 161, 225, 285, 352, 426, 484, 549, 557, 623)

# length(st) == length(end)
# 
# 
# for (i in 1:length(st)) {
#   
#   outfile <- paste0(prefix, i, "_p", st[i], "-", end[i], ".pdf")
#   pdf_subset(infile, pages = st[i]:end[i], out = outfile)
#   
# }

