#occurr10.r plotting species-area relationships for native and alien Coleoptera

library(tidyverse)
library(vegan)
library(ggrepel)
library(gridExtra)
library(corrplot)

# read numbers of species by family for native assemblages
native <- read_csv("/Users/andrewliebhold/sandy/SESYNC/insect establishment/Analysis/NatColAug08.csv")
colnames(native)  <- c("Family", "Australia", "N_America", "Hawaii", "Japan", "S_Korea",  "Galapagos", "NZ",  "Ogasawara",  "Okinawa", "Europe", "world", "suborder", "superfamily")
native.SF <- native %>%
  select(-Family, -suborder) %>% ## delete columns not for use
  group_by(superfamily) %>%  ## grouping by superfamily
  summarise_each(funs(sum))  ## sum up all in each superfamily in each region

# read numbers of species by family for non-native assemblages
alien <- read_csv("/Users/andrewliebhold/sandy/SESYNC/insect establishment/Analysis/EstColOct16.csv")
colnames(alien)  <- c("Family", "Australia", "N_America", "Hawaii", "Japan", "S_Korea",  "Galapagos", "NZ",  "Ogasawara",  "Okinawa", "Europe", "all_alien", "suborder", "superfamily")
alien.SF <- alien %>%
  select(-Family, -suborder) %>% ## delete columns not for use
  group_by(superfamily) %>%  ## grouping by superfamily
  summarise_each(funs(sum)) ## sum up all in each superfamily in each region

area<-c(8.E+06, 2.E+07, 3.E+04, 4.E+05, 1.E+05, 8.E+03, 3.E+05, 1.E+02, 1.E+03, 1.E+07)

alien1<-t(rbind(area,colSums(alien[,2:11]),area))
p1<-ggplot(alien1, aes(area, X)) 
+ geom_point()) 
+ geom_text(label=rownames(alien1))
+ scale_x_continuous(trans = "log10")
+ scale_y_continuous(trans = "log10")
+ geom_smooth(method=lm, se=FALSE)

alien2(<-t(rbind(area,alien[,"Curculionidae"]) ))
p1<-ggplot(alien2, aes(area, X)) 
+ geom_point()) 
+ geom_text(label=rownames(alien2))
+ scale_x_continuous(trans = "log10")
+ scale_y_continuous(trans = "log10")
+ geom_smooth(method=lm, se=FALSE)

alien3(<-t(rbind(area,alien[,"Staphylinidae"]) ))
p1<-ggplot(alien3, aes(area, X)) 
+ geom_point()) 
+ geom_text(label=rownames(alien3))
+ scale_x_continuous(trans = "log10")
+ scale_y_continuous(trans = "log10")
+ geom_smooth(method=lm, se=FALSE)

native1<-t(rbind(area,colSums(native[,2:11]),area))
p1<-ggplot(alien1, aes(area, X)) 
+ geom_point()) 
+ geom_text(label=rownames(native1))
+ scale_x_continuous(trans = "log10")
+ scale_y_continuous(trans = "log10")
+ geom_smooth(method=lm, se=FALSE)

native2(<-t(rbind(area,native[,"Curculionidae"]) ))
p1<-ggplot(alien5, aes(area, X)) 
+ geom_point()) 
+ geom_text(label=rownames(native2))
+ scale_x_continuous(trans = "log10")
+ scale_y_continuous(trans = "log10")
+ geom_smooth(method=lm, se=FALSE)

native3(<-t(rbind(area,native[,"Staphylinidae"]) ))
p1<-ggplot(alien6, aes(area, X)) 
+ geom_point()) 
+ geom_text(label=rownames(native3))
+ scale_x_continuous(trans = "log10")
+ scale_y_continuous(trans = "log10")
+ geom_smooth(method=lm, se=FALSE)

multiplot(p1, p4, p2, p5, p3, p4, p6 cols=2)

################################################################################
### Revised code based on above code
### by Rachael Blake 11/2/2020
################################################################################
# Plotting species-area relationships for native and alien Coleoptera
# load packages
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(grid)
library(cowplot)

# read in the data
# read numbers of species by family for native assemblages
native <- read_csv("nfs_data/data/raw_data/Coleoptera_data/NatColAug08.csv")
colnames(native)  <- c("Family", "Australia", "N_America", "Hawaii", "Japan", "S_Korea",  "Galapagos", "NZ",  "Ogasawara",  "Okinawa", "Europe", "world", "suborder", "superfamily")
# native.SF <- native %>%
#              select(-Family, -suborder) %>% ## delete columns not for use
#              group_by(superfamily) %>%  ## grouping by superfamily
#              summarise(across(where(is.double), sum))  ## sum up all in each superfamily in each region

# read numbers of species by family for non-native assemblages
alien <- read_csv("nfs_data/data/raw_data/Coleoptera_data/EstColOct16.csv")
colnames(alien)  <- c("Family", "Australia", "Europe", "Galapagos", "Hawaii", "Japan", "S_Korea", "NZ", "N_America", "Ogasawara", "Okinawa", "all_alien", "suborder", "superfamily")
# alien.SF <- alien %>%
#             select(-Family, -suborder) %>% ## delete columns not for use
#             group_by(superfamily) %>%  ## grouping by superfamily
#             summarise(across(where(is.double), sum)) ## sum up all in each superfamily in each region

# Organize your data for your plots
# this creates a data frame of the areas
area <- c(7692024, 10180000, 7880, 28311, 377975, 100210, 268020, 19819000, 106, 1207) # this creates a character vector
names(area) <- names(alien[,2:11])  # this names the elements of the character vector with the names of alien
area <- as.data.frame(area) %>% rownames_to_column("name") # this converts a named character vector to a data frame of 2 columns

alien1 <- alien[,2:11] %>% 
          summarise(across(where(is.double), sum)) %>% 
          pivot_longer(everything()) %>% 
          full_join(area) %>% 
          mutate(value_log = log10(value),    # calculate the log10 in your data rather than your plot
                 area_log = log10(area))

native1 <- native[,2:11] %>% 
           summarise(across(where(is.double), sum)) %>% 
           pivot_longer(everything()) %>% 
           full_join(area) %>% 
           mutate(value_log = log10(value),    # calculate the log10 in your data rather than your plot
                  area_log = log10(area))


# # fit linear log-log species-area linear models for each assemblage
# mod_alien1 <- lm(alien1$value_log ~ alien1$area_log, data=alien1)
# mod_alien2 <- lm(alien2$value_log ~ alien2$area_log, data=alien2)
# mod_alien3 <- lm(alien2$value_log ~ alien2$area_log, data=alien3)
# mod_native1 <- lm(native1$value_log ~ native1$area_log, data=native1)
# mod_native2 <- lm(native2$value_log ~ native2$area_log, data=native2)
# mod_native3 <- lm(native3$value_log ~ native3$area_log, data=native1)

family_function <- function(df, family){
                    
                   fam_area <-  df %>%
                                filter(Family == family) %>% 
                                select(-suborder, -superfamily, -Family) %>% 
                                pivot_longer(everything()) %>% 
                                full_join(area) %>% 
                                mutate(value_log = log10(value + 1),    # calculate the log10 in your data rather than your plot
                                       area_log = log10(area + 1))   
                       
                   return(fam_area)     
                   
                   }

alien2 <- family_function(alien, "Curculionidae")
alien3 <- family_function(alien, "Staphylinidae")
native2 <- family_function(native, "Curculionidae")
native3 <- family_function(native, "Staphylinidae")

# Some reasons why your code didn't work:
# ggplot expects a data frame; you gave it a matrix/list
# you have to put the plus sign at the end of a row, not the beginning
# you were using different super family names than your data

# Function to create scatterplots  
make_scatterplots <- function(df, title){
                     model <- lm(df$value ~ df$area, data = df)

                     p <- ggplot(df, aes_string(x = df$area, y = df$value)) +
                          geom_point() +
                          geom_text_repel(label = df$name) +  # from the package ggrepel; puts the text in good places
                          geom_smooth(method = lm, se = FALSE) + 
                          theme_cowplot() + 
                          xlab(expression(paste("Area (km" ^ "-2", ")"))) + ylab("No. Species") +
                          scale_x_log10() + 
                          scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000)) +
                          labs(title = title)
                     
                     p <- ggdraw(p) + draw_label(label = paste0("R-squared = ", round(summary(model)$r.squared, 3)), x = 0.85, y = 0.2) +
                          draw_label(label = paste0("Slope = ", round(summary(model)$coefficients[[2]], 6)), x = 0.85, y = 0.25) 

                     print(paste0("plot_", deparse(substitute(df))))
                     
                     return(p)
         
                     }


a1 <- make_scatterplots(alien1, "b)") 
a2 <- make_scatterplots(alien2, "d)")
a3 <- make_scatterplots(alien3, "f)")
n1 <- make_scatterplots(native1, "a)")
n2 <- make_scatterplots(native2, "c)")
n3 <- make_scatterplots(native3, "e)")
  
all_panels <- grid.arrange(n1, a1, n2, a2, n3, a3, ncol = 2)

