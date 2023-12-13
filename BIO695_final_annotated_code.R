### BIO695 final code. All data can be found on github
# Data preprocessing: -----------------------------------------------------

### Load packages
library(tidyverse) ## data manipulation and modeling
library(ggplot2)   ## plots
library(lubridate) ## dealing with time
library(ggpubr) ## plots
library(glatos) ## interpolate points between detections
library(sf)     ## For reading in shp
library(raster) ## For reading in raster
library(tidymodels) ## For models
library(ggspatial)  ## add a scale bar

setwd("C:/Users/lmcgi/OneDrive/Desktop/BIO_695/Final project")

### Read in data
det_raw <- read_csv("Aug_21_detections_raw.csv") ## raw detection data
str(det_raw)

#animals
animals <- read_csv("Bio695_Filtered_animal_data.csv") ## animal data

#receivers w/ habitat type
receivers <- read_csv("Bio695_filtered_rec_loc.csv")

# Merge animal info and detections
det <- merge(det_raw, animals, by = "Transmitter ID")

# filter detections to just LMB
bass_det <- det %>% filter(Species == "Largemouth_bass")

# Merge detections with receiver info
bass_det <- rename(bass_det, "Receiver_Serial" = `Receiver Serial`)
bass_det <- merge(bass_det, receivers, by = "Receiver_Serial")


## make CDT column
bass_det$detection_timestamp_CDT <- with_tz(bass_det$`Date Time`,
                                            tzone = "America/Chicago") 

##### remove unnecessary columns. Get final dataset #####
bass_det_final <- subset(bass_det, select = c("Receiver_Serial", "Tag_number", 
                                              "Lat", "Long", "habitat_type_2", 
                                              "detection_timestamp_CDT"))

bass_det_final <- rename(bass_det_final, "habitat_type" = habitat_type_2)

#write_csv(bass_det_final, "bass_det_final.csv")


# Start here if using data downloaded from github:  -----------------------

# Read in bass detection df
bass_det_final <- read_csv("bass_det_final.csv")

# Change timezone. diplyr reads it in as GMT
bass_det_final$detection_timestamp_CDT <- with_tz(bass_det_final$detection_timestamp_CDT,
                                            tzone = "America/Chicago")

## Get hourly summary for each receiver

start_times <- seq.POSIXt(from = as.POSIXct("2023-06-12 00:00:00", tz = "America/Chicago"),
                          to = as.POSIXct("2023-08-21 23:00:00", tz = "America/Chicago"),
                          by = "1 hour")
end_times <- start_times + 3600  # Add 3600 seconds (1 hour) to get end times

## Blank df
summary_df <- data.frame(
  Start_Time = start_times,
  End_Time = end_times,
  Receiver_Serial = numeric(length(start_times)),
  Unique_tag_IDs = numeric(length(start_times)),
  Detections = numeric(length(start_times))
)

# Count individuals detected per hour for each Receiver_Serial ## Takes a long time
for (i in 1:length(start_times)) {
  for (receiver in unique(bass_det_final$Receiver_Serial)) {
    block_subset <- bass_det_final %>%
      filter(detection_timestamp_CDT >= start_times[i] & detection_timestamp_CDT < end_times[i] & Receiver_Serial == receiver)
    unique_tag_ids <- length(unique(block_subset$Tag_number))
    detections <- nrow(block_subset)
    
    summary_df[nrow(summary_df) + 1, ] <- list(start_times[i], end_times[i], receiver, unique_tag_ids, detections)
  }
}

## Remove serial 0
summary_df2 <- summary_df %>% filter(Receiver_Serial != 0)

## For some reason, there's duplicates for 6-12 through 6-15
## Remove those rows
summary_df3 <- summary_df2 %>% distinct()

## Remove rows before june 21 (7 days after tagging) 
summary_df4 <- subset(summary_df3, Start_Time >= as.POSIXct("2023-06-21 00:00:00", tz = "America/Chicago"))


## Remove Franklin-Orleans (490324) and Loomis st (139251) observations before 7-10 (receivers weren't deployed until then)
summary_df5 <- summary_df4 %>%
  filter(!(Start_Time < as.POSIXct("2023-07-10 00:00:00", tz = "America/Chicago") & (Receiver_Serial %in% c("490324", "139251"))))

## Add in habitat data again
# df of just serial and habitat type
df <- subset(receivers, select = c("Receiver_Serial", "habitat_type_2"))
df <- df %>% rename(habitat_type = habitat_type_2)

#### Final detection summary by habitat
summary_df_final <- merge(summary_df5, df, by = "Receiver_Serial")

# read in temp and DO data: -----------------------------------------------
setwd("C:/Users/lmcgi/OneDrive/Desktop/BIO_695/Final project/BIO695_MWRD_CSVs")

## 
listfiles <- list.files(path = ".", full.names = T)

##Read in initial csv
csv1 <- read_csv(listfiles[1])

### Read in and bind all temp and DO csvs
for(f in 2:length(listfiles)){
  csv <- read_csv(listfiles[f])
  csv1 <- rbind(csv1, csv)
}

## Force tz
csv1$Start_Time <- force_tz(csv1$Date_Time, tzone = "America/Chicago")

## Round down to nearest hour
csv1$Start_Time <-floor_date(csv1$Start_Time, unit =  "hour")

## Remove unnecessary columns
csv1 <- subset(csv1, select = -c(Date_Time, DO_Sat))

## merge with detections summary
det_habitat_summary <- merge(summary_df_final, csv1, 
                             by = c("Receiver_Serial", "Start_Time"))

#### Now have two df to work with "bass_det" and "det_habitat_summary"


# Fish detection maps: ----------------------------------------------------

### First read in transition layer and caws map
shapefile_path <- "C:/Users/lmcgi/OneDrive/Desktop/BIO_695/Final Project/CAWSoutline/CAWSoutline.shp"
CAWS <- st_read(shapefile_path)
plot(CAWS) ## read in shapefile of CAWS

## Choosing 4 bass to show detections. rename columns
bass_17916 <- bass_det_final %>% filter(Tag_number == 17916) %>% 
  group_by(Receiver_Serial, Lat, Long) %>% 
  summarize(detections = n())

bass_17918 <- bass_det_final %>% filter(Tag_number == 17918) %>% 
  group_by(Receiver_Serial, Lat, Long) %>% 
  summarize(detections = n())

bass_17962 <- bass_det_final %>% filter(Tag_number == 17962) %>% 
  group_by(Receiver_Serial, Lat, Long) %>% 
  summarize(detections = n())

bass_17966 <- bass_det_final %>% filter(Tag_number == 17966) %>% 
  group_by(Receiver_Serial, Lat, Long) %>% 
  summarize(detections = n())

## plot points
CAWS_wgs84 <- st_transform(CAWS, crs = 4326) ## Convert coordinate system to WGS84

bass_17916_plot <- ggplot(CAWS_wgs84)+
  geom_sf()+
  geom_point(data = bass_17916, 
             aes(x = Long, y = Lat, size = detections), color = "blue")+
  coord_sf(xlim = c(-87.71, -87.61), ylim = c(41.82, 41.92))+ ## Change bounds of plot 
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Bass 17916 detections")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))

bass_17918_plot <- ggplot(CAWS_wgs84)+
  geom_sf()+
  geom_point(data = bass_17918, 
             aes(x = Long, y = Lat, size = detections), color = "red")+
  coord_sf(xlim = c(-87.71, -87.61), ylim = c(41.82, 41.92))+ ## Change bounds of plot 
  ylab("Latitude")+
  ggtitle("Bass 17918 detections")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))


bass_17962_plot <- ggplot(CAWS_wgs84)+
  geom_sf()+
  geom_point(data = bass_17962, 
             aes(x = Long, y = Lat, size = detections), color = "blue")+
  coord_sf(xlim = c(-87.71, -87.61), ylim = c(41.82, 41.92))+ ## Change bounds of plot 
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Bass 17962 detections")+
  theme_bw()+
  theme(plot.title = element_text(size = 14, face = "bold"))

bass_17966_plot <- ggplot(CAWS_wgs84)+
  geom_sf()+
  geom_point(data = bass_17966, 
             aes(x = Long, y = Lat, size = detections), color = "darkgreen")+
  coord_sf(xlim = c(-87.71, -87.61), ylim = c(41.82, 41.92))+ ## Change bounds of plot 
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Bass 17966 detections")+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 14, face = "bold"))

### Combine all plots and save
x <- ggarrange(bass_17916_plot, bass_17918_plot, bass_17962_plot, bass_17966_plot,
               ncol = 2, nrow = 2, widths = c(0.01, 0.01))

### Save file
setwd("C:/Users/lmcgi/OneDrive/Desktop/BIO_695/Final project")
#ggsave(filename = "detections of 4 bass.png", x, width = 12, height = 9)

# Comparing detections by habitat type: -----------------------------------

# Get summary by habitat type
hourly_summary <- det_habitat_summary %>%
  group_by(habitat_type, Start_Time) %>%
  summarise(total_detections = sum(Detections),
            unique_receivers = n_distinct(Receiver_Serial))

# Control for different number of receivers per habitat type
hourly_summary <- hourly_summary %>%
  group_by(habitat_type) %>%
  mutate(weighted_detections = total_detections / unique_receivers)

anova_model <- aov(weighted_detections ~ habitat_type, data = hourly_summary)
summary(anova_model) 

### They are significantly different

### barplot
# get std for errorbars on plot
mean_std <- hourly_summary %>%
  group_by(habitat_type) %>%
  summarise(mean_weighted_detections = mean(weighted_detections),
            std_error = sd(weighted_detections) / sqrt(n()))

## plot
habitat_barplot <- ggplot(mean_std, aes(x = habitat_type, y = mean_weighted_detections)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = mean_weighted_detections - std_error,
                    ymax = mean_weighted_detections + std_error),
                color = "black", width = 0.4) +
  labs(title = "Average hourly detections per receiver by habitat type",
       x = "Habitat Type",
       y = "Average Detections per Reciever") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))+
  scale_x_discrete(labels = c("barge_slip" = "Barge Slip", "BC" = "Bubbly Creek", 
                              "CSSC" = "San. & Ship Canal", "MS" = "Main Stem",
                              "NBCR" = "North Branch", "SBCR" = "South Branch",
                              "turning_basin" = "Turning Basin", "wild_mile" = "Wild Mile"))
### Save plot
#ggsave("habitat_barplot.png", habitat_barplot)

# Random forest regression model: -----------------------------------------

### Remove all data without a detection 
detections <- det_habitat_summary %>% filter(!Detections == 0) 

### Create new column to get detections/unique_tag_ids
detections$det_per_tag <- detections$Detections/detections$Unique_tag_IDs

###Turn habitat_type column into factor
detections$habitat_type <- as.factor(detections$habitat_type)

## set seed for reproducibility
set.seed(1234)

### Split data
data_split <- initial_split(detections, strata = "det_per_tag", prop = 0.75)

det_train <- training(data_split)
det_test  <- testing(data_split)

### Specify random forest model
rf_defaults <- rand_forest(mode = "regression")
rf_defaults

### non-formula interface
# set predictions
preds <- c("habitat_type", "DO_mgL", "Water_Temp")

# Fit model based on training data
rf_xy_fit <- 
  rf_defaults %>%
  set_engine("ranger") %>%
  fit_xy(
    x = det_train[, preds],
    y = det_train$det_per_tag
  )

rf_xy_fit

# Make predictors for testing data
test_results <- 
  det_test %>%
  dplyr::select(det_per_tag) %>%
  bind_cols(
    predict(rf_xy_fit, new_data = det_test[, preds])
  )
test_results %>% slice(1:5)

test_results %>% metrics(truth = det_per_tag, estimate = .pred) 

# glm linear model: -------------------------------------------------------
### Using glmnet package that can perform regularization which avoids overfitting

norm_recipe <- 
  recipe(
    det_per_tag ~ DO_mgL + habitat_type + Water_Temp, 
    data = det_train
  ) %>%
  step_dummy(all_nominal()) %>%       # Have to create dummy variables for nominal data (habitat type)
  step_center(all_predictors()) %>%   # center all data
  step_scale(all_predictors()) %>%    # scale all data
  prep(training = det_train, retain = TRUE) # estimate the means and standard deviations

### fit model
glmn_fit <- 
  linear_reg(penalty = 0.001, mixture = 0.5) %>% 
  set_engine("glmnet") %>%
  fit(det_per_tag ~ ., data = bake(norm_recipe, new_data = NULL))
glmn_fit

# Processed version of predictors
test_normalized <- bake(norm_recipe, new_data = det_test, all_predictors())

### Add glmnet predictions to randforest predictions
test_results <- 
  test_results %>%
  rename(`random forest` = .pred) %>%
  bind_cols(
    predict(glmn_fit, new_data = test_normalized) %>%
      rename(glmnet = .pred)
  )
test_results

test_results %>% metrics(truth = det_per_tag, estimate = glmnet) 

# Comparing glm vs randfors model predictions -----------------------------

test_results_plot <- test_results %>% 
  gather(model, prediction, -det_per_tag) %>% 
  ggplot(aes(x = prediction, y = det_per_tag)) + 
  geom_abline(col = "blue", lty = 2, linewidth = 1) + 
  geom_point(alpha = .4) + 
  facet_wrap(~model) + 
  coord_fixed()+
  theme_bw()+
  ylab("Detections per individual tag")+
  ggtitle("Comparing glm vs random forest regression predictions")+
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))

#ggsave("test_results_plot.png", test_results_plot)

### Random forest is better

# Random forest predictions plot: -----------------------------------------
randFors_plot <- test_results%>%ggplot(aes(det_per_tag,`random forest`))+geom_hex()+
  scale_fill_viridis_c(trans="log10",option="magma")+
  geom_abline(col = "blue", lty = 2, linewidth = 1)+
  theme_bw()+
  xlim(0,25)+
  ylim(0,25)+
  xlab("Detections per tag")+
  ylab("Random forest prediction")+
  ggtitle("Evaluating Random Forest predictions")+
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))


#ggsave("RandomForestPredsPlot.png", randFors_plot)


# Just comparing detections with environment ------------------------------

det_v_DO <- ggplot(detections, aes(x = DO_mgL, y = det_per_tag))+
  geom_hex()+
  scale_fill_viridis_c(trans="log10", option="magma")+
  theme_bw()+
  ggtitle("Detections per Tag vs Dissolved Oxygen")+
  xlab("Dissolved Oxygen (mg/L)")+
  ylab("Detections per Tag")+
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))

#ggsave("det_v_DO_plot.png", det_v_DO)

det_v_temp <- ggplot(detections, aes(x = Water_Temp, y = det_per_tag))+
  geom_hex()+
  scale_fill_viridis_c(trans="log10", option="magma")+
  theme_bw()+
  ggtitle("Detections per Tag vs Water Temperature")+
  xlab("Water Temperature (\u00B0C)")+
  ylab("Detections per Tag")+
  theme(axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))

#ggsave("det_v_temp_plot.png", det_v_temp)


# Map of receivers and sensors --------------------------------------------

## read in sensor locations
sensors <- read_csv("BIO695_sensor_locations.csv")

colors <- c("environmental" = "yellow", "receiver" =  "blue")

map <- ggplot(CAWS_wgs84)+
  geom_sf()+
  geom_point(data = sensors, 
             aes(x = Long, y = Lat, color = sensor_type), size = 2)+
  coord_sf(xlim = c(-87.84, -87.60), ylim = c(41.80, 41.98))+ ## Change bounds of plot 
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Map of installed receivers and sensors in the CAWS")+
  theme_bw()+
  theme(plot.title = element_text(size = 14, face = "bold"))+
  annotation_scale()+
  scale_color_manual(values = colors)

#ggsave("receivers_sensors_map.png", map)



