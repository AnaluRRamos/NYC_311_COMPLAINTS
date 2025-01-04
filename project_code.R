
# |------------------------------ Set-up ------------------------------|
# Load libraries
packs <- c('data.table', 'tidyverse', 'conflicted', 'skimr', 'patchwork', 'sf',
           'leaflet')
mapply(library, packs, character.only=TRUE)
# Solve function conflicts
conflicts_prefer(dplyr::filter, lubridate::hour, lubridate::wday)
Sys.setlocale("LC_TIME", "English")


# |------------------- Load data + first impression -------------------|
# Load dataset
# (Non explicit strings are parsed as NA)
df <- fread('~\NYC_311_Data_20241009.csv',
            sep = ';', na.strings = c('NA', '', 'N/A', 'Unspecified'))

# Let's take a quick look at the dataset’s overall structure and summary 
# statistics to understand what we’re dealing with.
data_summary <- skim(df)
data_summary

# |------------------------ Data Pre-Processing ------------------------|
# |--- CLEANING ---|
# From the analysis of this summary + visual inspection of the dataset,
# there are many columns that can be removed for a better interaction
# with the data.
clean_df <- df %>%
  select(# Very low completion rate:
    -"Cross Street 1", -"Cross Street 2", -"Intersection Street 1",
    -"Intersection Street 2", -"Landmark", -"Due Date",
    -"Vehicle Type", -"Taxi Company Borough", -"Taxi Pick Up Location",
    -"Bridge Highway Name", -"Bridge Highway Direction", -"Road Ramp",
    -"Bridge Highway Segment",
    # Visual inspection of the dataset:
    # (considered redundant OR lacking extra information)
    -"Facility Type", -"Community Board", -"X Coordinate (State Plane)",
    -"Y Coordinate (State Plane)", -"Park Facility Name", -"Location",
    -"Address Type", -"Park Borough")

# Make column names more user-friendly:
clean_df <- clean_df %>%
  rename(uniq_ID=`Unique Key`, date_created=`Created Date`,
         date_closed=`Closed Date`, agency=Agency, agency_name=`Agency Name`,
         complaint_type=`Complaint Type`, complaint_descript=Descriptor,
         location_type=`Location Type`, zip_code=`Incident Zip`,
         address=`Incident Address`, street_name=`Street Name`, city=City,
         status=Status, resolution_descript=`Resolution Description`,
         resolution_act_last_date=`Resolution Action Updated Date`,
         borough=Borough, lat=Latitude, lon=Longitude)

# Reformat the dates to POSIX-compliant objects:
clean_df <- clean_df %>%
  mutate(date_created=format(mdy_hms(clean_df$date_created, tz='America/New_York'),
                             '%Y-%m-%d %H:%M:%S %Z'),
         date_closed=format(mdy_hms(clean_df$date_closed, tz='America/New_York'),
                            '%Y-%m-%d %H:%M:%S %Z'),
         resolution_act_last_date=format(mdy_hms(clean_df$resolution_act_last_date,
                                                 tz='America/New_York'),
                                         '%Y-%m-%d %H:%M:%S %Z'),
         hour_created=hour(date_created))

# Solve detected writing inconsistencies
# (different casing for equal entries)
clean_df$complaint_type <- str_to_title(clean_df$complaint_type)
clean_df$complaint_descript <- str_to_title(clean_df$complaint_descript)
clean_df$location_type <- str_to_title(clean_df$location_type)
clean_df$street_name <- toupper(clean_df$street_name)
clean_df$city <- str_to_title(clean_df$city)
clean_df$resolution_descript <- str_to_sentence(clean_df$resolution_descript)
# Change 'borough' names to 2-letter code for easier analysis:
clean_df$borough <- str_to_title(clean_df$borough)
clean_df$borough <- ifelse(clean_df$borough == 'Bronx', 'BX',
                           ifelse(clean_df$borough == 'Brooklyn', 'BK',
                                  ifelse(clean_df$borough == 'Manhattan', 'MN',
                                         ifelse(clean_df$borough == 'Queens', 'QN',
                                                ifelse(clean_df$borough == 'Staten Island', 'SI', clean_df$borough)))))

# Use information in 'city' to impute boroughs
# ('city' can be removed afterwards)
clean_df$borough <- 
  ifelse(clean_df$city == 'Bronx', 'BX',
         ifelse(clean_df$city == 'Brooklyn', 'BK',
                ifelse(clean_df$city == 'Manhattan', 'MN',
                       ifelse(clean_df$city == 'Queens', 'QN',
                              ifelse(clean_df$city == 'Staten Island', 'SI', clean_df$borough)))))

clean_df <- clean_df %>% select(-city)

# |--- FILTERING ---|
# We are excluding complaints whose borough is not specified because they're
# not many, and we can focus on the rest. The entries with NULL values for
# all kinds of dates will also be cut down, because we want to focus on
# complaints where we can track beginning, ending, and resolution actions.
clean_df <- subset(clean_df, !is.na(borough))
clean_df <- subset(clean_df, !is.na(date_closed))
clean_df <- subset(clean_df, !is.na(resolution_act_last_date))
# To play with maps, NAs are not allowed! Removing them:
clean_df <- subset(clean_df, !is.na(lon))
clean_df <- subset(clean_df, !is.na(lat))
# Missing ZIP codes will not be considered too:
# (important for the map network!)
clean_df <- subset(clean_df, !is.na(zip_code))

# |--- MERGING ---|
# We are adding a column with population counts for each borough, so we
# can balance and better compare the distribution of the complaints
# in our further analyses:
# ---------------------------------------------------------------------
# (data from the Census Bureau Estimates:)
# => "https://www.bers.nyc.gov/assets/planning/download/pdf/planning-level/nyc-population/population-estimates/current-population-estimates-2016.pdf"
# ---------------------------------------------------------------------
clean_df$population_count <-
  ifelse(clean_df$borough == 'BX', 1455720,
         ifelse(clean_df$borough == 'BK', 2629150,
                ifelse(clean_df$borough == 'MN', 1643734,
                       ifelse(clean_df$borough == 'QN', 2333054, 476015))))
# Let's also add a column with the times taken for resolving the problems
# (time differences are calculated in DAYS!):
clean_df$resolution_time <-
  as.Date(clean_df$date_closed) - as.Date(clean_df$date_created)
# A column with the days of the week matching the complaint dates will also
# be created, to allow further exploration:
clean_df$weekday_created <- factor(wday(as.Date(clean_df$date_created), 
                                        label=TRUE, abbr=TRUE),
                                   levels = c('Mon', 'Tue', 'Wed', 'Thu',
                                              'Fri', 'Sat', 'Sun'))

# |-------------------------- Visualizations --------------------------|
# Fig. 1 "Top-5 borough complaints per weekday, normalized by population."
# 1) Get the data
top5_complaints <- as.data.frame(table(clean_df$complaint_type)) %>%
  rename(complaint_type=Var1, count=Freq) %>%
  top_n(5, count)

weekday_top_complaints <- clean_df %>%
  filter(complaint_type %in% top5_complaints$complaint_type) %>%
  group_by(weekday_created, complaint_type, borough) %>%
  summarise(complaints=n(), .groups='drop')
# 2) Apply Normalization
weekday_top_complaints$complaints <-
  weekday_top_complaints$complaints /
  ifelse(weekday_top_complaints$borough == 'BX', 1455720,
         ifelse(weekday_top_complaints$borough == 'BK', 2629150,
                ifelse(weekday_top_complaints$borough == 'MN', 1643734,
                       ifelse(weekday_top_complaints$borough == 'QN', 2333054, 476015))))
# 3) Build plot
ggplot(weekday_top_complaints,
       aes(x=weekday_created, y=complaints, fill=complaint_type)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~ borough, ncol=1) +
  labs(x='Day of the Week', y='#Complaints Per Capita') +
  theme_minimal()

# Fig. 2 'Expression of the complaint types that can be associated with
# "Unsanitary Condition".'
# 1) Retrieve the complaint types of interest
complaint_types <-
  c('Adopt-A-Basket', 'Dirty Conditions', 'Food Establishment',
    'Hazardous Materials', 'Indoor Sewage', 'Industrial Waste',
    'Litter Basket / Request', 'Mold', 'Overflowing Litter Baskets',
    'Overflowing Recycling Baskets', 'Recycling Enforcement',
    'Rodent', 'Sanitation Condition', 'Sewer',
    'Unsanitary Animal Pvt Property', 'Unsanitary Pigeon Condition',
    'Urinating In Public', 'Water Conservation', 'Water Quality')
# 2) Filter data of interest
complaints_df <- clean_df %>%
  filter(complaint_type %in% complaint_types) %>%
  group_by(complaint_type) %>%
  summarise(count=n(), .groups='drop')
# 3) Build plot
ggplot(complaints_df,
       aes(x=reorder(complaint_type, count), y=count,
           fill=complaint_type)) +
  geom_bar(stat='identity') + coord_flip() +
  labs(x='', y="#Complaints") +
  theme_minimal() + theme(legend.position="none")
# Let's also look at these data per borough to gather more information
# and help future decisions:
# 1) Function to create a plot for a specific borough
create_plot <- function(borough_name) {
  borough_df <- clean_df %>%
    filter(complaint_type %in% complaint_types) %>%
    filter(borough == !!borough_name) %>%
    group_by(complaint_type) %>%
    summarise(count=n(), .groups='drop')
  
  ggplot(borough_df, aes(
    x=reorder(complaint_type, count), y=count, fill=complaint_type)) +
    geom_bar(stat="identity") +
    coord_flip() + labs(title=borough_name, x='', y="#Complaints") +
    theme_minimal() + theme(legend.position="none")
}
# 2) Create plots for each borough
boroughs <- unique(clean_df$borough)
plots <- lapply(boroughs, create_plot)
# 3) Combine plots into one figure
combined_vis <- wrap_plots(plots, ncol=2)  # Adjust 'ncol' as needed
combined_vis

# Considering the expression and relevance of the analysed complaint types,
# we will focus our attention on the following:
relevant_complaint_types <-
  c('Dirty Conditions','Unsanitary Condition','Food Establishment', 
    'Sanitation Condition','Industrial Waste', 'Litter Basket / Request', 
     'Overflowing Litter Baskets', 'Rodent','Sewer', 'Water Quality')

# Fig. 3 "Hourly evolution of the complaints of interest per borough."
# 1) Get the data
hourly_complaints_borough <- clean_df %>%
  filter(complaint_type %in% relevant_complaint_types) %>%
  group_by(hour_created, borough) %>%
  summarise(complaints=n(), .groups='drop')
# 2) Build plot
ggplot(hourly_complaints_borough,
       aes(x=hour_created, y=complaints, colour=borough, group=borough)) +
  geom_line(linewidth=1) + geom_point(size=2) +
  labs(x='Hour of the day', y='#Complaints') +
  scale_x_continuous(breaks=seq(0, 23, 2)) + theme_minimal()

# => There is a very big peak at Midnight! What can it be?
# Fig. 4 "Hourly distribution of the complaints of interest."
# 1) Get data
complaints_type_hour <- clean_df %>%
  filter(complaint_type %in% relevant_complaint_types) %>%
  group_by(hour_created, complaint_type) %>%
  summarise(complaints=n(), .groups='drop')
# 2) Order data by nº of complaints
total_complaints <- complaints_type_hour %>%
  group_by(complaint_type) %>%
  summarise(total_complaints=sum(complaints), .groups='drop')

complaints_type_hour$complaint_type <- 
  factor(complaints_type_hour$complaint_type,
         levels=total_complaints$complaint_type[order(-total_complaints$total_complaints)])
# 3) Build plot
ggplot(complaints_type_hour,
       aes(x=hour_created, y=complaints, fill=complaint_type)) +
  geom_bar(stat='identity') +
  labs(x='Hour of the day', y='#Complaints') +
  scale_x_continuous(breaks=seq(0, 23, 2)) + theme_minimal()
# => Explanation: The Midnight peak are rodent sightings!
