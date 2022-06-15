################################################################################
# This code load data from the OECD API
################################################################################

# Get Date
if (month(today())<10) {month = paste0("0", month(today()))} else {month =month(today())} 
end_date <- paste0(year(today()), "-",month)

# API URL from OECD website
url <- paste0("https://stats.oecd.org/SDMX-JSON/data/MEI/AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+EU28+EU27_2020+G4E+G-7+NAFTA+OECDE+G-20+OECD+OXE+SDR+ONM+A5M+NMEC+ARG+BRA+BGR+CHN+CYP+IND+IDN+MLT+ROU+RUS+SAU+ZAF+BRIICS.CPALTT01+CPGRLE01+CPGREN01+CP010000.GY.M/all?startTime=2006-01&endTime=", end_date, "&dimensionAtObservation=allDimensions")
res <- GET(url)
dat_parsed <- parse_json(content(res, "text"))

# Code for JSON parsing is from  https://stackoverflow.com/questions/59529074/fetching-data-from-oecd-into-r-via-sdmxxml
# Get values
dat_obs <- dat_parsed[["dataSets"]][[1]][["observations"]]
dat0 <- do.call(rbind, dat_obs)  # get a matrix
new_features <- matrix(as.numeric(do.call(rbind, strsplit(rownames(dat0), ":"))), nrow = nrow(dat0))
dat1 <- cbind(new_features, dat0)  # add feature columns
dat1_df <- as.data.frame(dat1)  # optionally transform to data frame

## Get keys of features
keys <- dat_parsed[["structure"]][["dimensions"]][["observation"]]

# apply keys
get_features <- function(data_input, keys_input, feature_index, value = FALSE) {
  keys_temp <- keys_input[[feature_index]]$values
  keys_temp_matrix <- do.call(rbind, keys_temp)
  keys_temp_out <- keys_temp_matrix[, value + 1][unlist(data_input[, feature_index])+1]  # column 1 is id, 2 is value
  return(unlist(keys_temp_out))
}
feat_1 <- get_features(dat1_df, keys, 1, value = TRUE)
feat_2 <- get_features(dat1_df, keys, 1, value = FALSE)
feat_3 <- get_features(dat1_df, keys, 2, value = TRUE)
feat_4 <- get_features(dat1_df, keys, 5, value = FALSE)

# Build dataframe 
data <- tibble(
 Country_name = feat_1,
 Country = feat_2,
 Variable = feat_3,
 Date = feat_4,
 Value = as.vector(unlist(dat1_df$V6))
) %>% 
  mutate(
   Date = ym(Date),
   Variable = ifelse(Variable=="Consumer Price Index > OECD Groups > All items non-food non-energy > Total", "Core", Variable),
   Variable = ifelse(Variable=="Consumer Price Index > OECD Groups > Energy (Fuel, electricity & gasoline) > Total", "Energy", Variable),
   Variable = ifelse(Variable=="Consumer Price Index > All items > Total > Total", "Non Core", Variable),
   Variable = ifelse(Variable=="Consumer Price Index > Food and non-Alcoholic beverages (COICOP 01) > Total > Total", "Food", Variable)
  ) %>% 
  select(Country, Country_name, Date, Value, Variable)


################################################################################
# Export
################################################################################
write_csv(data, "./data_clean.csv")
rm(list=(ls()))





