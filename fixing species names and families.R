library(dplyr)

dat <- read.csv("taxa_check.csv")

ref <- rWCVPdata::wcvp_names
accepted <- ref %>% filter(taxon_status == "Accepted")

# 1. Fill only missing families using genus.
dat <- dat %>%
  rowwise() %>%
  mutate(
    family = if (is.na(family)) {
      query_genus <- tolower(trimws(genus))

      matches <- accepted %>%
        filter(
          tolower(.data$genus) == .env$query_genus,
          !is.na(.data$family),
          .data$family != ""
        ) %>%
        pull(family) %>%
        unique()

      if (length(matches) == 1) matches else NA_character_
    } else family
  ) %>%
  ungroup()

# 2. Find accepted names using exact genus-species matches.
dat <- dat %>%
  rowwise() %>%
  mutate(
    accepted_name = {
      query_name <- tolower(paste(trimws(genus), trimws(species)))

      ids <- ref %>%
        filter(tolower(.data$taxon_name) == .env$query_name) %>%
        pull(accepted_plant_name_id)

      matches <- accepted %>%
        filter(
          .data$plant_name_id %in% .env$ids,
          !is.na(.data$taxon_name),
          .data$taxon_name != ""
        ) %>%
        pull(taxon_name) %>%
        unique()

      if (length(matches) == 1) matches else NA_character_
    }
  ) %>%
  ungroup()

write.csv(dat, 'taxa_clean.csv')
