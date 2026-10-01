# ============================================================
# Merge a downloaded Kaggle GSMArena-style CSV into
# phones_combined.csv, using the app's existing schema.
#
# Usage:
#   1. Place the GSMArena CSV in this same folder (already done — gsm.csv
#      is already here and already merged into phones_combined.csv;
#      re-run this only if you replace gsm.csv with a newer download)
#   2. source("merge_gsmarena.R")
#
# ============================================================

library(dplyr)
library(stringr)
library(readr)

# ------------------------------------------------------------
# 1. Find CSV file
#    (looks for a file with "gsm" in the name first, so it never
#    mistakes phones_combined.csv itself for the raw download)
# ------------------------------------------------------------

raw_dir <- "."

csv_files <- list.files(
  raw_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)
csv_files <- csv_files[!grepl("phones_combined\\.csv$", csv_files)]

if (length(csv_files) == 0) {
  stop(
    "No raw GSM CSV found in this folder (besides phones_combined.csv). ",
    "Place the GSMArena CSV here first."
  )
}

gsm_named <- csv_files[grepl("gsm", basename(csv_files), ignore.case = TRUE)]
raw_path <- if (length(gsm_named) > 0) gsm_named[1] else csv_files[1]

message("Reading: ", raw_path)

raw <- read_csv(
  raw_path,
  show_col_types = FALSE,
  guess_max = 20000
)

message(
  "Raw dataset: ",
  nrow(raw),
  " rows, ",
  ncol(raw),
  " columns"
)

message(
  "Raw columns: ",
  paste(names(raw), collapse = ", ")
)

# ------------------------------------------------------------
# 2. Flexible column finder
# ------------------------------------------------------------

find_col <- function(df, patterns) {
  
  nm <- names(df)
  
  for (p in patterns) {
    
    hit <- nm[
      str_detect(
        str_to_lower(nm),
        p
      )
    ]
    
    if (length(hit) > 0) {
      return(hit[1])
    }
  }
  
  return(NA_character_)
}

# ------------------------------------------------------------
# 3. Find columns in downloaded dataset
# ------------------------------------------------------------

col_brand <- find_col(
  raw,
  c(
    "^brand$",
    "brand_name",
    "^oem$"
  )
)

col_model <- find_col(
  raw,
  c(
    "^model$",
    "model_name",
    "^name$"
  )
)

col_launch <- find_col(
  raw,
  c(
    "announced",
    "launch",
    "release"
  )
)

col_status <- find_col(
  raw,
  c(
    "^status$"
  )
)

col_display <- find_col(
  raw,
  c(
    "display_size"
  )
)

col_res <- find_col(
  raw,
  c(
    "display_resolution",
    "resolution"
  )
)

col_os <- find_col(
  raw,
  c(
    "^os$"
  )
)

col_chipset <- find_col(
  raw,
  c(
    "chipset"
  )
)

col_ram <- find_col(
  raw,
  c(
    "^ram$"
  )
)

col_storage <- find_col(
  raw,
  c(
    "internal_memory",
    "storage",
    "memory_internal"
  )
)

col_battery <- find_col(
  raw,
  c(
    "battery"
  )
)

col_rearcam <- find_col(
  raw,
  c(
    "primary_camera",
    "rear_camera",
    "main_camera"
  )
)

col_frontcam <- find_col(
  raw,
  c(
    "secondary_camera",
    "front_camera",
    "selfie"
  )
)

col_wlan <- find_col(
  raw,
  c(
    "wlan",
    "wifi"
  )
)

col_bt <- find_col(
  raw,
  c(
    "bluetooth"
  )
)

col_gps <- find_col(
  raw,
  c(
    "^gps$"
  )
)

col_nfc <- find_col(
  raw,
  c(
    "^nfc$"
  )
)

col_price <- find_col(
  raw,
  c(
    "price"
  )
)

col_network <- find_col(
  raw,
  c(
    "network_technology",
    "network"
  )
)

# ------------------------------------------------------------
# 4. Print detected column mapping
# ------------------------------------------------------------

found <- c(
  brand = col_brand,
  model = col_model,
  launch = col_launch,
  display = col_display,
  resolution = col_res,
  os = col_os,
  chipset = col_chipset,
  ram = col_ram,
  storage = col_storage,
  battery = col_battery,
  rear_cam = col_rearcam,
  front_cam = col_frontcam,
  wlan = col_wlan,
  bluetooth = col_bt,
  gps = col_gps,
  nfc = col_nfc,
  price = col_price,
  network = col_network
)

message("")
message("--- Column mapping guessed ---")

print(found)

message(
  "If any of these are <NA>, open the raw CSV and edit ",
  "the find_col() calls above with the real column name."
)

# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

extract_num <- function(x) {
  
  as.numeric(
    str_extract(
      as.character(x),
      "[0-9]+\\.?[0-9]*"
    )
  )
}

parse_year <- function(x) {
  
  yr <- str_extract(
    as.character(x),
    "(19|20)\\d{2}"
  )
  
  as.integer(yr)
}

parse_res <- function(x) {
  
  m <- str_match(
    as.character(x),
    "(\\d+)\\s*x\\s*(\\d+)"
  )
  
  list(
    x = suppressWarnings(
      as.integer(m[, 2])
    ),
    y = suppressWarnings(
      as.integer(m[, 3])
    )
  )
}

yesno <- function(x) {
  
  ifelse(
    is.na(x),
    "NA",
    ifelse(
      str_detect(
        str_to_lower(
          as.character(x)
        ),
        "yes|802\\.11|v[0-9]|bt|a-gps"
      ),
      "Yes",
      "No"
    )
  )
}

# ------------------------------------------------------------
# 6. Parse resolution
# ------------------------------------------------------------

res <- parse_res(
  if (!is.na(col_res)) {
    raw[[col_res]]
  } else {
    rep(NA, nrow(raw))
  }
)

# ------------------------------------------------------------
# 7. Create mapped dataset
# ------------------------------------------------------------

mapped <- tibble(
  
  Name =
    if (
      !is.na(col_brand) &&
      !is.na(col_model)
    ) {
      
      paste(
        raw[[col_brand]],
        raw[[col_model]]
      )
      
    } else {
      
      NA_character_
    },
  
  Brand =
    if (!is.na(col_brand)) {
      raw[[col_brand]]
    } else {
      NA_character_
    },
  
  Model =
    if (!is.na(col_model)) {
      raw[[col_model]]
    } else {
      NA_character_
    },
  
  Era = NA_character_,
  
  Launch_Year =
    if (!is.na(col_launch)) {
      parse_year(
        raw[[col_launch]]
      )
    } else {
      NA_integer_
    },
  
  Price_INR =
    if (!is.na(col_price)) {
      extract_num(
        raw[[col_price]]
      )
    } else {
      NA_real_
    },
  
  Chipset =
    if (!is.na(col_chipset)) {
      raw[[col_chipset]]
    } else {
      NA_character_
    },
  
  Cores = NA_real_,
  
  RAM_MB =
    if (!is.na(col_ram)) {
      extract_num(
        raw[[col_ram]]
      ) * 1024
    } else {
      NA_real_
    },
  
  Storage_GB =
    if (!is.na(col_storage)) {
      extract_num(
        raw[[col_storage]]
      )
    } else {
      NA_real_
    },
  
  Battery_mAh =
    if (!is.na(col_battery)) {
      extract_num(
        raw[[col_battery]]
      )
    } else {
      NA_real_
    },
  
  FastCharging_W = NA_real_,
  
  Screen_inches =
    if (!is.na(col_display)) {
      extract_num(
        raw[[col_display]]
      )
    } else {
      NA_real_
    },
  
  RefreshRate_Hz = NA_real_,
  
  DisplayType = NA_character_,
  
  Touchscreen = "NA",
  
  Resolution_X = res$x,
  
  Resolution_Y = res$y,
  
  RearCamera_MP =
    if (!is.na(col_rearcam)) {
      extract_num(
        raw[[col_rearcam]]
      )
    } else {
      NA_real_
    },
  
  RearCameraSetup = NA_character_,
  
  FrontCamera_MP =
    if (!is.na(col_frontcam)) {
      extract_num(
        raw[[col_frontcam]]
      )
    } else {
      NA_real_
    },
  
  OS =
    if (!is.na(col_os)) {
      raw[[col_os]]
    } else {
      NA_character_
    },
  
  WiFi =
    if (!is.na(col_wlan)) {
      yesno(
        raw[[col_wlan]]
      )
    } else {
      "NA"
    },
  
  Bluetooth =
    if (!is.na(col_bt)) {
      yesno(
        raw[[col_bt]]
      )
    } else {
      "NA"
    },
  
  GPS =
    if (!is.na(col_gps)) {
      yesno(
        raw[[col_gps]]
      )
    } else {
      "NA"
    },
  
  NumSIMs = NA_real_,
  
  Network_3G =
    if (!is.na(col_network)) {
      
      yesno(
        str_detect(
          str_to_lower(
            raw[[col_network]]
          ),
          "hspa|3g"
        )
      )
      
    } else {
      
      "NA"
    },
  
  Network_4G =
    if (!is.na(col_network)) {
      
      yesno(
        str_detect(
          str_to_lower(
            raw[[col_network]]
          ),
          "lte|4g"
        )
      )
      
    } else {
      
      "NA"
    },
  
  Network_5G =
    if (!is.na(col_network)) {
      
      yesno(
        str_detect(
          str_to_lower(
            raw[[col_network]]
          ),
          "5g"
        )
      )
      
    } else {
      
      "NA"
    },
  
  NFC =
    if (!is.na(col_nfc)) {
      yesno(
        raw[[col_nfc]]
      )
    } else {
      "NA"
    },
  
  Source =
    "Kaggle GSMArena dataset (merged)",
  
  RAM_GB =
    if (!is.na(col_ram)) {
      
      round(
        extract_num(
          raw[[col_ram]]
        ),
        2
      )
      
    } else {
      
      NA_real_
    }
)

# ------------------------------------------------------------
# 8. Create Era
# ------------------------------------------------------------

mapped <- mapped %>%
  mutate(
    Era = case_when(
      
      is.na(Launch_Year) ~
        "Unknown",
      
      Launch_Year < 2007 ~
        "Classic (1996-2006)",
      
      Launch_Year < 2016 ~
        "Smartphone Era (2007-2015)",
      
      Launch_Year < 2021 ~
        "Legacy (2016-2020)",
      
      TRUE ~
        "Modern (2021-2026)"
    )
  ) %>%
  filter(
    !is.na(Name),
    Name != "NA NA"
  )

message("")
message(
  "Mapped rows ready to merge: ",
  nrow(mapped)
)

# ------------------------------------------------------------
# 9. Read existing combined dataset
# ------------------------------------------------------------

if (!file.exists("phones_combined.csv")) {
  
  stop(
    "phones_combined.csv was not found."
  )
}

existing <- read_csv(
  "phones_combined.csv",
  show_col_types = FALSE
)

message(
  "Existing dataset rows: ",
  nrow(existing)
)

# ------------------------------------------------------------
# 10. Remove duplicate phones
# ------------------------------------------------------------

existing_names <-
  str_to_lower(
    str_trim(
      existing$Name
    )
  )

mapped_dedup <- mapped %>%
  filter(
    !str_to_lower(
      str_trim(Name)
    ) %in% existing_names
  ) %>%
  distinct(
    Name,
    .keep_all = TRUE
  )

message(
  "New (non-duplicate) rows to add: ",
  nrow(mapped_dedup)
)

# ------------------------------------------------------------
# 11. FIXED row_id section
# ------------------------------------------------------------

if (nrow(mapped_dedup) > 0) {
  
  if (
    nrow(existing) > 0 &&
    "row_id" %in% names(existing)
  ) {
    
    valid_ids <- existing$row_id[
      !is.na(existing$row_id)
    ]
    
    if (length(valid_ids) > 0) {
      
      next_id <-
        max(
          valid_ids
        ) + 1
      
    } else {
      
      next_id <- 1
    }
    
  } else {
    
    next_id <- 1
  }
  
  mapped_dedup$row_id <-
    seq_len(
      nrow(mapped_dedup)
    ) +
    next_id -
    1
  
} else {
  
  # No new rows.
  # Keep mapped_dedup as a zero-row data frame.
  message(
    "No new rows to add. Existing dataset will be kept unchanged."
  )
}

# ------------------------------------------------------------
# 12. Make sure columns match existing dataset
# ------------------------------------------------------------

# Add any columns that exist in existing but are missing
# from mapped_dedup.

missing_columns <- setdiff(
  names(existing),
  names(mapped_dedup)
)

if (length(missing_columns) > 0) {
  
  for (col in missing_columns) {
    
    mapped_dedup[[col]] <-
      existing[[col]][NA_integer_]
    
  }
}

# ------------------------------------------------------------
# 13. Reorder columns exactly like existing dataset
# ------------------------------------------------------------

mapped_dedup <-
  mapped_dedup[
    ,
    names(existing),
    drop = FALSE
  ]

# ------------------------------------------------------------
# 14. Combine datasets
# ------------------------------------------------------------

combined <- bind_rows(
  existing,
  mapped_dedup
)

# ------------------------------------------------------------
# 15. Save combined dataset
# ------------------------------------------------------------

write_csv(
  combined,
  "phones_combined.csv"
)

# ------------------------------------------------------------
# 16. Final message
# ------------------------------------------------------------

message("")

message(
  "Done. phones_combined.csv now has ",
  nrow(combined),
  " rows",
  " (was ",
  nrow(existing),
  ", added ",
  nrow(mapped_dedup),
  ")."
)

message("")

message("==============================================")
message(" Merge completed successfully")
message("==============================================")