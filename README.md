# 📱 MobileLens

**MobileLens** is an R Shiny app for browsing, filtering, and comparing
mobile phone specifications and prices — built from real, named phone
models spanning three eras: classic phones, the early smartphone era, and
today's current phones.

## 📂 Files (6 total, no subfolders)

| File | Purpose |
|---|---|
| `phones_combined.csv` | Ready-to-use dataset — 10,873 real phones |
| `gsm.csv` | Raw GSMArena historical dataset (already merged into `phones_combined.csv`) |
| `app.R` | The Shiny app |
| `install_packages.R` | One-time setup — installs required R packages |
| `merge_gsmarena.R` | Re-run only if you replace `gsm.csv` with a newer download |
| `README.md` | This file |

## ▶️ How to run it

1. Put all 6 files in the same folder (already done if you downloaded this
   as one zip).
2. Open **`app.R`** in RStudio — this sets your working directory to this
   folder automatically, which is what lets `app.R` find `phones_combined.csv`
   sitting right next to it.
3. Install packages once: `source("install_packages.R")`
4. Click **Run App** (or run `shiny::runApp()`).

If you instead run R from a plain console (not RStudio), set your working
directory to this folder first: `setwd("path/to/this/folder")`.

You don't need to run `merge_gsmarena.R` for normal use — `phones_combined.csv`
already has everything merged in. Only re-run it if you replace `gsm.csv`
with a newer download and want to fold in whatever's new.

## 📊 What's in `phones_combined.csv` (10,873 rows)

| Source | Rows | Era |
|---|---|---|
| NDTV dataset | 1,359 | Legacy (2016-2020) |
| Hand-researched (91mobiles, Smartprix, Bajaj Finserv, TelecomTalk) | 65 | Current / Modern (2021-2026) |
| GSMArena dataset (`gsm.csv`) | 9,449 | Classic through modern (1996-2026) |

Every row is a real, named phone model — nothing is synthetic or fabricated
to inflate the row count. Newer spec columns (chipset, fast-charging
wattage, refresh rate, NFC, 5G, launch year) are blank for older phones
whose original source didn't capture them, rather than guessed.

**Note for your analysis:** the dataset spans 1996–2026, so averages and
charts that don't filter by `Era` will mix very different price/spec eras.
Use the Era filter on the Browse tab (or `filter(Era == ...)` in your own
analysis) when you want an apples-to-apples comparison.

## 🛠️ Technologies used

R, RStudio, Shiny, dplyr, DT, ggplot2, readr, stringr

## 👩‍💻 Author

**PRATHIKSHA V**
