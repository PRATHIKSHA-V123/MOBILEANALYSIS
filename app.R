# ============================================================
# MobileLens — R Shiny version
# Browse, filter, and compare phones from phones_combined.csv
# (1,359 legacy phones from the NDTV dataset + 65 hand-researched
#  current phones, 2021-2026, + 9,449 phones from a GSMArena
#  historical dataset — 10,873 real phones total, 1996-2026)
#
# Run locally with:  shiny::runApp()   (or click "Run App" in RStudio)
# Deploy online with: source("deploy.R")   (see deploy.R)
# Required packages: shiny, dplyr, tibble, DT, ggplot2, scales
# ============================================================

library(shiny)
library(dplyr)
library(tibble)
library(DT)
library(ggplot2)
library(scales)

MAX_COMPARE <- 4

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
data_file <- "phones_combined.csv"

if (!file.exists(data_file)) {
  stop(
    "phones_combined.csv was not found in the app folder.\n",
    "Make sure it is in the same folder as app.R.\n",
    "Current working directory is: ", getwd()
  )
}

phones_raw <- read.csv(
  data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

phones <- phones_raw %>%
  mutate(id = row_number())

brand_choices <- sort(unique(phones$Brand))
os_choices    <- sort(unique(phones$OS))
era_choices   <- sort(unique(phones$Era))

price_lo <- floor(min(phones$Price_INR, na.rm = TRUE))
price_hi <- ceiling(max(phones$Price_INR, na.rm = TRUE))

# Rows shown in the compare table: Spec label -> column key -> which
# direction counts as "best".
compare_specs <- tibble::tribble(
  ~Spec,                 ~key,              ~best,
  "Price (Rs.)",         "Price_INR",       "min",
  "Brand",               "Brand",           NA_character_,
  "Era",                 "Era",             NA_character_,
  "Launch year",         "Launch_Year",     "max",
  "Chipset",             "Chipset",         NA_character_,
  "RAM (GB)",            "RAM_GB",          "max",
  "Storage (GB)",        "Storage_GB",      "max",
  "Battery (mAh)",       "Battery_mAh",     "max",
  "Fast charging (W)",   "FastCharging_W",  "max",
  "Screen size (in)",    "Screen_inches",   "max",
  "Refresh rate (Hz)",   "RefreshRate_Hz",  "max",
  "Display type",        "DisplayType",     NA_character_,
  "Rear camera setup",   "RearCameraSetup", NA_character_,
  "Rear camera (MP)",    "RearCamera_MP",   "max",
  "Front camera (MP)",   "FrontCamera_MP",  "max",
  "Operating system",    "OS",              NA_character_,
  "5G",                  "Network_5G",      NA_character_,
  "4G / LTE",            "Network_4G",      NA_character_,
  "NFC",                 "NFC",             NA_character_,
  "Wi-Fi",               "WiFi",            NA_character_,
  "Bluetooth",           "Bluetooth",       NA_character_,
  "GPS",                 "GPS",             NA_character_
)

fmt_inr <- function(x) {
  if (length(x) == 0 || is.na(x)) return("-")
  paste0("Rs. ", format(round(x), big.mark = ",", scientific = FALSE))
}

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
ui <- navbarPage(
  title = "Mobile Compare",
  id = "nav",
  
  tabPanel("Browse",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               textInput("search", "Search", placeholder = "Name or model..."),
               selectInput("era", "Era", choices = c("All eras" = "", era_choices)),
               selectInput("brand", "Brand", choices = c("All brands" = "", brand_choices)),
               selectInput("os", "Operating system", choices = c("All systems" = "", os_choices)),
               sliderInput("priceRange", "Price range (Rs.)",
                           min = price_lo, max = price_hi,
                           value = c(price_lo, price_hi), step = 500, sep = ","),
               selectInput("ramMin", "Minimum RAM",
                           choices = c("Any" = 0, "2 GB+" = 2, "3 GB+" = 3,
                                       "4 GB+" = 4, "6 GB+" = 6, "8 GB+" = 8, "12 GB+" = 12)),
               actionButton("clearFilters", "Clear filters", class = "btn-sm"),
               tags$hr(),
               checkboxInput("onlySelected", "Show only phones I've selected", value = FALSE),
               tags$p(strong(textOutput("selectedCount", inline = TRUE)), " of ", MAX_COMPARE,
                      " selected — go to the Compare tab to see them side by side."),
               tags$p(tags$em("Tick rows in the table to add them to your comparison."))
             ),
             mainPanel(
               width = 9,
               DTOutput("phoneTable")
             )
           )
  ),
  
  tabPanel("Compare",
           h3("Side-by-side comparison"),
           uiOutput("compareUI")
  ),
  
  tabPanel("Market Insights",
           fluidRow(
             column(3, wellPanel(h4(textOutput("statCount")), p("Phones in dataset"))),
             column(3, wellPanel(h4(textOutput("statAvg")), p("Average price"))),
             column(3, wellPanel(h4(textOutput("statRange")), p("Price range"))),
             column(3, wellPanel(h4(textOutput("statBrand")), p("Most-listed brand")))
           ),
           fluidRow(
             column(12, plotOutput("brandPricePlot", height = "420px"))
           ),
           fluidRow(
             column(6, plotOutput("scatterPlot", height = "360px")),
             column(6, plotOutput("histPlot", height = "360px"))
           ),
           fluidRow(
             column(12, plotOutput("eraPlot", height = "360px"))
           )
  ),
  
  tabPanel("About the data",
           fluidRow(
             column(8, offset = 2,
                    tags$div(style = "padding-top: 20px; line-height: 1.6;",
                             h4("What's in this dataset"),
                             p(strong(textOutput("aboutCount", inline = TRUE)), " phones total, spanning 1996-2026: ",
                               strong(textOutput("aboutLegacy", inline = TRUE)), " legacy phones (2016-2020, NDTV dataset), ",
                               strong(textOutput("aboutCurrent", inline = TRUE)), " current phones (2021-2026, hand-researched from live India pricing pages), and ",
                               strong(textOutput("aboutGsm", inline = TRUE)), " classic-through-modern phones (1996-2026) from a GSMArena historical dataset."),
                             p("Every row is a real, named phone model — nothing here is synthetic or fabricated to inflate the row count."),
                             p("Newer spec columns (chipset, fast-charging wattage, refresh rate, NFC, 5G, launch year, rear camera setup) ",
                               "are blank for legacy phones because the original NDTV dataset didn't capture them — left as NA rather than guessed."),
                             tags$hr(),
                             h4("Sources"),
                             tags$ul(
                               tags$li("Legacy phones: NDTV Gadgets 360 mobile dataset"),
                               tags$li("Historical phones (1996-2020): GSMArena dataset (gsm.csv)"),
                               tags$li("Current phones: 91mobiles, Smartprix, Bajaj Finserv, TelecomTalk — pricing checked September 2026")
                             )
                    )
             )
           )
  )
)

# ---------------------------------------------------------
# Server
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  selected_ids <- reactiveVal(integer(0))
  
  filtered <- reactive({
    df <- phones
    if (nchar(trimws(input$search)) > 0) {
      # Plain-text match (case-insensitive) so characters like ( or + don't crash the search
      df <- df %>% filter(grepl(tolower(trimws(input$search)), tolower(Name), fixed = TRUE))
    }
    if (input$era != "")   df <- df %>% filter(Era == input$era)
    if (input$brand != "") df <- df %>% filter(Brand == input$brand)
    if (input$os != "")    df <- df %>% filter(OS == input$os)
    df <- df %>% filter(
      is.na(Price_INR) |
        (Price_INR >= input$priceRange[1] & Price_INR <= input$priceRange[2])
    )
    ram_min <- as.numeric(input$ramMin)
    df <- df %>% filter(is.na(RAM_GB) | RAM_GB >= ram_min)
    if (isTRUE(input$onlySelected)) df <- df %>% filter(id %in% selected_ids())
    df
  })
  
  output$phoneTable <- renderDT({
    df <- filtered() %>%
      transmute(
        id, Name, Brand, Era,
        Price          = Price_INR,
        `RAM (GB)`     = RAM_GB,
        `Storage (GB)` = Storage_GB,
        `Battery (mAh)`= Battery_mAh,
        `Screen (in)`  = Screen_inches,
        `Rear cam (MP)`= RearCamera_MP,
        Chipset,
        OS
      )
    datatable(
      df %>% select(-id),
      rownames = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options = list(
        pageLength = 50,
        lengthMenu = list(c(15, 25, 50, 100, -1), c("15", "25", "50", "100", "All")),
        scrollX = TRUE
      )
    ) %>% formatCurrency("Price", currency = "Rs. ", interval = 3, mark = ",", digits = 0)
  }, server = TRUE)
  
  # Keep the running set of selected phone ids, capped at MAX_COMPARE
  observeEvent(input$phoneTable_rows_selected, {
    df <- filtered()
    picked_ids <- df$id[input$phoneTable_rows_selected]
    combined <- union(selected_ids(), picked_ids)
    visible_unticked <- setdiff(df$id, picked_ids)
    combined <- setdiff(combined, visible_unticked)
    if (length(combined) > MAX_COMPARE) {
      combined <- tail(combined, MAX_COMPARE)
      showNotification(paste("You can compare up to", MAX_COMPARE, "phones at a time."),
                       type = "warning", duration = 3)
    }
    selected_ids(combined)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$clearFilters, {
    updateTextInput(session, "search", value = "")
    updateSelectInput(session, "era", selected = "")
    updateSelectInput(session, "brand", selected = "")
    updateSelectInput(session, "os", selected = "")
    updateSliderInput(session, "priceRange", value = c(price_lo, price_hi))
    updateSelectInput(session, "ramMin", selected = 0)
  })
  
  output$selectedCount <- renderText({ length(selected_ids()) })
  
  # ---------------- Compare tab ----------------
  output$compareUI <- renderUI({
    if (length(selected_ids()) == 0) {
      return(tags$div(
        style = "padding: 50px; text-align:center; color:#888;",
        "Nothing to compare yet — go to Browse and tick a couple of phones."
      ))
    }
    tagList(
      tableOutput("compareTable"),
      tags$p(tags$em("Blank cells mean that spec wasn't available for that phone."))
    )
  })
  
  output$compareTable <- renderTable({
    ids <- selected_ids()
    df <- phones %>% filter(id %in% ids)
    df <- df[match(ids, df$id), , drop = FALSE]
    
    out <- data.frame(Spec = compare_specs$Spec, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(df))) {
      col <- character(nrow(compare_specs))
      for (r in seq_len(nrow(compare_specs))) {
        key <- compare_specs$key[r]
        # If a column is missing from the CSV, show "-" instead of crashing
        v <- if (key %in% names(df)) df[[key]][i] else NA
        if (key == "Price_INR") {
          col[r] <- fmt_inr(v)
        } else if (key %in% c("Network_5G", "Network_4G", "NFC", "WiFi", "Bluetooth", "GPS")) {
          col[r] <- ifelse(identical(v, "Yes"), "Yes", ifelse(is.na(v), "-", "No"))
        } else if (is.numeric(v)) {
          col[r] <- ifelse(is.na(v), "-", format(round(v, 1), big.mark = ","))
        } else {
          col[r] <- ifelse(is.na(v), "-", as.character(v))
        }
      }
      out[[df$Name[i]]] <- col
    }
    out
  }, striped = TRUE, bordered = TRUE, spacing = "m", align = "l", sanitize.text.function = function(x) x)
  
  # ---------------- Insights tab ----------------
  output$statCount <- renderText({ format(nrow(phones), big.mark = ",") })
  output$statAvg   <- renderText({ fmt_inr(mean(phones$Price_INR, na.rm = TRUE)) })
  output$statRange <- renderText({
    paste0(fmt_inr(min(phones$Price_INR, na.rm = TRUE)), " - ",
           fmt_inr(max(phones$Price_INR, na.rm = TRUE)))
  })
  output$statBrand <- renderText({
    tbl <- sort(table(phones$Brand), decreasing = TRUE)
    paste0(names(tbl)[1], " (", tbl[1], " models)")
  })
  
  output$brandPricePlot <- renderPlot({
    top_brands <- phones %>% count(Brand, sort = TRUE) %>% slice_head(n = 12) %>% pull(Brand)
    phones %>%
      filter(Brand %in% top_brands) %>%
      group_by(Brand) %>%
      summarise(AvgPrice = mean(Price_INR, na.rm = TRUE), .groups = "drop") %>%
      arrange(AvgPrice) %>%
      mutate(Brand = factor(Brand, levels = Brand)) %>%
      ggplot(aes(x = Brand, y = AvgPrice)) +
      geom_col(fill = "#5b8cff") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Average price by brand (top 12 by model count)",
           x = NULL, y = "Average price (Rs.)") +
      theme_minimal(base_size = 13)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(phones, aes(x = RAM_GB, y = Price_INR, color = Era)) +
      geom_point(alpha = 0.5, na.rm = TRUE) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Price vs. RAM", x = "RAM (GB)", y = "Price (Rs.)", color = NULL) +
      theme_minimal(base_size = 13)
  })
  
  output$histPlot <- renderPlot({
    ggplot(phones, aes(x = Price_INR)) +
      geom_histogram(bins = 30, fill = "#ffb454", color = "white", na.rm = TRUE) +
      scale_x_continuous(labels = scales::comma) +
      labs(title = "Price distribution", x = "Price (Rs.)", y = "Number of phones") +
      theme_minimal(base_size = 13)
  })
  
  output$eraPlot <- renderPlot({
    phones %>%
      group_by(Era) %>%
      summarise(AvgPrice = mean(Price_INR, na.rm = TRUE), Count = n(), .groups = "drop") %>%
      ggplot(aes(x = Era, y = AvgPrice, fill = Era)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      geom_text(aes(label = paste0("n=", Count)), vjust = -0.5) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Average price by era", x = NULL, y = "Average price (Rs.)") +
      theme_minimal(base_size = 13)
  })
  
  # ---------------- About tab ----------------
  output$aboutCount   <- renderText({ format(nrow(phones), big.mark = ",") })
  output$aboutLegacy  <- renderText({
    format(sum(phones$Era == "Legacy (2016-2020)", na.rm = TRUE), big.mark = ",")
  })
  output$aboutCurrent <- renderText({
    format(sum(phones$Era %in% c("Current (2025-2026)", "Modern (2021-2026)"), na.rm = TRUE),
           big.mark = ",")
  })
  output$aboutGsm     <- renderText({
    if ("Source" %in% names(phones)) {
      format(sum(phones$Source %in% "Kaggle GSMArena dataset (msainani)", na.rm = TRUE),
             big.mark = ",")
    } else {
      "0"
    }
  })
}

shinyApp(ui, server)