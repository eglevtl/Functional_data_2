library(quantmod)
library(dplyr)
library(writexl)

# Tickeriai
tickers <- c(
  "GC=F",      # Gold
  "SI=F",      # Silver
  "PA=F",      # Palladium
  "PL=F",      # Platinum
  "DX-Y.NYB",  # US Dollar Index
  "JPY=X",     # Japanese Yen
  "CHF=X",     # Swiss Franc
  "EURUSD=X",  # Euro
  "ZN=F",      # 10Y Treasury Note
  "HG=F",      # Copper
  "ALI=F",     # Aluminum
  "RB=F",      # Gasoline
  "CL=F",      # Crude Oil
  "NG=F",      # Natural Gas
  "ZW=F",      # Wheat
  "ZC=F",      # Corn
  "ZS=F",      # Soybeans
  "KC=F",      # Coffee
  "SB=F",      # Sugar
  "CT=F"       # Cotton
)

# Stulpelių pavadinimai
col_names <- c(
  "Gold",
  "Silver",
  "Palladium",
  "Platinum",
  "US_Dollar_Index",
  "Japanese_Yen",
  "Swiss_Franc",
  "Euro",
  "10-Year_Treasury_Note",
  "Copper",
  "Aluminum",
  "Gasoline",
  "Crude_Oil",
  "Natural_Gas",
  "Wheat",
  "Corn",
  "Soybeans",
  "Coffee",
  "Sugar",
  "Cotton"
)

start_date <- as.Date("2024-04-02")
end_date   <- as.Date("2025-04-02")

prices <- list()

for(i in seq_along(tickers)) {

  tk <- tickers[i]
  nm <- col_names[i]

  cat("Downloading:", tk, "(", nm, ")\n")

  tryCatch({

    x <- getSymbols(
      tk,
      src = "yahoo",
      from = start_date,
      to   = end_date,
      auto.assign = FALSE
    )

    # Close price
    cl <- Cl(x)

    df <- data.frame(
      Date = as.Date(index(cl)),
      value = as.numeric(cl)
    )

    names(df)[2] <- nm

    # Euro correction
    if(nm == "Euro") {
      df[[nm]] <- 1 / df[[nm]]
    }

    prices[[i]] <- df

  }, error = function(e) {

    cat("FAILED:", tk, "-", e$message, "\n")

  })
}

px <- Reduce(
  function(x, y)
    dplyr::inner_join(x, y, by = "Date"),
  prices
)

px <- px %>%
  dplyr::arrange(Date)

px <- px[, c("Date", col_names)]


write_xlsx(
  px,
  "Yfinance_close_prices_volatility.xlsx"
)
