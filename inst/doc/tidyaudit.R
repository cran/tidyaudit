## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(tidyaudit)
library(dplyr)

## ----basic-trail--------------------------------------------------------------
# Sample data
orders <- data.frame(
  id       = 1:20,
  customer = rep(c("Alice", "Bob", "Carol", "Dan", "Eve"), 4),
  amount   = c(150, 200, 50, 300, 75, 120, 400, 90, 250, 60,
               180, 210, 45, 320, 85, 130, 380, 95, 270, 55),
  status   = rep(c("complete", "pending", "complete", "cancelled", "complete"), 4)
)

trail <- audit_trail("order_pipeline")

result <- orders |>
  audit_tap(trail, "raw") |>
  filter(status == "complete") |>
  audit_tap(trail, "complete_only") |>
  mutate(tax = amount * 0.1) |>
  audit_tap(trail, "with_tax")

## ----print-trail--------------------------------------------------------------
print(trail)

## ----join-tap-----------------------------------------------------------------
customers <- data.frame(
  customer = c("Alice", "Bob", "Carol", "Dan"),
  region   = c("East", "West", "East", "North")
)

trail2 <- audit_trail("join_pipeline")

result2 <- orders |>
  audit_tap(trail2, "raw") |>
  left_join_tap(customers, by = "customer",
                .trail = trail2, .label = "with_region")

print(trail2)

## ----filter-tap---------------------------------------------------------------
trail3 <- audit_trail("filter_pipeline")

result3 <- orders |>
  audit_tap(trail3, "raw") |>
  filter_tap(status == "complete",
             .trail = trail3, .label = "complete_only") |>
  filter_tap(amount > 100,
             .trail = trail3, .label = "high_value",
             .stat = amount)

print(trail3)

## ----audit-diff---------------------------------------------------------------
audit_diff(trail3, "raw", "high_value")

## ----audit-report-------------------------------------------------------------
audit_report(trail3)

## ----custom-fns---------------------------------------------------------------
trail4 <- audit_trail("custom_example")

result4 <- orders |>
  audit_tap(trail4, "raw", .fns = list(
    n_complete   = ~sum(.$status == "complete"),          # scalar
    amount_stats = ~c(mean = mean(.$amount),              # named vector
                      max  = max(.$amount))
  )) |>
  filter(status == "complete") |>
  audit_tap(trail4, "complete_only", .fns = list(
    n_complete   = ~sum(.$status == "complete"),
    amount_stats = ~c(mean = mean(.$amount),
                      max  = max(.$amount))
  ))

## ----print-custom-------------------------------------------------------------
print(trail4)

## ----show-custom-false--------------------------------------------------------
print(trail4, show_custom = FALSE)

## ----snapshot-controls--------------------------------------------------------
wide_data <- cbind(orders, matrix(rnorm(20 * 50), nrow = 20))

trail_ctrl <- audit_trail("snapshot_controls")

wide_data |>
  audit_tap(trail_ctrl, "full_snapshot") |>
  audit_tap(trail_ctrl, "minimal",
            .numeric_summary = FALSE,
            .cols_include = c("id", "amount", "status"))

print(trail_ctrl)

## ----tab-standalone-----------------------------------------------------------
tab(orders, status)
tab(orders, status, customer)

## ----tab-tap------------------------------------------------------------------
trail_tab <- audit_trail("tab_pipeline")

result_tab <- orders |>
  tab_tap(status, .trail = trail_tab, .label = "status_dist") |>
  filter(status == "complete") |>
  tab_tap(customer, .trail = trail_tab, .label = "customer_dist",
          .sort = "freq_desc")

print(trail_tab)

## ----null-trail---------------------------------------------------------------
# Plain filter -- no diagnostics
orders |> filter_tap(amount > 100) |> nrow()

# Diagnostics without a trail
orders |> filter_tap(amount > 100, .stat = amount) |> invisible()

## ----trail-to-objects---------------------------------------------------------
# As a plain R list (suitable for jsonlite::toJSON())
trail_list <- trail_to_list(trail3)
str(trail_list, max.level = 2)

# As a data.frame (one row per snapshot)
trail_df <- trail_to_df(trail3)
print(trail_df)

## ----trail-rds----------------------------------------------------------------
tmp_rds <- tempfile(fileext = ".rds")
write_trail(trail3, tmp_rds)
restored <- read_trail(tmp_rds)
print(restored)

## ----trail-json---------------------------------------------------------------
tmp_json <- tempfile(fileext = ".json")
write_trail(trail3, tmp_json, format = "json")

## ----trail-html---------------------------------------------------------------
audit_export(trail3, tempfile(fileext = ".html"))

