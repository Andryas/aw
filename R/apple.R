#' @title Apple
#'
#' @description
#' It process data exported from apple health
#'
#' @param path to the export.zip from your apple health
#'
#' @export
apple <- function(path) {
    unzip(path)

    data <- XML::xmlParse("apple_health_export/export.xml")

    active_summary <- as_tibble(XML:::xmlAttrsToDataFrame(data["//ActivitySummary"]))
    active_summary <- active_summary |>
        mutate(dateComponents = ymd(dateComponents)) |>
        mutate_at(vars(-dateComponents, -activeEnergyBurnedUnit), as.numeric) |>
        filter(between(dateComponents, start_filter, end_filter))

    workouts <- as_tibble(XML:::xmlAttrsToDataFrame(data["//Workout"]))
    workouts <- workouts |>
        mutate(workoutActivityType = str_replace_all(workoutActivityType, "HKWorkoutActivityType", "")) |>
        mutate_at(vars(matches("Date$")), function(.x) ymd_hms(.x) - hours(3)) |>
        mutate_at(vars(duration, totalDistance, totalEnergyBurned), as.numeric) |>
        filter(between(ymd(format(creationDate, "%Y-%m-%d")), start_filter, end_filter))
    workouts <- workouts |>
        select(startDate, workoutActivityType, duration, durationUnit) |>
        rename(ymd = startDate, group = workoutActivityType, value = duration, type = durationUnit) |>
        mutate(
            ymd = ymd(format(ymd_hms(ymd), "%Y-%m-%d")),
            source = "workouts"
        ) |>
        group_by(ymd, group, type, source) |>
        summarise(value = sum(value))

    records <- as_tibble(XML:::xmlAttrsToDataFrame(data["//Record"]))
    records <- records |>
        mutate(
            sourceName = stringi::stri_trans_general(tolower(sourceName), "Latin-ASCII"),
            type = str_replace_all(type, "HKQuantityTypeIdentifier|HKDataType|HKCategoryTypeIdentifier", ""),
            value = as.numeric(value)
        ) |>
        mutate_at(vars(matches("Date$")), function(.x) ymd_hms(.x) - hours(3)) |>
        distinct(creationDate, startDate, endDate, type, value, .keep_all = TRUE) |>
        filter(between(ymd(format(creationDate, "%Y-%m-%d")), start_filter, end_filter))

    records <- records |>
        filter(str_detect(sourceName, "apple watch")) |>
        filter(type %in% c("BodyMass", "HeartRate", "RespiratoryRate", "BodyFatPercentage", "StepCount", "ActiveEnergyBurned", "SleepAnalysis", "HeartRateVariabilitySDNN")) |>
        mutate(
            value = ifelse(type == "SleepAnalysis", as.numeric(endDate - startDate) / (3600), value),
            ymd = ymd(format(startDate, "%Y-%m-%d"))
        )

    r1 <- records |>
        filter(!(type %in% c("HeartRate", "HeartRateVariabilitySDNN")))

    r2 <- records |>
        filter(type %in% c("HeartRate", "HeartRateVariabilitySDNN"))

    r1 <- r1 |>
        group_by(ymd, type) |>
        summarise(value = sum(value), unit = unique(unit)) |>
        ungroup() |>
        mutate(
            unit = ifelse(type == "SleepAnalysis", "hour", tolower(unit)),
            source = "records"
        ) |>
        rename(group = type, type = unit)

    r2 <- r2 |>
        group_by(ymd, type) |>
        summarise(
            min = min(value),
            avg = mean(value),
            max = max(value),
            sd = sd(value),
            unit = unique(unit)
        ) |>
        gather(variable, value, -ymd, -type, -unit) |>
        ungroup() |>
        mutate(
            unit = str_c(unit, "_avg"),
            type = str_c(type, "_", variable),
            source = "records"
        ) |>
        select(-variable) |>
        rename(group = type, type = unit)

    r <- r1 |>
        bind_rows(r2)

    dbWriteTable(con, "processed", r, append = TRUE, row.names = FALSE)

    unlink("apple_health_export", recursive = TRUE)
}
