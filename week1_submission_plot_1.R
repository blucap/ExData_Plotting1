library(tidyverse)
library(readr)

get_df <- function(zipfile, fn) {
    df <- read_csv2(unz(zipfile, fn))
    df <- as_tibble(df)
}

dubbel <- function(x) (as.double(x))

df <- get_df("exdata_data_household_power_consumption.zip", "household_power_consumption.txt")
df <- df %>%
    replace(.=="?", NA) %>%
    filter(complete.cases(.)) %>%
    mutate(newdate = strptime(as.character(Date), format = "%d/%m/%Y")) %>%
    filter(newdate >= "2007-02-01" & newdate <= "2007-02-02") %>%
    mutate_at(c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), dubbel) %>%
    mutate(datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))

# Plot 1
png("plot1.png", width=480, height=480)
hist(df$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()
