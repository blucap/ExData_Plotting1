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

variables <- paste0("Sub_metering_", 1:3)
colors <- c("black", "red", "blue")

# Plot 4
png("plot4.png", width=480, height=480)
par(mfrow=c(2, 2))

### Subplot 1
plot(df$datetime, df$Global_active_power, type="l", xlab="", ylab="Global Active Power")

### Subplot 2
plot(df$datetime, df$Voltage, type="l", xlab="datetime", ylab="Voltage")

### Subplot 3
plot(df$datetime, df$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
for (i in seq_along(variables)) {
    lines(df$datetime, df[[variables[i]]], col=colors[i])
}
legend("topright", bty="n", legend=variables, col=colors, lty="solid")

### Subplot 4
plot(df$datetime, df$Global_reactive_power, type="l", xlab="datetime")
dev.off()
