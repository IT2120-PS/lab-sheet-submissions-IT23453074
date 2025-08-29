setwd("C:/Users/rusir/OneDrive/Desktop/IT23453074") 

# Read the data (it's comma-separated with a header)
shareholders_data <- read.csv("Data.txt", header = TRUE)

# Extract the shareholder numbers into a vector
shareholders <- shareholders_data$Number_of_Shareholders.thousands.

# View the data to confirm
print(shareholders)

# Default histogram
hist(shareholders, main = "Histogram of Shareholders", xlab = "Number of Shareholders (thousands)", col = "lightblue")

# Define breaks for seven classes: 130 to 150, 150 to 170, ..., 250 to 270
breaks <- seq(130, 270, by = 20)

# Histogram with specified breaks
hist(shareholders, breaks = breaks, main = "Histogram of Shareholders (7 Classes)", xlab = "Number of Shareholders (thousands)", col = "lightblue", right = FALSE)  # right=FALSE for left-closed intervals

# Create frequency distribution
classes <- cut(shareholders, breaks = breaks, include.lowest = TRUE, right = FALSE)
freq_table <- table(classes)

# Display the table
print(freq_table)

# Get midpoints of the classes
midpoints <- seq(140, 260, by = 20)  # Midpoints: 140, 160, ..., 260

# Plot frequency polygon
plot(midpoints, freq_table, type = "o", main = "Frequency Polygon of Shareholders", xlab = "Number of Shareholders (thousands)", ylab = "Frequency", col = "blue", pch = 19)

# Cumulative frequencies
cum_freq <- cumsum(freq_table)

# Plot ogive (add 0 at the start for the lower bound)
plot(c(130, midpoints), c(0, cum_freq), type = "o", main = "Ogive of Shareholders", xlab = "Number of Shareholders (thousands)", ylab = "Cumulative Frequency", col = "green", pch = 19)

# Import the data (assuming it's tab-separated or space-separated; adjust sep if needed)
delivery_times <- read.table("Exercise - Lab 05.txt", header = TRUE, sep = ",")

# If it's one column, extract to a vector
delivery <- delivery_times$Delivery_Time_.minutes.

# View the data to confirm
print(delivery)

# Define breaks for nine classes
breaks_delivery <- seq(20, 70, length.out = 10)  # 10 points for 9 intervals

# Histogram
hist(delivery, breaks = breaks_delivery, main = "Histogram of Delivery Times (9 Classes)", xlab = "Delivery Time (minutes)", col = "lightgreen", right = FALSE)

# Frequency table
classes_delivery <- cut(delivery, breaks = breaks_delivery, include.lowest = TRUE, right = FALSE)
freq_table_delivery <- table(classes_delivery)

# Cumulative frequencies
cum_freq_delivery <- cumsum(freq_table_delivery)

# Midpoints (average of breaks)
midpoints_delivery <- (breaks_delivery[-1] + breaks_delivery[-length(breaks_delivery)]) / 2

# Plot ogive
plot(c(20, midpoints_delivery), c(0, cum_freq_delivery), type = "o", main = "Ogive of Delivery Times", xlab = "Delivery Time (minutes)", ylab = "Cumulative Frequency", col = "red", pch = 19)


