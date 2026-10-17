#..........................Data Science Project Group-09(MID)......................
#link : https://drive.google.com/file/d/1WOPXkKl4L6VQemKx_-53FLY6flG2_uyf/view?usp=sharing
# INSTALL & LOAD PACKAGES

#======================================================
#       ID      Name                Group No
#  21-45953-3   HASAN, MD. FAHAD      9
#  22-47152-1   SAHA, BEDANTA         9
#  22-47153-1   SAHA, BIDHAN          9

#======================================================
packages <- c(
  "ggplot2",
  "corrplot",
  "dplyr",
  "RColorBrewer",
  "tidyr",
  "e1071",
  "treemap",
  "infotheo"
)
for(pkg in packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#--------------------------------------
# Create folder for plots
#--------------------------------------
if(!dir.exists("plots")){
  dir.create("plots")
}

# ============================================================
# A. DATA UNDERSTANDING
# ============================================================

#--------------------------------------------------------------
# 1. Load the dataset into R using a link to your Google Drive.
#--------------------------------------------------------------
url  <- "https://drive.google.com/uc?export=download&id=1WOPXkKl4L6VQemKx_-53FLY6flG2_uyf"
data <- read.csv(url)
#------------------------------------------------------------
# 2. Display the first few rows of the dataset. 
#------------------------------------------------------------
print(" --- First 5 Rows of Dataset --- ")
print (head(data))
View(data)
#------------------------------------------------------------
# 3. Show shape (rows × columns). 
#------------------------------------------------------------
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")
#------------------------------------------------------------
# 4. Display data types of each column. 
#------------------------------------------------------------
str(data)
summary(data)
#------------------------------------------------------
# Identify categorical and numerical features.
# Convert BMI to numeric (handle "N/A" string) 
#------------------------------------------------------
data$bmi <- as.numeric(ifelse(data$bmi == "N/A", NA, data$bmi))
data$smoking_status[data$smoking_status == "Unknown"] <- NA
numerical_features  <- names(data)[sapply(data, is.numeric)]
categorical_features <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
cat("Numerical features: ", paste(numerical_features,  collapse = ", "), "\n")
cat("Categorical features:", paste(categorical_features, collapse = ", "), "\n")
#-----------------------------------------------------------------------------------------
# Helper function
#-----------------------------------------------------------------------------------------
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
#-----------------------------------------------------------------------------------------
# 5. Loop through all numerical columns to generate required stats
#-----------------------------------------------------------------------------------------
for (col in numerical_features) {
  if (col != "id") { 
    cat("\n --- Statistics for:", col, " --- \n")
# Descriptive statistics
numeric_cols <- sapply(data, is.numeric)
summary_stats <- data.frame(
  Mean     = sapply(data[, numeric_cols], mean,     na.rm = TRUE),
  Median   = sapply(data[, numeric_cols], median,   na.rm = TRUE),
  Mode     = sapply(data[, numeric_cols], get_mode),
  Std       = sapply(data[, numeric_cols], sd,       na.rm = TRUE),
  Min      = sapply(data[, numeric_cols], min,      na.rm = TRUE),
  Max      = sapply(data[, numeric_cols], max,      na.rm = TRUE),
  Count    = sapply(data[, numeric_cols], function(x) sum(!is.na(x))),
  Skewness = sapply(data[, numeric_cols], skewness, na.rm = TRUE)
)
print(summary_stats)
   }
  }
# ============================================================
# B. DATA EXPLORATION & VISUALIZATION
# ============================================================
#-------------------------------------------------------------------------
# Data Imputation
#-------------------------------------------------------------------------
bmi_median   <- median(data$bmi, na.rm = TRUE)
smoking_mode <- get_mode(data$smoking_status)
data$bmi[is.na(data$bmi)]                       <- bmi_median
data$smoking_status[is.na(data$smoking_status)] <- smoking_mode
cat("BMI NAs replaced with median:", bmi_median, "\n")
cat("Smoking NAs replaced with mode:", smoking_mode, "\n")
cat("Remaining NAs — BMI:", sum(is.na(data$bmi)),
    "| Smoking:", sum(is.na(data$smoking_status)), "\n")
#-----------------------------------------------------------
#   Univariate Analysis
#-----------------------------------------------------------
#-----------------------------------------------------------
# 1. Histograms for all numeric columns
#-----------------------------------------------------------
graphics.off()

numeric_cols <- names(data)[sapply(data, is.numeric)]
numeric_cols <- numeric_cols[numeric_cols != "id"]

for (col in numeric_cols) {
  
  png(paste0("plots/Histogram_", col, ".png"),
      width = 1200, height = 800)
  
  x <- data[[col]]
  
  hist(x,
       main = paste("Distribution of", col),
       xlab = col,
       col = "steelblue",
       border = "white",
       breaks = 30)
  
  mean_val <- mean(x, na.rm = TRUE)
  
  abline(v = mean_val, col = "red", lwd = 2, lty = 2)
  
  legend("topright",
         legend = paste("Mean =", round(mean_val, 2)),
         col = "red",
         lty = 2,
         lwd = 2,
         cex = 0.8)
  
  dev.off()
}

#-----------------------------------------------------------
# 2. Boxplots for all numeric columns
#-----------------------------------------------------------
graphics.off()

for (col in numeric_cols) {
  
  png(paste0("plots/Boxplot_", col, ".png"),
      width = 1200, height = 800)
  
  x <- data[[col]]
  
  boxplot(x,
          main = paste("Boxplot of", col),
          ylab = col,
          col = "tomato",
          outline = TRUE)
  
  dev.off()
}

#-----------------------------------------------------------
# 3. Bar Charts for Categorical Variables
#-----------------------------------------------------------
graphics.off()

cat_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

for (col in cat_cols) {
  
  png(paste0("plots/Bar_", col, ".png"),
      width = 1200, height = 800)
  
  freq_table <- sort(table(data[[col]]), decreasing = TRUE)
  
  n_colors <- max(3, min(length(freq_table), 8))
  
  barplot(freq_table,
          main = paste("Frequency:", col),
          col = RColorBrewer::brewer.pal(n_colors, "Set2"),
          las = 2,
          cex.names = 0.8,
          ylab = "Count")
  
  dev.off()
}

#-----------------------------------------------------------
# 4. Bubble Charts for Numeric Pairs
#-----------------------------------------------------------
graphics.off()

for (i in 1:(length(numeric_cols)-1)) {
  for (j in (i+1):length(numeric_cols)) {
    
    x_col <- numeric_cols[i]
    y_col <- numeric_cols[j]
    
    png(paste0("plots/Bubble_", x_col, "_vs_", y_col, ".png"),
        width = 1200, height = 800)
    
    size_col <- numeric_cols[j]
    
    plot(data[[x_col]], data[[y_col]],
         main = paste("Bubble Chart:", x_col, "vs", y_col),
         xlab = x_col,
         ylab = y_col,
         pch = 21,
         bg = rgb(0.2,0.4,0.6,0.5),
         cex = ifelse(is.na(data[[size_col]]), 1,
                      data[[size_col]] / max(data[[size_col]], na.rm=TRUE) * 5))
    
    abline(lm(data[[y_col]] ~ data[[x_col]]),
           col = "red",
           lwd = 2)
    
    dev.off()
  }
}

#-----------------------------------------------------------
# 5. Pie Charts for Categorical Variables
#-----------------------------------------------------------
graphics.off()

for (col in cat_cols) {
  
  png(paste0("plots/Pie_", col, ".png"),
      width = 1200, height = 800)
  
  freq_table <- table(data[[col]])
  
  n_colors <- max(3, min(length(freq_table), 8))
  
  pie(freq_table,
      main = paste("Pie Chart of", col),
      col = RColorBrewer::brewer.pal(n_colors, "Set2"))
  
  dev.off()
}

#-----------------------------------------------------------
# 6. Treemaps for Categorical Variables
#-----------------------------------------------------------
graphics.off()

for (col in cat_cols) {
  
  png(paste0("plots/Treemap_", col, ".png"),
      width = 1200, height = 800)
  
  treemap_data <- data |>
    dplyr::count(.data[[col]])
  
  treemap::treemap(treemap_data,
                   index = col,
                   vSize = "n",
                   title = paste("Treemap of", col),
                   palette = "Set2",
                   border.col = "white")
  
  dev.off()
}

#---------------------------------------------------------
# 1.2 Frequency Table for Categorical Variables
#---------------------------------------------------------
# Identify categorical columns
cat_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
for (col in cat_cols) {
  freq_table <- table(data[[col]])
  cat("Frequency Table for:", col, "\n")
  print(freq_table)
}

#----------------------------------------
#   Bivariate Analysis
#----------------------------------------
#---------------------------------------------------
# 1 Correlation matrix (heatmap)
#---------------------------------------------------
graphics.off()

numeric_cols <- names(data)[sapply(data, is.numeric)]

numeric_cols <- setdiff(numeric_cols,
                        c("id","gender","ever_married",
                          "Residence_type","stroke_factor"))

cor_matrix <- cor(data[, numeric_cols], use = "complete.obs")

png("plots/Correlation_Heatmap.png", width = 1200, height = 900)

corrplot::corrplot(cor_matrix,
                   method="color",
                   type="upper",
                   tl.col="black",
                   tl.cex=0.85,
                   addCoef.col="black",
                   number.cex=0.7,
                   col=colorRampPalette(c("#2166AC","white","#D6604D"))(200),
                   title="Correlation Heatmap",
                   mar=c(0,0,1,0))

dev.off()

#-----------------------------------------------
# 2 Scatter plots for numeric pairs
#-----------------------------------------------
graphics.off()

data$stroke_factor <- factor(data$stroke,
                             labels = c("No Stroke", "Stroke"))

numeric_cols <- names(data)[sapply(data, is.numeric)]
numeric_cols <- setdiff(numeric_cols, "id")

numeric_pairs <- combn(numeric_cols, 2, simplify = FALSE)

for (pair in numeric_pairs) {
  
  x_col <- pair[1]
  y_col <- pair[2]
  
  png(paste0("plots/Scatter_", x_col, "_vs_", y_col, ".png"),
      width = 1200, height = 800)
  
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x_col]],
                 y = .data[[y_col]],
                 color = stroke_factor)
  ) +
    ggplot2::geom_point(alpha = 0.5, size = 2) +
    ggplot2::scale_color_manual(values = c("steelblue","tomato")) +
    ggplot2::labs(title = paste(x_col,"vs",y_col),
                  color = "Stroke") +
    ggplot2::theme_minimal()
  
  print(p)
  
  dev.off()
}
#-----------------------------------------------------
# 3 Boxplots (Categorical vs Numeric)
#-----------------------------------------------------
graphics.off()

data$stroke_factor <- factor(data$stroke,
                             labels = c("No Stroke","Stroke"))

numeric_cols <- names(data)[sapply(data, is.numeric)]
numeric_cols <- setdiff(numeric_cols, "id")

cat_cols <- c("stroke_factor","work_type","smoking_status")

for (cat_col in cat_cols) {
  
  for (num_col in numeric_cols) {
    
    png(paste0("plots/Boxplot_", cat_col, "_vs_", num_col, ".png"),
        width = 1200, height = 800)
    
    p <- ggplot2::ggplot(data,
                         ggplot2::aes_string(x = cat_col,
                                             y = num_col,
                                             fill = cat_col)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(title = paste(cat_col,"vs",num_col),
                    x = cat_col,
                    y = num_col) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
      )
    
    if (cat_col == "stroke_factor") {
      p <- p + ggplot2::scale_fill_manual(values = c("steelblue","tomato"))
    }
    
    print(p)
    
    dev.off()
  }
}
#-----------------------------------------------------------------------
#  Identify patterns, skewness, and possible outliers
#-----------------------------------------------------------------------

# Skewness & Outlier Summary
# Identify numeric columns
numeric_cols_names <- names(data)[sapply(data, is.numeric)]
print(numeric_cols_names)
# Calculate skewness for numeric features
skew_values <- sapply(data[, numeric_cols_names], skewness, na.rm = TRUE)
print(skew_values)
# Function to count outliers
count_outliers <- function(x) {
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  sum(x < lower | x > upper, na.rm = TRUE)
}

# Count outliers for each numeric column
outlier_counts <- sapply(data[, numeric_cols_names], count_outliers)
print(outlier_counts)




#-----------------------------------------------
#  Create skewness and outlier table
#-----------------------------------------------
skew_df <- data.frame(
  Column   = numeric_cols_names,
  Skewness = sapply(data[, numeric_cols_names], skewness, na.rm = TRUE),
  Outliers = sapply(data[, numeric_cols_names], function(x) {
    Q1      <- quantile(x, 0.25, na.rm = TRUE)
    Q3      <- quantile(x, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    sum(x < (Q1 - 1.5 * IQR_val) | x > (Q3 + 1.5 * IQR_val), na.rm = TRUE)
  })
)
#-------------------------------------------------
#  Interpret skewness
#-------------------------------------------------
skew_df$Skew_Interpretation <- ifelse(
  abs(skew_df$Skewness) < 0.5, "Symmetric",
  ifelse(skew_df$Skewness > 0, "Right-Skewed", "Left-Skewed")
)
print(skew_df)

#===============================================
# C. DATA PREPROCESSING
#===============================================
#-----------------------------------------------
#  1 Missing Value Handling 
#-----------------------------------------------
#Handling
print(colSums(is.na(data)))
cat("Total missing values:", sum(is.na(data)), "\n")
# Add Number of missing values to insert per column
set.seed(123)  # for reproducibility
n_na <- 500  # change as needed
for (col in names(data)) {
  na_indices <- sample(1:nrow(data), n_na)
  data[na_indices, col] <- NA
}
# Check missing values
colSums(is.na(data))
#--------------------------------------------
#  Replace or remove 
#--------------------------------------------
# Helper function to get mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
# Choose method for numeric columns: "mean" or "median"
numeric_method <- "median"  # change to "mean" if you want mean
# Loop through all columns
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    if (numeric_method == "mean") {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    } else if (numeric_method == "median") {
      data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
    }
  } else if (is.character(data[[col]]) || is.factor(data[[col]])) {
    # Replace categorical NAs with mode
    mode_val <- get_mode(data[[col]])
    data[[col]][is.na(data[[col]])] <- mode_val
  }
}
# Check missing values
colSums(is.na(data))

#---------------------------------------------------
#  2 Outlier Handling (IQR Method)
#---------------------------------------------------
# Automatically select all numeric columns (excluding ID if present)
numeric_features <- names(data)[sapply(data, is.numeric)]
numeric_features <- setdiff(numeric_features, "id")  # remove 'id' if needed
print(numeric_features)
numeric_features <- c()  # empty vector
for (col in names(data)) {
  if (is.numeric(data[[col]]) && col != "id") {
    numeric_features <- c(numeric_features, col)
  }
}
print(numeric_features)
cat("=== Outlier Detection (IQR Method) ===\n")
outlier_summary <- data.frame(
  Feature       = numeric_features,
  Q1            = sapply(data[, numeric_features], function(x) quantile(x, 0.25, na.rm = TRUE)),
  Q3            = sapply(data[, numeric_features], function(x) quantile(x, 0.75, na.rm = TRUE)),
  IQR           = sapply(data[, numeric_features], IQR, na.rm = TRUE),
  Lower_Fence   = sapply(data[, numeric_features], function(x) quantile(x, 0.25) - 1.5 * IQR(x)),
  Upper_Fence   = sapply(data[, numeric_features], function(x) quantile(x, 0.75) + 1.5 * IQR(x)),
  Outlier_Count = sapply(data[, numeric_features], function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    sum(x < Q1 - 1.5 * IQR(x) | x > Q3 + 1.5 * IQR(x), na.rm = TRUE)
  })
)
print(outlier_summary)

#------------------ Boxplots BEFORE capping ------------------
graphics.off()
for (col in numeric_features) {
  png(paste0("plots/Boxplot_BeforeCap_", col, ".png"),
      width = 1200, height = 800)
  boxplot(data[[col]],
          main = paste("Before Cap:", col),
          ylab = col,
          col = "tomato",
          border = "black",
          outline = TRUE)
  
  dev.off()
}
#-------------------------------------------------
# 2 Handling Outliers  
#-------------------------------------------------
#-------- 2.2Inject extreme outliers for demonstration
set.seed(99)
data$bmi[sample(nrow(data), 15)]              <- c(rep(80, 8), rep(5, 7))
data$avg_glucose_level[sample(nrow(data), 10)] <- c(rep(400, 5), rep(20, 5))
cat("Manual outliers injected into bmi and avg_glucose_level\n")

# Winsorization (IQR capping)
cap_outliers <- function(x) {
  Q1    <- quantile(x, 0.25, na.rm = TRUE)
  Q3    <- quantile(x, 0.75, na.rm = TRUE)
  iqr   <- IQR(x, na.rm = TRUE)
  lower <- Q1 - 1.5 * iqr
  upper <- Q3 + 1.5 * iqr
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

data$bmi               <- cap_outliers(data$bmi)
data$avg_glucose_level <- cap_outliers(data$avg_glucose_level)
data$age               <- cap_outliers(data$age)
cat("Outliers capped using Winsorization (IQR fences)\n")

# Boxplots AFTER capping
graphics.off()
# create plots folder if not exists
if(!dir.exists("plots")){
  dir.create("plots")
}
for (col in numeric_features) {
  png(paste0("plots/Boxplot_AfterCap_", col, ".png"),
      width = 1200, height = 800)
  boxplot(data[[col]],
          main = paste("After Cap:", col),
          ylab = col,
          col = "steelblue",
          border = "black",
          outline = TRUE)
  dev.off()
  }
#--------------------------------------------------
#  C.3 Encoding Categorical Variables 
#--------------------------------------------------
data$id <- NULL  # drop ID column

# Binary encoding
data$gender         <- ifelse(data$gender == "Male", 1, 0)
data$ever_married   <- ifelse(data$ever_married == "Yes", 1, 0)
data$Residence_type <- ifelse(data$Residence_type == "Urban", 1, 0)
cat("Binary encoding applied: gender, ever_married, Residence_type\n")

# One-hot encoding
work_dummies  <- model.matrix(~ work_type - 1, data = data)
smoke_dummies <- model.matrix(~ smoking_status - 1, data = data)
colnames(work_dummies)  <- gsub("work_type",      "work_",  colnames(work_dummies))
colnames(smoke_dummies) <- gsub("smoking_status", "smoke_", colnames(smoke_dummies))

data_encoded <- cbind(
  data[, !names(data) %in% c("work_type", "smoking_status", "stroke_factor")],
  work_dummies,
  smoke_dummies
)

cat("One-hot encoding applied: work_type, smoking_status\n")
cat("Encoded dataset dimensions:", dim(data_encoded), "\n")
cat("All columns numeric:", all(sapply(data_encoded, is.numeric)), "\n")
print(head(data_encoded, 3))

# ------------------------------------------------------------
# 4 DATA TRANSFORMATION
# ------------------------------------------------------------

# Numeric columns to transform
numeric_features <- c("age", "avg_glucose_level", "bmi")

# --- Skewness BEFORE transformation ---
cat("=== Skewness BEFORE Transformation ===\n")
skew_before <- sapply(data_encoded[, numeric_features], skewness, na.rm = TRUE)
print(round(skew_before, 3))

# --- Log / Sqrt transformation ---
data_encoded$avg_glucose_level <- log1p(data_encoded$avg_glucose_level)
data_encoded$bmi <- sqrt(data_encoded$bmi)

# --- Skewness AFTER transformation ---
cat("\n=== Skewness AFTER Transformation ===\n")
skew_after <- sapply(data_encoded[, numeric_features], skewness, na.rm = TRUE)
print(round(skew_after, 3))

# -------- Min-Max Scaling  --------
min_max_scale <- function(x){
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

cols_to_scale <- c("age", "avg_glucose_level", "bmi")

data_final <- data_encoded
data_final[, cols_to_scale] <- lapply(data_encoded[, cols_to_scale], min_max_scale)

cat("\nMin-Max Scaling Applied\n")
print(sapply(data_final[, cols_to_scale], range))
cat("Final dataset dimensions:", dim(data_final), "\n")

#-------- Z-score normalization function----------
z_score_scale <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# --------Apply Z-score normalization---------------
data_final[, cols_to_scale] <- lapply(
  data_encoded[, cols_to_scale],
  z_score_scale
)
# --- ADD THIS TO SEE THE RESULTS ---
cat("\n=== Z-Score Normalization Applied ===\n")

# Check the first few rows
print(head(data_final[, cols_to_scale]))

# Verify the properties of Z-score (Mean should be ~0, SD should be 1)
cat("\nSummary of Z-scored columns (Mean and SD):\n")
summary_stats <- sapply(data_final[, cols_to_scale], function(x) {
  c(Mean = round(mean(x, na.rm=TRUE), 4), SD = sd(x, na.rm=TRUE))
})
print(summary_stats)

# ------------------------------------------------------
# 5 FEATURE SELECTION
# ------------------------------------------------------

#-------------------------------------------------------
# Method 1 : Correlation with Stroke
#-------------------------------------------------------

numeric_only <- data_final[, sapply(data_final, is.numeric)]

cor_values <- cor(numeric_only, use="complete.obs")[,"stroke"]

cor_df <- data.frame(
  Feature = names(cor_values),
  Correlation = abs(cor_values)
)

cor_df <- cor_df[cor_df$Feature != "stroke", ]
cor_df <- cor_df[order(-cor_df$Correlation), ]

print(cor_df)

# Save correlation bar chart
png("plots/Feature_Correlation_with_Stroke.png",
    width = 1200, height = 800)

p <- ggplot2::ggplot(
  cor_df,
  ggplot2::aes(x = reorder(Feature, Correlation),
               y = Correlation,
               fill = Correlation)
) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_gradient(low="steelblue", high="tomato") +
  ggplot2::labs(title="Feature Correlation with Stroke",
                x="Feature",
                y="Correlation") +
  ggplot2::theme_minimal()
print(p)
dev.off()
#-------------------------------------------------------
# Method 2 : Variance Threshold
#-------------------------------------------------------

cat("\n=== Method 2: Variance Threshold ===\n")

feature_vars <- sapply(numeric_only[, names(numeric_only)!="stroke"], var)

var_df <- data.frame(
  Feature = names(feature_vars),
  Variance = feature_vars
)

print(var_df)

# Save variance bar chart
png("plots/Feature_Variance.png",
    width = 1200, height = 800)

p <- ggplot2::ggplot(
  var_df,
  ggplot2::aes(x = reorder(Feature, Variance),
               y = Variance,
               fill = Variance)
) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_gradient(low="steelblue", high="orange") +
  ggplot2::labs(title="Feature Variance",
                x="Feature",
                y="Variance") +
  ggplot2::theme_minimal()

print(p)

dev.off()


#-------------------------------------------------------
# Method 3 : ANOVA F Score
#-------------------------------------------------------

cat("\n=== Method 3: ANOVA F Score ===\n")

stroke_factor <- factor(data_final$stroke)

features <- setdiff(names(numeric_only),"stroke")

f_scores <- c()

for(col in features){
  
  f_val <- tryCatch({
    
    model <- aov(numeric_only[[col]] ~ stroke_factor)
    summary(model)[[1]]$`F value`[1]
    
  }, error=function(e) NA)
  
  f_scores <- c(f_scores, f_val)
}

f_score_df <- data.frame(
  Feature = features,
  F_Score = f_scores
)

f_score_df <- f_score_df[!is.na(f_score_df$F_Score),]
f_score_df <- f_score_df[order(-f_score_df$F_Score),]

print(f_score_df)

# Save ANOVA feature importance plot
png("plots/Feature_ANOVA_FScore.png",
    width = 1200, height = 800)

p <- ggplot2::ggplot(
  f_score_df,
  ggplot2::aes(x = reorder(Feature, F_Score),
               y = F_Score,
               fill = F_Score)
) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_gradient(low="steelblue", high="darkred") +
  ggplot2::labs(title="Feature Importance (ANOVA F Score)",
                x="Feature",
                y="F Score") +
  ggplot2::theme_minimal()

print(p)

dev.off()

# Method 4 : Mutual Information

#-------------------------------------------------------
# Method 4 : Mutual Information
#-------------------------------------------------------

cat("\n=== Method 4: Mutual Information ===\n")

library(infotheo)

# Discretize target variable
stroke_disc <- infotheo::discretize(data_final$stroke)

# Select numeric features except stroke
numeric_feats <- setdiff(names(data_final)[sapply(data_final,is.numeric)], "stroke")

# Compute MI scores
mi_scores <- sapply(numeric_feats, function(col){
  
  x <- data_final[[col]]
  
  infotheo::mutinformation(
    infotheo::discretize(x, nbins=10),
    stroke_disc
  )
})

mi_df <- data.frame(
  Feature = numeric_feats,
  MI_Score = mi_scores
)

mi_df <- mi_df[order(-mi_df$MI_Score),]

print(mi_df)

# Save Mutual Information plot
png("plots/Feature_Mutual_Information.png",
    width = 1200, height = 800)

p <- ggplot2::ggplot(
  mi_df,
  ggplot2::aes(x = reorder(Feature, MI_Score),
               y = MI_Score,
               fill = MI_Score)
) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_gradient(low="steelblue", high="tomato") +
  ggplot2::labs(title="Feature Importance via Mutual Information",
                x="Feature",
                y="MI Score") +
  ggplot2::theme_minimal()
print(p)
dev.off()

#-------------------------------------------------------
# Final Feature Selection
#-------------------------------------------------------

high_cor_features <- cor_df$Feature[cor_df$Correlation > 0.05]
top_f_features <- head(f_score_df$Feature, 10)
selected_features <- unique(c(high_cor_features, top_f_features))
cat("\n=== FINAL SELECTED FEATURES ===\n")
print(selected_features)
# Keep only existing columns
selected_features <- selected_features[selected_features %in% names(data_final)]
# Create final modeling dataset
data_model <- data_final[, c(selected_features,"stroke")]
cat("\nFinal modeling dataset size:", dim(data_model), "\n")
print(head(data_model))
cat("\n Data preprocessing complete. Dataset ready for ML.\n")
