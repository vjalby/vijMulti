testData <- data.frame(
    Origin = factor(c("American", "Japanese", "Japanese", "American", "American", "Japanese", "American", "European", "American", "American", "European", "European", "American", "Japanese", "Japanese", "American", "American", "American", "Japanese", "European", "Japanese", "Japanese", "American", "European", "European", "European", "Japanese", "American", "Japanese", "American", "Japanese", "American", "European", "Japanese", "American", "Japanese", "American", "European", "European", "American", "Japanese", "American", "American", "Japanese", "American", "Japanese", "Japanese", "American", "European", "European", "Japanese", "American", "Japanese", "Japanese", "Japanese", "American", "Japanese", "American", "European", "American", "American", "Japanese", "Japanese", "American", "Japanese", "Japanese", "European", "American", "American", "Japanese", "European", "Japanese", "American", "American", "European", "Japanese", "European", "Japanese", "Japanese", "American", "Japanese", "American", "American", "American", "Japanese", "Japanese", "Japanese", "American", "Japanese", "American", "Japanese", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "Japanese", "European", "Japanese", "Japanese", "American", "Japanese", "Japanese", "Japanese", "European", "Japanese", "American", "American", "American", "Japanese", "European", "American", "Japanese", "European", "Japanese", "European", "Japanese", "European", "American", "Japanese", "Japanese", "American", "Japanese", "American", "American", "American", "Japanese", "American", "European", "Japanese", "Japanese", "Japanese", "American", "European", "American", "American", "Japanese", "Japanese", "American", "Japanese", "American", "American", "American", "American", "Japanese", "European", "Japanese", "American", "Japanese", "Japanese", "Japanese", "Japanese", "European", "Japanese", "American", "Japanese", "Japanese", "American", "Japanese", "Japanese", "European", "Japanese", "American", "American", "American", "American", "American", "American", "American", "Japanese", "American", "American", "Japanese", "Japanese", "Japanese", "American", "Japanese", "American", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "American", "European", "Japanese", "American", "American", "European", "American", "Japanese", "Japanese", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "American", "American", "Japanese", "European", "Japanese", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "Japanese", "American", "American", "Japanese", "American", "Japanese", "Japanese", "American", "American", "Japanese", "American", "American", "Japanese", "Japanese", "Japanese", "Japanese", "Japanese", "European", "Japanese", "Japanese", "American", "American", "American", "Japanese", "American", "American", "American", "Japanese", "American", "American", "Japanese", "American", "Japanese", "American", "European", "American", "American", "Japanese", "American", "European", "American", "European", "Japanese", "European", "American", "American", "American", "Japanese", "Japanese", "Japanese", "American", "European", "Japanese", "American", "Japanese", "Japanese", "American", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "European", "Japanese", "Japanese", "American", "Japanese", "Japanese", "American", "Japanese", "American", "Japanese", "Japanese", "Japanese", "American", "American", "Japanese", "European", "Japanese", "European", "Japanese", "Japanese", "American", "American", "Japanese", "European", "American", "Japanese", "Japanese", "Japanese", "Japanese", "American", "Japanese", "Japanese", "American", "European", "Japanese", "European", "Japanese", "Japanese", "American", "Japanese", "Japanese", "Japanese", "Japanese", "Japanese", "European", "American", "Japanese", "American", "American", "American", "Japanese", "American", "European", "American", "Japanese", "American"), levels = c("American", "Japanese", "European")),
    Size = factor(c("Large", "Small", "Small", "Large", "Medium", "Medium", "Large", "Medium", "Medium", "Medium", "Small", "Medium", "Medium", "Small", "Medium", "Large", "Small", "Medium", "Small", "Medium", "Small", "Medium", "Large", "Small", "Medium", "Medium", "Small", "Medium", "Small", "Small", "Medium", "Small", "Medium", "Small", "Large", "Medium", "Medium", "Small", "Medium", "Small", "Small", "Medium", "Small", "Medium", "Medium", "Small", "Medium", "Medium", "Small", "Small", "Small", "Medium", "Medium", "Small", "Medium", "Small", "Medium", "Large", "Small", "Large", "Large", "Small", "Small", "Medium", "Medium", "Small", "Medium", "Large", "Large", "Small", "Small", "Small", "Large", "Medium", "Large", "Small", "Medium", "Small", "Small", "Small", "Small", "Large", "Medium", "Medium", "Small", "Small", "Medium", "Small", "Medium", "Large", "Small", "Medium", "Small", "Small", "Small", "Small", "Small", "Large", "Large", "Medium", "Small", "Medium", "Medium", "Medium", "Medium", "Small", "Small", "Medium", "Small", "Small", "Small", "Medium", "Medium", "Small", "Large", "Medium", "Medium", "Medium", "Small", "Medium", "Small", "Small", "Medium", "Medium", "Small", "Medium", "Small", "Small", "Large", "Medium", "Medium", "Medium", "Small", "Medium", "Medium", "Large", "Large", "Small", "Small", "Small", "Medium", "Large", "Medium", "Medium", "Medium", "Small", "Large", "Medium", "Large", "Small", "Small", "Small", "Small", "Small", "Medium", "Medium", "Small", "Medium", "Large", "Medium", "Medium", "Small", "Small", "Large", "Medium", "Small", "Medium", "Large", "Medium", "Medium", "Small", "Large", "Medium", "Small", "Medium", "Small", "Medium", "Medium", "Large", "Small", "Small", "Small", "Large", "Large", "Small", "Small", "Small", "Small", "Medium", "Small", "Small", "Small", "Small", "Medium", "Small", "Medium", "Small", "Medium", "Small", "Small", "Medium", "Medium", "Small", "Medium", "Medium", "Medium", "Medium", "Small", "Medium", "Medium", "Small", "Medium", "Medium", "Small", "Small", "Small", "Medium", "Medium", "Small", "Small", "Medium", "Medium", "Small", "Small", "Small", "Small", "Small", "Medium", "Small", "Small", "Large", "Medium", "Small", "Medium", "Medium", "Small", "Small", "Small", "Small", "Small", "Medium", "Small", "Medium", "Small", "Medium", "Large", "Medium", "Small", "Large", "Medium", "Small", "Large", "Medium", "Large", "Small", "Medium", "Medium", "Medium", "Medium", "Small", "Medium", "Small", "Small", "Small", "Small", "Small", "Large", "Medium", "Small", "Small", "Large", "Medium", "Medium", "Large", "Medium", "Small", "Large", "Small", "Medium", "Small", "Large", "Small", "Medium", "Medium", "Small", "Medium", "Medium", "Small", "Small", "Medium", "Small", "Small", "Small", "Medium", "Small", "Large", "Medium", "Small", "Medium", "Small", "Small", "Medium", "Medium", "Small", "Medium", "Medium", "Small", "Medium", "Medium", "Small", NA, "Medium", "Medium", "Medium", "Small", "Medium", "Medium", "Small", "Medium", "Small", "Small", "Small", "Medium", "Small", "Small", "Medium", "Small", "Medium", "Small", "Small", "Medium", "Small", "Large", "Small", "Medium", "Small", "Medium", "Medium", "Medium"), levels = c("Small", "Medium", "Large"), ordered = TRUE),
    Type = factor(c("Family", "Sporty", "Family", "Family", "Family", "Family", "Family", "Family", "Sporty", "Family", "Sporty", "Sporty", "Sporty", "Sporty", "Family", "Family", "Sporty", "Work", "Family", "Family", "Family", "Family", "Family", "Sporty", "Family", "Sporty", "Sporty", "Sporty", "Sporty", "Sporty", "Family", "Sporty", "Family", "Work", "Work", "Sporty", "Family", NA, "Family", "Work", "Sporty", "Sporty", "Sporty", "Sporty", "Family", "Family", "Family", "Sporty", "Work", "Sporty", "Family", "Family", "Family", "Family", "Family", "Family", "Sporty", "Family", "Sporty", "Family", "Family", "Work", "Family", "Family", "Family", "Family", "Family", "Family", "Family", "Work", "Sporty", "Sporty", "Work", "Family", "Work", "Work", "Family", "Work", "Sporty", "Family", "Family", "Work", "Family", "Family", "Sporty", "Work", "Family", "Family", "Family", "Family", "Family", "Work", "Sporty", "Work", "Work", "Sporty", "Family", "Work", "Work", "Sporty", "Family", "Family", "Family", "Family", "Sporty", "Family", "Family", "Sporty", "Sporty", "Work", "Sporty", "Sporty", "Sporty", "Sporty", "Work", "Work", "Sporty", "Work", "Family", "Sporty", "Family", "Family", "Sporty", "Family", "Family", "Family", "Family", "Sporty", "Work", "Sporty", "Sporty", "Sporty", "Sporty", "Family", "Family", "Work", "Family", "Work", "Work", "Family", "Family", "Work", "Family", "Family", "Family", "Sporty", "Family", "Sporty", "Family", "Sporty", "Family", "Sporty", "Sporty", "Family", "Work", "Family", "Family", "Sporty", "Family", "Family", "Sporty", "Sporty", "Work", "Family", "Family", "Work", "Sporty", "Family", "Family", "Family", "Sporty", "Family", "Family", "Sporty", "Work", "Family", "Family", "Work", "Family", "Family", "Sporty", "Family", "Work", "Family", "Work", "Sporty", "Sporty", "Family", "Family", "Family", "Sporty", "Family", "Sporty", "Family", "Work", "Family", "Sporty", "Family", "Sporty", "Family", "Sporty", "Family", "Sporty", "Family", "Family", "Family", "Family", "Work", "Family", "Family", "Work", "Sporty", "Family", "Sporty", "Family", "Sporty", "Family", "Family", "Sporty", "Family", "Family", "Family", "Family", "Sporty", "Sporty", "Family", "Sporty", "Family", "Family", "Work", "Family", "Family", "Family", "Family", "Family", "Sporty", "Sporty", "Sporty", "Sporty", "Family", "Family", "Family", "Sporty", "Sporty", "Sporty", "Family", "Family", "Sporty", "Family", "Work", "Family", "Family", "Family", "Sporty", "Sporty", "Family", "Sporty", "Family", "Work", "Sporty", "Sporty", "Family", "Work", "Sporty", "Sporty", "Work", "Family", "Sporty", "Sporty", "Work", "Family", "Sporty", "Sporty", "Family", "Family", "Family", "Family", "Sporty", "Family", "Sporty", "Family", "Work", "Family", "Sporty", "Sporty", "Work", "Family", "Family", "Family", "Family", "Sporty", "Work", "Work", "Family", "Family", "Family", "Work", "Family", "Sporty", "Sporty", "Sporty", "Sporty", "Family", "Family", "Family", "Work", "Sporty", "Work", "Family", "Family", "Sporty", "Family", "Family", "Family", "Family", "Family", "Family", "Family", "Family", "Family", "Work", "Work", "Sporty", "Work", "Work", "Family", "Sporty", "Family", "Family", "Family", "Sporty", "Sporty", "Family", "Sporty", "Family", "Sporty", "Family", "Family", "Family"), levels = c("Sporty", "Family", "Work")),
    Home = factor(c("Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Rent", "Rent", NA, "Own", "Own", "Own", "Own", "Own", "Rent", "Rent", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Rent", "Rent", "Rent", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Rent", "Own", "Rent", "Own", "Rent", "Own", "Own", "Own", "Own", "Rent", "Rent", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Rent", "Rent", "Rent", "Rent", "Rent", "Own", "Rent", "Own", "Own", "Rent", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Rent", NA, "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Rent", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Rent", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", NA, "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Rent", "Rent", "Own", "Own", "Rent", "Own", "Rent", "Own", "Rent", "Rent", "Own", "Own", "Own", "Own", "Own", "Rent", "Rent", "Own", "Own", "Rent", "Rent", "Own", "Own", "Own", "Rent", "Rent", "Own", "Rent", "Own", "Own", "Rent", "Own", "Rent", "Own", "Own", "Rent", "Rent", "Own", "Own", "Rent", "Rent", "Rent", "Rent", "Own", "Rent", "Rent", "Own", "Own", "Rent", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Rent", "Own", "Own", "Own", "Own", "Rent", "Own", "Rent", "Rent", "Own", "Rent", "Own", "Own", "Own", NA, "Own", "Own", "Own", "Own", "Rent", "Rent", "Rent", "Rent", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Own", "Rent", "Own", "Rent", "Own", "Own", "Own", "Own", "Rent", "Own", "Own", "Own", "Rent", "Rent", "Own", "Own", "Own", "Rent", "Rent", "Own", "Own", "Own", "Rent", "Rent", "Rent", "Own", "Own", "Own", "Rent", "Rent", "Rent", "Rent"), levels = c("Own", "Rent")),
    Income = factor(c("1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "2 Incomes", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "2 Incomes", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "2 Incomes", "2 Incomes", "1 Income", "1 Income", "1 Income", "2 Incomes", "1 Income"), levels = c("1 Income", "2 Incomes")),
    Marital = factor(c("Married with Kids", "Single", "Married", "Single", "Married with Kids", "Single with Kids", "Married with Kids", "Married with Kids", "Married", "Married", "Married with Kids", "Single", "Married", "Single", "Single", "Married with Kids", "Single", "Single", "Married with Kids", "Married with Kids", "Married", "Married with Kids", "Married with Kids", "Single", "Married with Kids", "Married", "Single", "Married", "Married", "Married", "Married", "Single", "Married", "Married with Kids", "Married with Kids", "Single", "Single", "Married", "Married with Kids", "Married with Kids", "Married", "Married", "Single", "Single", "Married with Kids", "Single", "Married with Kids", "Single", "Married with Kids", "Married", "Married with Kids", "Married with Kids", "Married with Kids", "Married with Kids", "Single", "Married", "Married", "Single", "Married", "Married with Kids", "Married", "Single", "Married with Kids", "Married with Kids", "Married with Kids", "Married", "Married with Kids", "Married with Kids", "Married with Kids", "Single", "Married", "Single", "Single", "Married", "Single", "Single", "Married with Kids", "Married with Kids", "Married with Kids", "Married with Kids", "Married with Kids", "Married", "Single with Kids", "Married with Kids", "Married", "Married with Kids", "Single", "Married", "Married", "Married with Kids", "Married", "Married", "Single", "Single", "Married", "Married", "Married with Kids", "Married", "Married", "Married", "Married", "Married", "Married with Kids", "Married", "Married with Kids", "Married", "Single", "Married with Kids", "Married", "Single with Kids", "Married", "Single", "Single", "Single", "Single", "Single", "Single", "Married", "Married", "Single", "Single", "Single", "Single", "Married", "Married", "Married with Kids", "Married", "Single", "Single with Kids", "Single with Kids", "Single", "Single", "Married", "Married with Kids", "Married", "Single", "Married with Kids", "Married", "Married with Kids", "Married with Kids", "Married", "Married", "Single with Kids", "Married with Kids", "Married", "Single", "Married with Kids", "Single", "Married with Kids", "Single", "Married with Kids", "Single", "Single", "Married", "Married with Kids", "Married with Kids", "Single with Kids", "Married with Kids", "Married with Kids", "Married", "Single", "Married", "Married with Kids", "Married with Kids", "Married with Kids", "Married with Kids", "Married", "Married with Kids", "Single with Kids", "Married with Kids", "Single", "Married with Kids", "Married", "Single", "Married", "Married with Kids", "Single", "Married with Kids", "Married with Kids", "Single", "Single", "Married", "Married with Kids", "Married with Kids", "Married with Kids", "Single", "Married", "Married with Kids", "Married", "Married", "Single", "Married with Kids", "Single", "Married", "Single", "Single with Kids", "Married", "Single with Kids", "Single", "Single", "Single", "Married with Kids", "Single", "Married with Kids", "Married with Kids", "Married with Kids", "Married", "Married with Kids", "Single", "Married", "Single", "Single", "Single", "Single", "Married", "Married", "Married", "Married", "Single", "Single", "Married with Kids", "Single", "Married with Kids", "Married with Kids", "Married", "Married with Kids", "Single with Kids", "Married with Kids", "Married with Kids", "Single", "Single", "Married with Kids", "Married", "Single", "Single", "Single", "Single", "Single", "Married", "Single", "Married with Kids", "Married with Kids", "Single", "Married", "Married", "Married", "Married with Kids", "Single", "Single", "Married with Kids", "Married", "Single", "Married with Kids", "Single", "Married with Kids", "Single", "Single", "Married with Kids", "Married", "Married with Kids", "Married", "Single", "Single", "Single", "Single", "Married", "Married", "Single", "Married with Kids", "Married", "Married with Kids", "Single", "Married with Kids", "Married with Kids", "Married", "Single with Kids", "Married", "Married", "Married", "Single", "Married", "Married", "Married with Kids", "Single", "Married", "Married with Kids", "Married with Kids", "Single with Kids", "Single with Kids", "Married with Kids", "Married", "Married with Kids", "Married", "Married with Kids", "Married", "Single", "Married with Kids", "Married", "Married", "Married with Kids", "Married with Kids", "Married", "Married", "Married", "Single", "Single with Kids", "Single", "Married", "Single", "Married with Kids", "Single", "Married with Kids", "Married with Kids", "Single", "Single", "Single", "Married with Kids", "Married with Kids", "Married with Kids", "Single", "Single", "Single", "Married", "Married", "Single", "Married", "Single", "Married with Kids", "Married", "Single", "Single", "Single", "Married with Kids", "Married", "Married with Kids", "Single", "Single", "Married", "Single"), levels = c("Single with Kids", "Married with Kids", "Single", "Married")),
    Kids = c(2L, 0L, 0L, 0L, 1L, 2L, 1L, 2L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 1L, 3L, 0L, 1L, 3L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 6L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 2L, 0L, 2L, 0L, 1L, 0L, 1L, 2L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 1L, 2L, 1L, 0L, 3L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 2L, 1L, 2L, 1L, 0L, 2L, 1L, 0L, 2L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 2L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, NA, 0L, 1L, 0L, 2L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 3L, 1L, 1L, 0L, 0L, 0L, 2L, 1L, 2L, 2L, 0L, 2L, 2L, 1L, 0L, 2L, 0L, 0L, 0L, 1L, 0L, 1L, 2L, 0L, 0L, 0L, 2L, 2L, 1L, 0L, 0L, 2L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 3L, 2L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 2L, 1L, 0L, 1L, 1L, 2L, 3L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 2L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 2L, 0L, 0L, 2L, 0L, 1L, 0L, 0L, 2L, 0L, 3L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 5L, 0L, 2L, 4L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 0L, 2L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 2L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 2L, 0L, 0L, 0L, 0L),
    Sex = factor(c("Male", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Female", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Female", "Female", "Female", "Male", "Male", "Female", "Male", "Male", "Male", "Female", "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Male", "Female", "Male", "Male", "Male", "Male", "Female", "Female", "Male", "Female", "Male", "Male", "Female", "Male", "Male", "Male", "Female", "Male", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Male", "Male", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Male", "Female", "Male", "Male", "Male", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Male", "Female", "Female", "Male", "Female", "Male", "Male", "Male", "Female", "Female", "Male", "Male", "Male", "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Male", "Male", "Male", "Female", "Male", "Female", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Male", "Male", "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Male", "Female", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Male", "Male", "Male", "Male", "Female", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Male", "Female", "Female", "Male", "Female", "Female", "Female", "Male", "Male", "Male", "Female", "Female", "Male"), levels = c("Male", "Female"))
)

test_that("multcorresp: eigenvalues (inertia) table", {
    r <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL
    )
    eig <- r$eigenvalues$asDF
    expect_equal(unname(eig$dim), c("1", "2", "3", "4", "5", "6", "Total"))
    expect_equal(unname(eig$inertia), c(0.550503201, 0.3994445036, 0.3496235193, 0.2778174656, 0.2482832413, 0.1743280692, 2), tolerance = 1e-6)
    expect_equal(unname(eig$proportion), c(0.2752516005, 0.1997222518, 0.1748117597, 0.1389087328, 0.1241416206, 0.0871640346, 1), tolerance = 1e-6)
})

test_that("multcorresp: discrimination table", {
    r <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        showDiscriminations = TRUE
    )
    discrim <- r$discrim$asDF
    expect_equal(unname(discrim$var), c("Origin", "Size", "Type"))
    expect_equal(unname(discrim$dim1), c(0.5819193823, 0.6689833814, 0.4006068393), tolerance = 1e-6)
    expect_equal(unname(discrim$dim2), c(0.1827314764, 0.4642725668, 0.5513294676), tolerance = 1e-6)
})

test_that("multcorresp: observations table (first 5 observations)", {
    r <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        showObservations = TRUE
    )
    obs <- head(r$observations$asDF, 5)
    expect_equal(unname(obs$obs), c("1", "2", "3", "4", "5"))
    expect_equal(unname(obs$inertia), c(0.004641342497, 0.002180545112, 0.00156988241, 0.004641342497, 0.001939082425), tolerance = 1e-6)
    expect_equal(unname(obs$qlt), c(0.7281439941, 0.7185336034, 0.1752300992, 0.7281439941, 0.7092607679), tolerance = 1e-6)
    expect_equal(unname(obs$coord1), c(1.459169, -1.014399672, -0.3746485421, 1.459169, 0.807060557), tolerance = 1e-6)
    expect_equal(unname(obs$coord2), c(0.3855555546, 0.1643565993, 0.2122488358, 0.3855555546, -0.5249910742), tolerance = 1e-6)
})

test_that("multcorresp: observations table shows the observation number even when labelVar is set", {
    r <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = "Sex",
        showObservations = TRUE
    )
    obs <- head(r$observations$asDF, 5)
    expect_equal(unname(obs$obs), c("1", "2", "3", "4", "5"))
    expect_true("label" %in% names(obs))
    expect_equal(which(names(obs) == "label"), which(names(obs) == "obs") + 1)
    expect_equal(unname(obs$label), c("Male", "Male", "Male", "Male", "Male"))
})

test_that("multcorresp: observations table has no label column when labelVar is not set", {
    r <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        showObservations = TRUE
    )
    obs <- r$observations$asDF
    expect_false("label" %in% names(obs))
})

test_that("multcorresp: Burt method with Benzecri and Greenacre corrections (summary table)", {
    r <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        method = "Burt",
        BenzecriAdj = TRUE,
        GreenacreAdj = TRUE
    )
    eig <- r$eigenvalues$asDF
    expect_equal(unname(eig$dim), c("1", "2", "3", "4", "5", "6", "Total"))
    expect_equal(unname(eig$inertia), c(0.3030537743, 0.1595559115, 0.1222366053, 0.07718254421, 0.06164456791, 0.03039027571, 0.7540636788), tolerance = 1e-6)
    expect_equal(unname(eig$adjB), c(0.1061161907, 0.009834045375, 0.0005970828578, NA, NA, NA, 0.1165473189), tolerance = 1e-6)
    expect_equal(unname(eig$`%B`), c(0.9104987714, 0.0843781347, 0.005123093893, NA, NA, NA, 1), tolerance = 1e-6)
    # Greenacre: the total is the adjusted total inertia, which the adjusted axes don't fully account for
    expect_equal(unname(eig$adjG), c(0.1061161907, 0.009834045375, 0.0005970828578, NA, NA, NA, 0.1310955182), tolerance = 1e-6)
    expect_equal(unname(eig$`%G`), c(0.8094570441, 0.07501435219, 0.004554563465, NA, NA, NA, 1), tolerance = 1e-6)
})

test_that("multcorresp: categories table", {
    r <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL
    )
    cat <- r$categories$asDF
    expect_equal(unname(cat$factor), c("Origin", "Origin", "Origin", "Size", "Size", "Size", "Type", "Type", "Type"))
    expect_equal(unname(cat$level), c("American", "Japanese", "European", "Small", "Medium", "Large", "Sporty", "Family", "Work"))
    expect_equal(unname(cat$coord1), c(0.9727249174, -0.628075388, -0.474462509, -0.7503850635, 0.279153839, 1.730667082, -0.879468537, 0.5445388458, -0.03214265541), tolerance = 1e-6)
})

test_that("multcorresp: observation plot", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        showObservationPlot = TRUE
    )$obsplot
    expect_plot_snapshot("multcorresp-obsplot", testPlot)
})

test_that("multcorresp: category plot", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL
    )$categoryplot
    expect_plot_snapshot("multcorresp-categoryplot", testPlot)
})

test_that("multcorresp: biplot", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        showBiPlot = TRUE
    )$biplot
    expect_plot_snapshot("multcorresp-biplot", testPlot)
})

test_that("multcorresp: discrimination plot", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        showDiscriminationPlot = TRUE
    )$discrimplot
    expect_plot_snapshot("multcorresp-discrimplot", testPlot)
})

test_that("multcorresp: Home as supplementary variable", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = "Home",
        labelVar = NULL
    )$categoryplot
    expect_plot_snapshot("multcorresp-supplVars", testPlot)
})

test_that("multcorresp: Burt matrix method", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        method = "Burt"
    )$categoryplot
    expect_plot_snapshot("multcorresp-burt", testPlot)
})

test_that("multcorresp: standard normalization", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        normalization = "standard"
    )$categoryplot
    expect_plot_snapshot("multcorresp-standard", testPlot)
})

test_that("multcorresp: connect ordinal categories (Size is ordered)", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = NULL,
        labelVar = NULL,
        connectOrdinalCat = TRUE
    )$categoryplot
    expect_plot_snapshot("multcorresp-connectOrdinal", testPlot)
})

test_that("multcorresp: more active variables (Origin, Size, Type, Marital, Sex)", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type", "Marital", "Sex"),
        supplVars = NULL,
        labelVar = NULL
    )$categoryplot
    expect_plot_snapshot("multcorresp-moreVars", testPlot)
})

test_that("multcorresp: titles, axis and legend text options", {
    testPlot <- vijMulti::multcorresp(
        data = testData,
        vars = c("Origin", "Size", "Type"),
        supplVars = "Home",
        labelVar = NULL,
        catTitleText = "Car categories",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        catSubtitleText = "Multiple correspondence analysis",
        subtitleFontFace = "italic",
        catCaptionText = "Source: fictitious survey data",
        captionAlign = "1",
        legendText = "Variable",
        legendPosition = "bottom"
    )$categoryplot
    expect_plot_snapshot("multcorresp-titles-axis-legend", testPlot)
})

test_that("multcorresp: null-inertia axes are shown but not usable (variables with the same partition)", {
    a <- rep(c("A", "B", "C"), c(10, 12, 8))
    dupData <- data.frame(
        a = factor(a),
        b = factor(c(A = "x", B = "y", C = "z")[a]),   # same partition as a
        c = factor(rep(c("u", "v"), 15)),
        d = factor(rep(c("s", "t"), 15))               # same partition as c
    )
    for (method in c("Indicator", "Burt")) {
        r <- vijMulti::multcorresp(data = dupData, vars = c("a", "b", "c"), supplVars = NULL,
                                   labelVar = NULL, method = method)
        # All J - Q = 5 eigenvalues are shown, the 2 null ones as exactly 0
        eig <- r$eigenvalues$asDF
        expect_equal(unname(eig$dim), c("1", "2", "3", "4", "5", "Total"))
        expect_identical(unname(eig$inertia[4:5]), c(0, 0))
        # ... but only the 3 non-null ones are usable
        expect_error(vijMulti::multcorresp(data = dupData, vars = c("a", "b", "c"), supplVars = NULL,
                                           labelVar = NULL, method = method, dimNum = 4),
                     "cannot be greater than 3")
        # Only one non-null axis: a 2-dimension MCA is rejected
        expect_error(vijMulti::multcorresp(data = dupData, vars = c("c", "d"), supplVars = NULL,
                                           labelVar = NULL, method = method),
                     "at least two dimensions")
    }
    # The Summary table is filled even when the analysis is then rejected
    a <- multcorrespClass$new(options = multcorrespOptions$new(vars = c("c", "d")), data = dupData)
    a$init()
    expect_error(a$run(), "at least two dimensions")
    expect_equal(unname(a$results$eigenvalues$asDF$dim), c("1", "2", "Total"))
})

test_that("multcorresp: variables with a single category are rejected", {
    a <- rep(c("A", "B", "C"), c(10, 12, 8))
    constData <- data.frame(
        a = factor(a),
        b = factor(rep(c("u", "v"), 15)),
        c = factor(rep(c("s", "t", "t"), 10)),
        k = factor(rep("k", 30)),                             # constant
        m = factor(c(rep("m1", 25), rep("m2", 5))),           # constant once n's incomplete cases are removed
        n = factor(c(rep(c("n1", "n2"), c(13, 12)), rep(NA, 5)))
    )
    # do.call(): the wrapper's vars/supplVars arguments are quoted, they can't be passed through variables
    run <- function(vars, supplVars = NULL) do.call(vijMulti::multcorresp, list(data = constData, vars = vars,
                                                                                supplVars = supplVars, labelVar = NULL))
    msg <- "The following variable(s) have a single category and cannot be used in a multiple correspondence analysis: "
    expect_no_error(run(c("a", "b", "c")))
    # Active or supplementary
    expect_error(run(c("a", "b", "k")), paste0(msg, "k."), fixed = TRUE)
    expect_error(run(c("a", "b", "c"), supplVars = "k"), paste0(msg, "k."), fixed = TRUE)
    # Constant only among the complete cases
    expect_no_error(run(c("a", "b", "m")))
    expect_error(run(c("a", "m", "n")), paste0(msg, "m."), fixed = TRUE)
    # Every constant variable is named
    expect_error(run(c("a", "k", "m"), supplVars = "n"), paste0(msg, "k, m."), fixed = TRUE)
})

test_that("multcorresp: variable names that are not syntactic R names (spaces, accents, symbols)", {
    plainData <- testData[, c("Origin", "Size", "Type", "Home")]
    oddData <- stats::setNames(plainData, c("Pays d'origine", "Taille du véhicule", "Type (usage)", "Logement"))
    run <- function(data) vijMulti::multcorresp(data = data, vars = names(data)[1:3],
                                                supplVars = names(data)[4], labelVar = NULL,
                                                showDiscriminations = TRUE, showCategories = TRUE)
    ref <- run(plainData)
    r <- run(oddData)
    # Real names in the tables
    expect_equal(unname(r$discrim$asDF$var), c("Pays d'origine", "Taille du véhicule", "Type (usage)", "Logement *"))
    expect_equal(unique(unname(r$categories$asDF$factor)), c("Pays d'origine", "Taille du véhicule", "Type (usage)", "Logement *"))
    # ... and the same results as with plain names
    expect_equal(unname(r$discrim$asDF$dim1), unname(ref$discrim$asDF$dim1))
    expect_equal(unname(r$categories$asDF$coord1), unname(ref$categories$asDF$coord1))
    expect_equal(unname(r$categories$asDF$coord2), unname(ref$categories$asDF$coord2))
    # The category plot still finds the ordered variable (connecting path)
    state <- r$categoryplot$state
    expect_equal(unique(state$cat$factors), c("Pays d'origine", "Taille du véhicule", "Type (usage)", "Logement"))
    expect_equal(state$ordVars, "Taille du véhicule")
})
