library(drc)
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggrepel)
library(scales)
library(ggprism)
library(GetoptLong)
library(ggpmisc)
library(latex2exp)
# options(error = recover)

#' @note calculate pseduo R2 from standards and fit
#' @param std_dat standard dat
#' @param my_fit is the result of the drc fit
pseudo_R2 <- function(std_dat, my_fit) {
  observed <- std_dat$Absorbance

  # Get the predicted responses from the model
  predicted <- predict(my_fit)

  # Calculate the total sum of squares
  SST <- sum((observed - mean(observed))^2)

  # Calculate the residual sum of squares
  SSR <- sum((observed - predicted)^2)

  # Calculate pseudo R^2
  pseudo_R2 <- 1 - (SSR / SST)
  return(pseudo_R2)
}

run_elisa_analysis <- function(data_sheet, project_name) {
  #' *DEBUG*
  #' data_sheet = data_sheet_fn; output_dir = output_dir_proj
  standard_dat <- read_excel(data_sheet, sheet = 1) %>%
    # assuming that Concentration of Standard is the 1st column
    mutate(
      concentration = .[[1]],
      log2concentration = log2(.[[1]]),
      Absorbance = .[[2]], .before = 1
    ) %>%
    filter(!is.na(Absorbance) & Absorbance >= 0) %>%
    arrange(desc(concentration))
  unit <- colnames(read_excel(data_sheet, sheet = 1))[1]

  if (nrow(standard_dat) == 0) {
    message("You forgot to analyze the standards.")
    return()
  }

  results_temp <- read_excel(data_sheet, sheet = 2) %>%
    mutate(
      Samples = .[[1]],
      Absorbance = .[[2]]
    ) %>%
    mutate(Samples = make.unique(Samples, sep = "__")) %>%
    filter(!is.na(Samples))

  results <- results_temp %>%
    filter(Absorbance >= 0)



  # extract the full sample list, to make downstream analysis easier
  full_sample_list <- tibble(Samples = results_temp$Samples) %>%
    mutate(idx = row_number())

  # message(data_sheet)
  # message("Inputs:")
  # message("   standard")
  # print(standard_dat, n = Inf)
  # message("   abs samps")
  # print(results, n = Inf)

  #' *INFORMATION ABOUT LOGISTIC REGRESSION MODEL FOR ELISAS*
  # logistic regression model; estimate slope, ED50, and upper and lower bounds using non-linear method
  # logic behind this model: where we model the ED50 in log space,  four-parameter log-logistic model
  # may be preferred for dose-response analysis involving very small datasets (<15-20) where a normally
  # distributed parameter estimate may more reasonably be assumed on a logarithm-transformed dose scale
  # than the original dose scale.
  # drc paper: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146021#sec001

  fit <- drm(
    formula = Absorbance ~ log2concentration,
    data = standard_dat,
    fct = LL.4(names = c("Slope", "Lower", "Upper", "ED50")),
    logDose = 2
  ) # log 2 dose

  # this is the adjusted absorbances of each of the samples; we want to predict their concentration based off the standard curve
  response <- results$Absorbance
  # In absolute quantification, the precise amount of the message or template used for the curve is known.
  # In relative quantification, the template is simply known to contain the message of interest in high abundance,
  # but its absolute amount is not necessarily known. Unknowns are compared to either standard curve and a value is extrapolated.
  # The absolute quantification standard curve provides the final answer. The relative quantification calibration curve result for
  # the gene of interest is normalized to that of a housekeeping gene in the same sample, and then the normalized numbers are compared between
  # samples to obtain a fold change.

  # SO, we want ABSOLUTE QUANTIFICATION, because no housekepping gene is being used here and we are measuring the absorbance which is a surrogate for absolute amount
  #  estimate along with associated error  --> no log2 base because i want concentration non-transformed
  DOSEx_temp <- ED(fit,
    respLev = response,
    type = "absolute", display = FALSE
  ) %>% as_tibble()
  DOSEx_temp

  if (!(nrow(DOSEx_temp %>% na.omit()) > 0)) {
    message("None of the samples were in range of the standard curve.")
    # write_tsv(tibble(`NO DATA` = 0), file.path(output_dir_proj, "README.tsv"))
    return(list(plot = NULL, data = NULL))
  }

  DOSEx_t1 <- DOSEx_temp %>%
    as.data.frame() %>%
    replace_na(replace = list(Estimate = NA, `Std. Error` = NA)) %>%
    mutate(
      up = Estimate + `Std. Error`,
      down = Estimate - `Std. Error`
    )
  DOSEx <- bind_cols(DOSEx_t1, DOSEx_t1 %>%
    mutate(across(.cols = everything(), .fns = log2)) %>%
    rename_with(.fn = ~ str_c(.x, "_log2"), .cols = everything()))

  combined_res <- bind_cols(results, DOSEx) %>%
    dplyr::rename(
      `fit_to_curve_concentration_estimate` = Estimate,
      `log2_fit_to_curve_concentration_estimate` = Estimate_log2
    )

  # will need this later for helping to name the files
  # helper_name <- basename(output_dir)

  # Combine the standard data and the sample data into one data frame for plotting
  plot_data <- bind_rows(
    standard_dat %>% mutate(Type = "Standard"),
    combined_res %>% mutate(Type = "Sample", log2concentration = log2_fit_to_curve_concentration_estimate)
  )

  # Generate a sequence of log2 concentrations for the fit
  fit_vals <- data.frame(log2concentration = seq(
    from = min(plot_data$log2concentration[is.finite(plot_data$log2concentration)] - 0.2, na.rm = TRUE),
    to = max(plot_data$log2concentration[is.finite(plot_data$log2concentration)] + 0.2, na.rm = TRUE),
    length.out = 100
  ))

  # Predict the absorbance for these concentrations
  fit_vals$Absorbance <- predict(fit, newdata = fit_vals)

  # Add confidence intervals to the predictions
  fit_vals$Absorbance_CI <- suppressWarnings(predict(fit, newdata = fit_vals, interval = "confidence"))
  fit_vals


  pseudo_R2_result <- pseudo_R2(std_dat = standard_dat, my_fit = fit)
  # Create a label with the parameter estimates'
  params <- fit$fit$par
  names(params) <- fit$parNames[[2]]
  # Create a label with the equation and parameter estimates
  d <- round(params["Upper"], 3)
  a <- round(params["Lower"], 3)
  c <- round(params["ED50"], 3)
  b <- round(params["Slope"], 3)

  # https://www.myassays.com/four-parameter-logistic-regression.html
  four_PL_regression_equation <- qq("y = @{d} + ( ( @{a} - @{d} ) / ( 1 + ( x / @{c} ) ^ @{b} )")
  label <- paste(
    four_PL_regression_equation,
    "\n'Pseudo R^2' ~ ", round(pseudo_R2_result, 3)
  )
  label

  my_theme <- theme(
    panel.grid.major = element_line(colour = "gray", linetype = 3, linewidth = rel(0.5)),
    panel.grid.minor = element_line(colour = "gray", linetype = 2, linewidth = rel(0.25)),
    axis.text = element_text(size = rel(1.2)),
    plot.title = element_text(size = rel(1.5))
  )

  # Plot the data
  final_plot_data <- plot_data %>%
    filter(is.finite(log2concentration)) %>%
    dplyr::select(log2concentration, Absorbance, Samples, Type)


  if ("dilution" %in% tolower(colnames(results_temp))) {
    # message(qq("\nNote: Correcting for dilution factors..."))
    final_result <- combined_res %>%
      dplyr::select(Samples, Absorbance, fit_to_curve_concentration_estimate) %>%
      left_join(results_temp %>% dplyr::select(Samples, Dilution), by = "Samples") %>%
      mutate(Dilution = ifelse(is.na(Dilution), 1, Dilution)) %>%
      mutate(
        uncorrected_conc_est = fit_to_curve_concentration_estimate,
        final_concentration_estimate = fit_to_curve_concentration_estimate * Dilution
      ) %>%
      dplyr::select(-fit_to_curve_concentration_estimate)
  } else {
    final_result <- combined_res %>%
      left_join(results_temp %>% dplyr::select(Samples), by = "Samples")
  }

  # complete the final result with all samples to make downstream analysis easier
  left_out_samples <- setdiff(full_sample_list$Samples, final_result$Samples)
  final_result <- bind_rows(final_result, tibble(Samples = left_out_samples)) %>%
    arrange() %>%
    left_join(full_sample_list, by = "Samples") %>%
    arrange(idx) %>%
    dplyr::select(-idx)

  return(list(data = list(standard_dat, final_result, final_plot_data, fit_vals, unit, label)))
}


# # Test
# data_sheet_fn <- "test.xlsx"
# run_elisa_analysis(
#   data_sheet = data_sheet_fn,
#   project_name = "Test"
# )
