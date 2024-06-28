# R code for bootstrapping Dow Jones and S&P 500 Indices
This code performs a bootstrap analysis to compare the Dow Jones Industrial Average (DJIA) and S&P 500 historical annual returns.  If you are interested in learning how to perform analyses like this, consider one of my data science couses: https://wp.me/P5xMk4-5p 

Here's a breakdown of the key steps:

1. Loading Data and Preprocessing:

    Define functions to calculate summary statistics (i.e., median, variance) and proportions for a data matrix.
    The code reads data for Dow Jones and S&P 500 from CSV files and prepares them for analysis.
    It removes unnecessary characters from the "percent change" columns and converts them to numeric values.

2. Summary Statistics and Returns:

    It calculates summary statistics (mean, median, etc.) for both indices.
    You calculate the total return achieved by each index over the entire period.

3. Visualizing Historical Returns:

    The code defines a function to calculate the moving average (ma) for a time series with a specified window size.
    It creates separate histograms for the proportion changes of Dow Jones and S&P 500 with different breakpoints.
    Several plots are generated to visualize 10, 20, 30, 40, and 50-year moving averages for both indices.

4. Bootstrap Analysis:

    The code sets a seed for reproducibility and defines the number of bootstrap iterations (runs) and number of years (num_years).
    It creates empty lists to store bootstrapped data for Dow Jones (dow_boot) and S&P 500 (sp_boot).
    Two matrices (dow_boot_summary and sp_boot_summary) are created to store summary statistics from each bootstrap iteration.
    The loop iterates for runs times:
        In each iteration, it randomly samples num_years data points (with replacement) from the S&P 500 data using the sample function.
        Similar samples are drawn for the Dow Jones data.
        Summary statistics are calculated for these sampled "years" of data and stored in the respective summary matrices.

5. Analyzing Bootstrap Results:

    Separate histograms are generated to visualize the distribution of means and medians obtained from the bootstrapped samples for both indices.
    The code calculates the column means, medians, and variances of the bootstrap summary statistics matrices.

6. Proportion Below a Return Threshold:

    It defines a sequence of possible return thresholds (rate_thresh).
    Two matrices (dow_prop_mat and sp_prop_mat) are created to store the proportion of data points in the bootstrapped samples that fall below each return threshold.
    The colProp function is used to calculate these proportions for each threshold.

7. Visualizing Proportion Below Threshold:

    The code creates a plot to compare the simulated median return distributions (proportion below the threshold) for Dow Jones and S&P 500.

Overall, this code utilizes bootstrapping to generate simulated samples (with replacement) from the historical data and analyze the distribution of summary statistics (mean, median, variance) for both indices. This helps estimate the variability of these statistics and compare the return characteristics of Dow Jones and S&P 500.
