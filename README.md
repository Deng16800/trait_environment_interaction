### Help file of species_specific_parameters.xlsx and R code
## Species-specific parameters
"species_specific_parameters.xlsx" is a file that includes species-specific parameters at different scales (20 m and 10 m) and different intervals.

Column meanings:

The first column contains species abbreviation names.
The second column (beta0) represents the species-specific intercept.
Columns three to ten represents the species-specific response to environmental variables.
Columns 11 to 14 represents the species-specific response to latent variables.
The last column represents the species-specific dispersion of recruitment.

Worksheet names:

The worksheet names include fs2.20, fs3.20, fs4.20, fsall.20, fs2.10, fs3.10, fs4.10, and fsall.10.
The first four and the last four worksheets represent the 20 m and 10 m scales, respectively.
fs2, fs3, fs4, and fsall indicate the first, second, third, and full intervals, respectively.

Values represent medians of posterior distribution, while values in the brackets are upper and lower bounds of 95% credible intervals (CI).

All parameter estimates are presented in the link scale.

## Model
"model_code.R" is the R file that run boral model

It include two scales (20 m and 10 m)

I used different periods of data (fs2, fs3, fs4 and fsall) to run the model

Total have 8 model

#Note that in 10 m scale I first cut the edge of plot because the package I used to generate aspect (environmental variable) data are not good enough, so If you have finer scale of environment data, you don't have to cut the edge of plot.
