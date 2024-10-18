
load(url("http://www.stat.berkeley.edu/users/nolan/data/babiesLab133.rda"))


## PLOTS FOR graphics.pptx

require(ggplot2)

# Histogram of mother's wt
plotQN1.0 = ggplot(babies) + 
  geom_histogram(aes(x = wt), 
                 binwidth = 5,
                 fill = "white", col = "black") + 
  scale_x_continuous("Weight (lb)") +
  theme_bw()

# Normalize to proportions
# The variable ..density.. is produced by the default 
# stat in the geom_histogram layer
plotQN1.1 = ggplot(babies) + 
  geom_histogram(aes(x = wt, y = ..density..), 
                 binwidth = 5,
                 fill = "white", col = "black") + 
  scale_x_continuous("Weight (lb)") +
  theme_bw()

# Zoom in to see the main portion of the data (set limits)
plotQN1.2 = ggplot(babies) + 
  geom_histogram(aes(x = wt, y = ..density..), 
                 binwidth = 5,
                 fill = "white", col = "black") + 
  scale_x_continuous(name = "Weight (lb)", limits = c(85, 225)) +
  theme_bw()

# Alternative density plot, bandwidth too small,
#  added rug plot layer
plotQN1.3 = ggplot(babies, aes(x = wt)) + 
  geom_density(bw = 2) +  geom_rug() +
  scale_x_continuous(name = "Weight (lb)", 
                     limits = c(85, 225)) +
  theme_bw()

## Bar plot of education level, control th esize of bins
# Drop NAs so they don't appear as a category
plotQL1.1 = ggplot(babies[!is.na(babies$ed), ]) + 
  geom_bar(aes(x = ed), width = 0.5, na.rm = TRUE) +
  scale_x_discrete(name = "Education Level") +
  theme_bw()


# Normalize the heights to percentages rather than counts
plotQL1.2 = ggplot(babies[ !is.na(babies$ed), ]) + 
  geom_bar(aes(x = ed, y = 100 * ..count../sum(..count..)), 
           width = 0.75) +
  labs(x  = "Education Level", y = "Percent") +
  theme_bw()

# Make a dot plot rather than bar plot
plotQL1.3 = ggplot(babies[!is.na(babies$ed),]) + 
  geom_point(aes(x = ed), stat="count", size = 3) +
  coord_flip() +
  theme_bw() 

# Pie Chart
plotQL1.4 = ggplot(babies[!is.na(babies$ed), ], 
                   aes(x = factor(1), fill = ed )) +
      geom_bar(width = 1) + 
      coord_polar(theta = "y") +
      labs(x = "", y = "") + theme_bw()


# Bar vs Histogram with Parity

plotQL1.5 = ggplot(babies) + 
  geom_histogram(aes(x = parity, y = ..density..), 
                 binwidth = 1,
                 fill = "white", col = "black") + 
  scale_x_continuous(name = "Number of Previous Pregnancies") +
  theme_bw()

plotQL1.6 = ggplot(babies, aes(x = factor(parity, levels = 0:13))) + 
  geom_bar(aes(y = 100 * ..count.. / sum(..count..)), width = 0.5) +
  scale_x_discrete(name = "Number of Previous Pregnancies", drop = FALSE) +
  scale_y_continuous(name = "Percent") +
  theme_bw()

# Bwt and Smoke
# Overlay 2 density plots
plotQN1QL1.1 = 
  ggplot(babies[ babies$smoke == "Never" | 
                 babies$smoke == "Current", ], 
         aes(x = bwt, y = ..density.., 
             color = smoke, group = smoke)) + 
  geom_density() +
  scale_x_continuous(name = "Birth Weight (oz)") +
  theme_bw()

## stacked histogram - Very hard to read
plotQN1QL1.2 = 
  ggplot(babies[babies$smoke == "Never" | 
                babies$smoke == "Current", ],
       aes(x = bwt, y = ..density.., color = smoke, 
           group = smoke)) + 
  geom_histogram() + 
  scale_x_continuous("Birth Weight (oz)") +
  theme_bw()

# Overlaid density plot - very hard to read
plotQN1QL1.3 = 
  ggplot(babies[babies$smoke == "Never" | 
                             babies$smoke == "Current", ],
         aes(x = bwt, y = ..density.., color = smoke)) + 
  geom_histogram(position="identity", alpha = 0.3) + 
  scale_x_continuous("Birth Weight (oz)") +
  theme_bw()

# Side-by-side Box plots
plotQN1QL1.4 = 
  ggplot(babies[!is.na(babies$smoke), ], 
         aes(x = smoke, y = bwt)) + 
  geom_boxplot() + 
  scale_x_discrete("Smoking Status") +
  scale_y_continuous("Birth Weight (oz)") +
  theme_bw()

# Side-by-side violin plots
plotQN1QL1.5 = 
  ggplot(babies[!is.na(babies$smoke), ], aes(x = smoke, y = bwt)) + 
    geom_violin() + 
    scale_x_discrete("Smoking Status") +
    scale_y_continuous("Birth Weight (oz)") +
    theme_bw()

# Smoking status and education level
# Misleading counts rather than proportions
# Side-by-side bar plots
plotQL2.1 = 
  ggplot(babies[!(is.na(babies$smoke) | is.na(babies$ed) ), ], 
         aes(x = smoke, fill = ed)) + 
  geom_bar(position = "dodge")

# Aternative normalization within groups
tableEdSmoke = table(babies$smoke, babies$ed)
tableEdSmokeC1 = tableEdSmoke/
  matrix(colSums(tableEdSmoke), nrow = 4, ncol = 5, byrow = TRUE)
newDF = data.frame(prop = tableEdSmokeC1)
names(newDF) = c("smoke", "ed", "prop")

plotQL2.2 = 
  ggplot(data = newDF, 
         aes(x = smoke, y = prop, fill = ed)) + 
  geom_bar(stat = 'identity', position = "dodge")

# Line plot - best of the possibilities
plotQL2.3 = 
  ggplot(data = newDF, 
         aes(x = smoke, y = prop, group = ed, color = ed)) + 
  geom_point(stat = 'identity') +
  geom_line(stat = 'identity')


## mosiac plot - still looking for a nice way to do in ggplot...
require(vcd)
vnames = list(set_varnames = c(ed = "", smoke = ""))
mosaic(~ smoke + ed, data = babies, 
       expected = ~ ed + smoke,
       legend = FALSE, shading = TRUE, 
       labeling_args = vnames, split_vertical = TRUE)


## two quantitative variables 
# Scatter plot with added line (and error band)
# Points are jittered to avoid over plotting

plotQN2.1 = 
  ggplot(babies, aes(x = ht, y = dht)) + 
  geom_point(position = 'jitter', size = 0.5) + 
  geom_smooth(method = lm) +
  scale_x_continuous("Mother's Height (in)", limits = c(58, 70)) +
  scale_y_continuous("Father's Height (in)") +
  theme_bw()

# Color points by education level and 
# fit separate lines
plotQN2.2 = 
  ggplot(babies[babies$ed %in% c("HS", "Some Col", "College"), ], 
         aes(x = ht, y = dht, col = ed)) + 
  geom_point(position = 'jitter', size = 0.5) + 
  geom_smooth(method = lm) +
  scale_x_continuous("Mother's Height (in)", limits = c(58, 70)) +
  scale_y_continuous("Father's Height (in)") +
  theme_bw()

