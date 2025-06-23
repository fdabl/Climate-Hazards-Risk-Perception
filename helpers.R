code_disaster <- function(disaster_type, code) {
  disaster_type[is.na(disaster_type)] <- 0
  ifelse(disaster_type == code, 1, 0)
}

recode_01 <- function(x) {
  x[x == 2] <- 0
  x[x == 1] <- 1
  x
}

remap_countries <- function(df) {
  # Recode country names --> Has to match with country names in mapping_tib
  df[df$country == 'United States',]$country <- 'USA' 
  df[df$country == 'United Kingdom',]$country <- 'UK' 
  df[df$country == 'Bosnia Herzegovina',]$country <- 'Bosnia and Herzegovina' 
  df[df$country == 'Congo Brazzaville',]$country <- 'Republic of Congo' 
  df[df$country == 'Congo Kinshasa',]$country <- 'Democratic Republic of the Congo' 
  df[df$country == 'Eswatini',]$country <- 'Swaziland' 
  df[df$country == 'Hong Kong',]$country <- 'China, Hong Kong Special Administrative Region' 
  df
}

#' Estimates the model using brms, takes the formula, a filename, and data
#' if use_model != null, then it updates the model provided
run_model <- function(form, filename, dat, force = FALSE, use_model = NULL, type = 'categorical', ...) {
  if (!file.exists(filename) || force) {
    # Weakly informative default prior (Gelman et al., 2008; except for the scaling)
    
    if (!is.null(use_model)) {
      fit <- update(use_model, newdata = dat, formula = form, recompile = FALSE, ...)
    } else {
      if (type == 'categorical') {
        priors <- (
          set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "mu2") +
          set_prior("student_t(3, 0, 2.5)", class = "b", dpar = "mu3")
        )
      } else {
        priors <- (
          set_prior("student_t(3, 0, 2.5)", class = "b")
        )
      }
      fit <- brm(form, data = dat, prior = priors, ...) 
    }
    saveRDS(fit, filename)
  } else {
    fit <- readRDS(filename)
  }
  
  fit
}

get_effects <- function(model, filename, force = FALSE, re_formula = NULL, ...) {
  
  if (!file.exists(filename) || force) {
    # df_eff <- avg_slopes(model, re_formula = re_formula, ...)
    # saveRDS(df_eff, filename)
    df_eff <- data.frame(avg_slopes(model, re_formula = re_formula, ...))
    write.csv(df_eff, filename, row.names = FALSE)
  } else {
    # df_eff <- data.frame(readRDS(filename))
    df_eff <- read.csv(filename)
  }
  
  df_eff
}

# Wrapper for `get_country_effects_variation`
get_country_variation <- function(models, predictor_name, type, outcome) {
  m <- models
  varname <- rownames(fixef(m))[2]
  
  # Sanity check
  stopifnot(predictor_name == varname)
  
  get_country_effects_variation(m, type, varname = varname) %>% 
    mutate(outcome = outcome)
}


# Get the country variation around the fixed effects
get_country_effects_variation <- function(fit, varname, type) {
  fixef <- fixef(fit) %>% 
    data.frame %>% 
    rownames_to_column()
  
  b_var <- (fixef %>% filter(rowname == !!varname))$Estimate
  
  random_effects <- ranef(fit)
  b_var_country <- random_effects$country[, , 2][, 1]
  
  b_var <- 
  b_country <- b_var + b_var_country
  
  ci_lo <- quantile(b_country, 0.025)
  ci_hi <- quantile(b_country, 0.975)
  
  df <- data.frame(
    'Effect' = c('Main'),
    'rowname' = varname,
    'Estimate' = b_var,
    'Q2.5' = ci_lo,
    'Q97.5' = ci_hi,
    'type' = type,
    'metric' = 'log odds'
  )
  
  rownames(df) <- NULL
  df
}

plot_effects <- function(
    df_eff, group = 'Very serious threat',
    type = 'Within-country',
    ybreaks = seq(-10, 20, 5),
    ylims = c(-10, 20),
    fixed_levels = NULL
  ) {
  
  df_eff <- df_eff %>% 
    mutate(group = as.character(group)) %>% 
    filter(
      event != 'Tsunami', # huge uncertainty bands
      group == !!group,
      type == !!type
    )
  
  if (type == 'Between-country') {
    df_eff <- df_eff %>% 
      mutate(
        estimate = estimate / 10,
        conf.low = conf.low / 10,
        conf.high = conf.high / 10,
      )
  }
  
  if (group == 'No serious threat') {
    df_eff <- df_eff %>%
      arrange(estimate) %>% 
      mutate(event = factor(event, levels = event))
  }
  
  if (!is.null(fixed_levels)) {
    df_eff <- df_eff %>% mutate(event = factor(event, levels = fixed_levels))
  } else {
    df_eff <- df_eff %>%
      arrange(estimate) %>% 
      mutate(event = factor(event, levels = event))
  }
  
  ggplot(
    df_eff, aes(x = event, y = estimate * 100, color = factor(climate_related))
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(position = position_dodge(width = 0.25), size = 3) +
    geom_errorbar(aes(ymin = conf.low * 100, ymax = conf.high * 100),
                  position = position_dodge(width = 0.25), width = 0.2, size = 1) +
    labs(
      title = "",
      x = "Hazard",
      y = "Percentage difference (very serious threat)"
    ) +
    scale_color_manual(values = c('firebrick', 'gray40')) +
    scale_y_continuous(breaks = ybreaks, limits = ylims) +
    # scale_y_continuous(breaks = seq(-8, 18, 2), limits = c(-8, 18)) +
    theme_minimal() +
    theme(
      legend.position = 'top',
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0)
    )
}

make_disaster_map <- function(df_country_disasters, disaster = 'flooding', title = 'a flooding') {
  # cols_world <- brewer.pal(10, 'YlOrRd')
  cols_world <- colorRampPalette(brewer.pal(9, "YlOrRd"))(11)
  mapping_tib <- read.csv('data/map_data.csv') %>% filter(country != 'Antarctica')
  
  disaster_var <- paste0(disaster, '_prop')
  df_agg_disaster <- df_country_disasters %>% 
    filter(name == !!disaster_var) %>% 
    mutate(value = value * 100)
  
  breaks <- seq(0, 50, 5)
  labels <- paste0('< ', breaks[seq(2, length(breaks))], '%')
  
  df_agg_disaster <- df_agg_disaster %>%
    mutate(
      category = cut(value, breaks = breaks, labels = labels, include.lowest = TRUE),
      country = as.character(country)
    ) %>% 
    remap_countries()
  
  world_subset <- mapping_tib %>%
    mutate(country_recoded = df_agg_disaster$country[match(mapping_tib$country, df_agg_disaster$country)]) %>%
    mutate(country_recoded = ifelse(!is.na(country_recoded), country_recoded, country)) %>% 
    left_join(df_agg_disaster, by = join_by('country'), multiple = 'first')
  
  pworld <- world_subset %>%
    mutate(
      text = paste('Country: ', country_recoded, '\nSample size (n): ', category, sep = '')
    ) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = factor(category), text = text)) +
    geom_polygon(size = 0, alpha = 0.9) +
    theme_void() +
    scale_fill_manual(
      values = cols_world,
      na.value = '#f0f0f0', name = '',
      breaks = labels,
      labels = labels,
      guide = guide_legend(
        keyheight = unit(2, units = 'mm'),
        keywidth = unit(3, units = 'mm'),
        label.position = 'bottom',
        title.position = 'top', nrow = 1
      )) +
    labs(title = title) +
    theme(
      legend.position = c(0.50, 0),
      plot.title = element_text(hjust = 0.5, size = 10),
      legend.title = element_text(hjust = 0.5)
    ) +
    coord_fixed(1.3)
  
  pworld
}

read_effects <- function(varname, type = NULL) {
  filenames <- paste0('results/', varname, '_countries', seq(5), type, '.csv')
  df <- do.call('rbind', lapply(filenames, read.csv))
  df$term <- strsplit(varname, '_')[[1]][1]
  df
}

# Run for different countries sequentially to avoid memory error
calculate_effects <- function(df_country, mb_country, variables) {
  countries <- unique(df_country$country)
  
  countries1 <- countries[seq(30)]
  countries2 <- countries[seq(31,60)]
  countries3 <- countries[seq(61, 90)]
  countries4 <- countries[seq(91, 120)]
  countries5 <- countries[seq(121, 142)]
  
  for (varname in variables) {
    print(varname)
    
    filename1a <- paste0('results/', varname, '_countries1.csv')
    filename1b <- paste0('results/', varname, '_countries2.csv')
    filename1c <- paste0('results/', varname, '_countries3.csv')
    filename1d <- paste0('results/', varname, '_countries4.csv')
    filename1e <- paste0('results/', varname, '_countries5.csv')
    
    filename2a <- paste0('results/', varname, '_countries1_logodds.csv')
    filename2b <- paste0('results/', varname, '_countries2_logodds.csv')
    filename2c <- paste0('results/', varname, '_countries3_logodds.csv')
    filename2d <- paste0('results/', varname, '_countries4_logodds.csv')
    filename2e <- paste0('results/', varname, '_countries5_logodds.csv')
    
    get_effects(
      mb_country, filename1a, by = 'country',
      variables = varname, type = 'response',
      newdata = df_country %>% filter(country %in% countries1)
    )
    
    get_effects(
      mb_country, filename1b, by = 'country',
      variables = varname, type = 'response',
      newdata = df_country %>% filter(country %in% countries2)
    )
    
    get_effects(
      mb_country, filename1c, by = 'country',
      variables = varname, type = 'response',
      newdata = df_country %>% filter(country %in% countries3)
    )
    
    get_effects(
      mb_country, filename1d, by = 'country',
      variables = varname, type = 'response',
      newdata = df_country %>% filter(country %in% countries4)
    )
    
    get_effects(
      mb_country, filename1e, by = 'country',
      variables = varname, type = 'response',
      newdata = df_country %>% filter(country %in% countries5)
    )
    
    get_effects(
      mb_country, filename2a, by = 'country',
      variables = varname, type = 'link',
      newdata = df_country %>% filter(country %in% countries1)
    )
    
    get_effects(
      mb_country, filename2b, by = 'country',
      variables = varname, type = 'link',
      newdata = df_country %>% filter(country %in% countries2)
    )
    
    get_effects(
      mb_country, filename2c, by = 'country',
      variables = varname, type = 'link',
      newdata = df_country %>% filter(country %in% countries3)
    )
    
    get_effects(
      mb_country, filename2d, by = 'country',
      variables = varname, type = 'link',
      newdata = df_country %>% filter(country %in% countries4)
    )
    
    get_effects(
      mb_country, filename2e, by = 'country',
      variables = varname, type = 'link',
      newdata = df_country %>% filter(country %in% countries5)
    )
  }
}
