library(tidyverse)
library(arrow)
library(skimr)

fn_event_name_to_country <- '/Volumes/Extreme SSD/cricket/clean_data/event_name_to_country.csv'
fn_results <- '/Volumes/Extreme SSD/cricket/clean_data/results.parquet'

EventNameToCountry <- read_csv(fn_event_name_to_country) %>%
	select(event_name, country)

Results <- read_parquet(fn_results) %>%
	mutate(
		season = str_extract(season, '\\d{4}'),  # If a season spans two calendar years, round down to the first year.
		season = parse_integer(season),
		gender = str_to_title(gender)
	)

test_countries <- c(
	'Afghanistan',
	'Australia',
	'Bangladesh',
	'England and Wales',
	'India',
	'Ireland',
	'New Zealand',
	'Pakistan',
	'South Africa',
	'Sri Lanka',
	'West Indies',
	'Zimbabwe'
)

# Collect event_name, gender, season, match_type, and country in a single table

# Ensure there are no events that, in a given season, span multiple formats
Results %>%
	distinct(match_type, season, event_name) %>%
	count(season, event_name) %>%
	filter(n > 1) %>%
	nrow == 0

Context <-
	Results %>%
	distinct(event_name, gender, season, match_type) %>%
	left_join(EventNameToCountry, by = 'event_name') %>%
	filter(country %in% test_countries)
				 
	
# Munge observations
Outcomes <-
	Results %>%
	select(event_name, gender, season, team1, team2, result, winner) %>%
	pivot_longer(cols = c(team1, team2), names_to = 'variable', values_to = 'team') %>%
	select(-variable) %>%
	mutate(
		outcome = case_when(
			result == 'tie' ~ 'tie',
			!is.na(winner) & (winner == team) ~ 'win',
			!is.na(winner) & (winner != team) ~ 'loss',
		),
		score = case_when(
			outcome == 'tie' ~ 0L,
			outcome == 'win' ~ 1L,
			outcome == 'loss' ~ -1L,
		)
	)

EventSD <-
	Outcomes %>%
	drop_na(score) %>%
	group_by(event_name, gender, season, team) %>%
	summarize(team_overall_score = sum(score)) %>%
	ungroup %>%
	group_by(event_name, gender, season) %>%
	summarize(event_sd = sd(team_overall_score)) %>%
	ungroup %>%
	inner_join(Context, by = c('event_name', 'gender', 'season')) %>%
	relocate(country, gender, match_type, season)


EventSD %>%
	filter(country == 'Australia') %>%
	ggplot(aes(x = season, y = event_sd)) +
	geom_point() +
	facet_grid(gender ~ match_type) +
	geom_smooth(method = 'lm')

EventSD %>%
	filter(country == 'England and Wales') %>%
	ggplot(aes(x = season, y = event_sd)) +
	geom_point() +
	facet_grid(gender ~ match_type) +
	geom_smooth(method = 'lm')

