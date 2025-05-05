library(tidyverse)
library(jsonlite)
library(arrow)
# library(tidyjson)



fn_event_name_to_country <- '/Volumes/Extreme SSD/cricket/clean_data/event_name_to_country.csv'
# where the input json files are
dir <- '/Volumes/Extreme SSD/cricket/input_data/all_json'

# save clean data
fn_collected_results <- '/Volumes/Extreme SSD/cricket/clean_data/results_raw.csv'
fn_collected_results_munged <- '/Volumes/Extreme SSD/cricket/clean_data/results_munged.csv'

test_countries <- c(
	# 'Afghanistan',
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
EventNameToCountry <- read_csv(fn_event_name_to_country) %>%
	filter(country %in% test_countries)


# Iterate over all of the CricSheet data, to find the game outcomes.
fn_list <- list.files(path = dir, full.names = TRUE, pattern = '\\.json$')
i_range <- seq(1, length(fn_list))
CollectedResults <- data.frame()
for (i in i_range){
	print(sprintf('%i of %i: %.3f', i, length(fn_list), i / length(fn_list)))
	
	fn <- fn_list[i]
	Data <- jsonlite::read_json(fn)
	if(Data$info$team_type == 'club'){
		Results <- 
			tibble(
				dates = Data$info$dates[[1]],
				event_name = Data$info$event$name,
				gender = Data$info$gender,
				match_type = Data$info$match_type,
				method = Data$info$outcome$method,
				winner = Data$info$outcome$winner,
				result = Data$info$outcome$result,
				overs = Data$info$overs,
				season = Data$info$season,
				team_type = Data$info$team_type,
				team1 = 	Data$info$teams[[1]],
				team2 = Data$info$teams[[2]],
				stage = Data$info$event$stage,
			) %>%
			mutate_all(as.character)
		CollectedResults <- CollectedResults %>% bind_rows(Results)
	}
}

# Write the results of the for-loop to disk
CollectedResults %>%
	write_csv(fn_collected_results)

# Clean up these results into something we can use, and write to disk
ResultsMunged <-
	CollectedResults %>%
	as_tibble %>%
	inner_join(EventNameToCountry, by = 'event_name') %>%
	pivot_longer(cols = c(team1, team2), names_to = 'variable', values_to = 'team') %>%
	mutate(
		year = lubridate::year(dates),
		gender = str_to_title(gender),
		team_outcome = case_when(
			result == 'tie' ~ 'Tie',
			!is.na(winner) & (winner == team) ~ 'Win',
			!is.na(winner) & (winner != team) ~ 'Loss',
			TRUE ~ 'DEFAULT_UNKNOWN'
		),
		team_score = case_when(
			team_outcome == 'Tie' ~ 1L,
			team_outcome == 'Win' ~ 3L,
			team_outcome == 'Loss' ~ -0L,
		)
	) %>%
	select(country, gender, year, match_type, event_name, team, team_outcome, team_score) %>%
	filter(team_outcome != 'DEFAULT_UNKNOWN')

ResultsMunged %>%
	write_csv(fn_collected_results_munged)
