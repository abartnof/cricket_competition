library(tidyverse)
library(jsonlite)
library(arrow)
# library(tidyjson)



# where the json files are
dir <- '/Volumes/Extreme SSD/cricket/input_data/all_json'

fn_out <- '/Volumes/Extreme SSD/cricket/clean_data/results.parquet'

fn_list <- list.files(path = dir, full.names = TRUE, pattern = '\\.json$')
i_range <- seq(1, length(fn_list))
# collected_team_type <- rep(NA_character_, length(fn_list))
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

CollectedResults %>%
	as_tibble %>%
	mutate(
		dates = lubridate::ymd(dates),
		overs = as.factor(overs),
		year = lubridate::year(dates),
		gender = as.factor(gender),
		match_type = as.factor(match_type)
	)  %>%
	write_parquet(fn_out)
