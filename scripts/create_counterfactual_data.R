# CREATE COUNTERFACTUAL DATA
# show how a highly-predictable league (represented by the participants in 
# There Was an Old Lady Who Swallowed a Fly) has much higher sd of scores
# than a league where anybody could win 


library(tidyverse)
fn_counterfactuals <- '/Volumes/Extreme SSD/cricket/clean_data/counterfactuals.csv'


teams_scenario1 <- c('The Flies', 'The Spiders', 'The Birds', 'The Cats')

OutcomeCounterfactual1 <-
	bind_rows(
		expand_grid(team1 = teams_scenario1, team2 = teams_scenario1) %>%
			mutate(stage = 1),
		expand_grid(team1 = teams_scenario1, team2 = teams_scenario1) %>%
			mutate(stage = 2),
		expand_grid(team1 = teams_scenario1, team2 = teams_scenario1) %>%
			mutate(stage = 3),
		expand_grid(team1 = teams_scenario1, team2 = teams_scenario1) %>%
			mutate(stage = 4)
	) %>%
	filter(team1 != team2) %>%
	rowid_to_column('game_num') %>%
	relocate(game_num, stage) %>%
	# Note if the game was a tie; if not, see who won.
	mutate_at(c('team1', 'team2'), ordered, levels = teams_scenario1) %>%
	mutate(
		game_outcome = sample(  # 1 in 3 games will be a tie in this scenario
			size = n(), 
			x = c('Win', 'Tie'), 
			replace = TRUE, 
			prob = c(2, 1)
		),
		winner = if_else(
			game_outcome == 'Tie', NA_character_, pmax(team1, team2))
	) %>%
	# wide to long, and apply result to each team
	pivot_longer(
		cols = starts_with('team'),
		names_to = 'variable',
		values_to = 'team'
	) %>%
	select(-variable) %>%
	mutate(
		team_outcome = case_when(
			game_outcome == 'Tie' ~ 'Tie',
			team == winner ~ 'Win',
			team != winner ~ 'Loss'
		),
		team_score = case_when(
			team_outcome == 'Loss' ~ 0L,
			team_outcome == 'Tie' ~ 1L,
			team_outcome == 'Win' ~ 3L
		)) %>%
	mutate(name_of_scenario = 'Hierarchical') %>%
	relocate(name_of_scenario)

# Scenario 2: teams who have no intrinsic hierarchy
teams_scenario2 <- c('The Goats', 'The Horses', 'The Lambs', 'The Llamas')

OutcomeCounterfactual2 <-
	bind_rows(
		expand_grid(team1 = teams_scenario2, team2 = teams_scenario2) %>%
			mutate(stage = 1),
		expand_grid(team1 = teams_scenario2, team2 = teams_scenario2) %>%
			mutate(stage = 2),
		expand_grid(team1 = teams_scenario2, team2 = teams_scenario2) %>%
			mutate(stage = 3),
		expand_grid(team1 = teams_scenario2, team2 = teams_scenario2) %>%
			mutate(stage = 4)
	) %>%
	filter(team1 != team2) %>%
	rowid_to_column('game_num') %>%
	# In this scenario, each time a team plays, there's a 2/3 chance of a winner (1/3 chance of a tie), 1/3 chance of a win, and 1/3 chance of a loss. totally equal odds for any team going into a game, for the three scenarios.
	mutate(
		game_outcome = sample(size = n(), x = c('Team 1 wins', 'Team 2 wins', 'Tie'), replace = TRUE, prob = c(1, 1, 1)),
		winner = case_when(
			game_outcome == 'Tie' ~ NA_character_,
			game_outcome == 'Team 1 wins' ~ team1,
			game_outcome == 'Team 2 wins' ~ team2,
		)
	) %>%
	# wide to long, and apply result to each team
	pivot_longer(
		cols = starts_with('team'),
		names_to = 'variable',
		values_to = 'team'
	) %>%
	select(-variable) %>%
	mutate(
		team_outcome = case_when(
			game_outcome == 'Tie' ~ 'Tie',
			team == winner ~ 'Win',
			team != winner ~ 'Loss'
		),
		team_score = case_when(
			team_outcome == 'Loss' ~ 0L,
			team_outcome == 'Tie' ~ 1L,
			team_outcome == 'Win' ~ 3L
		)) %>%
	mutate(name_of_scenario = 'Equal Odds') %>%
	relocate(name_of_scenario)

Counterfactuals <-
	bind_rows(OutcomeCounterfactual1, OutcomeCounterfactual2)	%>%
	mutate(
		team_outcome = fct_reorder(team_outcome, team_score),
		team = ordered(team, c(teams_scenario1, teams_scenario2))
	) %>%
	select(name_of_scenario, team, team_outcome, team_score)

Counterfactuals %>%
	write_csv(fn_counterfactuals)
