library(tidyverse)
library(arrow)
library(skimr)
library(lme4)
library(psych)
set.seed(1)

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

# Collect event_name, gender, season, match_type, and country in a single table

	# qc: ensure there are no events that, in a given season, span multiple formats
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
			outcome == 'tie' ~ 1L,
			outcome == 'win' ~ 3L,
			outcome == 'loss' ~ -0L,
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

# Note the mean standard deviation for each event type x gender
EventTypeMeanSD <-
	EventSD %>%
	group_by(gender, match_type) %>%
	summarize(event_type_mean_sd = mean(event_sd)) %>%
	ungroup

#### Research questions: men v women ####

# T20

ModelSubset <-
	EventSD %>%
	filter(match_type %in% c('T20', 'ODM'))

mod_gender1 <- lmer(data = ModelSubset, formula = event_sd ~ season + match_type + country + gender + (1|event_name))
mod_gender0 <- lmer(data = ModelSubset, formula = event_sd ~ season + match_type + country + (1|event_name))
anova(mod_gender1, mod_gender0)

with(ModelSubset, cohen.d(x = event_sd, group = factor(gender)))


ggplot(data = ModelSubset, aes(x = gender, y = event_sd, group = gender)) +
	geom_boxplot() +
	labs(x = '', y = 'Event Standard Deviation')

#### Research questions: match type ####

mod_type1 <- lmer(data = EventSD, formula = event_sd ~ season + gender + country + match_type + (1|event_name))
mod_type0 <- lmer(data = EventSD, formula = event_sd ~ season + gender + country + (1|event_name))
anova(mod_type1, mod_type0)

with(ModelSubset, cohen.d(x = event_sd, group = factor(match_type)))

ggplot(data = EventSD, aes(x = match_type, y = event_sd, group = match_type)) +
	geom_boxplot() +
	labs(x = '', y = 'Event Standard Deviation')


#### Research questions: country ####

mod_country1 <- lmer(data = EventSD, formula = event_sd ~ season + gender + match_type + country + (1|event_name))
mod_country0 <- lmer(data = EventSD, formula = event_sd ~ season + gender + match_type + (1|event_name))
anova(mod_country1, mod_country0)

with(ModelSubset, chisq.test(x = event_sd, y = factor(country)))

EventSD %>%
	mutate(
		country = str_replace_all(country, '\\s', '\n'),
		country = fct_reorder(country, -event_sd),
		) %>%
	ggplot(aes(x = country, y = event_sd, group = country)) +
	geom_hline(yintercept = median(EventSD$event_sd), linetype = 'dashed') +
	geom_boxplot() +
	labs(x = '', y = 'Event Standard Deviation')

#### Diagrams ####

diagram_country <- function(country_name, EventSD){
	g <-
		EventSD %>%
		filter(country == country_name) %>%
		ggplot(aes(x = season, y = event_sd)) +
		geom_hline(data = EventTypeMeanSD, aes(yintercept = event_type_mean_sd), linetype = 'dashed') +
		geom_point(aes(color = event_name)) +
		geom_smooth(method = 'lm', color = 'dodgerblue', size = 2, se = TRUE) +
		facet_grid(gender ~ match_type) +
		scale_x_continuous(limits = c(2007, 2025)) +
		scale_y_continuous(limits = c(0.0, 8), breaks = seq(0.0, 8, by = 2)) +
		theme(
			text = element_text(family = 'serif'),
			legend.position = 'bottom'
		) +
		labs(x = 'Season', y = 'Mean standard deviations', color = '', title = country_name)
	return(g)
}

diagram_country(country_name = 'England and Wales', EventSD = EventSD)

#### Demonstrate the phenomenon with counterfactuals ####
# show how a highly-predictable league (represented by the participants in 
# There Was an Old Lady Who Swallowed a Fly) has much higher sd of scores
# than a league where anybody could win 


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
OutcomeCounterfactual1$game_num

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

bind_rows(OutcomeCounterfactual1, OutcomeCounterfactual2)	%>%
mutate(
	team_outcome = fct_reorder(team_outcome, team_score),
	team = ordered(team, c(teams_scenario1, teams_scenario2))
	) %>%
count(name_of_scenario, team, team_outcome) %>%
ggplot(aes(x = team, y = n, fill = team_outcome)) +
geom_col(position = 'stack') +
facet_wrap(~name_of_scenario, scales = 'free_x') +
scale_fill_brewer(palette = 'RdYlBu') +
labs(x = 'Team', y = 'Count', title = 'Count of final outcomes per team, per scenario', fill = '') +
theme(
	text = element_text(family = 'serif'),
	axis.ticks.x = element_blank()
)


# make a table of the standings at the end of the tournament

CteTeamCount <-
	bind_rows(OutcomeCounterfactual1, OutcomeCounterfactual2) %>%
	mutate(
		team = ordered(team, c(teams_scenario1, teams_scenario2)),
		) %>%
	count(name_of_scenario, team, team_outcome) %>%
	spread(team_outcome, n) %>%
		rowwise %>%
		mutate(
			`Games Played` = sum(c(Loss, Tie, Win), na.rm = TRUE)
		) %>%
		ungroup %>%
		relocate(name_of_scenario, team, `Games Played`, Loss, Tie, Win) %>%
		mutate_if(is.numeric, replace_na, 0L)

CteTeamScores <-
	bind_rows(OutcomeCounterfactual1, OutcomeCounterfactual2) %>%
	mutate(
		team = ordered(team, c(teams_scenario1, teams_scenario2)),
		) %>%
	group_by(name_of_scenario, team) %>%
	summarize(team_subtotal = sum(team_score)) %>%
	ungroup

CteTeamCount %>%
	left_join(CteTeamScores) %>%
	rename(League = name_of_scenario, Team = team, `Points Awarded` = team_subtotal) %>%
	arrange(League, desc(`Points Awarded`))


# Graph each team's scores v the mean	

CounterfactualTeamwiseScores <-
	bind_rows(OutcomeCounterfactual1, OutcomeCounterfactual2) %>%
	mutate(
		team = ordered(team, c(teams_scenario1, teams_scenario2))
	) %>%
	group_by(name_of_scenario, team) %>%
	summarize(team_subtotal = sum(team_score)) %>%
	ungroup


CounterfactualMeanScores <-
	CounterfactualTeamwiseScores %>%
	group_by(name_of_scenario) %>%
	summarize(mean_score = mean(team_subtotal)) %>%
	ungroup

CounterfactualTeamwiseScores %>%
	left_join(CounterfactualMeanScores) %>%
	mutate(color = team_subtotal >= mean_score) %>%
	ggplot(aes(x = team, y = team_subtotal)) +
	geom_hline(data = CounterfactualMeanScores, aes(yintercept = mean_score), linetype = 'dashed') +
	geom_point(aes(y = team_subtotal, color = color), size = 3) +
	facet_wrap(~name_of_scenario, scales = 'free_x') +
	labs(x = 'Team', y = 'Team final score', title = 'Comparison of each team\'s score', subtitle = 'League mean scores are indicated with the horizontal dotted lines') +
	theme(
		legend.position = 'none',
		axis.ticks.x = element_blank(),
		text = element_text(family = 'serif')
	)

CounterfactualTeamwiseScores %>%
	group_by(name_of_scenario) %>%
	summarize(sd = sd(team_subtotal)) %>%
	ungroup %>%
	ggplot(aes(x = name_of_scenario, y = sd)) +
	geom_col() +
	theme(
		axis.ticks.x = element_blank(),
		text = element_text(family = 'serif'),
	) +
	labs(x = 'League', y = 'Standard deviation of scores', title = str_wrap('Standard deviation of scores in each league'))
#
	
	
	
	
bind_rows(
	OutcomeCounterfactual1 %>% mutate(scenario = 'Heirarchical & Predictable'),
	OutcomeCounterfactual2 %>% mutate(scenario = 'Surprising'),
) %>%
	
	
select(scenario, team, score) %>%
mutate(factor = case_when(
	score == 3L ~ 'Win',
	score == 1L ~ 'Tie',
	TRUE ~ 'Loss'
)) %>%
mutate(factor = fct_reorder(factor, score)) %>%
count(scenario, team, factor) %>%
ggplot(aes(x = team, y = n, fill = factor)) +
geom_col(position = 'stack') +
facet_wrap(~scenario, scales = 'free_x') +
scale_fill_brewer(palette = 'RdYlBu', direction = 1) +
labs(x = 'Team', y = 'n', fill = '', title = 'Example of two scenarios') +
theme(
	panel.grid.major.x = element_blank(),
	axis.ticks.x = element_blank()
)

bind_rows(
	Counterfactual1 %>% mutate(scenario = 'Predictable'),
	Counterfactual2 %>% mutate(scenario = 'Unpredictable')
) %>%
select(scenario, team, score) %>%
group_by(scenario, team) %>%
summarize(team_score = sum(score)) %>%
ungroup 

bind_rows(
	Counterfactual1 %>% mutate(scenario = 'Predictable'),
	Counterfactual2 %>% mutate(scenario = 'Unpredictable')
) %>%
select(scenario, team, score) %>%
group_by(scenario, team) %>%
summarize(team_score = sum(score)) %>%
ungroup %>%
group_by(scenario) %>%
summarize(sd = sd(team_score))
	

