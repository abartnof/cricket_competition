library(tidyverse)
library(arrow)
library(skimr)
library(lme4)
library(psych)
set.seed(1)

fn_counterfactuals <- '/Volumes/Extreme SSD/cricket/clean_data/counterfactuals.csv'
fn_results <- '/Volumes/Extreme SSD/cricket/clean_data/results_munged.csv'

Results <- read_csv(fn_results, col_types = cols(.default = 'c', year = 'i', team_score = 'i'))

counterfactual_teams <- c(
	'The Flies', 'The Spiders', 'The Birds', 'The Cats', 
	'The Goats', 'The Horses', 'The Lambs', 'The Llamas'
)
Counterfactuals <- read_csv(fn_counterfactuals, col_types = cols(.default = 'c', team_score = 'i'))
Counterfactuals$team <- ordered(Counterfactuals$team, levels = counterfactual_teams)

TeamTotalScores <-
	Results %>%
	group_by(country, match_type, event_name, year, gender, team) %>%
	summarize(team_total_score = sum(team_score)) %>%
	ungroup
TeamTotalScores

# Calculate the standard deviation of team scores per event.
EventSD <-
	TeamTotalScores %>%
	group_by(country, match_type, event_name, year, gender) %>%
	summarize(event_sd = sd(team_total_score)) %>%
	ungroup
EventSD

#### Research questions: gender ####

ModelSubset <-
	EventSD %>%
	filter(match_type %in% c('T20', 'ODM'))

mod_gender1 <- lmer(data = ModelSubset, formula = event_sd ~ year + match_type + country + gender + (1|event_name))
mod_gender0 <- lmer(data = ModelSubset, formula = event_sd ~ year + match_type + country + (1|event_name))
anova(mod_gender1, mod_gender0)

with(ModelSubset, cohen.d(x = event_sd, group = factor(gender)))

ggplot(data = ModelSubset, aes(x = gender, y = event_sd, group = gender)) +
	geom_boxplot(varwidth = TRUE) +
	stat_summary(fun.y=median, geom='label', show_guide = FALSE, 
							 aes( label=round(after_stat(y), digits=2))) +
	labs(x = '', y = 'Standard Deviation of Scores', title = 'Impact of gender on standard deviation of scores') +
	theme(
		axis.ticks.x = element_blank(),
		text = element_text(family = 'serif')
	)

#### Research questions: match type ####

mod_type1 <- lmer(data = EventSD, formula = event_sd ~ year + gender + country + match_type + (1|event_name))
mod_type0 <- lmer(data = EventSD, formula = event_sd ~ year + gender + country + (1|event_name))
anova(mod_type1, mod_type0)

chisq.test(EventSD$event_sd, EventSD$match_type, simulate.p.value = TRUE)

ggplot(data = EventSD, aes(x = match_type, y = event_sd, group = match_type)) +
	geom_boxplot(varwidth = TRUE) +
	stat_summary(fun.y=median, geom='label', show_guide = FALSE, 
							 aes( label=round(after_stat(y), digits=2))) +
	labs(x = '', y = 'Standard Deviation of Scores', title = 'Impact of match type on standard deviation of scores') +
	theme(
		axis.ticks.x = element_blank(),
		text = element_text(family = 'serif')
	)


#### Research questions: country ####

mod_country1 <- lmer(data = EventSD, formula = event_sd ~ year + gender + match_type + country + (1|event_name))
mod_country0 <- lmer(data = EventSD, formula = event_sd ~ year + gender + match_type + (1|event_name))
anova(mod_country1, mod_country0)

with(ModelSubset, chisq.test(x = event_sd, y = factor(country), simulate.p.value = TRUE))

EventSD %>%
	mutate(
		country = str_replace_all(country, '\\s', '\n'),
		country = fct_reorder(country, -event_sd),
		) %>%
	ggplot(aes(x = country, y = event_sd, group = country)) +
	geom_hline(yintercept = median(EventSD$event_sd), linetype = 'dashed') +
	geom_boxplot(varwidth = TRUE) +
	labs(x = '', y = 'Standard deviation of scores', title = 'Impact of country on standard deviation of scores') +
	theme(
		axis.ticks.x = element_blank(),
		text = element_text(family = 'serif')
	)

#### Demonstrate the phenomenon with counterfactuals ####
# show how a highly-predictable league (represented by the participants in 
# There Was an Old Lady Who Swallowed a Fly) has much higher sd of scores
# than a league where anybody could win 

Counterfactuals %>%
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

CteCounterfactualCount <-
	Counterfactuals %>%
	count(name_of_scenario, team, team_outcome) %>%
	spread(team_outcome, n) %>%
	mutate_if(is.numeric, replace_na, 0L) %>%
	rowwise %>%
	mutate(
		`Games Played` = sum(c(Loss, Tie, Win), na.rm = TRUE)
	) %>%
	ungroup %>%
	relocate(name_of_scenario, team, `Games Played`, Loss, Tie, Win)

CteCounterfactualPoints <-
	Counterfactuals %>%
	group_by(name_of_scenario, team) %>%
	summarize(Points = sum(team_score)) %>%
	ungroup

CteCounterfactualCount %>%
	left_join(CteCounterfactualPoints) %>%
	relocate(name_of_scenario, team, `Games Played`, Win, Tie, Loss, Points) %>%
	rename(
		League = name_of_scenario,
		Team = team, 
		Wins = Win,
		Ties = Tie,
		Losses = Loss,
	) %>%
	arrange(desc(League), desc(Team))


# Graph each team's scores v the mean	

CteScenarioMeans <-
	CteCounterfactualPoints %>%
	group_by(name_of_scenario) %>%
	summarize(mean = mean(Points)) %>%
	ungroup

CteCounterfactualPoints %>%
	left_join(CteScenarioMeans, by = 'name_of_scenario') %>%
	mutate(is_greater_than_mean = Points > mean) %>%
	ggplot(aes(x = team, y = Points)) +
	geom_hline(aes(yintercept = mean), linetype = 'dashed') +
	geom_point(aes(color = is_greater_than_mean), size = 3) +
	facet_wrap(~name_of_scenario, scales='free_x') +
	labs(x = 'Team', y = 'Team final score', title = 'Comparison of each team\'s score', subtitle = 'League mean scores are indicated with the horizontal dashed lines') +
	theme(
		legend.position = 'none',
		axis.ticks.x = element_blank(),
		text = element_text(family = 'serif')
	)

# Standard deviations
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
	
CteCounterfactualPoints
	
	
	
	

