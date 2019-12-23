library(tidyverse)
library(nflscrapR)

pbp2018 = scrape_season_play_by_play(2018)
write_csv(pbp2018,'pbp2018.csv')

pbp2017 = scrape_season_play_by_play(2017)
write_csv(pbp2017,'pbp2017.csv')

pbp2016 = scrape_season_play_by_play(2016)
write_csv(pbp2016,'pbp2016.csv')

pbp2015 = scrape_season_play_by_play(2015)
write_csv(pbp2015,'pbp2015.csv')

pbp2014 = scrape_season_play_by_play(2014)
write_csv(pbp2014,'pbp2014.csv')

pbp2013 = scrape_season_play_by_play(2013)
write_csv(pbp2013,'pbp2013.csv')

pbp2012 = scrape_season_play_by_play(2012)
write_csv(pbp2012,'pbp2012.csv')

pbp2011 = scrape_season_play_by_play(2011)
write_csv(pbp2011,'pbp2011.csv')

pbp2010 = scrape_season_play_by_play(2010)
write_csv(pbp2010,'pbp2010.csv')

pbp2009 = scrape_season_play_by_play(2009)
write_csv(pbp2009,'pbp2009.csv')

