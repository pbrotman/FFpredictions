library(tidyverse)
library(glmnet)


## Import betting lines data

tempBettingLines = read_csv('data/betting/spreadspoke_scores.csv')
tempBettingLines%>%
  filter(schedule_season>=2009,schedule_playoff==FALSE)%>%
  mutate(game_date=parse_date(schedule_date,format = '%m/%d/%Y'))%>%
  mutate(schedule_week=parse_integer(schedule_week,na=c('\\D')))%>%
  select(game_date,everything())%>%
  select(-schedule_playoff)-> bettingLines


## Import pbp
# (based on https://blog.exploratory.io/how-to-read-multiple-excel-or-csv-files-together-42af5d314a10)

files <- list.files(path = "data/pbp", pattern = "pbp20\\d{2}.csv", full.names = T) 

pbpAll <- sapply(files, read_csv, col_types=cols(.default='c',
                                                 yards_gained=col_number(),
                                                 rush_touchdown=col_number(),
                                                 pass_touchdown=col_number(),
                                                 interception=col_number(),
                                                 safety=col_number(),
                                                 sack=col_number(),
                                                 game_date=col_date()), simplify=FALSE) %>% 
  bind_rows(.id = "id")

## Generate game statistics

PAFantasyPoints = function(points){
  if(points==0) return(5)
  else if(points<7) return(4)
  else if(points<14) return(3)
  else if(points<18) return(1)
  else if(points<28) return(0)
  else if(points<35) return(-1)
  else if(points<46) return(-3)
  else if(points>=46) return(-5)
}

YAFantasyPoints = function(yards){
  if(yards<100) return(5)
  else if(yards<200) return(3)
  else if(yards<300) return(2)
  else if(yards<350) return(0)
  else if(yards<400) return(-1)
  else if(yards<450) return(-3)
  else if(yards<500) return(-5)
  else if(yards<550) return(-6)
  else if(yards>=550) return(-7)
}

pbpAll%>%
  filter(!is.na(defteam),!is.na(posteam),play_type!='no_play',!is.na(play_type))%>%
  filter(game_id!=2013120101,game_id!=2013112401)%>%
  mutate(year=parse_number(id))%>%
  mutate(defteam=replace(defteam,defteam=='SD','LAC'),
         defteam=replace(defteam,defteam=='STL','LAR'),
         defteam=replace(defteam,defteam=='LA$','LAR'))%>%
  mutate(posteam=replace(posteam,posteam=='SD','LAC'),
         posteam=replace(posteam,posteam=='STL','LAR'),
         posteam=replace(posteam,posteam=='LA$','LAR'))%>%
  mutate(td_team=replace(td_team,td_team=='SD','LAC'),
         td_team=replace(td_team,td_team=='STL','LAR'),
         td_team=replace(td_team,td_team=='LA$','LAR'))%>%
  mutate(fumble_recovery_1_team=replace(fumble_recovery_1_team,fumble_recovery_1_team=='SD','LAC'),
         fumble_recovery_1_team=replace(fumble_recovery_1_team,fumble_recovery_1_team=='STL','LAR'),
         fumble_recovery_1_team=replace(fumble_recovery_1_team,fumble_recovery_1_team=='LA$','LAR'))%>%
  group_by(year,game_date,id,game_id,defteam,posteam)%>% #
  summarize(allowedYards = sum(yards_gained,na.rm=TRUE), 
            blockedKicks = sum(field_goal_result=='blocked',na.rm=TRUE)+sum(extra_point_result=='blocked',na.rm=TRUE), #+blockedpunts
            allowedFGs = sum(field_goal_result=='made',na.rm=TRUE),
            allowedPATs = sum(extra_point_result=='good',na.rm=TRUE),
            allowed2PCs = sum(two_point_conv_result=='success',na.rm=TRUE),
            allowedTDs = sum(rush_touchdown,na.rm=TRUE)+sum(pass_touchdown,na.rm=TRUE),
            allowedPoints = 3*allowedFGs+allowedPATs+2*allowed2PCs+6*allowedTDs, 
            TDs = sum(td_team==defteam,na.rm=TRUE), #should include TDs from turnovers, punt/kick returns
            interceptions = sum(interception), 
            safeties = sum(safety),
            sacks = sum(sack), 
            FRs = sum(fumble_recovery_1_team==defteam,na.rm=TRUE), #+ sum(fumble_recovery_2_team==defteam,na.rm=TRUE)
  )%>%
  mutate(fantasyPoints = 6*TDs+sacks+2*blockedKicks+2*interceptions+2*FRs+2*safeties+PAFantasyPoints(allowedPoints)+YAFantasyPoints(allowedYards))-> #+2*2ptReturns+1*1ptSafeties
  byGameAll


## Join the data sets

byGameAll%>%
  left_join(bettingLines,by=c('game_date','defteam'='team_favorite_id'))%>%
  semi_join(bettingLines) -> part1

byGameAll%>%
  left_join(bettingLines,by=c('game_date','posteam'='team_favorite_id'))%>%
  semi_join(bettingLines)%>%
  mutate(spread_favorite=spread_favorite*-1)-> part2

full_join(part1,part2)%>%
  mutate(vegasPA=(over_under_line+spread_favorite)/2)-> joinedData


## Generate cummultive averages

joinedData%>%
  group_by(year,defteam)%>%
  mutate(DgameNum=rank(game_id))%>%
  arrange(game_id)%>%
  mutate(DcaFP=lag(cumsum(fantasyPoints)/rank(game_id),1),
         DcaYA=lag(cumsum(allowedYards)/rank(game_id),1),
         DcaPA=lag(cumsum(allowedPoints)/rank(game_id),1),
         DcaTDs=lag(cumsum(TDs)/rank(game_id),1),
         DcaInts=lag(cumsum(interceptions)/rank(game_id),1),
         DcaSacks=lag(cumsum(sacks)/rank(game_id),1),
         DcaFR=lag(cumsum(FRs)/rank(game_id),1),
         DcaSafeties=lag(cumsum(safeties)/rank(game_id),1))%>%
  ungroup()%>%
  group_by(year,posteam)%>%
  mutate(PgameNum=rank(game_id))%>%
  arrange(game_id)%>%
  mutate(PcaFP=lag(cumsum(fantasyPoints)/rank(game_id),1),
         PcaYA=lag(cumsum(allowedYards)/rank(game_id),1),
         PcaPA=lag(cumsum(allowedPoints)/rank(game_id),1),
         PcaTDs=lag(cumsum(TDs)/rank(game_id),1),
         PcaInts=lag(cumsum(interceptions)/rank(game_id),1),
         PcaSacks=lag(cumsum(sacks)/rank(game_id),1),
         PcaFR=lag(cumsum(FRs)/rank(game_id),1),
         PcaSafeties=lag(cumsum(safeties)/rank(game_id),1))%>%
  arrange(game_id)%>%
  ungroup() -> CAbyGameAllLagged

CAbyGameAllLagged%>%
  filter(schedule_week>3) ->
  CAbyGameAllLagged4


## Build model
# (based on https://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html)

CAbyGameAllLagged4%>%
  select(vegasPA,PcaFP,PcaYA,PcaPA,PcaTDs,PcaInts,PcaSacks,PcaFR,
         DcaFP,DcaYA,DcaPA,DcaTDs,DcaInts,DcaSacks,DcaFR)%>%
  as.matrix()->
  x.all

CAbyGameAllLagged4%>%
  select(fantasyPoints)%>%
  as.matrix()->
  y.all

glmtest = function(yr,folds){
  CAbyGameAllLagged4%>%
    filter(year!=yr)%>%
    select(vegasPA,PcaFP,PcaYA,PcaPA,PcaTDs,PcaInts,PcaSacks,PcaFR,
           DcaFP,DcaYA,DcaPA,DcaTDs,DcaInts,DcaSacks,DcaFR)%>%
    as.matrix()->
    x.train
  
  CAbyGameAllLagged4%>%
    filter(year==yr)%>%
    select(vegasPA,PcaFP,PcaYA,PcaPA,PcaTDs,PcaInts,PcaSacks,PcaFR,
           DcaFP,DcaYA,DcaPA,DcaTDs,DcaInts,DcaSacks,DcaFR)%>%
    as.matrix()->
    x.test
  
  CAbyGameAllLagged4%>%
    filter(year!=yr)%>%
    select(fantasyPoints)%>%
    as.matrix()->
    y.train
  
  CAbyGameAllLagged4%>%
    filter(year==yr)%>%
    select(fantasyPoints)%>%
    as.matrix()->
    y.test
  
  for (i in 0:10) {
    assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                              alpha=i/10,family="gaussian",nfolds=folds))
  }
  
  yhat0 <- predict(fit0, s=fit0$lambda.min, newx=x.test)
  yhat1 <- predict(fit1, s=fit1$lambda.min, newx=x.test)
  yhat2 <- predict(fit2, s=fit2$lambda.min, newx=x.test)
  yhat3 <- predict(fit3, s=fit3$lambda.min, newx=x.test)
  yhat4 <- predict(fit4, s=fit4$lambda.min, newx=x.test)
  yhat5 <- predict(fit5, s=fit5$lambda.min, newx=x.test)
  yhat6 <- predict(fit6, s=fit6$lambda.min, newx=x.test)
  yhat7 <- predict(fit7, s=fit7$lambda.min, newx=x.test)
  yhat8 <- predict(fit8, s=fit8$lambda.min, newx=x.test)
  yhat9 <- predict(fit9, s=fit9$lambda.min, newx=x.test)
  yhat10 <- predict(fit10, s=fit10$lambda.min, newx=x.test)
  
  mse0 <- mean((y.test - yhat0)^2)
  mse1 <- mean((y.test - yhat1)^2)
  mse2 <- mean((y.test - yhat2)^2)
  mse3 <- mean((y.test - yhat3)^2)
  mse4 <- mean((y.test - yhat4)^2)
  mse5 <- mean((y.test - yhat5)^2)
  mse6 <- mean((y.test - yhat6)^2)
  mse7 <- mean((y.test - yhat7)^2)
  mse8 <- mean((y.test - yhat8)^2)
  mse9 <- mean((y.test - yhat9)^2)
  mse10 <- mean((y.test - yhat10)^2)
  
  return(c(mse0,
           mse1,
           mse2,
           mse3,
           mse4,
           mse5,
           mse6,
           mse7,
           mse8,
           mse9,
           mse10))
}

MSEmatrix = sapply(2009:2018,glmtest,folds=100)
rowMeans(MSEmatrix) #use alpha = .8 or .9

model = cv.glmnet(x.all, y.all,family="gaussian", type.measure="mse",alpha=.8,nfolds=100) 
coef(model,s=model$lambda.min) #coefficients for model

model.lambda = model$lambda.min
yhat = predict(model,s=model.lambda,newx=x.all)
plot(yhat,y.all) #predicted vs actual
cor(yhat,y.all) #correlation between predicted and actual 
