getwd()
setwd("D:/R-progrms")
states_info <- read.csv('statedata.csv')

subset(states_info,state.region =='1')

states_info[states_info$state.region == '1', ]


states_info[states_info$population >1, ]

