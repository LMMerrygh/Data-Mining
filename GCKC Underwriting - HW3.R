# convenience function to quickly install packages that are needed if not installed and then load those packages
packages_to_be_loaded=c("caret","rpart","rpart.plot","forecast")

lapply(packages_to_be_loaded,function(x){
  if(x%in%installed.packages()[,1]==F){ install.packages(x)}
  require(x,character.only = T)
})

#Loaning Accidents.csv
Accident.df <- read.csv("Accidents.csv", header=TRUE)

Accident.ch <- as.character(Accident)

summary(Accident.df)

#Frequency by column
library(plyr)
count(Accident.df,c('INJURY_CRASH'))

#  INJURY_CRASH  freq
#1            0 21187
#2            1 20996
               #42183

#Entropy of Accidents involving bodily injury
Ent_INJURY<-((-20996/42183)*log2(20996/42183))-((21187/42183)*log2(21187/42183))
print(Ent_INJURY)
#[1] 0.9999852

#Entropy by column for comparison
count(Accident.df,c('RUSH_HR', 'INJURY_CRASH'))
#1 is yes and 0 is no
#  RUSH_HR INJURY_CRASH  freq
#1       0            0 12075
#2       0            1 11997
#3       1            0  9112
#4       1            1  8999
Ent_RUSH_HR_Y<-((-8999/18111)*log2(8999/18111))-((9112/18111)*log2(9112/18111))
print(Ent_RUSH_HR_Y)
Ent_RUSH_HR_N<-((-11997/24072)*log2(11997/24072))-((12075/24072)*log2(12075/24072))
print(Ent_RUSH_HR_N)
Ent_RUSH_HR <- (18111/42183)*Ent_RUSH_HR_Y + (24072/42183)*Ent_RUSH_HR_N
print(Ent_RUSH_HR)
InfoGain_RUSH_HR = Ent_INJURY - Ent_RUSH_HR #information gained.


count(Accident.df,c('ALCOHOL', 'INJURY_CRASH'))
#2 is no and 1 is yes
#  ALCOHOL INJURY_CRASH  freq
#1       1            0  1569
#2       1            1  2108
#3       2            0 19618
#4       2            1 18888
Ent_ALCOHOL_Y<-((-2108/3677)*log2(2108/3677))-((1569/3677)*log2(1569/3677))
print(Ent_ALCOHOL_Y)
Ent_ALCOHOL_N<-((-18888/38506)*log2(18888/38506))-((19618/38506)*log2(19618/38506))
print(Ent_ALCOHOL_N)
Ent_ALCOHOL <- (3677/42183)*Ent_ALCOHOL_Y + (38506/42183)*Ent_ALCOHOL_N
print(Ent_ALCOHOL)
InfoGain_ALCOHOL = Ent_INJURY - Ent_ALCOHOL #information gained.


count(Accident.df,c('ROAD_ALIGN', 'INJURY_CRASH'))
#2 is Curve and 1 is Straight
#  ROAD_ALIGN INJURY_CRASH  freq
#1          1            0 18400
#2          1            1 18234
#3          2            0  2787
#4          2            1  2762
Ent_ROAD_ALIGN_Y<-((-2762/5549)*log2(2762/5549))-((2787/5549)*log2(2787/5549))
print(Ent_ROAD_ALIGN_Y)
Ent_ROAD_ALIGN_N<-((-18234/36634)*log2(18234/36634))-((18400/36634)*log2(18400/36634))
print(Ent_ROAD_ALIGN_N)
Ent_ROAD_ALIGN <- (5549/42183)*Ent_ROAD_ALIGN_Y + (36634/42183)*Ent_ROAD_ALIGN_N
print(Ent_ROAD_ALIGN)
InfoGain_ROAD_ALIGN = Ent_INJURY - Ent_ROAD_ALIGN #information gained.


count(Accident.df,c('WORK_ZONE', 'INJURY_CRASH'))
#1 is Yes and 0 is No
#    WORK_ZONE INJURY_CRASH  freq
#1         0            0 20676
#2         0            1 20553
#3         1            0   511
#4         1            1   443
Ent_WORK_ZONE_Y<-((-443/954)*log2(443/954))-((511/954)*log2(511/954))
print(Ent_WORK_ZONE_Y)
Ent_WORK_ZONE_N<-((-20553/41229)*log2(20553/41229))-((20676/41229)*log2(20676/41229))
print(Ent_WORK_ZONE_N)
Ent_WORK_ZONE <- (954/42183)*Ent_WORK_ZONE_Y + (41229/42183)*Ent_WORK_ZONE_N
print(Ent_WORK_ZONE)
InfoGain_WORK_ZONE = Ent_INJURY - Ent_WORK_ZONE #information gained.

count(Accident.df,c('WEEKDAY', 'INJURY_CRASH'))
#1 is Yes and 0 is No
#   WEEKDAY INJURY_CRASH  freq
#1       0            0  4649
#2       0            1  4985
#3       1            0 16538
#4       1            1 16011
Ent_WEEKDAY_Y<-((-16011/32549)*log2(16011/32549))-((16538/32549)*log2(16538/32549))
print(Ent_WEEKDAY_Y)
Ent_WEEKDAY_N<-((-4985/9634)*log2(4985/9634))-((4649/9634)*log2(4649/9634))
print(Ent_WEEKDAY_N)
Ent_WEEKDAY <- (32549/42183)*Ent_WEEKDAY_Y + (9634/42183)*Ent_WEEKDAY_N
print(Ent_WEEKDAY)
InfoGain_WEEKDAY = Ent_INJURY - Ent_WEEKDAY #information gained.

count(Accident.df,c('INT_HWY', 'INJURY_CRASH'))
#1 is Yes, 0 is No, 9 is Unknown
#   INT_HWY INJURY_CRASH  freq
#1       0            0 17978
#2       0            1 18088
#3       1            0  3197
#4       1            1  2892
#5       9            0    12
#6       9            1    16
Ent_INT_HWY_Y<-((-2892/6089)*log2(2892/6089))-((3197/6089)*log2(3197/6089))
print(Ent_INT_HWY_Y)
Ent_INT_HWY_N<-((-18088/36066)*log2(18088/36066))-((17978/36066)*log2(17978/36066))
print(Ent_INT_HWY_N)
Ent_INT_HWY_U<-((-16/28)*log2(16/28))-((12/28)*log2(12/28))
print(Ent_INT_HWY_U)
Ent_INT_HWY <- (6089/42183)*Ent_INT_HWY_Y + (36066/42183)*Ent_INT_HWY_N + (28/42183)*Ent_INT_HWY_U 
print(Ent_INT_HWY)
InfoGain_INT_HWY = Ent_INJURY - Ent_INT_HWY #information gained.

count(Accident.df,c('LIGHT_COND', 'INJURY_CRASH'))
#1 is Daytime, 2 is Dark, 3 is Dark but lighted
#    LIGHT_COND INJURY_CRASH  freq
#1          1            0 14562
#2          1            1 14732
#3          2            0  2724
#4          2            1  2278
#5          3            0  3901
#6          3            1  3986
Ent_LIGHT_COND_DY<-((-14732/29294)*log2(14732/29294))-((14562/29294)*log2(14562/29294))
print(Ent_LIGHT_COND_DY)
Ent_LIGHT_COND_DK<-((-2278/5002)*log2(2278/5002))-((2724/5002)*log2(2724/5002))
print(Ent_LIGHT_COND_DK)
Ent_LIGHT_COND_L<-((-3986/7887)*log2(3986/7887))-((3901/7887)*log2(3901/7887))
print(Ent_LIGHT_COND_L)
Ent_LIGHT_COND <- (29294/42183)*Ent_LIGHT_COND_DY + (5002/42183)*Ent_LIGHT_COND_DK + (7887/42183)*Ent_LIGHT_COND_L 
print(Ent_LIGHT_COND)
InfoGain_LIGHT_COND = Ent_INJURY - Ent_LIGHT_COND #information gained.

count(Accident.df,c('MANCOL', 'INJURY_CRASH'))
#1 is HeadOn, 2 is Other, 0 is No Collision
#  MANCOL INJURY_CRASH  freq
#1      0            0  6720
#2      0            1  6799
#3      1            0   328
#4      1            1   598
#5      2            0 14139
#6      2            1 13599
Ent_MANCOL_HO<-((-598/926)*log2(598/926))-((328/926)*log2(328/926))
print(Ent_MANCOL_HO)
Ent_MANCOL_O<-((-13599/27738)*log2(13599/27738))-((14139/27738)*log2(14139/27738))
print(Ent_MANCOL_O)
Ent_MANCOL_N<-((-6799/13519)*log2(6799/13519))-((6720/13519)*log2(6720/13519))
print(Ent_MANCOL_N)
Ent_MANCOL <- (926/42183)*Ent_MANCOL_HO + (27738/42183)*Ent_MANCOL_O + (13519/42183)*Ent_MANCOL_N 
print(Ent_MANCOL)
InfoGain_MANCOL = Ent_INJURY - Ent_MANCOL #information gained.

count(Accident.df,c('PED_CYCLIST', 'INJURY_CRASH'))
#1 is Yes and 0 is No
#      PED_CYCLIST INJURY_CRASH  freq
#1           0            0 21076
#2           0            1 19398
#3           1            0   111
#4           1            1  1598
Ent_PED_CYCLIST_Y<-((-1598/1709)*log2(1598/1709))-((111/1709)*log2(111/1709))
print(Ent_PED_CYCLIST_Y)
Ent_PED_CYCLIST_N<-((-19398/40474)*log2(19398/40474))-((21076/40474)*log2(21076/40474))
print(Ent_PED_CYCLIST_N)
Ent_PED_CYCLIST <- (1709/42183)*Ent_PED_CYCLIST_Y + (40474/42183)*Ent_PED_CYCLIST_N
print(Ent_PED_CYCLIST)
InfoGain_PED_CYCLIST = Ent_INJURY - Ent_PED_CYCLIST #information gained.

count(Accident.df,c('INTERSTATE', 'INJURY_CRASH'))
#1 is Yes and 0 is No
#     INTERSTATE INJURY_CRASH  freq
#1          0            0  9806
#2          0            1  8842
#3          1            0 11381
#4          1            1 12154
Ent_INTERSTATE_Y<-((-12154/23535)*log2(12154/23535))-((11381/23535)*log2(11381/23535))
print(Ent_INTERSTATE_Y)
Ent_INTERSTATE_N<-((-8842/18648)*log2(8842/18648))-((9806/18648)*log2(9806/18648))
print(Ent_INTERSTATE_N)
Ent_INTERSTATE <- (23535/42183)*Ent_INTERSTATE_Y + (18648/42183)*Ent_INTERSTATE_N
print(Ent_INTERSTATE)
InfoGain_INTERSTATE = Ent_INJURY - Ent_INTERSTATE #information gained.

count(Accident.df,c('ROADWAY', 'INJURY_CRASH'))
#1 is Yes and 0 is No
#   ROADWAY INJURY_CRASH  freq
#1       0            0  5283
#2       0            1  4565
#3       1            0 15904
#4       1            1 16431
Ent_ROADWAY_Y<-((-16431/32335)*log2(16431/32335))-((15904/32335)*log2(15904/32335))
print(Ent_ROADWAY_Y)
Ent_ROADWAY_N<-((-4565/9848)*log2(4565/9848))-((5283/9848)*log2(5283/9848))
print(Ent_ROADWAY_N)
Ent_ROADWAY <- (32335/42183)*Ent_ROADWAY_Y + (9848/42183)*Ent_ROADWAY_N
print(Ent_ROADWAY)
InfoGain_ROADWAY = Ent_INJURY - Ent_ROADWAY #information gained.

count(Accident.df,c('ROAD_PROFILE', 'INJURY_CRASH'))
#1 is Yes and 0 is No
#    ROAD_PROFILE INJURY_CRASH  freq
#1            0            0 15865
#2            0            1 16058
#3            1            0  5322
#4            1            1  4938
Ent_ROAD_PROFILE_Y<-((-4938/10260)*log2(4938/10260))-((5322/10260)*log2(5322/10260))
print(Ent_ROAD_PROFILE_Y)
Ent_ROAD_PROFILE_N<-((-16058/31923)*log2(16058/31923))-((15865/31923)*log2(15865/31923))
print(Ent_ROAD_PROFILE_N)
Ent_ROAD_PROFILE <- (10260/42183)*Ent_ROAD_PROFILE_Y + (31923/42183)*Ent_ROAD_PROFILE_N
print(Ent_ROAD_PROFILE)
InfoGain_ROAD_PROFILE = Ent_INJURY - Ent_ROAD_PROFILE #information gained.

count(Accident.df,c('SPEED_LIMIT', 'INJURY_CRASH'))
#Segmented by MPH then condense into 5-25, 30-50, and 50-75 segments
#    SPEED_LIMIT INJURY_CRASH freq
#1            5            0    2
#2            5            1    4
#3           10            0   11
#4           10            1   11
#5           15            0   93
#6           15            1   90
#7           20            0  159
#8           20            1   92
#9           25            0 2270
#10          25            1 1935
#11          30            0 1828
#12          30            1 1887
#13          35            0 4048
#14          35            1 4493
#15          40            0 2025
#16          40            1 2279
#17          45            0 3311
#18          45            1 3276
#19          50            0  871
#20          50            1  794
#21          55            0 3417
#22          55            1 3177
#23          60            0  747
#24          60            1  911
#25          65            0 1413
#26          65            1 1302
#27          70            0  853
#28          70            1  601
#29          75            0  139
#30          75            1  144
Ent_SPEED_LIMIT_5_25<-((-2132/4667)*log2(2132/4667))-((2535/4667)*log2(2535/4667))
print(Ent_SPEED_LIMIT_5_25)
Ent_SPEED_LIMIT_30_50<-((-12729/24812)*log2(12729/24812))-((12083/24812)*log2(12083/24812))
print(Ent_SPEED_LIMIT_30_50)
Ent_SPEED_LIMIT_55_75<-((-6135/12704)*log2(3177/12704))-((6569/12704)*log2(6569/12704))
print(Ent_SPEED_LIMIT_55_75)
Ent_SPEED_LIMIT <- (4667/42183)*Ent_SPEED_LIMIT_5_25 + (24812/42183)*Ent_SPEED_LIMIT_30_50
  + (12704/42183)*Ent_SPEED_LIMIT_55_75
print(Ent_SPEED_LIMIT)
InfoGain_SPEED_LIMIT = Ent_INJURY - Ent_SPEED_LIMIT #information gained.

count(Accident.df,c('SURFACE_COND', 'INJURY_CRASH'))
#1 is Dry, 2 is Wet, 3 is snow/slush, 4 is ice, 9 is unknown
#  SURFACE_COND INJURY_CRASH  freq
#1             1            0 16494
#2             1            1 17111
#3             2            0  3686
#4             2            1  3232
#5             3            0   337
#6             3            1   228
#7             4            0   583
#8             4            1   326
#9             9            0    87
#10            9            1    99
Ent_SURFACE_COND_D<-((-17111/33605)*log2(17111/33605))-((16494/33605)*log2(16494/33605))
print(Ent_SURFACE_COND_D)
Ent_SURFACE_COND_W<-((-3232/6918)*log2(3232/6918))-((3686/6918)*log2(3686/6918))
print(Ent_SURFACE_COND_W)
Ent_SURFACE_COND_S<-((-228/565)*log2(228/565))-((337/565)*log2(337/565))
print(Ent_SURFACE_COND_S)
Ent_SURFACE_COND_I<-((-326/909)*log2(326/909))-((583/909)*log2(583/909))
print(Ent_SURFACE_COND_I)
Ent_SURFACE_COND_U<-((-99/186)*log2(99/186))-((87/186)*log2(87/186))
print(Ent_SURFACE_COND_U)
Ent_SURFACE_COND <- (33605/42183)*Ent_SURFACE_COND_D + (6918/42183)*Ent_SURFACE_COND_W
                    + (565/42183)*Ent_SURFACE_COND_S + (909/42183)*Ent_SURFACE_COND_I
                    + (186/42183)*Ent_SURFACE_COND_U
print(Ent_SURFACE_COND)
InfoGain_SURFACE_COND = Ent_INJURY - Ent_SURFACE_COND #information gained.

count(Accident.df,c('TRAFFIC_CTRL', 'INJURY_CRASH'))
#1 is Signal, 2 is Other, 0 is None
# TRAFFIC_CTRL INJURY_CRASH  freq
#1            0            0 14012
#2            0            1 12969
#3            1            0  3952
#4            1            1  4672
#5            2            0  3223
#6            2            1  3355
Ent_TRAFFIC_CTRL_N<-((-12969/26981)*log2(12969/26981))-((14012/26981)*log2(14012/26981))
print(Ent_TRAFFIC_CTRL_N)
Ent_TRAFFIC_CTRL_S<-((-4672/8624)*log2(4672/8624))-((3952/8624)*log2(3952/8624))
print(Ent_TRAFFIC_CTRL_S)
Ent_TRAFFIC_CTRL_O<-((-3355/6578)*log2(3355/6578))-((3223/6578)*log2(3223/6578))
print(Ent_TRAFFIC_CTRL_O)
Ent_TRAFFIC_CTRL <- (26981/42183)*Ent_TRAFFIC_CTRL_N + (8624/42183)*Ent_TRAFFIC_CTRL_S
+ (6578/42183)*Ent_TRAFFIC_CTRL_O
print(Ent_TRAFFIC_CTRL)
InfoGain_TRAFFIC_CTRL = Ent_INJURY - Ent_TRAFFIC_CTRL #information gained.

count(Accident.df,c('TRAFFIC_WAY', 'INJURY_CRASH'))
#1 is 2way Traffic, 2 is DivHwy, 3 is 1way
#  TRAFFIC_WAY INJURY_CRASH  freq
#1           1            0 12195
#2           1            1 11798
#3           2            0  7915
#4           2            1  8323
#5           3            0  1077
#6           3            1   875
Ent_TRAFFIC_WAY_2way<-((-11798/23993)*log2(11798/23993))-((12195/23993)*log2(12195/23993))
print(Ent_TRAFFIC_WAY_2way)
Ent_TRAFFIC_WAY_Hwy<-((-8323/16238)*log2(8323/16238))-((7915/16238)*log2(7915/16238))
print(Ent_TRAFFIC_WAY_Hwy)
Ent_TRAFFIC_WAY_1way<-((-875/1952)*log2(875/1952))-((1077/1952)*log2(1077/1952))
print(Ent_TRAFFIC_WAY_1way)
Ent_TRAFFIC_WAY <- (23993/42183)*Ent_TRAFFIC_WAY_2way + (16238/42183)*Ent_TRAFFIC_WAY_Hwy
+ (1952/42183)*Ent_TRAFFIC_WAY_1way
print(Ent_TRAFFIC_WAY)
InfoGain_TRAFFIC_WAY = Ent_INJURY - Ent_TRAFFIC_WAY #information gained.

count(Accident.df,c('VEH_INVL', 'INJURY_CRASH'))
#Number corresponds with vehichles involved in crash
#     VEH_INVL INJURY_CRASH  freq
#1         1            0  6378
#2         1            1  6422
#3         2            0 13351
#4         2            1 12011
#5         3            0  1223
#6         3            1  2019
#7         4            0   194
#8         4            1   411
#9         5            0    28
#10        5            1    98
#11        6            0     6
#12        6            1    19
#13        7            0     3
#14        7            1     6
#15        8            0     3
#16        8            1     6
#17        9            1     3
#18       10            0     1
#19       23            1     1
Ent_VEH_INVL_1<-((-6422/12800)*log2(6422/12800))-((6378/12800)*log2(6378/12800))
print(Ent_VEH_INVL_1)
Ent_VEH_INVL_2<-((-12011/25362)*log2(12011/25362))-((13351/25362)*log2(13351/25362))
print(Ent_VEH_INVL_2)
Ent_VEH_INVL_3<-((-2019/3242)*log2(2019/3242))-((1223/3242)*log2(1223/3242))
print(Ent_VEH_INVL_3)
Ent_VEH_INVL_4orMore<-((-544/779)*log2(544/779))-((235/779)*log2(235/779))
print(Ent_VEH_INVL_4orMore)
Ent_VEH_INVL <- (12800/42183)*Ent_VEH_INVL_1 + (25362/42183)*Ent_VEH_INVL_2
+ (3242/42183)*Ent_VEH_INVL_3 + (779/42183)*Ent_VEH_INVL_4orMore
print(Ent_VEH_INVL)
InfoGain_VEH_INVL = Ent_INJURY - Ent_VEH_INVL #information gained.

count(Accident.df,c('ADVERSE_WEATHER', 'INJURY_CRASH'))
#1 is None and 0 is Rain/Snow/Other
#    ADVERSE_WEATHER INJURY_CRASH  freq
#1               1            0 17854
#2               1            1 18306
#3               2            0  3333
#4               2            1  2690
Ent_ADVERSE_WEATHER_N<-((-18306/36160)*log2(18306/36160))-((17854/36160)*log2(17854/36160))
print(Ent_ROAD_PROFILE_N)
Ent_ADVERSE_WEATHER_O<-((-2690/6023)*log2(2690/6023))-((3333/6023)*log2(3333/6023))
print(Ent_ADVERSE_WEATHER_O)
Ent_ADVERSE_WEATHER <- (36160/42183)*Ent_ADVERSE_WEATHER_N + (2690/42183)*Ent_ADVERSE_WEATHER_O
print(Ent_ADVERSE_WEATHER)
InfoGain_ADVERSE_WEATHER = Ent_INJURY - Ent_ADVERSE_WEATHER #information gained.

count(Accident.df,c('PROPERTY_DMG', 'INJURY_CRASH'))
#1 is Yes and 0 is No
#    PROPERTY_DMG INJURY_CRASH  freq
#1            0            0   466
#2            0            1 20996
#3            1            0 20721
Ent_PROPERTY_DMG_Y<-((-20996/21462)*log2(20996/21462))-((466/21462)*log2(466/21462))
print(Ent_PROPERTY_DMG_Y)
Ent_PROPERTY_DMG_N<-((-0/20721)*log2(0/20721))-((20721/20721)*log2(20721/20721))
print(Ent_PROPERTY_DMG_N)
Ent_PROPERTY_DMG <- (21462/42183)*Ent_PROPERTY_DMG_Y + (20721/42183)*Ent_PROPERTY_DMG_N
print(Ent_PROPERTY_DMG)
InfoGain_PROPERTY_DMG = Ent_INJURY - Ent_PROPERTY_DMG #information gained.

count(Accident.df,c('REGION', 'INJURY_CRASH'))
#1 is Northeast, 2 is Midwest, 3 is South, 4 is West
#   REGION INJURY_CRASH  freq
#1      1            0   962
#2      1            1  1339
#3      2            0  6056
#4      2            1  4449
#5      3            0 10147
#6      3            1 10378
#7      4            0  4022
#8      4            1  4830
Ent_REGION_1<-((-1339/2301)*log2(1339/2301))-((962/2301)*log2(962/2301))
print(Ent_REGION_1)
Ent_REGION_2<-((-4449/10505)*log2(4449/10505))-((6056/10505)*log2(6056/10505))
print(Ent_REGION_2)
Ent_REGION_3<-((-10378/20525)*log2(10378/20525))-((10147/20525)*log2(10147/20525))
print(Ent_REGION_3)
Ent_REGION_4<-((-4830/8852)*log2(4830/8852))-((4022/8852)*log2(4022/8852))
print(Ent_REGION_4)
Ent_REGION <- (2301/42183)*Ent_REGION_1 + (10505/42183)*Ent_REGION_2
+ (20525/42183)*Ent_REGION_3 + (8852/42183)*Ent_REGION_4
print(Ent_REGION)
InfoGain_REGION = Ent_INJURY - Ent_REGION #information gained.

#Info Gain Summary
print(InfoGain_RUSH_HR)
print(InfoGain_ALCOHOL)
print(InfoGain_ROAD_ALIGN)
print(InfoGain_WORK_ZONE)
print(InfoGain_WEEKDAY)
print(InfoGain_INT_HWY)
print(InfoGain_LIGHT_COND)
print(InfoGain_MANCOL)
print(InfoGain_PED_CYCLIST)
print(InfoGain_INTERSTATE)
print(InfoGain_ROADWAY)
print(InfoGain_ROAD_PROFILE)
print(InfoGain_SPEED_LIMIT)
print(InfoGain_SURFACE_COND)
print(InfoGain_TRAFFIC_CTRL)
print(InfoGain_TRAFFIC_WAY)
print(InfoGain_VEH_INVL)
print(InfoGain_ADVERSE_WEATHER)
print(InfoGain_PROPERTY_DMG)
print(InfoGain_REGION)

#library(rpart)
#library(rpart.plot)
set.seed(123)

# 80% train; 20% test
data_train <- cust_data[1:11,]
data_test  <- cust_data[11:14,]
prop.table(table(data_train$buys_computer))
prop.table(table(data_test$buys_computer))

data_rpart <- rpart(buys_computer ~ ., data=cust_data, method="class", 
                    parms=list(split="information"), 
                    control=rpart.control(minsplit = 1))

prp(data_rpart, type=1, extra=1, split.font=1, varlen = -10)


