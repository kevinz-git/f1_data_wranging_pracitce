``` r
knitr::opts_chunk$set(echo = TRUE,root.dir="C:\\Users\\Zhixing Zhou\\Desktop\\TC - Spring Courses\\QMSS 5069 Applied Data Science for Social Scientists\\hw3 (local codes) resubmit")
```

Instructions: Answer the following questions using F1 data on the AWS S3
utilizing Databricks. You can use Pandas, R or PySpark.

\[10 pts\] What was the average time each driver spent at the pit stop
for each race?

\[20 pts\] Rank the average time spent at the pit stop in order of who
won each race

\[20 pts\] Insert the missing code (e.g: ALO for Alonso) for drivers
based on the ‘drivers’ dataset

\[20 pts\] Who is the youngest and oldest driver for each race? Create a
new column called “Age”

\[20 pts\] For a given race, which driver has the most wins and losses?

\[10 pts\] Continue exploring the data by answering your own question.

Commit your assignment to your individual Github classroom repo. Your
code and git commits should follow the basic principles we discussed so
far.

``` r
#System Setting
options(dplyr.width = Inf)

# Set Working Directory
setwd("C:\\Users\\Zhixing Zhou\\Desktop\\TC - Spring Courses\\QMSS 5069 Applied Data Science for Social Scientists\\hw3 (local codes) resubmit\\data")

# Load Libraries
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.2

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.2

    ## Warning: package 'dplyr' was built under R version 4.2.2

    ## Warning: package 'stringr' was built under R version 4.2.2

    ## Warning: package 'forcats' was built under R version 4.2.2

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(stringr)

# Load Datas
circuits<-read.csv("circuits.csv", header=TRUE)
constructor_results<-read.csv("constructor_results.csv", header=TRUE)
constructor_standings<-read.csv("constructor_standings.csv", header=TRUE)
constructors<-read.csv("constructors.csv", header=TRUE)
driver_standings<-read.csv("driver_standings.csv", header=TRUE)
drivers<-read.csv("drivers.csv", header=TRUE)
lap_times<-read.csv("lap_times.csv", header=TRUE)
pit_stops<-read.csv("pit_stops.csv", header=TRUE)
qualifying<-read.csv("qualifying.csv", header=TRUE)
races<-read.csv("races.csv", header=TRUE)
results<-read.csv("results.csv", header=TRUE)
seasons<-read.csv("seasons.csv", header=TRUE)
sprint_results<-read.csv("sprint_results.csv", header=TRUE)
status<-read.csv("status.csv", header=TRUE)
```

\[10 pts\] What was the average time each driver spent at the pit stop
for each race?

``` r
avg_pit_stops <- pit_stops %>%
  group_by(raceId, driverId) %>%
  summarise(meanduration_secs=
              mean(milliseconds)/1000) %>%
  left_join((drivers %>% select(driverId, forename, surname)),
            by= "driverId")
```

    ## `summarise()` has grouped output by 'raceId'. You can override using the
    ## `.groups` argument.

``` r
print(avg_pit_stops, n=50)
```

    ## # A tibble: 4,701 × 5
    ## # Groups:   raceId [239]
    ##    raceId driverId meanduration_secs forename   surname    
    ##     <int>    <int>             <dbl> <chr>      <chr>      
    ##  1    841        1              23.2 Lewis      Hamilton   
    ##  2    841        2              24.0 Nick       Heidfeld   
    ##  3    841        3              23.7 Nico       Rosberg    
    ##  4    841        4              24.1 Fernando   Alonso     
    ##  5    841        5              24.9 Heikki     Kovalainen 
    ##  6    841       10              23.8 Timo       Glock      
    ##  7    841       13              24.1 Felipe     Massa      
    ##  8    841       15              24.9 Jarno      Trulli     
    ##  9    841       16              24.9 Adrian     Sutil      
    ## 10    841       17              24.1 Mark       Webber     
    ## 11    841       18              21.0 Jenson     Button     
    ## 12    841       20              23.3 Sebastian  Vettel     
    ## 13    841       22              26.2 Rubens     Barrichello
    ## 14    841       30              24.5 Michael    Schumacher 
    ## 15    841       67              24.2 Sébastien  Buemi      
    ## 16    841      153              25.9 Jaime      Alguersuari
    ## 17    841      155              24.1 Kamui      Kobayashi  
    ## 18    841      808              25.1 Vitaly     Petrov     
    ## 19    841      814              24.6 Paul       di Resta   
    ## 20    841      815              23.4 Sergio     Pérez      
    ## 21    841      816              25.9 Jérôme     d'Ambrosio 
    ## 22    842        1              23.3 Lewis      Hamilton   
    ## 23    842        2              23.2 Nick       Heidfeld   
    ## 24    842        3              23.4 Nico       Rosberg    
    ## 25    842        4              24.5 Fernando   Alonso     
    ## 26    842        5              26.2 Heikki     Kovalainen 
    ## 27    842       10              24.5 Timo       Glock      
    ## 28    842       13              24.9 Felipe     Massa      
    ## 29    842       15              25.5 Jarno      Trulli     
    ## 30    842       16              25.4 Adrian     Sutil      
    ## 31    842       17              22.2 Mark       Webber     
    ## 32    842       18              22.5 Jenson     Button     
    ## 33    842       20              22.4 Sebastian  Vettel     
    ## 34    842       22              28.7 Rubens     Barrichello
    ## 35    842       24              31.1 Vitantonio Liuzzi     
    ## 36    842       30              22.4 Michael    Schumacher 
    ## 37    842       67              26.9 Sébastien  Buemi      
    ## 38    842      153              23.2 Jaime      Alguersuari
    ## 39    842      155              23.4 Kamui      Kobayashi  
    ## 40    842      808              24.9 Vitaly     Petrov     
    ## 41    842      814              23.1 Paul       di Resta   
    ## 42    842      815              23.6 Sergio     Pérez      
    ## 43    842      816              25.3 Jérôme     d'Ambrosio 
    ## 44    843        1              20.7 Lewis      Hamilton   
    ## 45    843        2              22.0 Nick       Heidfeld   
    ## 46    843        3              21.2 Nico       Rosberg    
    ## 47    843        4              21.9 Fernando   Alonso     
    ## 48    843        5              22.2 Heikki     Kovalainen 
    ## 49    843       10              26.1 Timo       Glock      
    ## 50    843       13              21.4 Felipe     Massa      
    ## # … with 4,651 more rows

\[20 pts\] Rank the average time spent at the pit stop in order of who
won each race

``` r
avg_pit_stops_winner_ranked <- avg_pit_stops %>%
  left_join(results %>%
              select(raceId, driverId, resultId, positionOrder, points),
            by = join_by(raceId, driverId)) %>%
  left_join((races %>% select(raceId,year,name,date)),
             by=join_by(raceId)) %>%
  filter(positionOrder==1) %>%
  arrange(meanduration_secs,.by_group = FALSE) 


print(avg_pit_stops_winner_ranked, n=50)
```

    ## # A tibble: 239 × 11
    ## # Groups:   raceId [239]
    ##    raceId driverId meanduration_secs forename  surname    resultId positionOrder
    ##     <int>    <int>             <dbl> <chr>     <chr>         <int>         <int>
    ##  1    869        4              17.8 Fernando  Alonso        21448             1
    ##  2   1076      844              18.0 Charles   Leclerc       25446             1
    ##  3   1088      830              18.3 Max       Verstappen    25686             1
    ##  4    959        1              18.5 Lewis     Hamilton      23159             1
    ##  5   1078      830              18.8 Max       Verstappen    25486             1
    ##  6   1064      830              18.9 Max       Verstappen    25206             1
    ##  7    999        1              19.0 Lewis     Hamilton      23983             1
    ##  8   1031      822              19.2 Valtteri  Bottas        24626             1
    ##  9    884        4              19.2 Fernando  Alonso        21800             1
    ## 10    987       20              19.4 Sebastian Vettel        23742             1
    ## 11    888       20              19.4 Sebastian Vettel        21888             1
    ## 12    858        1              19.4 Lewis     Hamilton      21184             1
    ## 13    936       20              19.4 Sebastian Vettel        22716             1
    ## 14    871       18              19.7 Jenson    Button        21496             1
    ## 15    870        1              19.7 Lewis     Hamilton      21472             1
    ## 16    875       20              19.8 Sebastian Vettel        21592             1
    ## 17   1013      822              20.0 Valtteri  Bottas        24263             1
    ## 18    955        3              20.1 Nico      Rosberg       23071             1
    ## 19    856       20              20.1 Sebastian Vettel        21136             1
    ## 20    909        3              20.2 Nico      Rosberg       22328             1
    ## 21    956        1              20.2 Lewis     Hamilton      23093             1
    ## 22    845       20              20.2 Sebastian Vettel        20873             1
    ## 23    867        4              20.3 Fernando  Alonso        21400             1
    ## 24    970        1              20.3 Lewis     Hamilton      23399             1
    ## 25    877        8              20.3 Kimi      Räikkönen     21640             1
    ## 26   1020      830              20.4 Max       Verstappen    24406             1
    ## 27    844       20              20.4 Sebastian Vettel        20849             1
    ## 28    882        4              20.4 Fernando  Alonso        21756             1
    ## 29    874       20              20.4 Sebastian Vettel        21568             1
    ## 30   1071        1              20.5 Lewis     Hamilton      25326             1
    ## 31    848       20              20.6 Sebastian Vettel        20944             1
    ## 32    850        1              20.6 Lewis     Hamilton      20992             1
    ## 33    843        1              20.7 Lewis     Hamilton      20825             1
    ## 34    876       20              20.7 Sebastian Vettel        21616             1
    ## 35   1075      830              20.7 Max       Verstappen    25426             1
    ## 36    872        1              20.7 Lewis     Hamilton      21520             1
    ## 37    878        1              20.8 Lewis     Hamilton      21664             1
    ## 38    859       17              20.8 Mark      Webber        21208             1
    ## 39    992        1              20.9 Lewis     Hamilton      23842             1
    ## 40    886       20              20.9 Sebastian Vettel        21844             1
    ## 41    862        3              20.9 Nico      Rosberg       21280             1
    ## 42   1001       20              20.9 Sebastian Vettel        24023             1
    ## 43    855       18              20.9 Jenson    Button        21112             1
    ## 44   1058      830              20.9 Max       Verstappen    25106             1
    ## 45    851       18              21.0 Jenson    Button        21016             1
    ## 46    852       20              21.0 Sebastian Vettel        21040             1
    ## 47   1081      830              21.1 Max       Verstappen    25546             1
    ## 48   1018      830              21.1 Max       Verstappen    24366             1
    ## 49   1060      830              21.2 Max       Verstappen    25126             1
    ## 50    881       20              21.2 Sebastian Vettel        21734             1
    ##    points  year name                     date      
    ##     <dbl> <int> <chr>                    <chr>     
    ##  1     25  2012 German Grand Prix        2012-07-22
    ##  2     26  2022 Australian Grand Prix    2022-04-10
    ##  3     26  2022 Dutch Grand Prix         2022-09-04
    ##  4     25  2016 German Grand Prix        2016-07-31
    ##  5     26  2022 Miami Grand Prix         2022-05-08
    ##  6     25  2021 Dutch Grand Prix         2021-09-05
    ##  7     25  2018 German Grand Prix        2018-07-22
    ##  8     25  2020 Austrian Grand Prix      2020-07-05
    ##  9     25  2013 Spanish Grand Prix       2013-05-12
    ## 10     25  2017 Brazilian Grand Prix     2017-11-12
    ## 11     25  2013 German Grand Prix        2013-07-07
    ## 12     25  2011 Abu Dhabi Grand Prix     2011-11-13
    ## 13     25  2015 Hungarian Grand Prix     2015-07-26
    ## 14     25  2012 Belgian Grand Prix       2012-09-02
    ## 15     25  2012 Hungarian Grand Prix     2012-07-29
    ## 16     25  2012 Korean Grand Prix        2012-10-14
    ## 17     25  2019 Azerbaijan Grand Prix    2019-04-28
    ## 18     25  2016 European Grand Prix      2016-06-19
    ## 19     25  2011 Korean Grand Prix        2011-10-16
    ## 20     25  2014 German Grand Prix        2014-07-20
    ## 21     25  2016 Austrian Grand Prix      2016-07-03
    ## 22     25  2011 Spanish Grand Prix       2011-05-22
    ## 23     25  2012 European Grand Prix      2012-06-24
    ## 24     25  2017 Chinese Grand Prix       2017-04-09
    ## 25     25  2012 Abu Dhabi Grand Prix     2012-11-04
    ## 26     26  2019 German Grand Prix        2019-07-28
    ## 27     25  2011 Turkish Grand Prix       2011-05-08
    ## 28     25  2013 Chinese Grand Prix       2013-04-14
    ## 29     25  2012 Japanese Grand Prix      2012-10-07
    ## 30     25  2021 São Paulo Grand Prix     2021-11-14
    ## 31     25  2011 European Grand Prix      2011-06-26
    ## 32     25  2011 German Grand Prix        2011-07-24
    ## 33     25  2011 Chinese Grand Prix       2011-04-17
    ## 34     25  2012 Indian Grand Prix        2012-10-28
    ## 35     25  2022 Saudi Arabian Grand Prix 2022-03-27
    ## 36     25  2012 Italian Grand Prix       2012-09-09
    ## 37     25  2012 United States Grand Prix 2012-11-18
    ## 38     25  2011 Brazilian Grand Prix     2011-11-27
    ## 39     25  2018 Azerbaijan Grand Prix    2018-04-29
    ## 40     25  2013 Canadian Grand Prix      2013-06-09
    ## 41     25  2012 Chinese Grand Prix       2012-04-15
    ## 42     25  2018 Belgian Grand Prix       2018-08-26
    ## 43     25  2011 Japanese Grand Prix      2011-10-09
    ## 44     25  2021 Styrian Grand Prix       2021-06-27
    ## 45     25  2011 Hungarian Grand Prix     2011-07-31
    ## 46     25  2011 Belgian Grand Prix       2011-08-28
    ## 47     25  2022 Azerbaijan Grand Prix    2022-06-12
    ## 48     26  2019 Austrian Grand Prix      2019-06-30
    ## 49     26  2021 Austrian Grand Prix      2021-07-04
    ## 50     25  2013 Malaysian Grand Prix     2013-03-24
    ## # … with 189 more rows

\[20 pts\] Insert the missing code (e.g: ALO for Alonso) for drivers
based on the ‘drivers’ dataset

``` r
split<-as.vector(drivers$"surname")
spllist<-as.data.frame(str_split_fixed(split," ",n=10)) %>% mutate(code = NA)

for (i in (1:nrow(spllist))) {
  if ((spllist[i,"V3"]=="") & (str_length(spllist[i,"V1"])>=3)) {
    spllist[i,"code"] <- str_to_upper(
      str_sub(
        spllist[i,"V1"],
        1,3))
  }
  else if ((spllist[i,"V3"]=="")&(str_length(spllist[i,"V1"])<3)) {
    spllist[i,"code"] <- str_to_upper(
      str_sub(
        str_c(
          spllist[i,"V1"],
          spllist[i,"V2"]),
        1,3))
  }
  else if (spllist[i,"V3"]!="") {
    spllist[i,"code"]<-str_to_upper(
      str_c(
        str_sub(
          spllist[i,"V1"],1,1),
        str_sub(
          spllist[i,"V2"],1,1),
        str_sub(
          spllist[i,"V3"],1,1)))
  }
}

drivers_fill_code <- drivers
drivers_fill_code$code <- spllist$code

print(as_tibble(drivers_fill_code), n=50)
```

    ## # A tibble: 855 × 9
    ##    driverId driverRef          number code  forename     surname      
    ##       <int> <chr>              <chr>  <chr> <chr>        <chr>        
    ##  1        1 hamilton           "44"   HAM   Lewis        Hamilton     
    ##  2        2 heidfeld           "\\N"  HEI   Nick         Heidfeld     
    ##  3        3 rosberg            "6"    ROS   Nico         Rosberg      
    ##  4        4 alonso             "14"   ALO   Fernando     Alonso       
    ##  5        5 kovalainen         "\\N"  KOV   Heikki       Kovalainen   
    ##  6        6 nakajima           "\\N"  NAK   Kazuki       Nakajima     
    ##  7        7 bourdais           "\\N"  BOU   Sébastien    Bourdais     
    ##  8        8 raikkonen          "7"    RÄI   Kimi         Räikkönen    
    ##  9        9 kubica             "88"   KUB   Robert       Kubica       
    ## 10       10 glock              "\\N"  GLO   Timo         Glock        
    ## 11       11 sato               "\\N"  SAT   Takuma       Sato         
    ## 12       12 piquet_jr          "\\N"  PIQ   Nelson       Piquet Jr.   
    ## 13       13 massa              "19"   MAS   Felipe       Massa        
    ## 14       14 coulthard          "\\N"  COU   David        Coulthard    
    ## 15       15 trulli             "\\N"  TRU   Jarno        Trulli       
    ## 16       16 sutil              "99"   SUT   Adrian       Sutil        
    ## 17       17 webber             "\\N"  WEB   Mark         Webber       
    ## 18       18 button             "22"   BUT   Jenson       Button       
    ## 19       19 davidson           "\\N"  DAV   Anthony      Davidson     
    ## 20       20 vettel             "5"    VET   Sebastian    Vettel       
    ## 21       21 fisichella         "\\N"  FIS   Giancarlo    Fisichella   
    ## 22       22 barrichello        "\\N"  BAR   Rubens       Barrichello  
    ## 23       23 ralf_schumacher    "\\N"  SCH   Ralf         Schumacher   
    ## 24       24 liuzzi             "\\N"  LIU   Vitantonio   Liuzzi       
    ## 25       25 wurz               "\\N"  WUR   Alexander    Wurz         
    ## 26       26 speed              "\\N"  SPE   Scott        Speed        
    ## 27       27 albers             "\\N"  ALB   Christijan   Albers       
    ## 28       28 markus_winkelhock  "\\N"  WIN   Markus       Winkelhock   
    ## 29       29 yamamoto           "\\N"  YAM   Sakon        Yamamoto     
    ## 30       30 michael_schumacher "\\N"  SCH   Michael      Schumacher   
    ## 31       31 montoya            "\\N"  PAB   Juan         Pablo Montoya
    ## 32       32 klien              "\\N"  KLI   Christian    Klien        
    ## 33       33 monteiro           "\\N"  MON   Tiago        Monteiro     
    ## 34       34 ide                "\\N"  IDE   Yuji         Ide          
    ## 35       35 villeneuve         "\\N"  VIL   Jacques      Villeneuve   
    ## 36       36 montagny           "\\N"  MON   Franck       Montagny     
    ## 37       37 rosa               "\\N"  DLR   Pedro        de la Rosa   
    ## 38       38 doornbos           "\\N"  DOO   Robert       Doornbos     
    ## 39       39 karthikeyan        "\\N"  KAR   Narain       Karthikeyan  
    ## 40       40 friesacher         "\\N"  FRI   Patrick      Friesacher   
    ## 41       41 zonta              "\\N"  ZON   Ricardo      Zonta        
    ## 42       42 pizzonia           "\\N"  PIZ   Antônio      Pizzonia     
    ## 43       43 matta              "\\N"  DAM   Cristiano    da Matta     
    ## 44       44 panis              "\\N"  PAN   Olivier      Panis        
    ## 45       45 pantano            "\\N"  PAN   Giorgio      Pantano      
    ## 46       46 bruni              "\\N"  BRU   Gianmaria    Bruni        
    ## 47       47 baumgartner        "\\N"  BAU   Zsolt        Baumgartner  
    ## 48       48 gene               "\\N"  GEN   Marc         Gené         
    ## 49       49 frentzen           "\\N"  FRE   Heinz-Harald Frentzen     
    ## 50       50 verstappen         "\\N"  VER   Jos          Verstappen   
    ##    dob        nationality
    ##    <chr>      <chr>      
    ##  1 1985-01-07 British    
    ##  2 1977-05-10 German     
    ##  3 1985-06-27 German     
    ##  4 1981-07-29 Spanish    
    ##  5 1981-10-19 Finnish    
    ##  6 1985-01-11 Japanese   
    ##  7 1979-02-28 French     
    ##  8 1979-10-17 Finnish    
    ##  9 1984-12-07 Polish     
    ## 10 1982-03-18 German     
    ## 11 1977-01-28 Japanese   
    ## 12 1985-07-25 Brazilian  
    ## 13 1981-04-25 Brazilian  
    ## 14 1971-03-27 British    
    ## 15 1974-07-13 Italian    
    ## 16 1983-01-11 German     
    ## 17 1976-08-27 Australian 
    ## 18 1980-01-19 British    
    ## 19 1979-04-18 British    
    ## 20 1987-07-03 German     
    ## 21 1973-01-14 Italian    
    ## 22 1972-05-23 Brazilian  
    ## 23 1975-06-30 German     
    ## 24 1980-08-06 Italian    
    ## 25 1974-02-15 Austrian   
    ## 26 1983-01-24 American   
    ## 27 1979-04-16 Dutch      
    ## 28 1980-06-13 German     
    ## 29 1982-07-09 Japanese   
    ## 30 1969-01-03 German     
    ## 31 1975-09-20 Colombian  
    ## 32 1983-02-07 Austrian   
    ## 33 1976-07-24 Portuguese 
    ## 34 1975-01-21 Japanese   
    ## 35 1971-04-09 Canadian   
    ## 36 1978-01-05 French     
    ## 37 1971-02-24 Spanish    
    ## 38 1981-09-23 Dutch      
    ## 39 1977-01-14 Indian     
    ## 40 1980-09-26 Austrian   
    ## 41 1976-03-23 Brazilian  
    ## 42 1980-09-11 Brazilian  
    ## 43 1973-09-19 Brazilian  
    ## 44 1966-09-02 French     
    ## 45 1979-02-04 Italian    
    ## 46 1981-05-30 Italian    
    ## 47 1981-01-01 Hungarian  
    ## 48 1974-03-29 Spanish    
    ## 49 1967-05-18 German     
    ## 50 1972-03-04 Dutch      
    ##    url                                                     
    ##    <chr>                                                   
    ##  1 http://en.wikipedia.org/wiki/Lewis_Hamilton             
    ##  2 http://en.wikipedia.org/wiki/Nick_Heidfeld              
    ##  3 http://en.wikipedia.org/wiki/Nico_Rosberg               
    ##  4 http://en.wikipedia.org/wiki/Fernando_Alonso            
    ##  5 http://en.wikipedia.org/wiki/Heikki_Kovalainen          
    ##  6 http://en.wikipedia.org/wiki/Kazuki_Nakajima            
    ##  7 http://en.wikipedia.org/wiki/S%C3%A9bastien_Bourdais    
    ##  8 http://en.wikipedia.org/wiki/Kimi_R%C3%A4ikk%C3%B6nen   
    ##  9 http://en.wikipedia.org/wiki/Robert_Kubica              
    ## 10 http://en.wikipedia.org/wiki/Timo_Glock                 
    ## 11 http://en.wikipedia.org/wiki/Takuma_Sato                
    ## 12 http://en.wikipedia.org/wiki/Nelson_Piquet,_Jr.         
    ## 13 http://en.wikipedia.org/wiki/Felipe_Massa               
    ## 14 http://en.wikipedia.org/wiki/David_Coulthard            
    ## 15 http://en.wikipedia.org/wiki/Jarno_Trulli               
    ## 16 http://en.wikipedia.org/wiki/Adrian_Sutil               
    ## 17 http://en.wikipedia.org/wiki/Mark_Webber_(racing_driver)
    ## 18 http://en.wikipedia.org/wiki/Jenson_Button              
    ## 19 http://en.wikipedia.org/wiki/Anthony_Davidson           
    ## 20 http://en.wikipedia.org/wiki/Sebastian_Vettel           
    ## 21 http://en.wikipedia.org/wiki/Giancarlo_Fisichella       
    ## 22 http://en.wikipedia.org/wiki/Rubens_Barrichello         
    ## 23 http://en.wikipedia.org/wiki/Ralf_Schumacher            
    ## 24 http://en.wikipedia.org/wiki/Vitantonio_Liuzzi          
    ## 25 http://en.wikipedia.org/wiki/Alexander_Wurz             
    ## 26 http://en.wikipedia.org/wiki/Scott_Speed                
    ## 27 http://en.wikipedia.org/wiki/Christijan_Albers          
    ## 28 http://en.wikipedia.org/wiki/Markus_Winkelhock          
    ## 29 http://en.wikipedia.org/wiki/Sakon_Yamamoto             
    ## 30 http://en.wikipedia.org/wiki/Michael_Schumacher         
    ## 31 http://en.wikipedia.org/wiki/Juan_Pablo_Montoya         
    ## 32 http://en.wikipedia.org/wiki/Christian_Klien            
    ## 33 http://en.wikipedia.org/wiki/Tiago_Monteiro             
    ## 34 http://en.wikipedia.org/wiki/Yuji_Ide                   
    ## 35 http://en.wikipedia.org/wiki/Jacques_Villeneuve         
    ## 36 http://en.wikipedia.org/wiki/Franck_Montagny            
    ## 37 http://en.wikipedia.org/wiki/Pedro_de_la_Rosa           
    ## 38 http://en.wikipedia.org/wiki/Robert_Doornbos            
    ## 39 http://en.wikipedia.org/wiki/Narain_Karthikeyan         
    ## 40 http://en.wikipedia.org/wiki/Patrick_Friesacher         
    ## 41 http://en.wikipedia.org/wiki/Ricardo_Zonta              
    ## 42 http://en.wikipedia.org/wiki/Ant%C3%B4nio_Pizzonia      
    ## 43 http://en.wikipedia.org/wiki/Cristiano_da_Matta         
    ## 44 http://en.wikipedia.org/wiki/Olivier_Panis              
    ## 45 http://en.wikipedia.org/wiki/Giorgio_Pantano            
    ## 46 http://en.wikipedia.org/wiki/Gianmaria_Bruni            
    ## 47 http://en.wikipedia.org/wiki/Zsolt_Baumgartner          
    ## 48 http://en.wikipedia.org/wiki/Marc_Gen%C3%A9             
    ## 49 http://en.wikipedia.org/wiki/Heinz-Harald_Frentzen      
    ## 50 http://en.wikipedia.org/wiki/Jos_Verstappen             
    ## # … with 805 more rows

\[20 pts\] Who is the youngest and oldest driver for each race? Create a
new column called “Age”

``` r
results_join <- results %>%
  select(resultId,raceId, driverId,positionOrder,points) %>%
  left_join((drivers%>%
               select(driverId, forename, surname, dob) %>%
               rename(driverdob=dob)),
            join_by(driverId)) %>%
  left_join((races %>%
               select(raceId, year, name, date) %>%
               rename(racename=name, raceyear=year,racedate=date)),
            join_by(raceId))

results_join$driverdob <- strptime(results_join$driverdob,
                                   format = "%Y-%m-%d")
results_join$racedate <- strptime(results_join$racedate,
                                  format = "%Y-%m-%d")
results_join <- results_join %>%
  mutate(age_at_race = time_length(
    difftime(racedate,driverdob),
    "years"))

results_join_agemin <- results_join %>%
  group_by(raceId) %>%
  filter(age_at_race==min(age_at_race)) %>%
  mutate(agetype= "Youngest at race")

results_join_agemax <- results_join %>%
  group_by(raceId) %>%
  filter(age_at_race==max(age_at_race)) %>%
  mutate(agetype= "Oldest at race")

results_join_ageminmax<- bind_rows(
  results_join_agemin,results_join_agemax) %>%
  arrange(raceId)

print(results_join_ageminmax, n=50)
```

    ## # A tibble: 2,165 × 13
    ## # Groups:   raceId [1,079]
    ##    resultId raceId driverId positionOrder points forename  surname    
    ##       <int>  <int>    <int>         <int>  <dbl> <chr>     <chr>      
    ##  1     7560      1       67             7      2 Sébastien Buemi      
    ##  2     7555      1       22             2      8 Rubens    Barrichello
    ##  3     7589      2       67            16      0 Sébastien Buemi      
    ##  4     7578      2       22             5      2 Rubens    Barrichello
    ##  5     7601      3       67             8      1 Sébastien Buemi      
    ##  6     7597      3       22             4      5 Rubens    Barrichello
    ##  7     7630      4       67            17      0 Sébastien Buemi      
    ##  8     7618      4       22             5      4 Rubens    Barrichello
    ##  9     7651      5       67            18      0 Sébastien Buemi      
    ## 10     7635      5       22             2      8 Rubens    Barrichello
    ## 11     7673      6       67            20      0 Sébastien Buemi      
    ## 12     7655      6       22             2      8 Rubens    Barrichello
    ## 13     7688      7       67            15      0 Sébastien Buemi      
    ## 14     7692      7       22            19      0 Rubens    Barrichello
    ## 15     7711      8       67            18      0 Sébastien Buemi      
    ## 16     7696      8       22             3      6 Rubens    Barrichello
    ## 17     7729      9       67            16      0 Sébastien Buemi      
    ## 18     7719      9       22             6      3 Rubens    Barrichello
    ## 19     7748     10      153            15      0 Jaime     Alguersuari
    ## 20     7743     10       22            10      0 Rubens    Barrichello
    ## 21     7769     11      153            16      0 Jaime     Alguersuari
    ## 22     7770     11       69            17      0 Luca      Badoer     
    ## 23     7793     12      153            20      0 Jaime     Alguersuari
    ## 24     7787     12       69            14      0 Luca      Badoer     
    ## 25     7811     13      153            18      0 Jaime     Alguersuari
    ## 26     7794     13       22             1     10 Rubens    Barrichello
    ## 27     7828     14      153            15      0 Jaime     Alguersuari
    ## 28     7819     14       22             6      3 Rubens    Barrichello
    ## 29     7851     15      153            18      0 Jaime     Alguersuari
    ## 30     7840     15       22             7      2 Rubens    Barrichello
    ## 31     7867     16      153            14      0 Jaime     Alguersuari
    ## 32     7861     16       22             8      1 Rubens    Barrichello
    ## 33    16086     17      153            20      0 Jaime     Alguersuari
    ## 34    16070     17       22             4      5 Rubens    Barrichello
    ## 35       20     18       20            20      0 Sebastian Vettel     
    ## 36       14     18       14            14      0 David     Coulthard  
    ## 37       40     19       20            18      0 Sebastian Vettel     
    ## 38       31     19       14             9      0 David     Coulthard  
    ## 39       66     20       20            22      0 Sebastian Vettel     
    ## 40       62     20       14            18      0 David     Coulthard  
    ## 41       88     21       20            22      0 Sebastian Vettel     
    ## 42       78     21       14            12      0 David     Coulthard  
    ## 43      105     22       20            17      0 Sebastian Vettel     
    ## 44       97     22       14             9      0 David     Coulthard  
    ## 45      113     23       20             5      4 Sebastian Vettel     
    ## 46      127     23       14            19      0 David     Coulthard  
    ## 47      136     24       20             8      1 Sebastian Vettel     
    ## 48      131     24       14             3      6 David     Coulthard  
    ## 49      160     25       20            12      0 Sebastian Vettel     
    ## 50      157     25       14             9      0 David     Coulthard  
    ##    driverdob           raceyear racename              racedate           
    ##    <dttm>                 <int> <chr>                 <dttm>             
    ##  1 1988-10-31 00:00:00     2009 Australian Grand Prix 2009-03-29 00:00:00
    ##  2 1972-05-23 00:00:00     2009 Australian Grand Prix 2009-03-29 00:00:00
    ##  3 1988-10-31 00:00:00     2009 Malaysian Grand Prix  2009-04-05 00:00:00
    ##  4 1972-05-23 00:00:00     2009 Malaysian Grand Prix  2009-04-05 00:00:00
    ##  5 1988-10-31 00:00:00     2009 Chinese Grand Prix    2009-04-19 00:00:00
    ##  6 1972-05-23 00:00:00     2009 Chinese Grand Prix    2009-04-19 00:00:00
    ##  7 1988-10-31 00:00:00     2009 Bahrain Grand Prix    2009-04-26 00:00:00
    ##  8 1972-05-23 00:00:00     2009 Bahrain Grand Prix    2009-04-26 00:00:00
    ##  9 1988-10-31 00:00:00     2009 Spanish Grand Prix    2009-05-10 00:00:00
    ## 10 1972-05-23 00:00:00     2009 Spanish Grand Prix    2009-05-10 00:00:00
    ## 11 1988-10-31 00:00:00     2009 Monaco Grand Prix     2009-05-24 00:00:00
    ## 12 1972-05-23 00:00:00     2009 Monaco Grand Prix     2009-05-24 00:00:00
    ## 13 1988-10-31 00:00:00     2009 Turkish Grand Prix    2009-06-07 00:00:00
    ## 14 1972-05-23 00:00:00     2009 Turkish Grand Prix    2009-06-07 00:00:00
    ## 15 1988-10-31 00:00:00     2009 British Grand Prix    2009-06-21 00:00:00
    ## 16 1972-05-23 00:00:00     2009 British Grand Prix    2009-06-21 00:00:00
    ## 17 1988-10-31 00:00:00     2009 German Grand Prix     2009-07-12 00:00:00
    ## 18 1972-05-23 00:00:00     2009 German Grand Prix     2009-07-12 00:00:00
    ## 19 1990-03-23 00:00:00     2009 Hungarian Grand Prix  2009-07-26 00:00:00
    ## 20 1972-05-23 00:00:00     2009 Hungarian Grand Prix  2009-07-26 00:00:00
    ## 21 1990-03-23 00:00:00     2009 European Grand Prix   2009-08-23 00:00:00
    ## 22 1971-01-25 00:00:00     2009 European Grand Prix   2009-08-23 00:00:00
    ## 23 1990-03-23 00:00:00     2009 Belgian Grand Prix    2009-08-30 00:00:00
    ## 24 1971-01-25 00:00:00     2009 Belgian Grand Prix    2009-08-30 00:00:00
    ## 25 1990-03-23 00:00:00     2009 Italian Grand Prix    2009-09-13 00:00:00
    ## 26 1972-05-23 00:00:00     2009 Italian Grand Prix    2009-09-13 00:00:00
    ## 27 1990-03-23 00:00:00     2009 Singapore Grand Prix  2009-09-27 00:00:00
    ## 28 1972-05-23 00:00:00     2009 Singapore Grand Prix  2009-09-27 00:00:00
    ## 29 1990-03-23 00:00:00     2009 Japanese Grand Prix   2009-10-04 00:00:00
    ## 30 1972-05-23 00:00:00     2009 Japanese Grand Prix   2009-10-04 00:00:00
    ## 31 1990-03-23 00:00:00     2009 Brazilian Grand Prix  2009-10-18 00:00:00
    ## 32 1972-05-23 00:00:00     2009 Brazilian Grand Prix  2009-10-18 00:00:00
    ## 33 1990-03-23 00:00:00     2009 Abu Dhabi Grand Prix  2009-11-01 00:00:00
    ## 34 1972-05-23 00:00:00     2009 Abu Dhabi Grand Prix  2009-11-01 00:00:00
    ## 35 1987-07-03 00:00:00     2008 Australian Grand Prix 2008-03-16 00:00:00
    ## 36 1971-03-27 00:00:00     2008 Australian Grand Prix 2008-03-16 00:00:00
    ## 37 1987-07-03 00:00:00     2008 Malaysian Grand Prix  2008-03-23 00:00:00
    ## 38 1971-03-27 00:00:00     2008 Malaysian Grand Prix  2008-03-23 00:00:00
    ## 39 1987-07-03 00:00:00     2008 Bahrain Grand Prix    2008-04-06 00:00:00
    ## 40 1971-03-27 00:00:00     2008 Bahrain Grand Prix    2008-04-06 00:00:00
    ## 41 1987-07-03 00:00:00     2008 Spanish Grand Prix    2008-04-27 00:00:00
    ## 42 1971-03-27 00:00:00     2008 Spanish Grand Prix    2008-04-27 00:00:00
    ## 43 1987-07-03 00:00:00     2008 Turkish Grand Prix    2008-05-11 00:00:00
    ## 44 1971-03-27 00:00:00     2008 Turkish Grand Prix    2008-05-11 00:00:00
    ## 45 1987-07-03 00:00:00     2008 Monaco Grand Prix     2008-05-25 00:00:00
    ## 46 1971-03-27 00:00:00     2008 Monaco Grand Prix     2008-05-25 00:00:00
    ## 47 1987-07-03 00:00:00     2008 Canadian Grand Prix   2008-06-08 00:00:00
    ## 48 1971-03-27 00:00:00     2008 Canadian Grand Prix   2008-06-08 00:00:00
    ## 49 1987-07-03 00:00:00     2008 French Grand Prix     2008-06-22 00:00:00
    ## 50 1971-03-27 00:00:00     2008 French Grand Prix     2008-06-22 00:00:00
    ##    age_at_race agetype         
    ##          <dbl> <chr>           
    ##  1        20.4 Youngest at race
    ##  2        36.8 Oldest at race  
    ##  3        20.4 Youngest at race
    ##  4        36.9 Oldest at race  
    ##  5        20.5 Youngest at race
    ##  6        36.9 Oldest at race  
    ##  7        20.5 Youngest at race
    ##  8        36.9 Oldest at race  
    ##  9        20.5 Youngest at race
    ## 10        37.0 Oldest at race  
    ## 11        20.6 Youngest at race
    ## 12        37.0 Oldest at race  
    ## 13        20.6 Youngest at race
    ## 14        37.0 Oldest at race  
    ## 15        20.6 Youngest at race
    ## 16        37.1 Oldest at race  
    ## 17        20.7 Youngest at race
    ## 18        37.1 Oldest at race  
    ## 19        19.3 Youngest at race
    ## 20        37.2 Oldest at race  
    ## 21        19.4 Youngest at race
    ## 22        38.6 Oldest at race  
    ## 23        19.4 Youngest at race
    ## 24        38.6 Oldest at race  
    ## 25        19.5 Youngest at race
    ## 26        37.3 Oldest at race  
    ## 27        19.5 Youngest at race
    ## 28        37.3 Oldest at race  
    ## 29        19.5 Youngest at race
    ## 30        37.4 Oldest at race  
    ## 31        19.6 Youngest at race
    ## 32        37.4 Oldest at race  
    ## 33        19.6 Youngest at race
    ## 34        37.4 Oldest at race  
    ## 35        20.7 Youngest at race
    ## 36        37.0 Oldest at race  
    ## 37        20.7 Youngest at race
    ## 38        37.0 Oldest at race  
    ## 39        20.8 Youngest at race
    ## 40        37.0 Oldest at race  
    ## 41        20.8 Youngest at race
    ## 42        37.1 Oldest at race  
    ## 43        20.9 Youngest at race
    ## 44        37.1 Oldest at race  
    ## 45        20.9 Youngest at race
    ## 46        37.2 Oldest at race  
    ## 47        20.9 Youngest at race
    ## 48        37.2 Oldest at race  
    ## 49        21.0 Youngest at race
    ## 50        37.2 Oldest at race  
    ## # … with 2,115 more rows

\[20 pts\] For a given race, which driver has the most wins and losses?

I treat wins and losses of drivers as the position of the drivers that
has most and least points before each race. That’s because it made
comparing who has most losses between drivers possible.

If one treat wins as the times that a driver finished in the first
place, the result might be different.

``` r
results_standings_join <- inner_join(
  results %>%
    select(resultId, raceId, driverId, positionOrder, points) %>%
    rename(singlerace_positionOrder=positionOrder) %>%
    left_join((races %>%
                 select(raceId,year,name,date)),
              by=join_by(raceId)) ,
  driver_standings %>%
    select(raceId, driverId, position,wins),
  by=join_by(raceId,driverId)) %>%
  group_by(raceId) %>%
  arrange(position,.by_group = TRUE)

results_standings_max <- results_standings_join %>%
  filter(position==min(position)) %>%
  mutate(Positiontype= "Most Wins")
results_standings_min<-results_standings_join %>%
  filter(position==max(position)) %>%
  mutate(Positiontype= "Most Losses")

results_standings_min_max<- bind_rows(results_standings_min,results_standings_max) %>%
  arrange(raceId)
print(results_standings_min_max , n=50)
```

    ## # A tibble: 2,163 × 11
    ## # Groups:   raceId [1,079]
    ##    resultId raceId driverId singlerace_positionOrder points  year
    ##       <int>  <int>    <int>                    <int>  <dbl> <int>
    ##  1     7568      1        8                       15      0  2009
    ##  2     7554      1       18                        1     10  2009
    ##  3     7593      2        5                       20      0  2009
    ##  4     7574      2       18                        1      5  2009
    ##  5     7609      3       12                       16      0  2009
    ##  6     7596      3       18                        3      6  2009
    ##  7     7631      4        9                       18      0  2009
    ##  8     7614      4       18                        1     10  2009
    ##  9     7646      5        6                       13      0  2009
    ## 10     7634      5       18                        1     10  2009
    ## 11     7668      6        6                       15      0  2009
    ## 12     7654      6       18                        1     10  2009
    ## 13     7685      7        6                       12      0  2009
    ## 14     7674      7       18                        1     10  2009
    ## 15     7704      8        6                       11      0  2009
    ## 16     7699      8       18                        6      3  2009
    ## 17     7725      9        6                       12      0  2009
    ## 18     7718      9       18                        5      4  2009
    ## 19     7748     10      153                       15      0  2009
    ## 20     7740     10       18                        7      2  2009
    ## 21     7770     11       69                       17      0  2009
    ## 22     7760     11       18                        7      2  2009
    ## 23     7791     12      154                       18      0  2009
    ## 24     7790     12       18                       17      0  2009
    ## 25     7810     13       24                       17      0  2009
    ## 26     7795     13       18                        2      8  2009
    ## 27     7828     14      153                       15      0  2009
    ## 28     7818     14       18                        5      4  2009
    ## 29     7851     15      153                       18      0  2009
    ## 30     7841     15       18                        8      1  2009
    ## 31     7867     16      153                       14      0  2009
    ## 32     7858     16       18                        5      4  2009
    ## 33    16086     17      153                       20      0  2009
    ## 34    16069     17       18                        3      6  2009
    ## 35        8     18        8                        8      1  2008
    ## 36        1     18        1                        1     10  2008
    ## 37       38     19       11                       16      0  2008
    ## 38       27     19        1                        5      4  2008
    ## 39       63     20       16                       19      0  2008
    ## 40       46     20        8                        2      8  2008
    ## 41       87     21       16                       21      0  2008
    ## 42       67     21        8                        1     10  2008
    ## 43      105     22       20                       17      0  2008
    ## 44       91     22        8                        3      6  2008
    ## 45      123     23       16                       15      0  2008
    ## 46      109     23        1                        1     10  2008
    ## 47      148     24       16                       20      0  2008
    ## 48      129     24        9                        1     10  2008
    ## 49      167     25       16                       19      0  2008
    ## 50      149     25       13                        1     10  2008
    ##    name                  date       position  wins Positiontype
    ##    <chr>                 <chr>         <int> <int> <chr>       
    ##  1 Australian Grand Prix 2009-03-29       15     0 Most Losses 
    ##  2 Australian Grand Prix 2009-03-29        1     1 Most Wins   
    ##  3 Malaysian Grand Prix  2009-04-05       20     0 Most Losses 
    ##  4 Malaysian Grand Prix  2009-04-05        1     2 Most Wins   
    ##  5 Chinese Grand Prix    2009-04-19       20     0 Most Losses 
    ##  6 Chinese Grand Prix    2009-04-19        1     2 Most Wins   
    ##  7 Bahrain Grand Prix    2009-04-26       20     0 Most Losses 
    ##  8 Bahrain Grand Prix    2009-04-26        1     3 Most Wins   
    ##  9 Spanish Grand Prix    2009-05-10       20     0 Most Losses 
    ## 10 Spanish Grand Prix    2009-05-10        1     4 Most Wins   
    ## 11 Monaco Grand Prix     2009-05-24       20     0 Most Losses 
    ## 12 Monaco Grand Prix     2009-05-24        1     5 Most Wins   
    ## 13 Turkish Grand Prix    2009-06-07       20     0 Most Losses 
    ## 14 Turkish Grand Prix    2009-06-07        1     6 Most Wins   
    ## 15 British Grand Prix    2009-06-21       20     0 Most Losses 
    ## 16 British Grand Prix    2009-06-21        1     6 Most Wins   
    ## 17 German Grand Prix     2009-07-12       20     0 Most Losses 
    ## 18 German Grand Prix     2009-07-12        1     6 Most Wins   
    ## 19 Hungarian Grand Prix  2009-07-26       21     0 Most Losses 
    ## 20 Hungarian Grand Prix  2009-07-26        1     6 Most Wins   
    ## 21 European Grand Prix   2009-08-23       23     0 Most Losses 
    ## 22 European Grand Prix   2009-08-23        1     6 Most Wins   
    ## 23 Belgian Grand Prix    2009-08-30       23     0 Most Losses 
    ## 24 Belgian Grand Prix    2009-08-30        1     6 Most Wins   
    ## 25 Italian Grand Prix    2009-09-13       24     0 Most Losses 
    ## 26 Italian Grand Prix    2009-09-13        1     6 Most Wins   
    ## 27 Singapore Grand Prix  2009-09-27       24     0 Most Losses 
    ## 28 Singapore Grand Prix  2009-09-27        1     6 Most Wins   
    ## 29 Japanese Grand Prix   2009-10-04       24     0 Most Losses 
    ## 30 Japanese Grand Prix   2009-10-04        1     6 Most Wins   
    ## 31 Brazilian Grand Prix  2009-10-18       24     0 Most Losses 
    ## 32 Brazilian Grand Prix  2009-10-18        1     6 Most Wins   
    ## 33 Abu Dhabi Grand Prix  2009-11-01       24     0 Most Losses 
    ## 34 Abu Dhabi Grand Prix  2009-11-01        1     6 Most Wins   
    ## 35 Australian Grand Prix 2008-03-16        8     0 Most Losses 
    ## 36 Australian Grand Prix 2008-03-16        1     1 Most Wins   
    ## 37 Malaysian Grand Prix  2008-03-23       18     0 Most Losses 
    ## 38 Malaysian Grand Prix  2008-03-23        1     1 Most Wins   
    ## 39 Bahrain Grand Prix    2008-04-06       21     0 Most Losses 
    ## 40 Bahrain Grand Prix    2008-04-06        1     1 Most Wins   
    ## 41 Spanish Grand Prix    2008-04-27       21     0 Most Losses 
    ## 42 Spanish Grand Prix    2008-04-27        1     2 Most Wins   
    ## 43 Turkish Grand Prix    2008-05-11       22     0 Most Losses 
    ## 44 Turkish Grand Prix    2008-05-11        1     2 Most Wins   
    ## 45 Monaco Grand Prix     2008-05-25       22     0 Most Losses 
    ## 46 Monaco Grand Prix     2008-05-25        1     2 Most Wins   
    ## 47 Canadian Grand Prix   2008-06-08       22     0 Most Losses 
    ## 48 Canadian Grand Prix   2008-06-08        1     1 Most Wins   
    ## 49 French Grand Prix     2008-06-22       22     0 Most Losses 
    ## 50 French Grand Prix     2008-06-22        1     3 Most Wins   
    ## # … with 2,113 more rows

\[10 pts\] Continue exploring the data by answering your own question.

What is the average pit stop time for all the drivers in each race? What
is each drivers performance in each race compared to that race’s average
pit stop time?

``` r
avg_pit_stops_for_all<-avg_pit_stops %>% 
  group_by(raceId) %>% 
  mutate(meanpitstopforall=mean(meanduration_secs),
         performance=round(meanduration_secs-meanpitstopforall,digits=5))
print(avg_pit_stops_for_all , n=50)
```

    ## # A tibble: 4,701 × 7
    ## # Groups:   raceId [239]
    ##    raceId driverId meanduration_secs forename   surname     meanpitstopforall
    ##     <int>    <int>             <dbl> <chr>      <chr>                   <dbl>
    ##  1    841        1              23.2 Lewis      Hamilton                 24.3
    ##  2    841        2              24.0 Nick       Heidfeld                 24.3
    ##  3    841        3              23.7 Nico       Rosberg                  24.3
    ##  4    841        4              24.1 Fernando   Alonso                   24.3
    ##  5    841        5              24.9 Heikki     Kovalainen               24.3
    ##  6    841       10              23.8 Timo       Glock                    24.3
    ##  7    841       13              24.1 Felipe     Massa                    24.3
    ##  8    841       15              24.9 Jarno      Trulli                   24.3
    ##  9    841       16              24.9 Adrian     Sutil                    24.3
    ## 10    841       17              24.1 Mark       Webber                   24.3
    ## 11    841       18              21.0 Jenson     Button                   24.3
    ## 12    841       20              23.3 Sebastian  Vettel                   24.3
    ## 13    841       22              26.2 Rubens     Barrichello              24.3
    ## 14    841       30              24.5 Michael    Schumacher               24.3
    ## 15    841       67              24.2 Sébastien  Buemi                    24.3
    ## 16    841      153              25.9 Jaime      Alguersuari              24.3
    ## 17    841      155              24.1 Kamui      Kobayashi                24.3
    ## 18    841      808              25.1 Vitaly     Petrov                   24.3
    ## 19    841      814              24.6 Paul       di Resta                 24.3
    ## 20    841      815              23.4 Sergio     Pérez                    24.3
    ## 21    841      816              25.9 Jérôme     d'Ambrosio               24.3
    ## 22    842        1              23.3 Lewis      Hamilton                 24.6
    ## 23    842        2              23.2 Nick       Heidfeld                 24.6
    ## 24    842        3              23.4 Nico       Rosberg                  24.6
    ## 25    842        4              24.5 Fernando   Alonso                   24.6
    ## 26    842        5              26.2 Heikki     Kovalainen               24.6
    ## 27    842       10              24.5 Timo       Glock                    24.6
    ## 28    842       13              24.9 Felipe     Massa                    24.6
    ## 29    842       15              25.5 Jarno      Trulli                   24.6
    ## 30    842       16              25.4 Adrian     Sutil                    24.6
    ## 31    842       17              22.2 Mark       Webber                   24.6
    ## 32    842       18              22.5 Jenson     Button                   24.6
    ## 33    842       20              22.4 Sebastian  Vettel                   24.6
    ## 34    842       22              28.7 Rubens     Barrichello              24.6
    ## 35    842       24              31.1 Vitantonio Liuzzi                   24.6
    ## 36    842       30              22.4 Michael    Schumacher               24.6
    ## 37    842       67              26.9 Sébastien  Buemi                    24.6
    ## 38    842      153              23.2 Jaime      Alguersuari              24.6
    ## 39    842      155              23.4 Kamui      Kobayashi                24.6
    ## 40    842      808              24.9 Vitaly     Petrov                   24.6
    ## 41    842      814              23.1 Paul       di Resta                 24.6
    ## 42    842      815              23.6 Sergio     Pérez                    24.6
    ## 43    842      816              25.3 Jérôme     d'Ambrosio               24.6
    ## 44    843        1              20.7 Lewis      Hamilton                 22.4
    ## 45    843        2              22.0 Nick       Heidfeld                 22.4
    ## 46    843        3              21.2 Nico       Rosberg                  22.4
    ## 47    843        4              21.9 Fernando   Alonso                   22.4
    ## 48    843        5              22.2 Heikki     Kovalainen               22.4
    ## 49    843       10              26.1 Timo       Glock                    22.4
    ## 50    843       13              21.4 Felipe     Massa                    22.4
    ##    performance
    ##          <dbl>
    ##  1     -1.07  
    ##  2     -0.234 
    ##  3     -0.564 
    ##  4     -0.225 
    ##  5      0.585 
    ##  6     -0.488 
    ##  7     -0.135 
    ##  8      0.593 
    ##  9      0.644 
    ## 10     -0.222 
    ## 11     -3.33  
    ## 12     -0.961 
    ## 13      1.89  
    ## 14      0.224 
    ## 15     -0.0594
    ## 16      1.62  
    ## 17     -0.152 
    ## 18      0.829 
    ## 19      0.317 
    ## 20     -0.842 
    ## 21      1.57  
    ## 22     -1.31  
    ## 23     -1.32  
    ## 24     -1.21  
    ## 25     -0.0551
    ## 26      1.64  
    ## 27     -0.0421
    ## 28      0.321 
    ## 29      0.951 
    ## 30      0.865 
    ## 31     -2.37  
    ## 32     -2.11  
    ## 33     -2.16  
    ## 34      4.17  
    ## 35      6.50  
    ## 36     -2.21  
    ## 37      2.29  
    ## 38     -1.33  
    ## 39     -1.20  
    ## 40      0.287 
    ## 41     -1.51  
    ## 42     -0.968 
    ## 43      0.769 
    ## 44     -1.70  
    ## 45     -0.346 
    ## 46     -1.20  
    ## 47     -0.477 
    ## 48     -0.177 
    ## 49      3.78  
    ## 50     -0.968 
    ## # … with 4,651 more rows
