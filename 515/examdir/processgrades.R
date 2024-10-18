###################################
## STAT 515 Spring 2015
###################################
grades= read.table("gradebook_final.csv", sep=",", header=TRUE)
gradesreduce=grades[,c(1,4,5,17,19,21)]
names(gradesreduce) = c("Name","Overall","Hw","Mid","Final","Takehome")
gradesreduce[order(grades$Overall),]

### special for incomplete (late completion of course)
QianChen = grades[6,]

## get maximum scores on the homework by slightly editing a student with
## nearly perfect scores:
maxhw = grades[3,6:15]
maxhw[1] = 100
sum(QianChen[6:14])/sum(maxhw[1:9]) # hw average for Qian Chen (ignoring last one)
[1] 0.9647059

QianChen$Homework.Average =0.9647059*30  # hw score for Qian Chen
QianChen$Final.exam = 54
## Overall grade (skipping take home; not easy to grade since it was same as take home given to others a while back)
QianChen$Overall= 100*(QianChen$Final.exam/2 + QianChen$Midterm + QianChen$Homework.Average)/85  ## (max: 30 pts + 25 pts+ 30)

                                      Name Overall    Hw  Mid Final Takehome
9                            COFFMAN, DONNA    0.00  0.00   NA    NA       NA
16                            HARAN, MURALI    0.00  0.00   NA    NA       NA
14                              GUAN, YAWEN   20.40  0.60 13.0  13.0      0.6

44.94 28.94 16.0    NA       NA 

#### D
22                       ORORBIA, ALEXANDER   27.03 15.78  6.5   9.5       NA
19                         KARLOVITZ, BRYAN   43.88 25.38 18.5    NA       NA


#### C+ 68-71
28                        THOMPSON, BRADLEY   68.46 22.71 18.0  34.5     21.0
25                           PHADKE, SAYALI   68.59 28.84 14.0  31.5     20.0
2                         BARTLEY, MERIDITH   68.73 27.98 12.0  29.5     28.0

#### B- 72-75
32                         ZECHMANN, EDWARD   72.64 27.64 17.0  30.0     26.0
24                        PETROVICH, JUSTIN   73.36 29.11 17.0  28.5     26.0
12 FERNANDES DO NASCIMENTO JUNIOR, MAURICIO   73.57 29.82 15.0  29.5     28.0

#### B 76-80
7                               CHEN, YUKUN   76.39 27.64 18.0  32.5     29.0
4                           CHEN, GENG-YUAN   76.74 28.49 12.0  43.5     29.0
5                                CHEN, MENG   78.16 29.16 18.0  33.0     29.0
11                           FENG, QINGZHOU   78.82 27.82 16.5  44.0     25.0

#### B+ 80-83
17                               HE, DAFANG   80.82 26.82 20.5  40.0     27.0
8                               CHEN, YUNSI   81.19 28.44 17.5  45.5     25.0
30                                YAO, BING   81.37 28.62 14.0  50.5     27.0
15                        HADJICOSTA, ELENA   81.73 29.73 18.0  38.0     30.0
26                            RAO, ABHISHEK   81.84 28.09 20.0  43.5     24.0

#### A-  83-87
10                             ENSLEY, JOHN   83.60 29.60 18.5  44.0     27.0
6                                CHEN, QIAN  84.63668 #### INCOMPLETE CHANGED ON JULY 8, 2015
13                                GAN, XIAO   84.69 27.69 17.0  53.0     27.0
1                             AGARWAL, AMAL   85.57 29.82 21.0  40.5     29.0
23                           PARSONS, JACOB   86.90 29.65 17.0  51.5     29.0

#### A 89+
3                             BOPP, GREGORY   89.66 29.91 15.0  59.5     30.0
31                              YUAN, ZHEYE   91.32 29.07 24.5  48.5     27.0
21                        MIRSHANI, ARDALAN   91.44 28.44 21.0  55.0     29.0
29                               YANG, GANG   91.89 28.89 22.5  52.0     29.0
18                     KAMBAMPATI, SANDILYA   93.26 28.76 23.0  56.0     27.0
33                              ZHANG, LING   93.71 28.71 22.5  56.0     29.0
27                               SHENG, BEN   96.73 29.73 23.5  57.0     30.0
20                              LIU, WANJUN   98.10 29.60 25.0  58.0     29.0
