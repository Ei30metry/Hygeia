# Hygeia
A CLI program and Daemon that keeps track of your moods and daily sleep

## Introduction 
Hygeia is a Daemon which does simple analysis on your mood entries and then gives you a summary of your general mood in whatever format you want!


default Format :

``` text
Name : FName LName
Date : YYYY-MM-DD

[Moods]
Mood1 : Intensity
Mood2 : Intensity
...
.
.
[Sleep] 

Wake up time :
Sleep time : 


[Productivity]

[Rating]

OPT [Alcohol]

OPT [Meditation]

OPT [Cigarette]
...
```

Available Moods:
 - **Excited**
 - **Focused**
 - **Happy**
 - **Stressed**
 - **Bored**
 - **Angry**
 - **Sad**

Available Intensities:
 - **Low**
 - **Normal**
 - **High**
 - **Extreme**

Available Ratings:
 - **Great**
 - **Good**
 - **Neutral**
 - **Bad**
 - **Awful**

Productivity ratings:
 - _A fraction with the numerator being accomplished tasks and the denominator being tasks you wanted to do_
