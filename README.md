# Hygeia
A CLI program and Daemon that keeps track of your moods and daily sleep

## Introduction 
Hygeia is a Daemon which does simple analysis on your mood entries and then gives you a summary of your general mood in whatever format you want!


Default format :

``` text
Date : YYYY-MM-DD

[Mood]
Mood1 : Intensity
Mood2 : Intensity
...
.
.

[Sleep] 

Wake up :
Sleep : 

OPT [Drink]

Name of the drink : shots 

OPT [Meditation]

HH:MM
HH:MM
...

OPT [Cigarette] 

Number :
Nicotone :
Tar : 


[Productivity]

n/ns


[Rating]

Some rating
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
 
Meditation entries:
 - _Meditation entries are written in the hours:minutes format and they'll be 
    sumed during the report process_
    
Drink entries:
 - _Drink entries are completed by writing the name of the drink followed by 
    a colon and the amount of shots drinked_
    
Cigarette entries: 
 - _Cigarette entries are the number of cigarettes smoked and the amount of nicotine and tar 
    the cigarette contained._

User Instructions: 

 There are two ways for writting the entries:

    1. Telling the program to generate template files for the user to fill in.
       the template will automatically generate dates, name and headers for the user 

    2. User decides to write the entry and templates won't be generated 
 
 The user can change how the entries work whenever they want.
 
## CLI 

### subcommands
- daemon 
- summary 
- generate 
- config 

## Configuration

### Paths
You can both create a configuration in an interactive way by using the CLI, or you can write your configuration manually in YAML format. 
These are the paths that Hygeia looks into: 

- `~/.config/Hygeia/config.yaml`
- `~/.Hygeia/config.yaml`
- `~/usr/share/Hygeia/config.yaml`

### Example
```YAML
Config: 
    info: 
        name: John Doe 
        email = ghasivand.artin@gmail.com
blah 
```
 Optional Headers like cigarette, Drink and ... should be specified by the user 
 in the config file located in ~/.Hygeia/config or any other directory that the 
 user specifies for Hygeia data during the instantiation process.
