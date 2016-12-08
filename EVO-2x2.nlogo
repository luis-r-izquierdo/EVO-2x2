;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EVO-2x2
;; EVO-2x2 is a computer simulation modelling framework designed to
;; formally investigate the evolution of strategies in 2-player 2-strategy (2x2)
;; symmetric games under various competing assumptions.
;; Copyright (C) 2006 Luis R. Izquierdo & Segismundo S. Izquierdo
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;; Contact information:
;; Luis R. Izquierdo
;;   University of Burgos, Spain.
;;   e-mail: lrizquierdo@ubu.es


;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

globals [
  numCC numCD/DC numDD
    ;; these variables store how many times each of the outcomes
    ;; has been observed in one match
  cum-numCC cum-numCD/DC cum-numDD
    ;; these variables store how many times each of the outcomes
    ;; has been observed in total
  cum-visits-histogram-list
    ;; The cum-visits-histogram-list keeps the value of
    ;; cum-visits-histogram at times that
    ;; can be chosen in the procedure setup (reporting-list)
  reporting-list
    ;; list with the n-of-generations at which you request a report.
    ;; Remember that 1 corresponds to the initial generation.
    ;; Thus, n-of-generations is equal to the number of times that
    ;; go is executed (i.e. ticks) plus one.
  reporter-counter
    ;; keeps track of what reporter is next
  n-categories
    ;; n-categories determines the number of categories for each
    ;; of the three dimensions in the cum-visits-histogram.
  my-random-seed
  almost-one
    ;; The variable almost-one is defined to avoid turtles
    ;; wrapping around when they shouldn't. This is done NOT
    ;; only for graphical purposes, but also to ensure that
    ;; patches have the (approximate) correct value for cum-visits.
  avg-PC
    ;; average PC across players and across generations
  avg-PC-list
    ;; The avg-PC-list keeps the value of avg-PC at times that
    ;; can be chosen in the procedure setup (reporting-list)
  avg-PC/C
    ;; average PC/C across players and across generations
  avg-PC/C-list
  avg-PC/D
    ;; average PC/D across players and across generations
  avg-PC/D-list
  n-of-generations
    ;; Number of (potentially) different generations in a simulation.
    ;; This is equal to the number of times that go is executed plus
    ;; one (since there is an initial population, created in setup).
  global-mark
    ;; an integer used to uniquely identify players throughout a simulation
  child-link-prob
]

;;;;;;;;;;;;
;; Breeds ;;
;;;;;;;;;;;;

breed [ players ]
breed [ avgsymbol ]
breed [ cum-visits-histogram-elements ]

  ;; players and avgsymbol (to mark average values) are turtles

  ;; cum-visits-histogram keeps track of the number of times
  ;; that a certain combination of three specific ranges of
  ;; values for PC, PC/C and PC/D has been observed
  ;; (e.g. [0,0.1]x[0.5,0.4]x[0.3,0.4]). It is a breed that is
  ;; meant to represent a 3-dimensional array. There are
  ;; n-categories categories for each of the three
  ;; dimensions (PC, PC/C, and PC/D).

players-own [
  fitness       ;; cumulative payoff in a match (series of rounds)
  PC            ;; Propensity to cooperate (first stage, unconditional)
  PC/C          ;; Conditional propensity to cooperate after cooperation
  PC/D          ;; Conditional propensity to cooperate after defection
  mate
  action        ;; C or D
  mate-last-action   ;; last action chosen by the other player (C or D)
  mark          ;; This is a unique label that identifies you
  parent-mark   ;; This is the mark of the player who hatched you
]

patches-own [cum-visits]

cum-visits-histogram-elements-own [value]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; the following procedure is called when the model is first loaded
to startup
  setup
end

to setup
  clear-all

  set n-categories 30
  set reporting-list [1 1001 10001 50001 100001 200001 300001 400001 500001 600001 700001 800001 900001 1000001]
    ;; Remember that 1 corresponds to the initial generation.
    ;; Thus, n-of-generations is equal to the number of times that
    ;; go is executed (i.e. ticks) plus one.

  set reporter-counter 0
  set cum-visits-histogram-list []
  set avg-PC-list []  set avg-PC/C-list []  set avg-PC/D-list []
  set avg-PC 0   set avg-PC/C 0   set avg-PC/D 0
  set cum-numCC 0   set cum-numCD/DC 0    set cum-numDD 0
  set global-mark -2147483648   ;; this is the minimum integer

  set almost-one 0.99999

  set my-random-seed new-seed ; generate a new seed
  random-seed my-random-seed  ; use the new seed

  ;create-cum-visits-histogram
  ask patches [set cum-visits 0]

  make-players
  set n-of-generations 1

  create-avgsymbol 1 [set shape "circle"]

  conduct-matches
  reset-ticks
  update-reporters
end

to make-players
  set-default-shape players "person"
  create-players num-players [
    set fitness 0
    if-else set-initial-players
    [
      if not infinite-strategies? [
        set initial-PC   (round (initial-PC * (num-strategies - 1))) / (num-strategies - 1)
        set initial-PC/C (round (initial-PC/C * (num-strategies - 1))) / (num-strategies - 1)
        set initial-PC/D (round (initial-PC/D * (num-strategies - 1))) / (num-strategies - 1)
      ]
      set PC initial-PC   set PC/C initial-PC/C   set PC/D initial-PC/D
    ]
    [
      if-else infinite-strategies?
        [
          set PC   random-float 1
          set PC/C random-float 1
          set PC/D random-float 1
        ]
        [
          set PC   (random num-strategies) / (num-strategies - 1)
          set PC/C (random num-strategies) / (num-strategies - 1)
          set PC/D (random num-strategies) / (num-strategies - 1)
        ]
    ]
    set color scale-color green PC -1 2
    set label-color red
    set xcor (almost-one * world-width) * (PC/C - 0.5)   ;; the more to the right the higher PC/C
        ;; we use almost-one because if we write: set xcor (screen-size-x * 0.5), then the turtle
        ;; wraps around, which is something very undesirable in this model.
    set ycor (almost-one * world-height) * (PC/D - 0.5)   ;; the more to the top the higher PC/D
    set mate nobody  ;; no initial mate
    set global-mark global-mark + 1
    set parent-mark global-mark
    set global-mark global-mark + 1
    set mark global-mark
  ]
end

to create-cum-visits-histogram
  create-cum-visits-histogram-elements (n-categories ^ 3) [
    hide-turtle
    set value 0
  ]
end

;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;

to go
  no-display
  make-selection
  set n-of-generations (n-of-generations + 1)
  conduct-matches
  tick
  update-reporters
  display
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Pairing Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;

to conduct-matches
  set numCC 0   set numCD/DC 0    set numDD 0

  if pairing-settings = "random pairings" [
    ask players [without-interruption [
      if (mate = nobody) [select-random-mate]
    ]]
    make-partnerships-play
    stop
  ]

  if pairing-settings = "round robin" [
    let n-of-players-minus-one (count players - 1)
    let pairs-minus-one (n-of-players-minus-one - 1) / 2
    let list-of-players [self] of players
    let fixed-player first list-of-players
    let rotating-list (but-first list-of-players)

    repeat n-of-players-minus-one [
      (foreach (fput fixed-player sublist rotating-list 0 pairs-minus-one)
               (sublist rotating-list pairs-minus-one n-of-players-minus-one)
               [ pair-off ?1 ?2 ])
      make-partnerships-play
        ;; rotate the rotating list
      set rotating-list (lput (first rotating-list) (but-first rotating-list))
    ]
    stop
  ]

  if pairing-settings = "children together" [
    let list-of-players shuffle [self] of players
      ;; Start by grouping siblings
    foreach list-of-players [
      if (random-float 1.0) < child-link-prob [
        if [mate] of ? = nobody [
          let the-mate one-of players with [
            parent-mark = ([parent-mark] of ?) and
            mate = nobody and
            self != ?
          ]
          if (the-mate != nobody) [
            ask ? [set mate the-mate]
            ask the-mate [set mate ?]
          ]
        ]
      ]
    ]

    ask players [without-interruption [
      if (mate = nobody) [select-random-mate]
    ]]
    make-partnerships-play
    stop
  ]

end

to select-random-mate
  set mate one-of players with [mate = nobody and self != myself]
  ask mate [set mate myself]
end

to pair-off [one another]
  ask one [set mate another]
  ask another [set mate one]
end

;;;;;;;;;;;;;;;;;;;
;;; Actual play ;;;
;;;;;;;;;;;;;;;;;;;

to make-partnerships-play
  ask players [select-initial-action]
  ask players [
    set mate-last-action [action] of mate
    update-fitness
    report-outcome
  ]
  let count-rounds 1

  while [count-rounds < rounds-per-match] [
    ask players [select-action]
    ask players [
      set mate-last-action [action] of mate
      update-fitness
      report-outcome
    ]
    set count-rounds count-rounds + 1
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selection mechanisms ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to make-selection

  if selection-mechanism = "winners take all" [
    let previous-generation [self] of players
    let winners players with-max [fitness]
    repeat num-players [
      ask one-of winners [
        hatch 1 [setup-child]
      ]
    ]
    foreach previous-generation [ask ? [die]]
    stop
  ]

  if selection-mechanism = "tournament" [
    let previous-generation [self] of players
    repeat num-players [
      ask max-one-of (n-of 2 players) [fitness] [
        hatch 1 [setup-child]
      ]
    ]
    foreach previous-generation [ask ? [die]]
  ]

  ;; the better of each pair gets replicated twice
  if selection-mechanism = "gladiator" [
    let previous-generation shuffle [self] of players
    foreach previous-generation [
      if [mate] of ? != nobody [
        let player-to-replicate nobody
        if-else [fitness] of ? >= [fitness] of [mate] of ?
          [set player-to-replicate ?]
          [set player-to-replicate [mate] of ?]
        ask player-to-replicate [
          hatch 2 [setup-child]
          ask mate [set mate nobody]
          set mate nobody
        ]
      ]
    ]
    foreach previous-generation [ask ? [die]]
    stop
  ]

  let previous-generation shuffle [self] of players
  let list-fitness map [[fitness] of ?] previous-generation

  if sum list-fitness = 0 [set list-fitness n-values (count players) [1]]
    ;; Applies when all players have zero fitness
  let cum-fitness [0]
    ;; cum-fitness starts at 0 and is one item longer than num-players
  foreach list-fitness [set cum-fitness lput (? + last cum-fitness) cum-fitness]


  if selection-mechanism = "roulette wheel" [
    let random-list n-values num-players [random-float (last cum-fitness)]
      ;; num-players may change during a simulation (number of children)
    let cum-children []
    foreach cum-fitness [
      let cum-fitness-value ?
      set cum-children lput length (filter [? < cum-fitness-value] random-list) cum-children
    ]
    let children (map [?1 - ?2] (but-first cum-children) (but-last cum-children))

    foreach previous-generation [
      ask ? [without-interruption [
        hatch first children [setup-child]
        set children but-first children
        die
      ]]
    ]
    stop
  ]

  if selection-mechanism = "Moran process" [
    let players-to-die n-of 2 players                                    ;; select 2 players to possibly die
    let rand random-float (last cum-fitness)                                    ;; to select the one to clone
    let player-to-clone-pos length (filter [? <= rand] but-first cum-fitness)   ;; select the to-clone position in the agentset
    let player-to-clone item player-to-clone-pos previous-generation         ;; calculate the to-clone id in the agentset
    if-else (count players = num-players)
      [ ;; if num-players does not change one dies and one reproduces
        ask player-to-clone [hatch 1 [setup-child]]
        ask one-of players-to-die [die]
      ]
      [
        if-else (count players > num-players)
          [ ask players-to-die [die] ] ;; two players die
          [ ;; two players reproduce
            set rand random-float (last cum-fitness)
              ;; to select the second one to clone
            let player2-to-clone-pos length (filter [? <= rand] but-first cum-fitness)
              ;; select the to-clone position in the agentset
            let player2-to-clone item player2-to-clone-pos previous-generation
              ;; calculate the to-clone id in the agentset
            ask player-to-clone [hatch 1 [setup-child]]
            ask player2-to-clone [hatch 1 [setup-child]]
          ]
      ]
    ask players [set mate nobody    set fitness 0]
    stop
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Players' Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to select-initial-action
  let num-rand random-float 1
  set action ifelse-value (num-rand <= PC) ["C"] ["D"]
end

to select-action
  let num-rand random-float 1
  ifelse (mate-last-action = "C")
    [set action ifelse-value (num-rand <= PC/C) ["C"] ["D"]]
    [set action ifelse-value (num-rand <= PC/D) ["C"] ["D"]]
end

to update-fitness
  ifelse action = "C"
     [set fitness fitness + ifelse-value ([action] of mate = "C") [CC-payoff] [CD-payoff]]
     [set fitness fitness + ifelse-value ([action] of mate = "C") [DC-payoff] [DD-payoff]]
end

to report-outcome
 ifelse action = "C"
   [ifelse ([action] of mate = "C")
       [set numCC numCC + .5]                    ; to keep the number of CC outcomes in a match (CC reports /2)
       [set numCD/DC numCD/DC + .5]
   ]
   [ifelse ([action] of mate = "C")
       [set numCD/DC numCD/DC + .5]
       [set numDD numDD + .5]
   ]
end

to setup-child
  set fitness 0
  set mate nobody
  set parent-mark mark
  set global-mark global-mark + 1
  set mark global-mark
  if (random-float 1 < mutation-rate) [
    if-else infinite-strategies?
    [
      set PC   random-float 1
      set PC/C random-float 1
      set PC/D random-float 1
    ]
    [
      set PC   (random num-strategies) / (num-strategies - 1)
      set PC/C (random num-strategies) / (num-strategies - 1)
      set PC/D (random num-strategies) / (num-strategies - 1)
    ]
    set color scale-color green PC -1 2
    set xcor (almost-one * world-width) * (PC/C - 0.5)
    set ycor (almost-one * world-height) * (PC/D - 0.5)
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plotting and other reporters ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-reporters
  ask patches [set cum-visits (cum-visits + count players-here)]
  set cum-numCC (cum-numCC + numCC)
  set cum-numCD/DC (cum-numCD/DC + numCD/DC)
  set cum-numDD (cum-numDD + numDD)
  update-cum-avgs
  ;update-cum-visits-histogram
  update-graphs
  if n-of-generations = (item reporter-counter reporting-list) [
    report-to-lists
    set reporter-counter min list (reporter-counter + 1) ((length reporting-list) - 1)
  ]
end

to update-cum-avgs
  set avg-PC   (avg-PC * (n-of-generations - 1) + mean [PC] of players) / n-of-generations
  set avg-PC/C (avg-PC/C * (n-of-generations - 1) + mean [PC/C] of players) / n-of-generations
  set avg-PC/D (avg-PC/D * (n-of-generations - 1) + mean [PC/D] of players) / n-of-generations
end

to update-cum-visits-histogram
  ask players [ without-interruption [
    let PC-category ifelse-value (PC >= 1)
      [ n-categories - 1 ]
      [ floor (PC * n-categories) ]
    let PC/D-category ifelse-value (PC/D >= 1)
      [ n-categories - 1 ]
      [ floor (PC/D * n-categories) ]
    let PC/C-category ifelse-value (PC/C >= 1)
      [ n-categories - 1 ]
      [ floor (PC/C * n-categories) ]
    let element PC-category * (n-categories * n-categories) +
                PC/D-category * n-categories +
                PC/C-category
    let previous-cum-visits [value] of turtle element
    ask turtle element [set value (previous-cum-visits + 1)]
  ] ]
end

to update-graphs
  update-patches-colour
  ask avgsymbol [draw-avgsymbol]
  ask players [set label count players-here]
  plot-PC  plot-PC/C  plot-PC/D  plot-fitness
  plot-averagePC  plot-averagePC/C  plot-averagePC/D  plot-averagefitness   plot-frequencies
end

to update-patches-colour
  let min-visits min [cum-visits] of patches
  let max-visits max [cum-visits] of patches
  ask patches [set pcolor scale-color blue cum-visits max-visits min-visits]
end

to draw-avgsymbol
    set color scale-color green mean [PC] of players -1 2
    set xcor (almost-one * world-width) * (mean [PC/C] of players - 0.5)   ;; the more to the right the higher PC/C
    set ycor (almost-one * world-height) * (mean [PC/D] of players - 0.5)   ;; the more to the top the higher PC/D
end

to plot-PC
  set-current-plot "PC Distribution"
  set-plot-x-range 0  1.1
  histogram [PC] of players
end

to plot-averagePC
  set-current-plot "Average PC"
  plot mean [PC] of players
end

to plot-PC/C
  set-current-plot "PC/C Distribution"
  set-plot-x-range 0  1.1
  histogram [PC/C] of players
end

to plot-averagePC/C
  set-current-plot "Average PC/C"
  plot mean [PC/C] of players
end

to plot-PC/D
  set-current-plot "PC/D Distribution"
  set-plot-x-range 0  1.1
  histogram [PC/D] of players
end

to plot-averagePC/D
  set-current-plot "Average PC/D"
  plot mean [PC/D] of players
end

to plot-fitness
  set-current-plot "Fitness Distribution"
  let maxfit max [fitness] of players
  set-plot-x-range 0  int (maxfit + 1)  ;; + 1 to make room for the width of the last bar
  set-histogram-num-bars 10
  histogram [fitness] of players
end

to plot-averagefitness
  set-current-plot "Average Fitness"
  plot mean [fitness] of players
end

to plot-frequencies
  let total (numCC + numDD + numCD/DC)
  set-current-plot "Outcome Frequencies"
  set-current-plot-pen "one"    plot 1
  set-current-plot-pen "DD"     plot 1 - numDD / total
  set-current-plot-pen "CC"     plot numCC / total
end

to report-to-lists
  set avg-PC-list lput avg-PC avg-PC-list
  set avg-PC/C-list lput avg-PC/C avg-PC/C-list
  set avg-PC/D-list lput avg-PC/D avg-PC/D-list
  set cum-visits-histogram-list lput ([value] of cum-visits-histogram-elements) cum-visits-histogram-list
end

to-report avg-PC/C-patches
  report (reduce [?1 + ?2]
    [cum-visits * ((pxcor / (almost-one * world-width)) + 0.5)] of patches)
      / (num-players * n-of-generations)
end

to-report avg-PC/D-patches
  report (reduce [?1 + ?2]
    [cum-visits * ((pycor / (almost-one * world-height)) + 0.5)] of patches)
      / (num-players * n-of-generations)
end
@#$#@#$#@
GRAPHICS-WINDOW
267
10
618
382
5
5
31.0
1
10
1
1
1
0
1
1
1
-5
5
-5
5
0
0
1
ticks
30.0

SLIDER
6
70
122
103
num-players
num-players
2
500
500
2
1
NIL
HORIZONTAL

SLIDER
4
324
163
357
rounds-per-match
rounds-per-match
1
50
5
1
1
NIL
HORIZONTAL

PLOT
824
10
1001
130
PC Distribution
PC
# Players
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.1 1 -16777216 true "" ""

PLOT
824
133
1002
253
PC/C Distribution
PC/C
# Players
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.1 1 -16777216 true "" ""

PLOT
824
256
1002
376
PC/D Distribution
PC/D
# Players
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.1 1 -16777216 true "" ""

BUTTON
79
11
141
44
Go Once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
14
11
77
44
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
824
379
1002
523
Fitness Distribution
Fitness
# Players
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

BUTTON
143
11
206
44
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
4
501
163
534
mutation-rate
mutation-rate
0
1
0.1
0.0010
1
NIL
HORIZONTAL

PLOT
621
10
821
130
Average PC
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" ""

PLOT
621
133
821
253
Average PC/C
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" ""

PLOT
621
256
821
376
Average PC/D
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" ""

PLOT
621
379
821
523
Average Fitness
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" ""

SLIDER
4
361
96
394
CC-payoff
CC-payoff
0
100
3
1
1
NIL
HORIZONTAL

SLIDER
98
361
190
394
CD-payoff
CD-payoff
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
4
396
96
429
DC-payoff
DC-payoff
0
100
5
1
1
NIL
HORIZONTAL

SLIDER
98
396
190
429
DD-payoff
DD-payoff
0
100
1
1
1
NIL
HORIZONTAL

SWITCH
60
140
256
173
set-initial-players
set-initial-players
1
1
-1000

SLIDER
59
177
151
210
initial-PC
initial-PC
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
153
177
256
210
initial-PC/C
initial-PC/C
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
152
212
256
245
initial-PC/D
initial-PC/D
0
1
0.5
0.1
1
NIL
HORIZONTAL

TEXTBOX
5
305
109
323
The Game
11
0.0
1

TEXTBOX
8
54
158
72
Population Parameters
11
0.0
1

PLOT
267
403
617
523
Outcome Frequencies
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"one" 1.0 1 -2674135 true "" ""
"DD" 1.0 1 -4539718 true "" ""
"CC" 1.0 1 -15390905 true "" ""

TEXTBOX
376
528
577
546
(CC blue) (DD red) (CD-DC grey)
11
0.0
0

CHOOSER
4
454
145
499
selection-mechanism
selection-mechanism
"roulette wheel" "Moran process" "tournament" "gladiator" "winners take all"
0

CHOOSER
68
252
231
297
pairing-settings
pairing-settings
"random pairings" "round robin" "children together"
0

TEXTBOX
5
436
146
454
Evolutionary forces
11
0.0
0

SWITCH
6
104
156
137
infinite-strategies?
infinite-strategies?
0
1
-1000

SLIDER
158
103
256
136
num-strategies
num-strategies
2
100
3
1
1
NIL
HORIZONTAL

TEXTBOX
13
261
67
294
Pairing Settings
11
0.0
1

TEXTBOX
406
385
556
403
-> PC/C ->
11
0.0
1

TEXTBOX
224
43
266
61
^ PC/D
11
0.0
1

@#$#@#$#@
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="masterExperiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
      <value value="&quot;Moran process&quot;"/>
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="m50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="t50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="cr" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="cm" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sr50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sm50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="st50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scr" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scm" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;Moran process&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sct" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;tournament&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ct-r50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;children together&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r5-d" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-strategies">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-d" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-strategies">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s50" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="r10-s100" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-strategies">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infinite-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="bruce" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>cum-numCC</metric>
    <metric>cum-numCD/DC</metric>
    <metric>cum-numDD</metric>
    <metric>reporting-list</metric>
    <metric>avg-PC-list</metric>
    <metric>avg-PC/D-list</metric>
    <metric>avg-PC/C-list</metric>
    <metric>my-random-seed</metric>
    <metric>n-of-generations</metric>
    <metric>cum-visits-histogram-list</metric>
    <enumeratedValueSet variable="num-players">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-initial-players">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DC-payoff">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CC-payoff">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DD-payoff">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CD-payoff">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pairing-settings">
      <value value="&quot;random pairings&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-mechanism">
      <value value="&quot;roulette wheel&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds-per-match">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0.001"/>
      <value value="0.005"/>
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/D">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-PC/C">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
