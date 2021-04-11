;**********************Welcome to the Videogame Suggester Expert System*********************

;Created by:-  Utkarsh Tamrakar and Ruchi Ninawe

;This program takes a yes or a no as input from you to suggest different genres, 
;platforms you can play the game on, 
;number of players and such features that can help you get videogame recommendations. 

;To get the videogame suggestions, first reset then run from clips program.
;Your input should be either yes or no.

;****************************Videogame Example*********************
;
;Batman: Arkham Knight:-
;Genre: Action, Adventure, Sandbox
;Platform: PS, PC, XBOX
;Player: Singleplayer
;Features: Challenging, good graphics, contains atoryline

;*****************************************************************************************
;The function ask-questions, asks for an input from the user and reads the input as well.
;Moreover, also gives an error message for any invalid input. 
;*****************************************************************************************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "Invalid Answer!! Please type yes or no" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

;*************************************
;Defining 4 classes: 
;GAMES has genres of games
;PLATFORM has different platforms a game is available for
;PLAYERS has options for single or multiplayer
;FEATURES has general features of games
;*************************************

(defclass GAMES "class of genres"
  (is-a USER)
  (role concrete)
  (slot rpg)
  (slot action)
  (slot sandbox)
  (slot racing)
  (slot adventure)
  (slot puzzle)
  (slot platformer)
  (slot strategy)
  (slot simulation)
  (slot shooter)
  (slot horror)
  (slot sports) )

(defclass PLATFORM "class for platforms"
  (is-a USER)
  (role concrete)
  (slot pcgamer)
  (slot playstation)
  (slot xbox)
  (slot mobile) )

(defclass PLAYERS "class for players"
  (is-a USER)
  (role concrete)
  (slot single)
  (slot multi))

(defclass FEATURES "class for features"
  (is-a USER)
  (role concrete)
  (slot storyline)
  (slot graphics)
  (slot difficulty))


;****************************************
;Defining instances
;****************************************


(definstances GAMES-INSTANCES
    (game of GAMES))

(definstances PLATFORM-INSTANCES
    (plf of PLATFORM))

(definstances PLAYERS-INSTANCES
    (ply of PLAYERS))

(definstances FEATURES-INSTANCES
    (feat of FEATURES))


;****************************************
;Defining rules for asking questions
;****************************************


(defrule gamegenre "rule for asking questions for genre"
  ?ins <- (object (is-a GAMES))
=>
  (send ?ins put-rpg (ask-question "How about role playing games?(yes/no) " yes no) )
  
  (send ?ins put-action (ask-question "Would you like to play action games? (yes/no)" yes no) )
  
  (send ?ins put-racing (ask-question "Are you interested in playing racing games? (yes/no)?" yes no) )
  
  (send ?ins put-adventure (ask-question "Up for an adventure game where you can become a prince or a hero or a villain to save or destroy the day? (yes/no)?" yes no) )
  
  (send ?ins put-sandbox (ask-question "Do you like to play games where you can explore freely in the game world? (yes/no)?" yes no) )
  
  (send ?ins put-puzzle(ask-question "Umm, who doesnt like puzzles! What about you? (yes/no)?? " yes no))
  
  (send ?ins put-platformer(ask-question "Do you like to play light games on your phone?  (yes/no)?" yes no))
  
   (send ?ins put-strategy(ask-question "Figure out thieves and murderers in strategy based games! Are you in? (yes/no)?" yes no))
   
   (send ?ins put-simulation (ask-question "Do you like simulation based games? (yes/no)? " yes no))
   
   (send ?ins put-shooter(ask-question "Are you interested in playing shooting games too? (yes/no)? " yes no))
   
   (send ?ins put-horror(ask-question "Can you deal with horror?  (yes/no)? " yes no))
   
   (send ?ins put-sports(ask-question "In for some sports? (yes/no)? " yes no))
   
   (printout t "" crlf)
   (printout t "************VIDEO GAMES FOR YOU************" crlf)
   (printout t "" crlf) 
  )

(defrule platformcheck "rule for asking questions for platform specifications"
  ?p-ins <- (object (is-a PLATFORM))
=>
  (send ?p-ins put-playstation (ask-question "Do you like to play on PlayStation? (yes/no)" yes no) )
  (send ?p-ins put-pcgamer (ask-question "Are you a PC gamer? (yes/no) " yes no) )
  (send ?p-ins put-xbox (ask-question "Do you like to play on Xbox? (yes/no) " yes no) )
  
  (send ?p-ins put-mobile(ask-question "Do you want games for mobile phones? (yes/no)? " yes no))
  )

(defrule player "rule for asking questions for number of players"
  ?pl-ins <- (object (is-a PLAYERS))
=>
  (send ?pl-ins put-single (ask-question "Do you want single player games? (yes/no)" yes no) )
  (send ?pl-ins put-multi (ask-question "Want to play with friends or other people? (yes/no) " yes no) )
  )

(defrule featuresofgames "rule for asking questions for additional features"
  ?f-ins <- (object (is-a FEATURES))
=>
  (send ?f-ins put-difficulty (ask-question "Can you deal with challenging games? (yes/no) " yes no) )
  
  (send ?f-ins put-graphics (ask-question "Do you need the feel of realistic graphics in the game you are playing? (yes/no) " yes no) )
  
  (send ?f-ins put-storyline(ask-question "Is storyline crucial for you in the game?  (yes/no)? " yes no))
  )


;***************************************************************************
;Defining rules for checking each game based on the answers from user input
;***************************************************************************

;#################################ACTION ADVENTURE GAMES#######################


(defrule for-batman "checking conditions for batman"
  ?ins <- (object (is-a GAMES) (adventure ?av) (action ?a) (sandbox ?sb) )
  ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
  ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
  ?f-ins <- (object (is-a FEATURES) (difficulty ?df) )
  (exists
  (and (test(eq ?df yes))
  (or(test (eq ?sb yes)) (test (eq ?a yes)) (test(eq ?av yes)) )
  (or (test (eq ?ps yes))  (test (eq ?pc yes))  (test (eq ?xb yes)) )
  (test(eq ?si yes))
  ))
=>
  (printout t "Batman: Arkham Knight" crlf)
  )

(defrule for-devilmaycry "checking conditions for Devil May Cry"
      ?ins <- (object (is-a GAMES) (adventure ?av) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) )
      (exists
      (and(test(eq ?df yes))
      ( or(test(eq ?av yes)) (test(eq ?a yes))  )
      ( or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)) )
      ( or(test(eq ?si yes)) (test(eq ?ml yes))  )
      ))
=>
      (printout t "Devil May Cry 5" crlf) )   
      
(defrule for-godofwar "checking conditions for God of War"
      ?ins <- (object (is-a GAMES) (adventure ?av) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) )
      (exists
      (and(test(eq ?df yes))
      ( or(test(eq ?av yes)) (test(eq ?a yes))  )
      (test(eq ?ps yes))
      ( or(test(eq ?si yes)) (test(eq ?ml yes))  )
      ))
=>
    (printout t "God of War: 2018" crlf))
    
(defrule for-darksouls "checking conditions for Dark Souls"
      ?ins <- (object (is-a GAMES) (rpg ?rpgg) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) )
      
      (exists
      (and(test(eq ?df yes))
      ( or(test(eq ?rpgg yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes))  )
      ))
=>
    (printout t "Dark Souls III" crlf))
    
(defrule for-sekiro "checking conditions for Sekiro"
      ?ins <- (object (is-a GAMES) (adventure ?av) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (graphics ?gp))
      
      (exists
      (and(test(eq ?df yes)) (test(eq ?gp no))
      ( or(test(eq ?av yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "Sekiro: Shadows Die Twice" crlf))


;####################################RPG GAMES ######################


(defrule for-elderscrolls "checking conditions for The Elder Scrolls "
      ?ins <- (object (is-a GAMES) (rpg ?rpgg) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si))
      ?f-ins <- (object (is-a FEATURES)  (graphics ?gp))
      
      (exists
      (and (test(eq ?gp no))
      ( or(test(eq ?rpgg yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "The Elder Scrolls V: Skyrim" crlf))
    
(defrule for-witcher "checking conditions for the Witcher"
      ?ins <- (object (is-a GAMES) (rpg ?rpgg) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si))
      ?f-ins <- (object (is-a FEATURES)  (difficulty ?df))
      
      (exists
      (and (test(eq ?df yes))
      ( or(test(eq ?rpgg yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "The Witcher 3: Wild Hunt" crlf))
    
(defrule for-fallout "checking conditions for Fallout"
      ?ins <- (object (is-a GAMES) (rpg ?rpgg) (action ?a))
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si))
      ?f-ins <- (object (is-a FEATURES)  (difficulty ?df))
      
      (exists
      (and (test(eq ?df yes))
      ( or(test(eq ?rpgg yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "Fallout 4" crlf))
    
(defrule for-masseffect "checking conditions for Mass Effect"
      ?ins <- (object (is-a GAMES) (rpg ?rpgg) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml))
      
      (exists
      (and ( or(test(eq ?rpgg yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes)) )
      ))
=>
    (printout t "Mass Effect: Andromeda" crlf))
    
(defrule for-cyberpunk "checking conditions for Cyberpunk"
      ?ins <- (object (is-a GAMES) (rpg ?rpgg) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si))
      ?f-ins <- (object (is-a FEATURES)  (difficulty ?df))
      
      (exists
      (and (test(eq ?df yes))
      ( or(test(eq ?rpgg yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "Cyberpunk 2077" crlf))
    

    
;##########################PUZZLE GAMES####################################


(defrule  for-potral "checking conditions for Portal 2"
      ?ins <- (object (is-a GAMES) (puzzle ?puzz)  )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES)  (difficulty ?df) (graphics ?gp))
      (exists
      (and (test(eq ?df yes))
      (test(eq ?gp no))
      (test(eq ?puzz yes))  
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes))  )
      ))
=>
    (printout t "Portal 2" crlf)
    )  
    
(defrule  for-worldofgoo "checking conditions for World of Goo"
      ?ins <- (object (is-a GAMES) (puzzle ?puzz) (platformer ?plf) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) (mobile ?mb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES)  (storyline ?st) (graphics ?gp))
      (exists
      (and (test(eq ?st no)) 
      (test(eq ?gp no))
      (or(test(eq ?puzz yes)) (test (eq ?plf yes))) 
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)) (test(eq ?mb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes))  )
      ))
=>
    (printout t "World of Goo" crlf)
    )  


(defrule  for-theroom "checking conditions for The Room"
      ?ins <- (object (is-a GAMES) (puzzle ?puzz) )
      ?p-ins <- (object (is-a PLATFORM)  (pcgamer ?pc) (mobile ?mb) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si))
      ?f-ins <- (object (is-a FEATURES)  (graphics ?gp))
      (exists
      (and (test(eq ?gp no))
      (test(eq ?puzz yes)) 
      (or (test(eq ?pc yes)) (test(eq ?mb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "The Room" crlf)
    )  
    
(defrule  for-opusmagnum "checking conditions for Opus Magnum"
      ?ins <- (object (is-a GAMES) (puzzle ?puzz) )
      ?p-ins <- (object (is-a PLATFORM)  (pcgamer ?pc) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si))
      ?f-ins <- (object (is-a FEATURES)  (graphics ?gp))
      (exists
      (and (test(eq ?gp no))
      (test(eq ?puzz yes)) 
      (test(eq ?pc yes))
      (test(eq ?si yes))
      ))
=>
    (printout t "Opus Magnum" crlf)
    )  


;############################SHOOTER GAMES##############################


(defrule  for-fortnite "checking conditions for Fortnite"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (sandbox ?sb) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) (mobile ?mb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES)  (graphics ?gp) (difficulty ?df) (storyline ?st))
      (exists
      (and (test(eq ?df yes)) 
      (test(eq ?gp no)) 
      (test(eq ?st no))
      (or(test(eq ?sh yes)) (test(eq ?sb yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)) (test(eq ?mb yes)))
      (test(eq ?ml yes))
      ))
=>
    (printout t "Fortnite" crlf)
    )  
    
(defrule  for-csgo "checking conditions for Counter Strike Go"
      ?ins <- (object (is-a GAMES) (shooter ?sh) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (storyline ?st))
      (exists
      (and (test(eq ?df yes)) 
      (test(eq ?st no))
      (test(eq ?sh yes)) 
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?ml yes))
      ))
        
=>
    (printout t "Counter Strike GO" crlf)
    )  
 
(defrule  for-destiny "checking conditions for Destiny 2"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (rpg ?rpgg) (sandbox ?sb) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes))
      (or(test(eq ?sh yes)) (test(eq ?a yes))  (test(eq ?rpgg yes)) (test(eq ?sb yes)))
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?ml yes))
      ))
=>
    (printout t "Destiny 2" crlf)
    )  

(defrule  for-overwatch "checking conditions for OverWatch"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (graphics ?gp) (storyline ?st))
      (exists
      (and (test(eq ?df yes)) 
      (test(eq ?gp no)) 
      (test(eq ?st no))
      (or(test(eq ?sh yes)) (test(eq ?a yes)))
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?ml yes))
      ))
=>
    (printout t "OverWatch" crlf)
    )  
    
(defrule  for-dota "checking conditions for Dota 2"
      ?ins <- (object (is-a GAMES) (strategy ?str) (action ?a) (adventure ?av))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (graphics ?gp) (storyline ?st))
      (exists
      (and (test(eq ?df yes)) 
      (test(eq ?gp no)) 
      (test(eq ?st no))
      (or(test(eq ?str yes)) (test(eq ?a yes)) (test(eq ?av yes)))
      (test(eq ?pc yes))
      (test(eq ?ml yes))
      ))
=>
    (printout t "Dota 2" crlf)
    )
    
(defrule  for-pubg "checking conditions for PUBG"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (sandbox ?sb) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb) (mobile ?mb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (storyline ?st))
      (exists
      (and (test(eq ?df yes))  
      (test(eq ?st no))
      (or(test(eq ?sh yes)) (test(eq ?sb yes)))
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)) (test(eq ?mb yes)))
      (test(eq ?ml yes))
      ))
        
=>
    (printout t "PlayerUnknown's Battlegrounds" crlf)
    ) 


;###################################PLATFORMER GAMES####################################


(defrule  for-mario "checking conditions for Mario"
      ?ins <- (object (is-a GAMES) (platformer ?plf))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (graphics ?gp) (difficulty ?df))
      (exists
      (and (test(eq ?df yes))  
      (test(eq ?gp no))
      (test(eq ?plf yes))
      (or(test(eq ?ml yes)) (test(eq ?si yes)))
      ))
=>
    (printout t "Super Mario Odyssey" crlf)
    )  
    
(defrule  for-cuphead "checking conditions for Cuphead"
      ?ins <- (object (is-a GAMES) (platformer ?plf))
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (storyline ?st) (graphics ?gp))
      (exists
      (and (test(eq ?df yes))  
      (test(eq ?st no))
      (test(eq ?gp no))
      (test(eq ?plf yes))
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (or(test(eq ?ml yes)) (test(eq ?si yes)))
      ))
=>
    (printout t "Cuphead" crlf)
    ) 
    
(defrule  for-celeste "checking conditions for Celeste"
      ?ins <- (object (is-a GAMES) (platformer ?plf))
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (graphics ?gp))
      (exists
      (and (test(eq ?df yes))  
      (test(eq ?gp no))
      (test(eq ?plf yes))
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "Celeste" crlf)
    )  
    
(defrule  for-undertale "checking conditions for Undertale"
      ?ins <- (object (is-a GAMES) (platformer ?plf) (rpg ?rpgg))
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (graphics ?gp))
      (exists
      (and (test(eq ?df yes))  
      (test(eq ?gp no))
      (or(test(eq ?plf yes)) (test(eq ?rpgg yes)))
      (or(test(eq ?ps yes)) (test(eq ?pc yes)))
      (test(eq ?si yes))
      ))        
=>
    (printout t "Undertale" crlf)
    )  

(defrule for_mblegends "checking conditions for Mobile Legends"

      ?ins <- (object (is-a GAMES) (strategy ?str) (platformer ?plf))
      ?p-ins <- (object (is-a PLATFORM) (mobile ?mb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (graphics ?gp))
      (exists
      (and (test(eq ?gp no))
      (or(test(eq ?str yes)) (test(eq ?plf yes)) )
      (test(eq ?mb yes))
      (test(eq ?ml yes))
      ))  
=>
  (printout t "Mobile Legends: Bang Bang" crlf)
  )


;#################################ACTION SHOOTER GAMES###########################


(defrule  for-cod "checking conditions for Call of Duty"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (sandbox ?sb) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) )
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?sh yes)) (test(eq ?sb yes)) (test(eq ?a yes))  )
      (or(test(eq ?ps yes)) (test(eq ?pc yes)) (test(eq ?xb yes)))
      (or(test(eq ?ml yes)) (test(eq ?si yes)))
      ))
        
=>
    (printout t "Call of Duty: Black Ops Cold War" crlf)
    )  
    
(defrule  for-halo "checking conditions for Halo 4"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (adventure ?av) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) )
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?sh yes)) (test(eq ?av yes)) (test(eq ?a yes))  )
      (or(test(eq ?pc yes)) (test(eq ?xb yes)))
      (or(test(eq ?ml yes)) (test(eq ?si yes)))
      ))
    
=>
    (printout t "Halo 4" crlf)
    ) 
   
   
(defrule  for-battlefield " checking conditions for Battlefield"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (sandbox ?sb) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) )
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?sh yes)) (test(eq ?sb yes)) (test(eq ?a yes))  )
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (or(test(eq ?ml yes)) (test(eq ?si yes)))
      ))
=>
    (printout t "Battlefield V" crlf)
    ) 
    
    
(defrule  for-tomclancy "checking conditions for Tom Clancy's Rainbow Six Siege"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (storyline ?st))
      (exists
      (and (test(eq ?df yes)) (test(eq ?st no))
      (or(test(eq ?sh yes)) (test(eq ?a yes))  )
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (or(test(eq ?ml yes)) (test(eq ?si yes)))
      ))
    
=>
    (printout t "Tom Clancy's Rainbow Six Siege" crlf)
    )  
    
    
(defrule  for-wolfenstien "checking conditions for Wolfenstien"
      ?ins <- (object (is-a GAMES) (shooter ?sh) (action ?a) (adventure ?av) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes))
      (or(test(eq ?sh yes)) (test(eq ?a yes)) (test(eq ?av yes))  )
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
=>
    (printout t "Wolfenstien II: The New Colossus" crlf)
    )  


;###########################SANDBOX GAMES##############################


(defrule  for-minecraft "checking conditions for Minecraft"
      ?ins <- (object (is-a GAMES) (sandbox ?sb) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (storyline ?st) (graphics ?gp))
      (exists
      (and (test(eq ?gp no)) (test(eq ?st no))
      (test(eq ?sb yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))
=>
    (printout t "Minecraft" crlf))
    
(defrule  for-gta "checking conditions for GTA V"
      ?ins <- (object (is-a GAMES) (sandbox ?sb) (action ?a) (adventure ?av) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes))
      (or(test(eq ?sb yes)) (test(eq ?a yes)) (test(eq ?av yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))
        
=>
    (printout t "Grand Theft Auto V" crlf)
    ) 
    
(defrule  for-assasinscreed "checking conditions for Assasin's Creed"
      ?ins <- (object (is-a GAMES) (sandbox ?sb) (action ?a) (rpg ?rpgg) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes))
      (or(test(eq ?sb yes)) (test(eq ?a yes)) (test(eq ?rpgg yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (test(eq ?si yes))
      ))
    
=>
    (printout t "Assasin's Creed Valhalla" crlf)
    )  
 
(defrule  for-reddeadredemption "checking conditions for Red Dead Redemption"
      ?ins <- (object (is-a GAMES) (sandbox ?sb) (action ?a) (rpg ?rpgg) (adventure ?av) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes))
      (or(test(eq ?sb yes)) (test(eq ?a yes)) (test(eq ?rpgg yes)) (test(eq ?av yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))
    
=>
    (printout t "Red Dead Redemption 2" crlf)
    )    

(defrule  for-saintsrow "checking conditions for Saints Row"
      ?ins <- (object (is-a GAMES) (sandbox ?sb) (action ?a) (adventure ?av) )
      ?p-ins <- (object (is-a PLATFORM) (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes))
      (or(test(eq ?sb yes)) (test(eq ?a yes)) (test(eq ?av yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))
       
=>
    (printout t "Saints Row IV" crlf)
    )  
 

;####################STRATEGY GAMES#######################################


(defrule  for-ageofempires "checking conditions for Age of Empires"
 
      ?ins <- (object (is-a GAMES)  (strategy ?str) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (graphics ?gp))
      (exists
      (and (test(eq ?df yes)) (test(eq ?gp no))
      (test(eq ?str yes)) 
      (test(eq ?pc yes))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))
    
=>
    (printout t "Age of Empires III" crlf)
    )  
    
(defrule  for-civilization "checking conditions for Civilization"

      ?ins <- (object (is-a GAMES)  (strategy ?str) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (test(eq ?str yes)) 
      (test(eq ?pc yes))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))
=>
    (printout t "Civilization V" crlf)
    )  
    
(defrule  for-xcom "checking conditions for XCOM"

      ?ins <- (object (is-a GAMES)  (strategy ?str) (rpg ?rpgg) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb) (mobile ?mb))
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) (multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?str yes)) (test(eq ?rpgg yes))) 
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) (test(eq ?mb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))
    
=>
    (printout t "XCOM 2" crlf)
    ) 
    
(defrule  for-worldofwarcraft "checking conditions for World of Warcraft"

      ?ins <- (object (is-a GAMES)  (strategy ?str) (rpg ?rpgg) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) )
      ?pl-ins <- (object (is-a PLAYERS)(multi ?ml))
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?str yes)) (test(eq ?rpgg yes))) 
      (test(eq ?pc yes)) 
      (test(eq ?ml yes))
      ))
=>
    (printout t "World of Warcraft: Shadowlands" crlf)
    ) 
    
(defrule  for-compofheros "checking conditions for Company of Heores"

      ?ins <- (object (is-a GAMES)  (strategy ?str) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) )
      ?pl-ins <- (object (is-a PLAYERS)(multi ?ml)(single ?si) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df) (graphics ?gp))
      (exists
      (and (test(eq ?df yes)) 
      (test(eq ?str yes))
      (test(eq ?pc yes)) 
      (or(test(eq ?ml yes)) (test(eq ?si yes)))
      ))
=>
    (printout t "Company of Heroes 2" crlf)
    ) 


;######################################SIMULATION GAMES##########################


(defrule  for-sims "checking conditions for The Sims"

      ?ins <- (object (is-a GAMES)  (simulation ?sim) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS)(single ?si) )
      ?f-ins <- (object (is-a FEATURES) (graphics ?gp) (storyline ?st))
      (exists
      (and (test(eq ?st no)) (test(eq ?gp no))
      (test(eq ?sim yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes))) 
      (test(eq ?si yes))
      ))
       
=>
    (printout t "The Sims 4" crlf)
    )  
    
(defrule  for-trucksimulator "checking conditions for Euro Truck Simulator"
 
      ?ins <- (object (is-a GAMES)  (simulation ?sim) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st))
      (exists
      (and (test(eq ?st no)) 
      (test(eq ?sim yes))
      (test(eq ?pc yes)) 
      (test(eq ?si yes))
      ))
        
=>
    (printout t "Euro Truck Simulator 2" crlf)
    )
    
    
(defrule  for-mechanicsimulation "checking conditions for Car Mechanic Simulator"

      ?ins <- (object (is-a GAMES)  (simulation ?sim) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st))
      (exists
      (and (test(eq ?st no)) 
      (test(eq ?sim yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))
    
=>
    (printout t "Car Mechanic Simulator" crlf)
    )
    

 (defrule  for-cities "checking conditions for Cities Skylines"

      ?ins <- (object (is-a GAMES)  (simulation ?sim) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st))
      (exists
      (and (test(eq ?st no)) 
      (test(eq ?sim yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))
=>
    (printout t "Cities: Skylines" crlf)
    )   
    
    
;###################################ADVENTURE GAMES###########################


(defrule for_life-is-strange "checking conditions for Life is Strange"

      ?ins <- (object (is-a GAMES)  (adventure ?av) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (graphics ?gp))
      (exists
      (and (test(eq ?gp no)) 
      (test(eq ?av yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))

=>
  (printout t "Life Is Strange 2" crlf)
  )


(defrule for_walking-dead "checking conditions for The Walking Dead"

      ?ins <- (object (is-a GAMES)  (adventure ?av) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb) (mobile ?mb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (graphics ?gp))
      (exists
      (and (test(eq ?gp no)) 
      (test(eq ?av yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) (test(eq ?mb yes)) )
      (test(eq ?si yes))
      ))  
=>
  (printout t "The Walking Dead" crlf)
  )


(defrule for_sottr "checking conditions for Shadow of the Tomb Raider"

      ?ins <- (object (is-a GAMES)  (adventure ?av) (action ?a) (sandbox ?sb))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?av yes)) (test(eq ?a yes)) (test(eq ?sb yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))  
=>
  (printout t "Shadow of the Tomb Raider" crlf)
  )

(defrule for-lastofus "checking conditions for The Last of Us"

      ?ins <- (object (is-a GAMES)  (horror ?hr) (action ?a) (adventure ?av))
      ?p-ins <- (object (is-a PLATFORM)(playstation ?ps) )
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?av yes)) (test(eq ?a yes)) (test(eq ?hr yes)))
      (test(eq ?ps yes))
      (or(test(eq ?si yes))(test(eq ?ml yes)))
      ))  
=>
  (printout t "The Last of Us" crlf)
  )

(defrule for_uncharted "checking conditions for Uncharted"

      ?ins <- (object (is-a GAMES)  (adventure ?av) (action ?a) (sandbox ?sb) (shooter ?sh) (platformer ?plf))
      ?p-ins <- (object (is-a PLATFORM)  (playstation ?ps) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?av yes)) (test(eq ?a yes)) (test(eq ?sb yes)) (test(eq ?plf yes)) (test(eq ?sh yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "Uncharted: The Lost Legacy" crlf)
  )


;##########################HORROR GAMES##############################


(defrule for_residentevil "checking conditions for Resident Evil 7"

      ?ins <- (object (is-a GAMES)  (horror ?hr) (action ?a) )
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?hr yes)) (test(eq ?a yes)) )
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))  
=>
  (printout t "Resident Evil 7:Biohazard" crlf)
  )

(defrule for_deadspace "checking conditions for Dead Space"

      ?ins <- (object (is-a GAMES)  (adventure ?av) (action ?a) (shooter ?sh) (horror ?hr))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?av yes)) (test(eq ?a yes)) (test(eq ?sh yes)) (test(eq ?hr yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "Dead Space" crlf)
  )
  
(defrule for_silenthill "checking conditions for Silent Hill"
      ?ins <- (object (is-a GAMES) (horror ?hr))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (graphics ?gp) (storyline ?st))
      (exists
      (and (test(eq ?gp no)) (test(eq ?st no)) 
      (test(eq ?hr yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))  
=>
  (printout t "Silent Hill" crlf)
  )


(defrule for_outlast "checking conditions for Outlast"

      ?ins <- (object (is-a GAMES) (horror ?hr))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      (exists
      (and  (test(eq ?hr yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))  
=>
  (printout t "Outlast" crlf)
  )

(defrule for_metroex "checking conditions for Metro Exodus"

      ?ins <- (object (is-a GAMES) (horror ?hr) (shooter ?sh))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) )
      ?f-ins <- (object (is-a FEATURES) (difficulty ?df))
      (exists
      (and (test(eq ?df yes)) 
      (or(test(eq ?hr yes)) (test(eq ?sh yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (test(eq ?si yes))
      ))  
=>
  (printout t "Metro Exodus" crlf)
  )


;#####################RACING GAMES#############################


(defrule for_needforspeed "checking conditions for Need for Speed Heat"

      ?ins <- (object (is-a GAMES) (racing ?rc) (sandbox ?sb))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st))
      (exists
      (and (test(eq ?st no)) 
      (or(test(eq ?rc yes)) (test(eq ?sb yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "Need for Speed Heat" crlf)
  )

(defrule for_forza "checking conditions for Forza Horizon 4"

      ?ins <- (object (is-a GAMES) (racing ?rc) (sandbox ?sb))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st) (difficulty ?df))
      (exists
      (and (test(eq ?st no)) (test(eq ?df yes))
      (or(test(eq ?rc yes)) (test(eq ?sb yes)))
      (or(test(eq ?pc yes)) (test(eq ?xb yes)) )
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "Forza Horizon" crlf)
  )
  
(defrule for_burnout "checking conditions for Burnout Paradise"

      ?ins <- (object (is-a GAMES) (racing ?rc) (sandbox ?sb))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st) (difficulty ?df))
      (exists
      (and (test(eq ?st no)) (test(eq ?df yes))
      (or(test(eq ?rc yes)) (test(eq ?sb yes)))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "Burnout Paradise" crlf)
  )
  
;########################SPORTS GAMES###############################


(defrule for_fifa "checking conditions for FIFA"

      ?ins <- (object (is-a GAMES) (sports ?sp))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st))
      (exists
      (and (test(eq ?st no))
      (test(eq ?sp yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "FIFA 20" crlf)
  )
  
 (defrule for_rocketleague "checking conditions for Rocket League"

      ?ins <- (object (is-a GAMES) (sports ?sp))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st))
      (exists
      (and (test(eq ?st no))
      (test(eq ?sp yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) )
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "Rocket League" crlf)
  )
  
  (defrule for_nba "checking conditions for NBA"

      ?ins <- (object (is-a GAMES) (sports ?sp))
      ?p-ins <- (object (is-a PLATFORM) (pcgamer ?pc) (playstation ?ps) (xbox ?xb) (mobile ?mb))
      ?pl-ins <- (object (is-a PLAYERS) (single ?si) (multi ?ml) )
      ?f-ins <- (object (is-a FEATURES) (storyline ?st))
      (exists
      (and (test(eq ?st no))
      (test(eq ?sp yes))
      (or(test(eq ?pc yes)) (test(eq ?ps yes)) (test(eq ?xb yes)) (test(eq ?mb yes)))
      (or(test(eq ?si yes)) (test(eq ?ml yes)))
      ))  
=>
  (printout t "NBA 2K20" crlf)
  )

 
 (defrule for-nogame "rule for checking if no condition is satisfied"

      ?ins <- (object (is-a GAMES) (strategy ?str) (platformer ?plf) (rpg ?rpgg) (action ?a)(sandbox ?sb)(racing ?rc) (adventure ?av) (puzzle ?puzz) (simulation ?sim)(shooter ?sh)(horror ?hr) (sports ?sp) )
      ?p-ins <- (object (is-a PLATFORM) (mobile ?mb) (pcgamer ?pc) (playstation ?ps) (xbox ?xb) )
      ?pl-ins <- (object (is-a PLAYERS) (multi ?ml) (single ?si))
      ?f-ins <- (object (is-a FEATURES) (graphics ?gp) (difficulty ?df) (storyline ?st))
      (exists
      (or
      (and (test(eq ?gp yes)) (test(eq ?df no)) (test(eq ?st yes)) )
      (and (test(eq ?str no)) (test(eq ?plf no)) (test(eq ?rpgg no)) (test(eq ?sb no)) (test(eq ?rc no)) (test(eq ?av no)) (test(eq ?puzz no)) (test(eq ?sim no)) (test(eq ?sh no)) (test(eq ?hr no)) (test(eq ?sp no)) )
      (and
      (test(eq ?mb no)) (test(eq ?ps no)) (test(eq ?pc no)) (test(eq ?xb no)) )
      (and
      (test(eq ?ml no)) (test(eq ?si no)) )
      ))  
      
=>
    (printout t "Sorry! Couldn't find any game for you. Please try again!!" crlf)
 )