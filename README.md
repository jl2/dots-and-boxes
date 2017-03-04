This is a [Dots and Boxes](https://en.wikipedia.org/wiki/Dots_and_Boxes) game written in Common Lisp.

The game allows two people to play against each other, human vs computer, and computer vs computer.

Right now the computer chooses edges randomly, but I plan to improve its technique.

The game is currently played at the REPL, but eventually I will add a Qt GUI front end.

Here's an example of a computer vs computer game of size 2:

``` common-lisp
CL-USER> (ql:quickload :dots-and-boxes)
To load "dots-and-boxes":
  Load 1 ASDF system:
    dots-and-boxes
; Loading "dots-and-boxes"

(:DOTS-AND-BOXES)
CL-USER> (time (dab:play-dab-game (dab:create-computer-computer-dab 2 "Alice" "Mallory")))
Alice: 0 |  Mallory: 0
================================
 0    1    2   
               
 3    4    5   
               
 6    7    8   
               
================================


It's Alice's turn!
Alice: 0 |  Mallory: 0
================================
 0    1    2   
               
 3    4    5   
               
 6--- 7    8   
               
================================


It's Mallory's turn!
Alice: 0 |  Mallory: 0
================================
 0    1    2   
           |   
 3    4    5   
               
 6--- 7    8   
               
================================


It's Alice's turn!
Alice: 0 |  Mallory: 0
================================
 0    1--- 2   
           |   
 3    4    5   
               
 6--- 7    8   
               
================================


It's Mallory's turn!
Alice: 0 |  Mallory: 0
================================
 0    1--- 2   
           |   
 3    4    5   
 |             
 6--- 7    8   
               
================================


It's Alice's turn!
Alice: 0 |  Mallory: 0
================================
 0    1--- 2   
 |         |   
 3    4    5   
 |             
 6--- 7    8   
               
================================


It's Mallory's turn!
Alice: 0 |  Mallory: 0
================================
 0    1--- 2   
 |         |   
 3--- 4    5   
 |             
 6--- 7    8   
               
================================


It's Alice's turn!
Alice: 0 |  Mallory: 0
================================
 0--- 1--- 2   
 |         |   
 3--- 4    5   
 |             
 6--- 7    8   
               
================================


It's Mallory's turn!
Alice: 0 |  Mallory: 1
================================
 0--- 1--- 2   
 |         |   
 3--- 4    5   
 |    |        
 6--- 7    8   
               
================================


It's Mallory's turn!
Alice: 0 |  Mallory: 2
================================
 0--- 1--- 2   
 |    |    |   
 3--- 4    5   
 |    |        
 6--- 7    8   
               
================================


It's Mallory's turn!
Alice: 0 |  Mallory: 3
================================
 0--- 1--- 2   
 |    |    |   
 3--- 4--- 5   
 |    |        
 6--- 7    8   
               
================================


It's Mallory's turn!
Alice: 0 |  Mallory: 3
================================
 0--- 1--- 2   
 |    |    |   
 3--- 4--- 5   
 |    |        
 6--- 7--- 8   
               
================================


It's Alice's turn!

Alice: 1 |  Mallory: 3
================================
 0--- 1--- 2   
 |    |    |   
 3--- 4--- 5   
 |    |    |   
 6--- 7--- 8   
               
================================

Game over!
The score was 1 to 3
The winner is Mallory

Evaluation took:
  0.002 seconds of real time
  0.004000 seconds of total run time (0.004000 user, 0.000000 system)
  200.00% CPU
  5,654,782 processor cycles
  32,768 bytes consed
```
