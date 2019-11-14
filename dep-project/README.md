# dep-project
Order of funtions
    antMain -> langtonsAnt -> antLoop -> nextMoveAnt -> antLoop -> langontsAnt -> antMain

inside NextmoveAnt:
    nextMoveAnt -> get state -> grid -> change state -> remove doubles -> return to nextMoveAnt

if show all:
    displayMoves


How grid coordinates work:

(-5,5) (-5,5)

[-5, 5][     ][     ][     ][     ][0 , 5][     ][     ][     ][     ][5 , 5]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[-5, 0][-4, 0][-3, 0][-2, 0][-1, 0][0 , 0][1 , 0][2 , 0][3 , 0][4 , 0][5 , 0]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ][     ][     ][     ][     ][     ][     ]
[-5,-5][     ][     ][     ][     ][0 ,-5][     ][     ][     ][     ][-5, 5]


(0,5) (-2,2)

[     ][     ][0 , 5][     ][     ]
[     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ]
[     ][     ][     ][     ][     ]
[-2, 0][-1, 0][0 , 0][1 , 0][2 , 0]

