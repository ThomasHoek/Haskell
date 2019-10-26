product' [] = 1
product' (x:xs) x * product xs
    
som [] = 0
som (head_som:tail_som) = head_som + som tail_som

raise_one [] = []
raise_one (head_raise:tail_raise) = (head_raise + 1) : raise_one tail_raise

mines_one [] = []
mines_one (head_mines:tail_mines) = negate head_mines : mines_one tail_mines

paste [single] list = single : list
paste [] list = list
paste (head_paste:tail_paste) list2 = head_paste : paste tail_paste list2 

-- count_p [] [] = []
-- count_p [] [value] = value
-- count_p [value] [] = value
-- count_p [single] (head_list:tail_list) = head_list : count_p single tail_list
-- count_p (head_list2:tail_list2) list2 = count_p tail_list2 list2
