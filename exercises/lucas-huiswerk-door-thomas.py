Guessword = input("Word to Guess: \n").upper()
test = list(map(str, Guessword.upper()))
WrongCounter = 0 

guessed_letters = []
while True:
    print("Wrong Letters:" ,guessed_letters)
    [print(a,end="") if a in guessed_letters else print("_",end="") for a in list(map(str, Guessword.upper()))]
    print()
    try:
        guess_letter = input("Letter? \n")[0].upper()
    except IndexError:
        guess_letter = " "
    
    guessed_letters.append(guess_letter)

    if (guess_letter in test):
        test.remove(guess_letter)
        if (len(test) == 0):
            print("You win, the word was:" + Guessword)
            break
    else:
        WrongCounter += 1
        if (WrongCounter > 6):
            print("You lose, the word was:" + Guessword)
            break