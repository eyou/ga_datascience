y = ""
end_s = ['.','!','?']
end_s[0] = '*'
while True:
    y_in = raw_input("Enter a word (. ! or ? to end): ")
    if y_in in end_s:
        y = y + y_in
        print y
        break
    elif len(y) == 0:
        y = y + y_in
    else:
        y = y + " " + y_in
#%run sentence.py