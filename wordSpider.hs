data WordSpider = Empty | Letter Char [WordSpider] | Start WordSpider

addWord _ [] = Empty
addWord Empty word = Start (Letter (head word) addWord (tail word) )
addWord (Start x) word =  Start