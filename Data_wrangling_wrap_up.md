Data wrangling wrap up
================
Sukey
October 31, 2018

-   [Character data](#character-data)
    -   [14.2.5 Exercises](#exercises)
    -   [14.3.1.1 Exercises](#exercises-1)
    -   [14.3.2.1 Exercises](#exercises-2)
    -   [14.3.3.1 Exercises](#exercises-3)
    -   [14.3.4.1 Exercises](#exercises-4)
    -   [14.3.5.1 Exercises](#exercises-5)
    -   [14.4.2 Exercises](#exercises-6)
    -   [14.4.3.1 Exercises](#exercises-7)
    -   [14.4.4.1 Exercises](#exercises-8)
    -   [14.4.5.1 Exercises](#exercises-9)
    -   [14.4.6.1 Exercises](#exercises-10)
    -   [14.5.1 Exercises](#exercises-11)
    -   [14.7.1 Exercises](#exercises-12)
-   [Writing functions](#writing-functions)

Character data
==============

``` r
suppressPackageStartupMessages(library( tidyr ))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library( dplyr ))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gtable))

library(repurrrsive)
library(robustbase)
library(gapminder)
library(stringr)



myt <- ttheme_default( 
         # Use hjust and x to center the text
         # Alternate the row fill colours
                 core = list( fg_params=list( hjust = 0.5 , x = 0.5 ),   # 1: right 0:left 0.5:center
                             bg_params=list( fill=c( "green", "pink" ) ) ),

         # Change column header to white text and red background
                 colhead = list( fg_params=list( col="white" ),
                                bg_params=list( fill="red" ) ) )
```

14.2.5 Exercises
----------------

-   In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?

-   The difference between `paste()` and `paste0()` is whether to use `sep`
    -   paste0(..., collapse) is equivalent to paste(..., sep = "", collapse), slightly more efficiently.
    -   The `usage` of this two function are as following:

``` r
# paste (..., sep = " ", collapse = NULL)
# paste0(..., collapse = NULL)
paste(1:10 , sep = ",")
paste0(1:10)
```

    ##  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
    ##  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"

-   They are equal to `str_c`

``` r
str_c(1:10, sep = ",")
```

    ##  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"

-   `paste()` and `paste0()` will return NA as a character while \``str_c` will only return NA as it cannot connect NA value.

``` r
paste( 1:12, c("st", "nd", NA, rep("th", 9)) ) 
```

    ##  [1] "1 st"  "2 nd"  "3 NA"  "4 th"  "5 th"  "6 th"  "7 th"  "8 th" 
    ##  [9] "9 th"  "10 th" "11 th" "12 th"

``` r
paste0( 1:12, c("st", "nd", NA, rep("th", 9)) )
```

    ##  [1] "1st"  "2nd"  "3NA"  "4th"  "5th"  "6th"  "7th"  "8th"  "9th"  "10th"
    ## [11] "11th" "12th"

``` r
str_c( 1:12, c("st", "nd", NA, rep("th", 9)) )
```

    ##  [1] "1st"  "2nd"  NA     "4th"  "5th"  "6th"  "7th"  "8th"  "9th"  "10th"
    ## [11] "11th" "12th"

-   In your own words, describe the difference between the sep and collapse arguments to str\_c(). sep is to insert string between the input vectors while collaspe is to convert the input vectors into a single string

``` r
str_c(letters, collapse = ",")
```

    ## [1] "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z"

``` r
str_c(letters, LETTERS, collapse = ",")
```

    ## [1] "aA,bB,cC,dD,eE,fF,gG,hH,iI,jJ,kK,lL,mM,nN,oO,pP,qQ,rR,sS,tT,uU,vV,wW,xX,yY,zZ"

``` r
str_c(letters, sep = ",")
```

    ##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
    ## [18] "r" "s" "t" "u" "v" "w" "x" "y" "z"

``` r
str_c(letters, LETTERS, sep = ",")
```

    ##  [1] "a,A" "b,B" "c,C" "d,D" "e,E" "f,F" "g,G" "h,H" "i,I" "j,J" "k,K"
    ## [12] "l,L" "m,M" "n,N" "o,O" "p,P" "q,Q" "r,R" "s,S" "t,T" "u,U" "v,V"
    ## [23] "w,W" "x,X" "y,Y" "z,Z"

-   Use str\_length() and str\_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?

``` r
even_string <- "STAT"
odd_string <- "547"

return_middle <- function( string)
{
  if ( str_length(string) %% 2 == 0)
    index = str_length( string )/2
  else
    index = ceiling(str_length( string )/2)
  
  return( index )
}

index_even <- return_middle( even_string )
index_odd <- return_middle( odd_string )
str_sub( even_string, index_even, index_even )
```

    ## [1] "T"

``` r
str_sub( odd_string, index_odd, index_odd )
```

    ## [1] "4"

-   What does str\_wrap() do? When might you want to use it?
-   `str_wrap()`: Controls the string output format. It can wrap a long paragraph into multiple lines.

``` r
thanks_path <- file.path(R.home("doc"), "THANKS")
thanks <- str_c(readLines(thanks_path), collapse = "\n")
( thanks <- word(thanks, 1, 3, fixed("\n\n")) )
```

    ## [1] "R would not be what it is today without the invaluable help of these\npeople outside of the R core team, who contributed by donating code, bug\nfixes and documentation:\n\nValerio Aimale, Suharto Anggono, Thomas Baier, Henrik Bengtsson, Roger Bivand,\nBen Bolker, David Brahm, G\"oran Brostr\"om, Patrick Burns, Vince Carey,\nSaikat DebRoy, Matt Dowle, Brian D'Urso, Lyndon Drake, Dirk Eddelbuettel,\nClaus Ekstrom, Sebastian Fischmeister, John Fox, Paul Gilbert,\nYu Gong, Gabor Grothendieck, Frank E Harrell Jr, Peter M. Haverty,\nTorsten Hothorn, Robert King, Kjetil Kjernsmo, Roger Koenker, Philippe Lambert,\nJan de Leeuw, Jim Lindsey, Patrick Lindsey, Catherine Loader,\nGordon Maclean, John Maindonald, David Meyer, Ei-ji Nakama,\nJens Oehlschaegel, Steve Oncley, Richard O'Keefe, Hubert Palme,\nRoger D. Peng, Jose' C. Pinheiro, Tony Plate, Anthony Rossini,\nJonathan Rougier, Petr Savicky, Guenther Sawitzki, Marc Schwartz,\nArun Srinivasan, Detlef Steuer, Bill Simpson, Gordon Smyth, Adrian Trapletti,\nTerry Therneau, Rolf Turner, Bill Venables, Gregory R. Warnes,\nAndreas Weingessel, Morten Welinder, James Wettenhall, Simon Wood, and\nAchim Zeileis.\n\nOthers have written code that has been adopted by R and is\nacknowledged in the code files, including"

``` r
cat(str_wrap(thanks), "\n")
```

    ## R would not be what it is today without the invaluable help of these people
    ## outside of the R core team, who contributed by donating code, bug fixes and
    ## documentation: Valerio Aimale, Suharto Anggono, Thomas Baier, Henrik Bengtsson,
    ## Roger Bivand, Ben Bolker, David Brahm, G"oran Brostr"om, Patrick Burns, Vince
    ## Carey, Saikat DebRoy, Matt Dowle, Brian D'Urso, Lyndon Drake, Dirk Eddelbuettel,
    ## Claus Ekstrom, Sebastian Fischmeister, John Fox, Paul Gilbert, Yu Gong, Gabor
    ## Grothendieck, Frank E Harrell Jr, Peter M. Haverty, Torsten Hothorn, Robert
    ## King, Kjetil Kjernsmo, Roger Koenker, Philippe Lambert, Jan de Leeuw, Jim
    ## Lindsey, Patrick Lindsey, Catherine Loader, Gordon Maclean, John Maindonald,
    ## David Meyer, Ei-ji Nakama, Jens Oehlschaegel, Steve Oncley, Richard O'Keefe,
    ## Hubert Palme, Roger D. Peng, Jose' C. Pinheiro, Tony Plate, Anthony Rossini,
    ## Jonathan Rougier, Petr Savicky, Guenther Sawitzki, Marc Schwartz, Arun
    ## Srinivasan, Detlef Steuer, Bill Simpson, Gordon Smyth, Adrian Trapletti, Terry
    ## Therneau, Rolf Turner, Bill Venables, Gregory R. Warnes, Andreas Weingessel,
    ## Morten Welinder, James Wettenhall, Simon Wood, and Achim Zeileis. Others have
    ## written code that has been adopted by R and is acknowledged in the code files,
    ## including

-   What does str\_trim() do? What’s the opposite of str\_trim()?
-   `str_trim` removes whitespace from start and end of string

``` r
str.with.sapce <- "  String with trailing and leading white space\t"
(str.without.space <- str_trim( str.with.sapce ))
```

    ## [1] "String with trailing and leading white space"

-   The oppsite of it is `str_pad`, width is the minimum width of padded strings.

``` r
str_pad("String with trailing and leading white space",
        side = "left",
        width = str_length(str.without.space) + 3,
        pad = " ")
```

    ## [1] "   String with trailing and leading white space"

-   Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.

``` r
vrctor_to_string <- function(vector)
{
  if( length(vector) == 0 )  ## if the length of vector is 0 
    return("This is an empty vector")
  
  else if( length(vector) == 1 )  ## if the length of vector is 1
    return( vector[1] )
  else
  {
    last_element <- tail( vector, n = 1 )
    other_element <- setdiff( vector, last_element )
    part1 <- str_c( other_element, collapse = "," )

    return(paste( part1, last_element, sep = " and ") )
    
  }
  
}



test1 <- c()
test2 <- c( "a" )
test3 <- c( "a", "b" )
test4 <- c( "a", "b", "c")

vrctor_to_string( test1 )
```

    ## [1] "This is an empty vector"

``` r
vrctor_to_string( test2 )
```

    ## [1] "a"

``` r
vrctor_to_string( test3 )
```

    ## [1] "a and b"

``` r
vrctor_to_string( test4 )
```

    ## [1] "a,b and c"

14.3.1.1 Exercises
------------------

-   Explain why each of these strings don’t match a : "", "\\", "\\".

`\` is used as an escape character in regular expressions, how do you match a literal `\`? Well you need to escape it, creating the regular expression \\. To create that regular expression, you need to use a string, which also needs to escape `\`. That means to match a literal `\` you need to write `"\\\\"` — you need four backslashes to match one!

-   How would you match the sequence `"'\`?

`"`,`'` and `\` need to be escaped and therefore, we should use`\"\'\\` to match it.

``` r
test_string <- "\"\'\\"
writeLines ( test_string )
```

    ## "'\

-   What patterns will the regular expression ...... match? How would you represent it as a string?

``` r
test_string1 <- "\\..\\..\\.."
writeLines( test_string1 )
```

    ## \..\..\..

14.3.2.1 Exercises
------------------

-   How would you match the literal string "$^$"?

``` r
test_string2 <- '$^$'
writeLines( test_string2 )
```

    ## $^$

-   Given the corpus of common words in stringr::words, create regular expressions that find all words that:
-   Start with “y”.

``` r
# str_view( stringr::words, "^y", match = TRUE)
```

-   End with “x”

``` r
# str_view( stringr::words, "x$", match = TRUE )
```

-   Are exactly three letters long. (Don’t cheat by using str\_length()!)

``` r
# str_view( stringr::words, "^...$", match = TRUE )
```

-   Have seven letters or more.

``` r
# str_view( stringr::words, ".......+", match = TRUE )
```

Since this list is long, you might want to use the match argument to str\_view() to show only the matching or non-matching words.

14.3.3.1 Exercises
------------------

-   Create regular expressions to find all words that:
-   Start with a vowel.

``` r
# str_view( stringr::words, "^[aeiou]", match = TRUE )
```

-   That only contain consonants. (Hint: thinking about matching “not”-vowels.)

``` r
# str_view( stringr::words, "[aeiou]", match = FALSE)
```

-   End with ed, but not with eed.

``` r
# str_view( stringr::words, "[^e]ed$", match = TRUE )
```

-   End with ing or ise.

``` r
# str_view( stringr::words, "(ing)|(ise)$", match = TRUE )
```

-   Empirically verify the rule “i before e except after c”.

``` r
# str_view( stringr::words, "[^c]ie", match = TRUE )
```

-   Is “q” always followed by a “u”?

``` r
# str_view(stringr::words, "q[^u]", match = TRUE)
# str_view(stringr::words, "qu", match = TRUE)
```

-   Write a regular expression that matches a word if it’s probably written in British English, not American English.

``` r
x <- c("centre", "center", "metre","meter")
# str_view(x, "re$", match = TRUE)
```

-   Create a regular expression that will match telephone numbers as commonly written in your country. In my country, telephone number is usually begins with 186, 189, 158,136...

``` r
telephone_number <- c("123456****", "186402****", "158408****", "1794***")
# str_view(telephone_number, "^(186)|(189)|(158)|(136)", match = TRUE)
```

14.3.4.1 Exercises
------------------

-   Describe the equivalents of ?, +, \* in {m,n} form.
-   ?: 0 or 1 {0,1}
-   +: 1 or more {1,}
-   \*: 0 or more {0,}
-   Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)
-   ^.\*$ : any string
-   "\\{.+\\}": at least one character that is in {}, e.g.{ab}
-   --: 4 digits-two digits-two digits
-   "\\\\{4}" 4`\`
-   Create regular expressions to find all words that:
-   Start with three consonants.

``` r
# str_view(stringr::words, "^[^aeiou][^aeiou][^aeiou]", match = TRUE)
```

-   Have three or more vowels in a row.

``` r
# str_view(stringr::words, "[aeiou]{3,}", match = TRUE)
```

-   Have two or more vowel-consonant pairs in a row.

``` r
# str_view(stringr::words, "([aeiou][^aeiou]){2}+", match = TRUE)
```

-   Solve the beginner regexp crosswords at <https://regexcrossword.com/challenges/beginner>.

![](https://github.com/STAT545-UBC-students/hw06-Sukeysun/blob/master/Data_wrangling_wrap_up_files/puzzles.jpg)

14.3.5.1 Exercises
------------------

-   Describe, in words, what these expressions will match:
-   (.) : anything repeated 3 times e.g. aaa
-   "(.)(.)\\2\\1" : "item1 item2 item2 item1" e.g. p`eppe`r
-   (..): "item1 item2 item1 item2" e.g.abab
-   "(.).\\1.\\1" "item1 any item item1 any item item1" e.g. b`anana`
-   "(.)(.)(.).\*\\3\\2\\1" "item1 item2 item3 any item (0 or more) item item3 item2 item2" e.g. abcdcba Construct regular expressions to match words that:
-   Start and end with the same character.

``` r
# str_view(stringr::words, "^(.).*\\1$", match = TRUE)
```

-   Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)

``` r
# str_view(stringr::words, "(.)(.).*\\1\\2", match = TRUE)
```

-   Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)

``` r
# str_view(stringr::words, "(.).*\\1.*\\1", match = TRUE)
```

14.4.2 Exercises
----------------

-   For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str\_detect() calls.
-   Find all words that start or end with x.

``` r
# 1
# str_view(stringr::words, "(^x)|(x$)", match = TRUE)
# 2
stringr::words[str_detect(stringr::words, "(^x)|(x$)") == TRUE]
```

    ## [1] "box" "sex" "six" "tax"

-   Find all words that start with a vowel and end with a consonant.

``` r
# 1
# str_view(stringr::words, "(^[aeiou])|([^aeiou]$)", match = TRUE)
# 2
head( stringr::words[str_detect(stringr::words, "(^[aeiou])|([^aeiou]$)") == TRUE] )
```

    ## [1] "a"        "able"     "about"    "absolute" "accept"   "account"

-   Are there any words that contain at least one of each different vowel?

``` r
hasA = str_detect(sentences, "a")
hasE = str_detect(sentences, "e")
hasO = str_detect(sentences, "o")
hasU = str_detect(sentences, "u")
hasI = str_detect(sentences, "i")
hasY = str_detect(sentences, "y")
output.with.na <- fruit[hasA & hasE & hasO & hasU & hasI & hasY]
(output.without.na <- output.with.na[!is.na(output.with.na)])
```

    ## [1] "date"        "fig"         "huckleberry" "mango"       "nectarine"  
    ## [6] "redcurrant"

-   What word has the highest number of vowels? What word has the highest proportion of vowels? (Hint: what is the denominator?)

``` r
counts = str_count(words, "[aeiou]")
lengths = str_length(words)
result <- tibble(words = words, counts = counts, lengths = lengths)

result1 <- result %>%
  arrange(desc(counts))

result2 <- result %>%
  mutate(prop = counts / lengths) %>%
  arrange(desc(prop))

grid.arrange(  top=textGrob( "vowels",
                            gp=gpar( fontsize = 22,font = 8 ) ),
             tableGrob( head( result1, 10 ),
             theme=myt,
             rows=NULL ),
             tableGrob( head( result2, 10 ),
             theme=myt,
             rows=NULL ),
             nrow = 1)
```

![](Data_wrangling_wrap_up_files/figure-markdown_github/vowels-1.png)

14.4.3.1 Exercises
------------------

-   In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a colour. Modify the regex to fix the problem.

``` r
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colours_mod <- c("[^a-zA-Z0-9]red[^a-zA-Z0-9]", 
                 "[^a-zA-Z0-9]orange[^a-zA-Z0-9]", 
                 "[^a-zA-Z0-9]yellow[^a-zA-Z0-9]",
                 "[^a-zA-Z0-9]green[^a-zA-Z0-9]", 
                 "[^a-zA-Z0-9]blue[^a-zA-Z0-9]", 
                 "[^a-zA-Z0-9]purple[^a-zA-Z0-9]")

colour_match_mod <- str_c(colours_mod, collapse = "|")
colour_match <- str_c(colours, collapse = "|")

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract_all(has_colour, colour_match)

has_colour_mod <- str_subset(sentences, colour_match_mod)
matches_mod <- str_extract_all(has_colour, colour_match_mod)

has_colour[!(has_colour %in% has_colour_mod)]
```

    ##  [1] "The colt reared and threw the tall rider."         
    ##  [2] "The wide road shimmered in the hot sun."           
    ##  [3] "See the cat glaring at the scared mouse."          
    ##  [4] "He ordered peach pie with ice cream."              
    ##  [5] "Pure bred poodles have curls."                     
    ##  [6] "Mud was spattered on the front of his white shirt."
    ##  [7] "Torn scraps littered the stone floor."             
    ##  [8] "The doctor cured him with these pills."            
    ##  [9] "The new girl was fired today at noon."             
    ## [10] "The third act was dull and tired the players."     
    ## [11] "Lire wires should be kept covered."                
    ## [12] "The wreck occurred by the bank on Main Street."    
    ## [13] "The prince ordered his head chopped off."          
    ## [14] "Nine men were hired to dig the ruins."             
    ## [15] "The flint sputtered and lit a pine torch."         
    ## [16] "The old pan was covered with hard fudge."          
    ## [17] "The store walls were lined with colored frocks."   
    ## [18] "The clan gathered on each dull night."             
    ## [19] "Smoke poured out of every crack."                  
    ## [20] "Serve the hot rum to the tired heroes."            
    ## [21] "He offered proof in the form of a lsrge chart."    
    ## [22] "The sip of tea revives his tired friend."          
    ## [23] "The door was barred, locked, and bolted as well."  
    ## [24] "A thick coat of black paint covered all."          
    ## [25] "He put his last cartridge into the gun and fired." 
    ## [26] "The ram scared the school children off."           
    ## [27] "Dimes showered down from all sides."               
    ## [28] "The hail pattered on the burnt brown grass."

``` r
has_colour[has_colour %in% has_colour_mod]
```

    ##  [1] "Glue the sheet to the dark blue background."     
    ##  [2] "Two blue fish swam in the tank."                 
    ##  [3] "A wisp of cloud hung in the blue air."           
    ##  [4] "Leaves turn brown and yellow in the fall."       
    ##  [5] "The spot on the blotter was made by green ink."  
    ##  [6] "The sofa cushion is red and of light weight."    
    ##  [7] "The sky that morning was clear and bright blue." 
    ##  [8] "A blue crane is a tall wading bird."             
    ##  [9] "It is hard to erase blue or red ink."            
    ## [10] "The lamp shone with a steady green flame."       
    ## [11] "The box is held by a bright red snapper."        
    ## [12] "The houses are built of red clay bricks."        
    ## [13] "The red tape bound the smuggled food."           
    ## [14] "Hedge apples may stain your hands green."        
    ## [15] "The plant grew large and green in the window."   
    ## [16] "The purple tie was ten years old."               
    ## [17] "Bathe and relax in the cool green grass."        
    ## [18] "The lake sparkled in the red hot sun."           
    ## [19] "Mark the spot with a sign painted red."          
    ## [20] "The couch cover and hall drapes were blue."      
    ## [21] "A man in a blue sweater sat at the desk."        
    ## [22] "The small red neon lamp went out."               
    ## [23] "Paint the sockets in the wall dull green."       
    ## [24] "Wake and rise, and step into the green outdoors."
    ## [25] "The green light in the brown box flickered."     
    ## [26] "Tear a thin sheet from the yellow pad."          
    ## [27] "The sky in the west is tinged with orange red."  
    ## [28] "The red paper brightened the dim stage."         
    ## [29] "The big red apple fell to the ground."

-   From the Harvard sentences data, extract:
-   The first word from each sentence.

``` r
( frist.word <- str_extract( sentences, "[^ ]+" ) %>% 
  head(10) )
```

    ##  [1] "The"   "Glue"  "It's"  "These" "Rice"  "The"   "The"   "The"  
    ##  [9] "Four"  "Large"

-   All words ending in ing.

``` r
has_ing <- str_subset(sentences, "[^ ]+ing[ .]")
str_extract(has_ing, "[^ ]+ing[ .]") %>% str_sub(1, -2) %>% head(10)
```

    ##  [1] "spring"  "evening" "morning" "winding" "living"  "king"    "Adding" 
    ##  [8] "making"  "raging"  "playing"

-   All plurals.

``` r
is_noun <- str_subset(sentences, "(a|the) [^ \\.]+")
nouns <- str_extract(is_noun, "(a|the) [^ \\.]+")
is_plural <- str_subset(nouns, "(a|the) [^ \\.\\']+(s|\\')$")
str_extract(is_plural, "(a|the) [^ \\.\\']+(s|\\')$")
```

    ##  [1] "a helps"       "the pants"     "the soldiers"  "the logs"     
    ##  [5] "the less"      "the eyes"      "the results"   "the players"  
    ##  [9] "the islands"   "a compass"     "the hot-cross" "the ruins"    
    ## [13] "the crackers"  "the records"   "a is"          "the woods"    
    ## [17] "the cuts"      "the ashes"     "the limits"    "a revives"    
    ## [21] "the mails"     "the edges"     "the sockets"   "a mass"       
    ## [25] "the boys"      "the kits"      "the contents"

14.4.4.1 Exercises
------------------

-   Find all words that come after a “number” like “one”, “two”, “three” etc. Pull out both the number and the word.
-   Find all contractions. Separate out the pieces before and after the apostrophe.

``` r
## 1
numbers = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
numbers_match = str_c("(", str_c(numbers, collapse = "|"), ") [^ .]+")

has_number <- str_subset(sentences, numbers_match)
str_extract(has_number, numbers_match) %>% head(10)
```

    ##  [1] "ten served"  "one over"    "seven books" "two met"     "two factors"
    ##  [6] "one and"     "three lists" "seven is"    "two when"    "one floor"

``` r
## 2
has_contraction = str_subset(sentences, "[^ ]+\\'[^ .]+")
str_extract(has_contraction, "[^ ]+\\'[^ .]+")
```

    ##  [1] "It's"       "man's"      "don't"      "store's"    "workmen's" 
    ##  [6] "Let's"      "sun's"      "child's"    "king's"     "It's"      
    ## [11] "don't"      "queen's"    "don't"      "pirate's"   "neighbor's"

14.4.5.1 Exercises
------------------

-   Replace all forward slashes in a string with backslashes.

``` r
str = "STAT/547/HW06"
writeLines(str)
```

    ## STAT/547/HW06

``` r
writeLines(str_replace_all(str, "/", "\\\\"))
```

    ## STAT\547\HW06

-   Implement a simple version of str\_to\_lower() using replace\_all().

``` r
str_replace_all(LETTERS, c("A" = "a", "B" = "b", "C" = "c"))
```

    ##  [1] "a" "b" "c" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q"
    ## [18] "R" "S" "T" "U" "V" "W" "X" "Y" "Z"

-   Switch the first and last letters in words. Which of those strings are still words?

``` r
switched <- str_replace(words, "^(.)(.*)(.)$", "\\3\\2\\1")
words[words %in% switched] %>%  head()
```

    ## [1] "a"       "america" "area"    "dad"     "dead"    "deal"

14.4.6.1 Exercises
------------------

-   Split up a string like "apples, pears, and bananas" into individual components.

``` r
str_split("apples, pears, and bananas", ", and |, ")
```

    ## [[1]]
    ## [1] "apples"  "pears"   "bananas"

-   Why is it better to split up by boundary("word") than " "? boundary(word) deals with whitespaces (any and any number)

-   What does splitting with an empty string ("") do? Experiment, and then read the documentation. Split by character (equivalent to boundary("character"))

14.5.1 Exercises
----------------

How would you find all strings containing  with regex() vs. with fixed()?

``` r
strings = c("STAT\\", "547\\", "HW06")
# str_view(strings, regex("\\\\"))
# str_view(strings, fixed("\\"))
```

What are the five most common words in sentences?

``` r
words <- str_split(sentences, boundary("word"), simplify = T) %>% 
  as.vector()
  
result <- tibble(words = str_to_lower(words)) %>%
  group_by(words) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(6) 

grid.arrange(  top=textGrob( "five most common words in sentences",
                            gp=gpar( fontsize = 22,font = 8 ) ),
             tableGrob( result,
             theme=myt,
             rows=NULL ) )
```

![](Data_wrangling_wrap_up_files/figure-markdown_github/five%20most%20common%20words%20in%20sentences-1.png)

14.7.1 Exercises
----------------

-   Find the stringi functions that:
-   Count the number of words. `stri_count_words`
-   Find duplicated strings. `stri_duplicated`
-   Generate random text. `stri_rand_strings (stri_rand_lipsum for Lorem ipsum)` How do you control the language that stri\_sort() uses for sorting? By the locale argument.

Writing functions
=================

Write one (or more) functions that do something useful to pieces of the Gapminder or Singer data. It is logical to think about computing on the mini-data frames corresponding to the data for each specific country, location, year, band, album, … This would pair well with the prompt below about working with a nested data frame, as you could apply your function there.

I would like to create a function to calculate the sum of the gdp of a specific continent in all years. The input of the funcion is: \* Continent: the specific continent which I want to compute, the default continent is Oceania

``` r
explore_gdp <- 
  function( Continent = "Oceania")
  {
    gapminder %>% 
      ## choose the continent and the year you are interested in
      filter (continent == Continent) %>% 
      group_by( year ) %>% 
      ## compute the gdp of each country in the given year of the input continent
      mutate ( gdp = gdpPercap * pop ) %>% 
      summarize( sumOfGdp = sum( gdp ) ) %>% 
      mutate ( continent = Continent)
      # str_c("The total gdp of", Continent, "in", Year, "is", totalgdp, "dollars", sep = " " )
  }
```

This function should return the sum of gdp of a specific continent in a given year. Let's check if it works.

``` r
gdp_Oceania <- explore_gdp( Continent = "Oceania")

grid.arrange(  top=textGrob( "Total Gdp of Oceania",
                            gp=gpar( fontsize = 22,font = 8 ) ),
             tableGrob( head( gdp_Oceania, 10 ),
             theme=myt,
             rows=NULL ) )
```

![](Data_wrangling_wrap_up_files/figure-markdown_github/total%20Gdp%20of%20Oceania-1.png) As shown in the table above, the total gdp of Oceania in 1957 is 133653656026.872 dollars

The `explore_gdp` function could only give me the total gdp of a given continent in a given year that are shown in the gapminder dataset. However, I also want to know the predicted data of all continents during some years that are not in gapminder dataset. First, let's check the relationship between year and total gdp in all continents

``` r
gdp_gapminder <- gapminder %>% 
  ## add a new column called totalgdp
  mutate( totalgdp = gdpPercap*pop )
  
gdp_gapminder %>% 
  ggplot()+
  facet_wrap(~continent)+
  geom_point( aes( year,log( totalgdp), colour = continent ) ) +
  theme( strip.text.x = element_text( size = 10 , angle = 30),
          strip.text.y = element_text( size = 10, face = "bold" ),
          strip.background = element_rect( colour = "red", fill = "#CCCCFF" )) +
  labs( title = "  total Gdp for each continent  ") +
 theme_bw()+
   theme(
         axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 10 ),
         axis.title = element_text( size = 14),
         plot.background = element_rect( fill = "white", colour = "black" ),
         plot.title = element_text( size = 15,hjust = 0.5),
         panel.border = element_rect(linetype = "dashed", fill = NA)
         )
```

![](Data_wrangling_wrap_up_files/figure-markdown_github/check%20the%20relationship%20between%20year%20and%20total%20gdp%20in%20all%20continents-1.png) As we can see in the graph above, the log of total gdp increases with year and there seems to be a linear regression between them. Now let's make a prediction function

``` r
predict_gdp <- 
  function( Year = c(1952,1953,1954, 1955, 1956) )
  {
    newyear <- tibble(year = Year)
    ## call the explore_gdp I created before to get the total gdp of a given continent
    continents = unique(gapminder$continent)
    
    #gdp <- data.frame()
    predict_data <- data.frame()
    for (con in continents)
    {
      
      temp_data <- explore_gdp( Continent = con)
      
      ## use robust regression to train data
      temp_lm <- lmrob( log(sumOfGdp) ~ year, data = temp_data)
      
      ## create a predicted dataframe
      temp_predict <- tibble(
        continent = con,
        year = Year,
        predict.log.gdp = predict( temp_lm, newdata = newyear)
        
      )
      
      ## paste the predicted data row by row
      predict_data <- bind_rows(predict_data, temp_predict)
    }
    
    return (predict_data)
  }



newyear_Oceania <- predict_gdp( Year = c(1950,1960,1970,1980))

plt_Oceania <- newyear_Oceania %>% 
  ggplot()+
  
  ## plot the data separately according to continent.
  facet_wrap(~continent)+
  geom_point( aes( year,predict.log.gdp, colour = continent ) ) +
  theme( strip.text.x = element_text( size = 10 , angle = 30),
          strip.text.y = element_text( size = 10, face = "bold" ),
          strip.background = element_rect( colour = "red", fill = "#CCCCFF" )) +
  labs( title = "  total Gdp for each continent  ") +
 theme_bw()+
   theme(
         axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 10 ),
         axis.title = element_text( size = 10),
         plot.background = element_rect( fill = "white", colour = "black" ),
         plot.title = element_text( size = 15,hjust = 0.5),
         panel.border = element_rect(linetype = "dashed", fill = NA)
         )


grid.arrange( top =textGrob( " Predicted total Gdp of given years ",
                            gp=gpar( fontsize = 20,font = 8 ) ),
             tableGrob( head( newyear_Oceania,10 ),
             theme=myt,
             rows=NULL ),
             plt_Oceania,
             nrow =1)
```

![](Data_wrangling_wrap_up_files/figure-markdown_github/Predicted%20total%20Gdp%20of%20given%20years-1.png)
