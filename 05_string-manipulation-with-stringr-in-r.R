
#' Para praticar, você vai digitar algumas linhas de Alice's Adventures in 
#' Wonderland , de Lewis Carroll . 

# Define line1
line1 <- "The table was a large one, but the three were all crowded together at one corner of it:"
  
# Define line2
line2 <- '"No room! No room!" they cried out when they saw Alice coming.'

# Define line3
line3 <- "\"There's plenty of room!\" said Alice indignantly, and she sat down in a large arm-chair at one end of the table."

# Putting lines in a vector
lines <- c(line1, line2, line3)

# Print lines
lines

# Use writeLines() on lines
writeLines(lines)

# Write lines with a space separator
writeLines(lines, sep = " ")

# Use writeLines() on the string "hello\n\U1F30D"
writeLines("hello\n\U1F30D")

#' Uma sequência em uma string que começa com a \é chamada de sequência de escape 
#' e nos permite incluir caracteres especiais em nossas strings. Você viu uma 
#' sequência de escape no primeiro exercício: \"é usado para denotar aspas duplas.
#' 
#' 
#' Unicode é um padrão para representar caracteres que podem não estar em seu 
#' teclado. Cada caractere disponível possui um ponto de código Unicode: um número 
#' que o identifica exclusivamente. Esses pontos de código são geralmente escritos 
#' em notação hexadecimal, ou seja, usando a base 16 e os dígitos 0-9 e AF. 
#' Você pode encontrar o ponto de código para um caractere específico pesquisando
#'  um gráfico de código . Se você precisar apenas de quatro dígitos para o 
#'  codepoint, uma sequência de escape alternativa é \u.
#'  
#'  https://www.unicode.org/charts/
#'  
#'  Quando R se depara com a \, ele assume que você está iniciando uma fuga, 
#'  então se você realmente precisar de uma barra invertida em sua string, 
#'  precisará da sequência \\.
#'  
#'  

# Should display: To have a \ you need \\
writeLines("To have a \\ you need \\\\")

# Should display: 
# This is a really 
# really really 
# long string
writeLines("This is a really \nreally really \nlong string")

# Use writeLines() with 
# "\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e"
writeLines("\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e")

# Some vectors of numbers
percent_change  <- c(4, -1.91, 3.00, -5.002)
income <-  c(72.19, 1030.18, 10291.93, 1189192.18)
p_values <- c(0.12, 0.98, 0.0000191, 0.00000000002)

# Format c(0.0011, 0.011, 1) with digits = 1
format(c(0.0011, 0.011, 1),digits=1)

# Format c(1.0011, 2.011, 1) with digits = 1
format(c(1.0011, 2.011, 1),digits=1)

# Format percent_change to one place after the decimal point
format(percent_change,digits=2)

# Format income to whole numbers
format(income,digits=2)

# Format p_values in fixed format
format(p_values,scientific=F)




formatted_income <- format(income, digits = 2)

# Print formatted_income
formatted_income

# Call writeLines() on the formatted income
writeLines(formatted_income)

# Define trimmed_income
trimmed_income = format(formatted_income, trim = T)

# Call writeLines() on the trimmed_income
writeLines(trimmed_income)

# Define pretty_income
pretty_income = format(income, 
                       digits = 2, 
                       big.mark = ",", 
                       # big.interval = 3, 
                       scientific = FALSE
                       )

# Call writeLines() on the pretty_income
writeLines(pretty_income)



# From the format() exercise
x <- c(0.0011, 0.011, 1)
y <- c(1.0011, 2.011, 1)

# formatC() on x with format = "f", digits = 1
formatC(x,format = "f", digits = 1)

# formatC() on y with format = "f", digits = 1
formatC(y,format = "f", digits = 1)

# Format percent_change to one place after the decimal point
formatC(percent_change,format = "f", digits = 1)

# percent_change with flag = "+"
formatC(percent_change,format = "f", digits = 1,flag = "+")

# Format p_values using format = "g" and digits = 2
formatC(p_values,format = "g", digits = 2)



# Add $ to pretty_income
paste("$", pretty_income, sep = "")

# Add % to pretty_percent
paste(pretty_percent,"%",sep = "")

# Create vector with elements like 2010: +4.0%`
year_percent <- paste(years,": ",pretty_percent,"%",sep = "")

# Collapse all years into single string
paste(year_percent,collapse=",")



# Define the names vector
income_names <- c("Year 0", "Year 1", "Year 2", "Project Lifetime")

# Create pretty_income
pretty_income <- format(income,digits = 2,big.mark = ",")

# Create dollar_income
dollar_income <- paste("$",pretty_income,sep="")

# Create formatted_names
formatted_names <- format(income_names,justify = "right")

# Create rows
rows = paste(formatted_names,dollar_income,sep="   ")

# Write rows
writeLines(rows)




# Randomly sample 3 toppings
my_toppings <- sample(toppings, size = 3)

# Print my_toppings
my_toppings

# Paste "and " to last element: my_toppings_and
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep = "")

# Collapse with comma space: these_toppings
these_toppings <- paste(my_toppings_and, collapse = ", ")

# Add rest of sentence: my_order
my_order <- paste("I want to order a pizza with ", these_toppings, ".", sep = "")

# Order pizza with writeLines()
writeLines(my_order)



library(stringr)

my_toppings <- c("cheese", NA, NA)
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep = "")

# Print my_toppings_and
my_toppings_and

# Use str_c() instead of paste(): my_toppings_str
my_toppings_str <- str_c(c("", "", "and "), my_toppings)

# Print my_toppings_str
my_toppings_str

# paste() my_toppings_and with collapse = ", "
paste(my_toppings_and,collapse = ", ")

# str_c() my_toppings_str with collapse = ", "
str_c(my_toppings_str,collapse = ", ")





# -------------------------------------------------------------------------


library(stringr)
library(babynames)
library(dplyr)



# Extracting vectors for boys' and girls' names
babynames_2014 <- filter(babynames, year == 2014)
boy_names <- filter(babynames_2014, sex == "M")$name
girl_names <- filter(babynames_2014, sex == "F")$name

# Take a look at a few boy_names
head(boy_names)

# Find the length of all boy_names
boy_length <- str_length(boy_names)

# Take a look at a few lengths
head(boy_length)

# Find the length of all girl_names
girl_length <- str_length(girl_names)

# Find the difference in mean length
mean(girl_length) - mean(boy_length)

# Confirm str_length() works with factors
head(str_length(factor(boy_names)))



# Extract first letter from boy_names
boy_first_letter <- str_sub(boy_names,1,1)

# Tabulate occurrences of boy_first_letter
table(boy_first_letter)

# Extract the last letter in boy_names, then tabulate
boy_last_letter <- str_sub(boy_names,-1,-1)
table(boy_last_letter)

# Extract the first letter in girl_names, then tabulate
girl_first_letter <- str_sub(girl_names,1,1)
table(girl_first_letter)

# Extract the last letter in girl_names, then tabulate
girl_last_letter <- str_sub(girl_names,-1,-1)
table(girl_last_letter)




# Look for pattern "zz" in boy_names
contains_zz <- str_detect(boy_names,"zz")

# Examine str() of contains_zz
str(contains_zz)

# How many names contain "zz"?
sum(contains_zz)

# Which names contain "zz"?
boy_names[contains_zz]

# Which rows in boy_df have names that contain "zz"?
filter(boy_df,str_detect(name,"zz"))




# Find boy_names that contain "zz"
str_subset(boy_names, fixed("zz"))

# Find girl_names that contain "zz"
str_subset(girl_names, fixed("zz"))

# Find girl_names that contain "U"
starts_U <- str_subset(girl_names, fixed("U"))
starts_U

# Find girl_names that contain "U" and "z"
str_subset(starts_U, fixed("z"))




# Count occurrences of "a" in girl_names
number_as <- str_count(girl_names,"a")

# Count occurrences of "A" in girl_names
number_As <- str_count(girl_names,"A")

# Histograms of number_as and number_As
hist(number_as)
hist(number_As)  

# Find total "a" + "A"
total_as <- number_as + number_As

# girl_names with more than 4 a's
girl_names[total_as > 4]




# Some date data
date_ranges <- c("23.01.2017 - 29.01.2017", "30.01.2017 - 06.02.2017")

# Split dates using " - "
split_dates <- str_split(date_ranges," - ")
split_dates

# Split dates with n and simplify specified
split_dates_n <- str_split(date_ranges, fixed(" - "), n = 2, simplify = TRUE)
split_dates_n


# Subset split_dates_n into start_dates and end_dates
start_dates <- split_dates_n[,1]

# Split start_dates into day, month and year pieces
str_split(start_dates, fixed("."), n = 3, simplify = TRUE)




both_names <- c("Box, George", "Cox, David")

# Split both_names into first_names and last_names
both_names_split <- str_split(both_names, fixed(", "), n = 2, simplify = TRUE)

# Get first names
first_names <- both_names_split[, 2]

# Get last names
last_names <- both_names_split[, 1]




# Como exemplo, você fará algumas estatísticas de texto simples em suas linhas 
# de Alice no País das Maravilhas do Capítulo 1. Seu objetivo será calcular 
# quantas palavras há em cada linha e o comprimento médio das palavras em cada 
# linha.

# Split lines into words
words <- str_split(lines, " ")

# Number of words per line
lapply(words, length)

# Number of characters in each word
word_lengths <- lapply(words, str_length)

# Average word length per line
lapply(word_lengths, mean)




# Some IDs
ids <- c("ID#: 192", "ID#: 118", "ID#: 001")

# Replace "ID#: " with ""
id_nums <- str_replace(ids,"ID#: ","")

# Turn id_nums into numbers
id_ints <- as.numeric(id_nums)

# Some (fake) phone numbers
phone_numbers <- c("510-555-0123", "541-555-0167")

# Use str_replace() to replace "-" with " "
str_replace(phone_numbers,"-"," ")

# Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers,"-"," ")

# Turn phone numbers into the format xxx.xxx.xxxx
str_replace_all(phone_numbers,"-",".")




# Find the number of nucleotides in each sequence
str_length(genes)

# Find the number of A's occur in each sequence
str_count(genes, fixed("A"))

# Return the sequences that contain "TTTTTT"
str_subset(genes, fixed("TTTTTT"))

# Replace all the "A"s in the sequences with a "_"
str_replace_all(genes, fixed("A"), "_")




# Define some full names
names <- c("Diana Prince", "Clark Kent")

# Split into first and last names
names_split <- str_split(names," ",simplify = T)

# Extract the first letter in the first name
abb_first <- str_sub(names_split[,1],1,1)

# Combine the first letter ". " and last name
str_c(abb_first,". ",names_split[,2])





# Use all names in babynames_2014
all_names <- babynames_2014$name

# Get the last two letters of all_names
last_two_letters <- str_sub(all_names, -2, -1)

# Does the name end in "ee"?
ends_in_ee <- str_detect(last_two_letters, "ee")

# Extract rows and "sex" column
sex <- babynames_2014$sex[ends_in_ee]

# Display result as a table
table(sex)



library(rebus)

# Some strings to practice with
x <- c("cat", "coat", "scotland", "tic toc")

# Print END
END

# Run me
str_view(x, pattern = START %R% "c")

# Match the strings that start with "co" 
str_view(x, pattern = START %R% "co")

# Match the strings that end with "at"
str_view(x, pattern = "at" %R% END)


# Match the string that is exactly "cat"
str_view(x, pattern = START %R% "cat" %R% END)


# Match two characters, where the second is a "t"
str_view(x, pattern = ANY_CHAR %R% "t")

# Match a "t" followed by any character
str_view(x, pattern = "t" %R% ANY_CHAR)

# Match two characters
str_view(x, pattern = ANY_CHAR %R% ANY_CHAR)

# Match a string with exactly three characters
str_view(x, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% END)




pattern <- "q" %R% ANY_CHAR

# Find names that have the pattern
names_with_q <- str_subset(boy_names, pattern)

# How many names were there?
length(names_with_q)



# Find part of name that matches pattern
part_with_q <- str_extract(boy_names, pattern)

# Get a table of counts
table(part_with_q)


# Did any names have the pattern more than once?
count_of_q <- str_count(boy_names,pattern)

# Get a table of counts
table(count_of_q)


# Which babies got these names?
with_q <- str_detect(boy_names, pattern)

# What fraction of babies got these names?
mean(with_q)




# Match Jeffrey or Geoffrey
whole_names <- or("Jeffrey", "Geoffrey")
str_view(boy_names, pattern = whole_names, match = TRUE)

# Match Jeffrey or Geoffrey, another way
common_ending <- or("Je", "Geo") %R% "ffrey"
str_view(boy_names, pattern = common_ending, match = TRUE)

# Match with alternate endings
by_parts <- or("Je", "Geo") %R% "ff" %R% or("ry", "ery","rey","erey")
str_view(boy_names, pattern = by_parts, match = TRUE)

# Match names that start with Cath or Kath
ckath <- or("C", "K") %R% "ath"
str_view(girl_names, pattern = ckath, match = TRUE)




# Create character class containing vowels
vowels <- char_class("aeiouAEIOU")

# Print vowels
vowels

# See vowels in x with str_view()
str_view(x, pattern = vowels)

# See vowels in x with str_view_all()
str_view_all(x,pattern = vowels)

# Number of vowels in boy_names
num_vowels <- str_count(boy_names, pattern = vowels)

# Number of characters in boy_names
name_length <-  str_length(boy_names)

# Calc mean number of vowels
mean(num_vowels)

# Calc mean fraction of vowels per name
mean(num_vowels / name_length)

# Vowels from last exercise
vowels <- char_class("aeiouAEIOU")

# See names with only vowels
str_view(boy_names, 
         pattern = exactly(one_or_more(vowels)), 
         match = TRUE)

# Use `negated_char_class()` for everything but vowels
not_vowels <- negated_char_class("aeiouAEIOU")

# See names with no vowels
str_view(boy_names, 
         pattern = exactly(one_or_more(not_vowels)), 
         match = TRUE)


contact = c("Call me at 555-555-0191","123 Main St","(555) 555 0191",
            "Phone: 555.555.0191 Mobile: 555.555.0192")

# Create a three digit pattern
three_digits <- DGT %R% DGT %R% DGT

# Test it
str_view_all(contact, pattern = three_digits)

# Create a separator pattern
separator <- char_class("-.() ") # <regex> [-.() ]

# Test it
str_view_all(contact, pattern = separator)

# Use these components
three_digits <- DGT %R% DGT %R% DGT
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")

# Create phone pattern # <regex> [\(]?\d\d\d[-.() ]*\d\d\d[-.() ]*\d\d\d\d
phone_pattern <- optional(OPEN_PAREN) %R% 
  three_digits %R% 
  zero_or_more(separator) %R% 
  three_digits %R% 
  zero_or_more(separator) %R%
  four_digits


# Test it           
str_view_all(contact, pattern = phone_pattern)


# Use this pattern
three_digits <- DGT %R% DGT %R% DGT
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")
phone_pattern <- optional(OPEN_PAREN) %R% 
  three_digits %R% 
  zero_or_more(separator) %R% 
  three_digits %R% 
  zero_or_more(separator) %R%
  four_digits

# Extract phone numbers
str_extract(contact, pattern = phone_pattern)

# Extract ALL phone numbers
str_extract_all(contact, pattern = phone_pattern)

narratives = c(
  "19YOM-SHOULDER STRAIN-WAS TACKLED WHILE PLAYING FOOTBALL W/ FRIENDS ",
  "31 YOF FELL FROM TOILET HITITNG HEAD SUSTAINING A CHI ",
  "ANKLE STR. 82 YOM STRAINED ANKLE GETTING OUT OF BED ",
  "TRIPPED OVER CAT AND LANDED ON HARDWOOD FLOOR. LACERATION ELBOW, LEFT. 33 YOF*",
  "10YOM CUT THUMB ON METAL TRASH CAN DX AVULSION OF SKIN OF THUMB ",
  "53 YO F TRIPPED ON CARPET AT HOME. DX HIP CONTUSION ",
  "13 MOF TRYING TO STAND UP HOLDING ONTO BED FELL AND HIT FOREHEAD ON RADIATOR DX LACERATION",
  "14YR M PLAYING FOOTBALL; DX KNEE SPRAIN ",
  "55YOM RIDER OF A BICYCLE AND FELL OFF SUSTAINED A CONTUSION TO KNEE ",
  "5 YOM ROLLING ON FLOOR DOING A SOMERSAULT AND SUSTAINED A CERVICAL STRA IN"
)

# Pattern to match one or two digits # <regex> \d[\d]?
age <- DGT %R% optional(DGT)

# Test it
str_view(narratives, pattern = age)

# Use this pattern
age <- DGT %R% optional(DGT)

# Pattern to match units 
unit <- or("YO","YR","MO")

# Test pattern with age then units
str_view(narratives, pattern = age %R% unit)

# Use these patterns
age <- DGT %R% optional(DGT)
unit <- optional(SPC) %R% or("YO", "YR", "MO")

# Pattern to match gender
gender <-  optional(SPC) %R% or("M", "F")

# Test pattern with age then units then gender
str_view(narratives, pattern = age %R% unit %R% gender)


# Use these patterns
age <- DGT %R% optional(DGT)
unit <- optional(SPC) %R% or("YO", "YR", "MO")
gender <- optional(SPC) %R% or("M", "F")

# Extract age, unit, gender
age_gender = str_extract(narratives,pattern=age %R% unit %R% gender)


# age_gender, age, gender, unit are pre-defined
ls.str()

# Extract age and make numeric
as.numeric(str_extract(age_gender,pattern=age))

# Replace age and units with ""
genders <- str_remove(age_gender, pattern = age %R% unit)

# Replace extra spaces # <regex> [\s]+
str_remove_all(genders, pattern = one_or_more(SPC))


# Numeric ages, from previous step
ages_numeric <- as.numeric(str_extract(age_gender, age))

# Extract units # <regex> [\s]?(?:YO|YR|MO)
time_units <- str_extract(age_gender,pattern=unit)

# Extract first word character
time_units_clean <- str_extract(time_units,pattern=WRD)

# Turn ages in months to years
ifelse(time_units_clean == "Y", ages_numeric, ages_numeric / 12)




## new class: 15.02.2022

# Capturing parts of a pattern --------------------------------------------

library(rebus)
library(stringr)
library(magrittr)

hero_contacts =
  c("(wolverine@xmen.com)","wonderwoman@justiceleague.org","thor@avengers.com")  

# Capture parts between @ and . and after .
email <- capture(one_or_more(WRD)) %R% 
  "@" %R% capture(one_or_more(WRD)) %R% 
  DOT %R% capture(one_or_more(WRD))

# Check match hasn't changed
str_view(hero_contacts,pattern=email)

# Pull out match and captures
email_parts <- str_match(hero_contacts,pattern=email)
email_parts

# Save host
host <- email_parts[,3]
host




# Pulling out parts of a phone number -------------------------------------

contact = c(
  "Call me at 555-555-0191",
  "123 Main St",
  "(555) 555 0191",
  "Phone: 555.555.0191 Mobile: 555.555.0192"
)

# View text containing phone numbers
contact

three_digits <- DGT %R% DGT %R% DGT
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")

# Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R% 
  capture(three_digits) %R% zero_or_more(separator) %R%
  capture(four_digits)

# Pull out the parts with str_match()
phone_numbers <- str_match(contact,pattern=phone_pattern)

# Put together the pieces with str_c() into the format (XXX) XXX-XXXX.
str_c(
  "(",
  phone_numbers[,2],
  ") ",
  phone_numbers[,3],
  "-",
  phone_numbers[,4]
)



# Extracting age and gender again -----------------------------------------

narratives = c(
  "19YOM-SHOULDER STRAIN-WAS TACKLED WHILE PLAYING FOOTBALL W/ FRIENDS ",
  "31 YOF FELL FROM TOILET HITITNG HEAD SUSTAINING A CHI ",
  "ANKLE STR. 82 YOM STRAINED ANKLE GETTING OUT OF BED ",
  "TRIPPED OVER CAT AND LANDED ON HARDWOOD FLOOR. LACERATION ELBOW, LEFT. 33 YOF*",
  "10YOM CUT THUMB ON METAL TRASH CAN DX AVULSION OF SKIN OF THUMB ",
  "53 YO F TRIPPED ON CARPET AT HOME. DX HIP CONTUSION ",
  "13 MOF TRYING TO STAND UP HOLDING ONTO BED FELL AND HIT FOREHEAD ON RADIATOR DX LACERATION",
  "14YR M PLAYING FOOTBALL; DX KNEE SPRAIN ",
  "55YOM RIDER OF A BICYCLE AND FELL OFF SUSTAINED A CONTUSION TO KNEE ",
  "5 YOM ROLLING ON FLOOR DOING A SOMERSAULT AND SUSTAINED A CERVICAL STRA IN"
)

# narratives has been pre-defined
narratives

# Add capture() to get age, unit and sex
pattern <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("YO", "YR", "MO")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Pull out from narratives
str_match(narratives,pattern=pattern)

# Edit to capture just Y and M in units
pattern2 <- capture(optional(DGT) %R% DGT) %R%  
  optional(SPC) %R% capture(or("Y", "M")) %R% optional(or("O","R")) %R%
  optional(SPC) %R% capture(or("M", "F"))

# Check pattern
str_view(narratives, pattern2)

# Pull out pieces
str_match(narratives, pattern2)




# Using backreferences in patterns ----------------------------------------

#' Backreferences can be useful in matching because they allow you to find 
#' repeated patterns or words. Using a backreference requires two things: 
#' you need to capture() the part of the pattern you want to reference, 
#' and then you refer to it with REF1.

#' Take a look at this pattern: capture(LOWER) %R% REF1. It matches and captures 
#' any lower case character, then is followed by the captured character: it 
#' detects repeated characters regardless of what character is repeated.

str_view(c("hello", "sweet", "kitten"), 
         pattern = capture(LOWER) %R% REF1)

library(babynames)
library(dplyr)

boy_names <- filter(babynames, sex == "M")$name

# Names with three repeated letters (e.g. aaa)
repeated_three_times <- capture(LOWER) %R% REF1 %R% REF1

# Test it
str_view(boy_names, pattern = repeated_three_times, match = TRUE)

# Names with a pair of repeated letters (e.g. abab)
pair_of_repeated <- capture(LOWER %R% LOWER) %R% REF1

# Test it
str_view(boy_names, pattern = pair_of_repeated, match = TRUE)

# Names with a pair that reverses (e.g. abba)
pair_that_reverses <- capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1

# Test it
str_view(boy_names, pattern = pair_that_reverses, match = TRUE)


# Four letter palindrome names (e.g. illi)
four_letter_palindrome <- exactly(
  capture(LOWER) %R% capture(LOWER) %R% REF2 %R% REF1
)

# Test it
str_view(boy_names, pattern = four_letter_palindrome, match = TRUE)




# Replacing with regular expressions --------------------------------------

# View text containing phone numbers
contact

# Replace digits with "X"
str_replace(contact, DGT, "X")

# Replace all digits with "X"
str_replace_all(contact, DGT, "X")

# Replace all digits with different symbol
str_replace_all(contact, DGT, c("X", ".", "*", "_"))




# Replacing with backreferences -------------------------------------------

# Exemplo
x <- c("hello", "sweet", "kitten")
str_replace(x, capture(ANY_CHAR), str_c(REF1, REF1))



# Build pattern to match words ending in "ING"
pattern <- one_or_more(WRD) %R% "ING"
str_view(narratives, pattern)

# Test replacement
str_replace(narratives, capture(pattern), 
            str_c("CARELESSLY", REF1, sep = " "))

adverbs = c(
"ABNORMALLY"       ,"ABSENTMINDEDLY"   ,"ACCIDENTALLY"    
,"ACIDLY"           ,"ACTUALLY"         ,"ADVENTUROUSLY"   
,"AFTERWARDS"       ,"ALMOST"           ,"ALWAYS"          
,"ANGRILY"          ,"ANNUALLY"         ,"ANXIOUSLY"       
,"ARROGANTLY"       ,"AWKWARDLY"        ,"BADLY"           
,"BASHFULLY"        ,"BEAUTIFULLY"      ,"BITTERLY"        
,"BLEAKLY"          ,"BLINDLY"          ,"BLISSFULLY"      
,"BOASTFULLY"       ,"BOLDLY"           ,"BRAVELY"         
,"BRIEFLY"          ,"BRIGHTLY"         ,"BRISKLY"         
,"BROADLY"          ,"BUSILY"           ,"CALMLY"          
,"CAREFULLY"        ,"CARELESSLY"       ,"CAUTIOUSLY"      
,"CERTAINLY"        ,"CHEERFULLY"       ,"CLEARLY"         
,"CLEVERLY"         ,"CLOSELY"          ,"COAXINGLY"       
,"COLORFULLY"       ,"COMMONLY"         ,"CONTINUALLY"     
,"COOLLY"           ,"CORRECTLY"        ,"COURAGEOUSLY"    
,"CROSSLY"          ,"CRUELLY"          ,"CURIOUSLY"       
,"DAILY"            ,"DAINTILY"         ,"DEARLY"          
,"DECEIVINGLY"      ,"DEEPLY"           ,"DEFIANTLY"       
,"DELIBERATELY"     ,"DELIGHTFULLY"     ,"DILIGENTLY"      
,"DIMLY"            ,"DOUBTFULLY"       ,"DREAMILY"        
,"EASILY"           ,"ELEGANTLY"        ,"ENERGETICALLY"   
,"ENORMOUSLY"       ,"ENTHUSIASTICALLY" ,"EQUALLY"         
,"ESPECIALLY"       ,"EVEN"             ,"EVENLY"          
,"EVENTUALLY"       ,"EXACTLY"          ,"EXCITEDLY"       
,"EXTREMELY"        ,"FAIRLY"           ,"FAITHFULLY"      
,"FAMOUSLY"         ,"FAR"              ,"FAST"            
,"FATALLY"          ,"FEROCIOUSLY"      ,"FERVENTLY"       
,"FIERCELY"         ,"FONDLY"           ,"FOOLISHLY"       
,"FORTUNATELY"      ,"FRANKLY"          ,"FRANTICALLY"     
,"FREELY"           ,"FRENETICALLY"     ,"FRIGHTFULLY"     
,"FULLY"            ,"FURIOUSLY"        ,"GENERALLY"       
,"GENEROUSLY"       ,"GENTLY"           ,"GLADLY"          
,"GLEEFULLY"        ,"GRACEFULLY"       ,"GRATEFULLY"      
,"GREATLY"          ,"GREEDILY"         ,"HAPPILY"         
,"HASTILY"          ,"HEALTHILY"        ,"HEAVILY"         
,"HELPFULLY"        ,"HELPLESSLY"       ,"HIGHLY"          
,"HONESTLY"         ,"HOPELESSLY"       ,"HOURLY"          
,"HUNGRILY"         ,"IMMEDIATELY"      ,"INNOCENTLY"      
,"INQUISITIVELY"    ,"INSTANTLY"        ,"INTENSELY"       
,"INTENTLY"         ,"INTERESTINGLY"    ,"INWARDLY"        
,"IRRITABLY"        ,"JAGGEDLY"         ,"JEALOUSLY"       
,"JOSHINGLY"        ,"JOVIALLY"         ,"JOYFULLY"        
,"JOYOUSLY"         ,"JUBILANTLY"       ,"JUDGEMENTALLY"   
,"JUSTLY"           ,"KEENLY"           ,"KIDDINGLY"       
,"KINDHEARTEDLY"    ,"KINDLY"           ,"KISSINGLY"       
,"KNAVISHLY"        ,"KNOTTILY"         ,"KNOWINGLY"       
,"KNOWLEDGEABLY"    ,"KOOKILY"          ,"LAZILY"          
,"LESS"             ,"LIGHTLY"          ,"LIKELY"          
,"LIMPLY"           ,"LIVELY"           ,"LOFTILY"         
,"LONGINGLY"        ,"LOOSELY"          ,"LOUDLY"          
,"LOVINGLY"         ,"LOYALLY"          ,"MADLY"           
,"MAJESTICALLY"     ,"MEANINGFULLY"     ,"MECHANICALLY"    
,"MERRILY"          ,"MISERABLY"        ,"MOCKINGLY"       
,"MONTHLY"          ,"MORE"             ,"MORTALLY"        
,"MOSTLY"           ,"MYSTERIOUSLY"     ,"NATURALLY"       
,"NEARLY"           ,"NEATLY"           ,"NEEDILY"         
,"NERVOUSLY"        ,"NEVER"            ,"NICELY"          
,"NOISILY"          ,"NOT"              ,"OBEDIENTLY"      
,"OBNOXIOUSLY"      ,"ODDLY"            ,"OFFENSIVELY"     
,"OFFICIALLY"       ,"OFTEN"            ,"ONLY"            
,"OPENLY"           ,"OPTIMISTICALLY"   ,"OVERCONFIDENTLY" 
,"OWLISHLY"         ,"PAINFULLY"        ,"PARTIALLY"       
,"PATIENTLY"        ,"PERFECTLY"        ,"PHYSICALLY"      
,"PLAYFULLY"        ,"POLITELY"         ,"POORLY"          
,"POSITIVELY"       ,"POTENTIALLY"      ,"POWERFULLY"      
,"PROMPTLY"         ,"PROPERLY"         ,"PUNCTUALLY"      
,"QUAINTLY"         ,"QUARRELSOMELY"    ,"QUEASILY"        
,"QUEERLY"          ,"QUESTIONABLY"     ,"QUESTIONINGLY"   
,"QUICKER"          ,"QUICKLY"          ,"QUIETLY"         
,"QUIRKILY"         ,"QUIZZICALLY"      ,"RAPIDLY"         
,"RARELY"           ,"READILY"          ,"REALLY"          
,"REASSURINGLY"     ,"RECKLESSLY"       ,"REGULARLY"       
,"RELUCTANTLY"      ,"REPEATEDLY"       ,"REPROACHFULLY"   
,"RESTFULLY"        ,"RIGHTEOUSLY"      ,"RIGHTFULLY"      
,"RIGIDLY"          ,"ROUGHLY"          ,"RUDELY"          
,"SADLY"            ,"SAFELY"           ,"SCARCELY"        
,"SCARILY"          ,"SEARCHINGLY"      ,"SEDATELY"        
,"SEEMINGLY"        ,"SELDOM"           ,"SELFISHLY"       
,"SEPARATELY"       ,"SERIOUSLY"        ,"SHAKILY"         
,"SHARPLY"          ,"SHEEPISHLY"       ,"SHRILLY"         
,"SHYLY"            ,"SILENTLY"         ,"SLEEPILY"        
,"SLOWLY"           ,"SMOOTHLY"         ,"SOFTLY"          
,"SOLEMNLY"         ,"SOLIDLY"          ,"SOMETIMES"       
,"SOON"             ,"SPEEDILY"         ,"STEALTHILY"      
,"STERNLY"          ,"STRICTLY"         ,"SUCCESSFULLY"    
,"SUDDENLY"         ,"SURPRISINGLY"     ,"SUSPICIOUSLY"    
,"SWEETLY"          ,"SWIFTLY"          ,"SYMPATHETICALLY" 
,"TENDERLY"         ,"TENSELY"          ,"TERRIBLY"        
,"THANKFULLY"       ,"THOROUGHLY"       ,"THOUGHTFULLY"    
,"TIGHTLY"          ,"TOMORROW"         ,"TOO"             
,"TREMENDOUSLY"     ,"TRIUMPHANTLY"     ,"TRULY"           
,"TRUTHFULLY"       ,"ULTIMATELY"       ,"UNABASHEDLY"     
,"UNACCOUNTABLY"    ,"UNBEARABLY"       ,"UNETHICALLY"     
,"UNEXPECTEDLY"     ,"UNFORTUNATELY"    ,"UNIMPRESSIVELY"  
,"UNNATURALLY"      ,"UNNECESSARILY"    ,"UPBEAT"          
,"UPLIFTINGLY"      ,"UPRIGHT"          ,"UPSIDE-DOWN"     
,"UPWARD"           ,"UPWARDLY"         ,"URGENTLY"        
,"USEFULLY"         ,"USELESSLY"        ,"USUALLY"         
,"UTTERLY"          ,"VACANTLY"         ,"VAGUELY"         
,"VAINLY"           ,"VALIANTLY"        ,"VASTLY"          
,"VERBALLY"         ,"VERY"             ,"VICIOUSLY"       
,"VICTORIOUSLY"     ,"VIOLENTLY"        ,"VIVACIOUSLY"     
,"VOLUNTARILY"      ,"WARMLY"           ,"WEAKLY"          
,"WEARILY"          ,"WELL"             ,"WETLY"           
,"WHOLLY"           ,"WILDLY"           ,"WILLFULLY"       
,"WISELY"           ,"WOEFULLY"         ,"WONDERFULLY"     
,"WORRIEDLY"        ,"WRONGLY"          ,"YAWNINGLY"       
,"YEARLY"           ,"YEARNINGLY"       ,"YESTERDAY"       
,"YIELDINGLY"       ,"YOUTHFULLY" 
)


# One adverb per narrative
adverbs_10 <- sample(adverbs, 10)

# Replace "***ing" with "adverb ***ly"
str_replace(narratives, 
            capture(pattern),
            str_c(adverbs_10, REF1, sep = " "))  



# Unicode -----------------------------------------------------------------

library(stringr)
library(stringi)
library(rebus)

# Example
x <- c("\u00e8", "\u0065\u0300")
writeLines(x)

str_view(x, "\u00e8")

as.hexmode(utf8ToInt(stri_trans_nfd("\u00e8")))
as.hexmode(utf8ToInt(stri_trans_nfc("\u0065\u0300")))

#' Em Unicode, um acento é conhecido como propriedade Unicode diacrítica e 
#' você pode combiná-lo usando o rebusvalor UP_DIACRITIC.
#' 
#' Vietnamita faz uso pesado de diacríticos para denotar os tons em suas 
#' palavras. Neste exercício, você manipulará os diacríticos nos nomes dos 
#' governantes vietnamitas.

# Names with builtin accents
(tay_son_builtin <- c(
  "Nguy\u1ec5n Nh\u1ea1c", 
  "Nguy\u1ec5n Hu\u1ec7",
  "Nguy\u1ec5n Quang To\u1ea3n"
))

# Convert to separate accents
tay_son_separate <- stri_trans_nfd(tay_son_builtin)

# Verify that the string prints the same
tay_son_separate

# Match all accents
str_view_all(tay_son_separate, pattern = "UP_DIACRITIC")



# Matching a single grapheme ----------------------------------------------

# Example
x <- c("Adele", "Ad\u00e8le", "Ad\u0065\u0300le")
writeLines(x)

str_view(x, "Ad" %R% ANY_CHAR %R% "le")

str_view(x, "Ad" %R% GRAPHEME %R% "le")



# tay_son_separate has been pre-defined
tay_son_separate

# View all the characters in tay_son_separate
str_view_all(tay_son_separate, ANY_CHAR)

# View all the graphemes in tay_son_separate
str_view_all(tay_son_separate, GRAPHEME)

# Combine the diacritics with their letters
tay_son_builtin <- stri_trans_nfc(tay_son_separate)
tay_son_builtin

# View all the graphemes in tay_son_builtin
str_view_all(tay_son_builtin, GRAPHEME)







# class, 16-02-2022


# Case studies ------------------------------------------------------------

# Texto: 'The Importance of Being Earnest, a Trivial Comedy for Serious People',
# uma peça de teatro de Oscar Wilde.

library(stringi)
library(stringr)

# Read play in using stri_read_lines()
earnest <- stri_read_lines('data/earnest_file.txt')

# Detect start and end lines
start <- str_which(earnest, "START OF THE PROJECT")
end <- str_which(earnest, "END OF THE PROJECT")

# Get rid of gutenberg intro text
earnest_sub  <- earnest[(start + 1):(end - 1)]

# Detect first act
lines_start <- str_which(earnest_sub, fixed("FIRST ACT"))

# Set up index
intro_line_index <- 1:(lines_start - 1)

# Split play into intro and play
intro_text <- earnest_sub[intro_line_index]
play_text <- earnest_sub[-intro_line_index]

# Take a look at the first 20 lines
writeLines(play_text[1:20])



# Identifying the lines, take 1 -------------------------------------------

# The first thing you might notice when you look at your vector play_text is 
# there are lots of empty lines. They don't really affect your task so you 
# might want to remove them. The easiest way to find empty strings is to use 
# the stringi function stri_isempty(), which returns a logical you can use to 
# subset the not-empty strings:

# Get rid of empty strings
empty <- stringi::stri_isempty(play_text)
play_lines <- play_text[!empty]

play_lines[10:15]

# How about looking for lines that start with a word followed by a .?

library(rebus)

# Pattern for start, word then .
# Pattern for start, word then .
pattern_1 <- START %R% one_or_more(WRD) %R% DOT    # <regex> ^[\w]+\.

# Test pattern_1
str_view(play_lines, pattern_1, match = TRUE) 
str_view(play_lines, pattern_1, match = FALSE)

# Pattern for start, capital, word then .
pattern_2 <- START %R% ascii_upper() %R% one_or_more(WRD) %R% DOT

# Test pattern_2
str_view(play_lines, pattern_2, match = TRUE)
str_view(play_lines, pattern_2, match = FALSE)


# Pattern from last step
pattern_2 <- START %R% ascii_upper() %R% one_or_more(WRD) %R% DOT

# Get subset of lines that match
lines <- str_subset(play_lines, pattern_2, negate = FALSE)

# Extract match from lines
who <- str_extract(lines, pattern_2)

# Let's see what we have
unique(who)


# Create vector of characters
characters <- c("Algernon", "Jack", "Lane", "Cecily", "Gwendolen", "Chasuble", 
                "Merriman", "Lady Bracknell", "Miss Prism")

# Match start, then character names then .
pattern_3 <- START %R% or1(characters) %R% DOT

# View matches of pattern_3
str_view(play_lines, pattern_3, match = TRUE) 

# View non-matches of pattern_3
str_view(play_lines, pattern_3, match = FALSE)

# Pull out matches
lines <- str_subset(play_lines, pattern_3)

# Extract match from lines
who <- str_extract(lines, pattern_3)

# Let's see what we have
unique(who)

# Count lines per character
table(who)





# -------------------------------------------------------------------------

x <- c("Cat", "CAT", "cAt") 
str_view(x, "cat")

str_view(str_to_lower(x), "cat")

# See if you can find the catcidents that also involved dogs. You'll see a 
# new rebus function called whole_word(). The argument to whole_word() will 
# only match if it occurs as a word on its own, for example whole_word("cat") 
# will match cat in "The cat " and "cat." but not in "caterpillar".

catcidents = c(
"79yOf Fractured fingeR tRiPPED ovER cAT ANd fell to FlOOr lAst nIGHT AT HOME*"                                                                  
,"21 YOF REPORTS SUS LACERATION OF HER LEFT HAND WHEN SHE WAS OPENING A CAN OF CAT FOOD JUST PTA. DX HAND LACERATION%"                            
,"87YOF TRIPPED OVER CAT, HIT LEG ON STEP. DX LOWER LEG CONTUSION "                                                                               
,"bLUNT CHest trAUma, R/o RIb fX, R/O CartiLAgE InJ To RIB cAge; 32YOM walKiNG DOG, dog took OfF aFtER cAt,FelL,stRucK CHest oN STepS,hiT rIbS"   
,"42YOF TO ER FOR BACK PAIN AFTER PUTTING DOWN SOME CAT LITTER DX: BACK PAIN, SCIATICA"                                                           
,"4YOf DOg jUst hAd PUpPieS, Cat TRIED 2 get PuPpIes, pT THru CaT dwn stA Irs, LoST foOTING & FELl down ~12 stePS; MInor hEaD iNJuRY"             
,"unhelmeted 14yof riding her bike with her dog when she saw a cat and sw erved c/o head/shoulder/elbow pain.dx: minor head injury,left shoulder" 
,"24Yof lifting a 40 pound bag of cat litter injured lt wrist; wrist sprain"                                                                      
,"3Yof-foot lac-cut on cat food can-@ home "                                                                                                      
,"Rt Shoulder Strain.26Yof Was Walking Dog On Leash And Dot Saw A Cat And Pulled Leash."                                                          
,"15 mO m cut FinGer ON cAT FoOd CAn LID. Dx:  r INDeX laC 1 cm."                                                                                 
,"31 YOM SUSTAINED A CONTUSION OF A HAND BY TRIPPING ON CAT & FALLING ON STAIRS."                                                                 
,"ACCIDENTALLY CUT FINGER WHILE OPENING A CAT FOOD CAN, +BLEEDING >>LAC"                                                                          
,"4 Yom was cut on cat food can. Dx:  r index lac 1 cm."                                                                                          
,"4 YO F, C/O FOREIGN BODY IN NOSE 1/2 HOUR, PT NOT REPORTING NATURE OF F B, PIECE OF CAT LITTER REMOVED FROM RT NOSTRIL, DX FB NOSE"             
,"21Yowf  pT STAteS 4-5 DaYs Ago LifTEd 2 - 50 lB BagS OF CAT lItter.  al So sORTIng ClOThES & W/ seVERe paIn.  DX .  sTrain  LUMbOSaCRal."       
,"67 YO F WENT TO WALK DOG, IT STARTED TO CHASE CAT JERKED LEASH PULLED H ER OFF PATIO, FELL HURT ANKLES. DX BILATERAL ANKLE FRACTURES"           
,"17Yof Cut Right Hand On A Cat Food Can - Laceration "                                                                                           
,"46yof taking dog outside, dog bent her fingers back on a door. dog jerk ed when saw cat. hand holding leash caught on door jamb/ct hand"        
,"19 YOF-FelL whIle WALKINg DOWn THE sTAIrS & TRiPpEd over a caT-fell oNT o \"TaIlBoNe\"         dx   coNtusIon LUMBaR, uti      *"               
,"50YOF CUT FINGER ON CAT FOOD CAN LID.  DX: LT RING FINGER LAC "                                                                                 
,"lEFT KNEE cOntusioN.78YOf triPPEd OVEr CaT aND fell and hIt knEE ON the fLoOr."                                                                 
,"LaC FInGer oN a meTAL Cat fOOd CaN "                                                                                                            
,"PUSHING HER UTD WITH SHOTS DOG AWAY FROM THE CAT'S BOWL&BITTEN TO FINGE R>>PW/DOG BITE"                                                         
,"DX CALF STRAIN R CALF: 15YOF R CALF PN AFTER FALL ON CARPETED STEPS, TR YING TO STEP OVER CAT, TRIPPED ON STAIRS, HIT LEG"                      
,"DISLOCATION TOE - 80 YO FEMALE REPORTS SHE FELL AT HOME - TRIPPED OVER THE CAT LITTER BOX & FELL STRIKING TOE ON DOOR JAMB - ALSO SHOULDER INJ" 
,"73YOF-RADIUS FX-TRIPPED OVER CAT LITTER BOX-FELL-@ HOME "                                                                                       
,"57Yom-Back Pain-Tripped Over A Cat-Fell Down 4 Steps-@ Home "                                                                                   
,"76YOF SUSTAINED A HAND ABRASION CLEANING OUT CAT LITTER BOX THREE DAYS AGO AND NOW THE ABRASION IS INFECTED CELLULITIS HAND"                    
,"DX R SH PN: 27YOF W/ R SH PN X 5D. STATES WAS YANK' BY HER DOG ON LEASH W DOG RAN AFTER CAT; WORSE' PN SINCE. FULL ROM BUT VERY PAINFUL TO MOVE"
,"35Yof FeLt POp iN aBdoMeN whIlE piCKInG UP 40Lb BaG OF CAt litTeR aBdomINAL sTrain"                                                             
,"77 Y/o f tripped over cat-c/o shoulder and upper arm pain. Fell to floo r at home. Dx proximal humerus fx"                                      
,"FOREHEAD LAC.46YOM TRIPPED OVER CAT AND FELL INTO A DOOR FRAME. "                                                                               
,"39Yof dog pulled her down the stairs while chasing a cat dx: rt ankle inj"                                                                      
,"10 YO FEMALE OPENING A CAN OF CAT FOOD.  DX HAND LACERATION "                                                                                   
,"44Yof Walking Dog And The Dof Took Off After A Cat And Pulled Pt Down B Y The Leash Strained Neck"                                              
,"46Yof has low back pain after lifting heavy bag of cat litter lumbar spine sprain"                                                              
,"62 yOf FELL PUShIng carT W/CAT liTtER 3 DAYs Ago. Dx:  l FIfTH rib conT."                                                                       
,"PT OPENING HER REFRIGERATOR AND TRIPPED OVER A CAT AND FELL ONTO SHOULD ER FRACTURED HUMERUS"                                                   
,"Pt Lifted Bag Of Cat Food. Dx:  Low Back Px, Hx Arthritic Spine." 
)

# catcidents has been pre-defined
head(catcidents)

# Construct pattern of DOG in boundaries
whole_dog_pattern <- whole_word("DOG")

# See matches to word DOG
str_view(catcidents,whole_dog_pattern,match = T)

# Transform catcidents to upper case
catcidents_upper <- str_to_upper(catcidents)

# View matches to word "DOG" again
str_view(catcidents_upper,whole_dog_pattern, match = T)

# Which strings match?
has_dog <- str_detect(catcidents_upper,whole_dog_pattern)

# Pull out matching strings in original 
catcidents[has_dog]




# ignore_case = TRUE # Ignora se as palavras estão escritas em maiúscula 
# ou minúscula


# Example
x <- c("Cat", "CAT", "cAt") 
str_view(x, "cat")

str_view(x, stringr::regex("cat", ignore_case = TRUE))



# View matches to "TRIP"
str_view(catcidents, pattern = "TRIP", match = TRUE)

# Construct case insensitive pattern
trip_pattern <- regex("TRIP", ignore_case = TRUE)

# View case insensitive matches to "TRIP"
str_view(catcidents, pattern = trip_pattern, match = TRUE)


# Get subset of matches
trip <- str_subset(catcidents, trip_pattern, negate = FALSE)

# Extract matches
str_extract(trip, trip_pattern)





# Finally, you might want to transform strings to a common case. You've seen 
# you can use str_to_upper() and str_to_lower(), but there is also str_to_title() 
# which transforms to title case, in which every word starts with a capital 
# letter.
# 


str_to_title()

dog <- "The quick brown dog"
str_to_upper(dog) # "THE QUICK BROWN DOG"
str_to_lower(dog) # "the quick brown dog"
str_to_title(dog) # "The Quick Brown Dog"


library(stringi)

# Get first five catcidents
cat5 <- catcidents[1:5]

# Take a look at original
writeLines(cat5)

# Transform to title case
writeLines(str_to_title(cat5))

# Transform to title case with stringi
writeLines(stri_trans_totitle(cat5))

# Esta é outra situação em que stringias funções oferecem um pouco mais de 
# funcionalidade do que as stringrfunções. A stringifunção stri_trans_totitle()
# permite especificar o typeque, por padrão, é "word", resultando em maiúsculas 
# e minúsculas, mas também pode ser "sentence"para dar maiúsculas e minúsculas: 
# apenas a primeira palavra de cada frase é maiúscula.

# Transform to sentence case with stringi
writeLines(stri_trans_totitle(cat5, 
                              type = "sentence"))





