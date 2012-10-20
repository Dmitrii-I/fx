# This script does this:
# 1) Puts leading zeroes into months, days, hours, minutes and seconds
# 2) Corrects milliseconds by adding leading zeroes that were stripped initially
#    because they were stored as integers. So 1.56 seconds is adjusted to 1.056
#
# To call this script type in terminal: 
# awk -f leadingzeroes.awk filetoprocess.csv > out.csv

BEGIN {
	# Begin clause is executed only once
	FS = "[,/:. ]"; # set the separator character to comma, dash, space
}
{
	# The section between these curly braces will be executed once 
	# for each line in the file 

	# delete the carret M (^M) which is windows CR/LF
	sub(//,"")

	# print date and use leading zeroes for month and day if needed
	printf "%d%s%02d%s%02d " , $1, "-", $2, "-", $3
	
	# print time and use leading zeroes and add timezone offset
	printf "%02d%s%02d%s%02d%s%03d%s", $4, ":", $5, ":", $6, ".", $7, "+01:00,"   
	
	# print quotes. we use length 5 and leading zeroes because decimal
	# part is accessed as integer from which the leading zeroes are stripped
	printf "%d%s%s%s%d%s%s\n", $8, ".", $9, ",", $10, ".", $11

}

END { }
