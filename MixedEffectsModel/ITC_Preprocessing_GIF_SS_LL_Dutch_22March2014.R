###INTERTEMPORAL CHOICE: THIS SCRIPT WILL CREATE VARIABLES FOR SS_AMOUNT, SS_TIME, LL_AMOUNT, & LL_TIME, AND ASSIGN THE APPROPRIATE VALUES; added Dec 29 2010: recoding of choice (instead of q/p)###
###Author: Hiro Kotabe, hpk2106@columbia.edu - adapated Dutch_AvD; some fixes and updates by Bernd, including Dec 29, 2010 [search for 'Accel15.gif' to find the fix]); 30 Dec, 2010; March 26, 2013: Stats III course; March 22, 2014: Stats III MixMod course

# this script only does some preprocessing and getting rid of some variables etc; no modeling at all is involved here


options(scipen=5) # this has the effect that the serials are shown in regular, not scientific notation



ITC = read.csv(file = "~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_MixMod_Choices_numlet.csv")

nrow(ITC) #1056
ncol(ITC) #13


±±±±±±±±±±±±±
from CCT script

# split p_serial into meaningful components



length(unique(substring(ITC$serial,9,10))) #32 different participants

ITC$pp_code_ITC <- as.factor(paste('pp', substring(ITC$serial,9,10), sep = '_')) # This is the relevant new variable!!!

table(ITC$pp_code_ITC) # ok, each pp has 33 entries (this includes the practice trials)

# task order
ITC$order <- substr(ITC$serial, 11, 11)

# first or second ITC? In this year, everybody did it only once and also only as second task always! thus, this variable is kind of silly here, as everybody did only 1 intertemp choice task and therefore it will always be the first of the (only 1) intertemporal choice tasks...
ITC$FirstSecondITC <- ordered(ifelse(ITC$order < 2.5, 'first', 'second'), levels = c('first', 'second'))

head(ITC)



# sort (i.e., order) data frame according to:
# 1. pp_code
# 2. order
# 3. r_trialnum


ITC_2 <- ITC[with(ITC, order(pp_code_ITC, order, blocknum, trial)),]

head(ITC_2)

# I don't want to change the whole code from ITC to ITC_3 below, so I'm saving it back into the variable name ITC... this is NOT good practice!!!
ITC <- ITC_2


#Set up SS_Amount, SS_Time, LL_Amount, and LL_Time variables with "NA" values
ITC$SS_Amount = rep(c(NA), each = nrow(ITC))
ITC$SS_Time = rep(c(NA), each = nrow(ITC))
ITC$LL_Amount = rep(c(NA), each = nrow(ITC))
ITC$LL_Time = rep(c(NA), each = nrow(ITC))        

#Add variable that codes whether delay or acceleration 
x = as.vector(ITC$stim)
x[grep("dutch_delay/Del|dutch_practice/delay_practice", x)] = "delay"
x[grep("dutch_accel/Accel|dutch_practice/accel_practice", x)] = "accelerate"

ITC$Frame = x

unique(ITC$Frame) # yep, this year it was always and only delay framing!



# (A) find all practice trials and assign values

# a) DELAY
hits_delay_practice_1<-grep('delay_practice_1.gif',ITC$stim)
	ITC$SS_Amount[hits_delay_practice_1] =  41
	ITC$SS_Time[hits_delay_practice_1] = 0  
	ITC$LL_Amount[hits_delay_practice_1] = 42   
	ITC$LL_Time[hits_delay_practice_1] = 28 
	
hits_delay_practice_2<-grep('delay_practice_2.gif',ITC$stim)
	ITC$SS_Amount[hits_delay_practice_2] =  1
	ITC$SS_Time[hits_delay_practice_2] = 14  
	ITC$LL_Amount[hits_delay_practice_2] = 100   
	ITC$LL_Time[hits_delay_practice_2] = 28 
	
	
# b) ACCEL: in 2014, we didn't do the accel framed task
hits_accel_practice_1<-grep('accel_practice_1.gif',ITC$stim)
	ITC$SS_Amount[hits_accel_practice_1] =  41
	ITC$SS_Time[hits_accel_practice_1] = 0  
	ITC$LL_Amount[hits_accel_practice_1] = 42   
	ITC$LL_Time[hits_accel_practice_1] = 28 
	
hits_accel_practice_2<-grep('accel_practice_2.gif',ITC$stim)
	ITC$SS_Amount[hits_accel_practice_2] =  1
	ITC$SS_Time[hits_accel_practice_2] = 14  
	ITC$LL_Amount[hits_accel_practice_2] = 100   
	ITC$LL_Time[hits_accel_practice_2] = 28 
	
	
# (B) Real Trials

# a) DELAY
hits_Del01<-grep('Del01.gif',ITC$stim)
	ITC$SS_Amount[hits_Del01] = 41
	ITC$SS_Time[hits_Del01] = 0
	ITC$LL_Amount[hits_Del01] = 42
	ITC$LL_Time[hits_Del01] = 14
	
hits_Del02<-grep('Del02.gif',ITC$stim)
	ITC$SS_Amount[hits_Del02] = 20
	ITC$SS_Time[hits_Del02] = 14
	ITC$LL_Amount[hits_Del02] = 21
	ITC$LL_Time[hits_Del02] = 28

hits_Del03<-grep('Del03.gif',ITC$stim)
	ITC$SS_Amount[hits_Del03] = 28
	ITC$SS_Time[hits_Del03] = 0
	ITC$LL_Amount[hits_Del03] = 29
	ITC$LL_Time[hits_Del03] = 28
	
hits_Del04<-grep('Del04.gif',ITC$stim)
	ITC$SS_Amount[hits_Del04] = 43
	ITC$SS_Time[hits_Del04] = 14
	ITC$LL_Amount[hits_Del04] = 44
	ITC$LL_Time[hits_Del04] = 42

hits_Del05<-grep('Del05.gif',ITC$stim)
	ITC$SS_Amount[hits_Del05] = 45
	ITC$SS_Time[hits_Del05] = 0
	ITC$LL_Amount[hits_Del05] = 46
	ITC$LL_Time[hits_Del05] = 3

hits_Del06<-grep('Del06.gif',ITC$stim)
	ITC$SS_Amount[hits_Del06] = 42
	ITC$SS_Time[hits_Del06] = 14
	ITC$LL_Amount[hits_Del06] = 43
	ITC$LL_Time[hits_Del06] = 17
	
hits_Del07<-grep('Del07.gif',ITC$stim)
	ITC$SS_Amount[hits_Del07] = 46
	ITC$SS_Time[hits_Del07] = 0
	ITC$LL_Amount[hits_Del07] = 51
	ITC$LL_Time[hits_Del07] = 14

hits_Del08<-grep('Del08.gif',ITC$stim)
	ITC$SS_Amount[hits_Del08] = 29
	ITC$SS_Time[hits_Del08] = 14
	ITC$LL_Amount[hits_Del08] = 32
	ITC$LL_Time[hits_Del08] = 28
	
hits_Del09<-grep('Del09.gif',ITC$stim)
	ITC$SS_Amount[hits_Del09] = 25
	ITC$SS_Time[hits_Del09] = 0
	ITC$LL_Amount[hits_Del09] = 27
	ITC$LL_Time[hits_Del09] = 28

hits_Del10<-grep('Del10.gif',ITC$stim)
	ITC$SS_Amount[hits_Del10] = 52
	ITC$SS_Time[hits_Del10] = 14
	ITC$LL_Amount[hits_Del10] = 57
	ITC$LL_Time[hits_Del10] = 42

hits_Del11<-grep('Del11.gif',ITC$stim)
	ITC$SS_Amount[hits_Del11] = 33
	ITC$SS_Time[hits_Del11] = 0
	ITC$LL_Amount[hits_Del11] = 36
	ITC$LL_Time[hits_Del11] = 3
	
hits_Del12<-grep('Del12.gif',ITC$stim)
	ITC$SS_Amount[hits_Del12] = 29
	ITC$SS_Time[hits_Del12] = 14
	ITC$LL_Amount[hits_Del12] = 32
	ITC$LL_Time[hits_Del12] = 17

hits_Del13<-grep('Del13.gif',ITC$stim)
	ITC$SS_Amount[hits_Del13] = 19
	ITC$SS_Time[hits_Del13] = 0
	ITC$LL_Amount[hits_Del13] = 23
	ITC$LL_Time[hits_Del13] = 14
	
hits_Del14<-grep('Del14.gif',ITC$stim)
	ITC$SS_Amount[hits_Del14] = 70
	ITC$SS_Time[hits_Del14] = 14
	ITC$LL_Amount[hits_Del14] = 84
	ITC$LL_Time[hits_Del14] = 28

hits_Del15<-grep('Del15.gif',ITC$stim)
	ITC$SS_Amount[hits_Del15] = 62
	ITC$SS_Time[hits_Del15] = 0
	ITC$LL_Amount[hits_Del15] = 75
	ITC$LL_Time[hits_Del15] = 28

hits_Del16<-grep('Del16.gif',ITC$stim)
	ITC$SS_Amount[hits_Del16] = 16
	ITC$SS_Time[hits_Del16] = 14
	ITC$LL_Amount[hits_Del16] = 19
	ITC$LL_Time[hits_Del16] = 42
	
hits_Del17<-grep('Del17.gif',ITC$stim)
	ITC$SS_Amount[hits_Del17] = 34
	ITC$SS_Time[hits_Del17] = 0
	ITC$LL_Amount[hits_Del17] = 41
	ITC$LL_Time[hits_Del17] = 3

hits_Del18<-grep('Del18.gif',ITC$stim)
	ITC$SS_Amount[hits_Del18] = 56
	ITC$SS_Time[hits_Del18] = 14
	ITC$LL_Amount[hits_Del18] = 67
	ITC$LL_Time[hits_Del18] = 17
	
hits_Del19<-grep('Del19.gif',ITC$stim)
	ITC$SS_Amount[hits_Del19] = 44
	ITC$SS_Time[hits_Del19] = 0
	ITC$LL_Amount[hits_Del19] = 57
	ITC$LL_Time[hits_Del19] = 14

hits_Del20<-grep('Del20.gif',ITC$stim)
	ITC$SS_Amount[hits_Del20] = 33
	ITC$SS_Time[hits_Del20] = 14
	ITC$LL_Amount[hits_Del20] = 43
	ITC$LL_Time[hits_Del20] = 28

hits_Del21<-grep('Del21.gif',ITC$stim)
	ITC$SS_Amount[hits_Del21] = 32
	ITC$SS_Time[hits_Del21] = 0
	ITC$LL_Amount[hits_Del21] = 41
	ITC$LL_Time[hits_Del21] = 28
	
hits_Del22<-grep('Del22.gif',ITC$stim)
	ITC$SS_Amount[hits_Del22] = 56
	ITC$SS_Time[hits_Del22] = 14
	ITC$LL_Amount[hits_Del22] = 73
	ITC$LL_Time[hits_Del22] = 42

hits_Del23<-grep('Del23.gif',ITC$stim)
	ITC$SS_Amount[hits_Del23] = 42
	ITC$SS_Time[hits_Del23] = 0
	ITC$LL_Amount[hits_Del23] = 55
	ITC$LL_Time[hits_Del23] = 3
	
hits_Del24<-grep('Del24.gif',ITC$stim)
	ITC$SS_Amount[hits_Del24] = 16
	ITC$SS_Time[hits_Del24] = 14
	ITC$LL_Amount[hits_Del24] = 21
	ITC$LL_Time[hits_Del24] = 17

hits_Del25<-grep('Del25.gif',ITC$stim)
	ITC$SS_Amount[hits_Del25] = 56
	ITC$SS_Time[hits_Del25] = 0
	ITC$LL_Amount[hits_Del25] = 84
	ITC$LL_Time[hits_Del25] = 14

hits_Del26<-grep('Del26.gif',ITC$stim)
	ITC$SS_Amount[hits_Del26] = 22
	ITC$SS_Time[hits_Del26] = 14
	ITC$LL_Amount[hits_Del26] = 33
	ITC$LL_Time[hits_Del26] = 28
	
hits_Del27<-grep('Del27.gif',ITC$stim)
	ITC$SS_Amount[hits_Del27] = 25
	ITC$SS_Time[hits_Del27] = 0
	ITC$LL_Amount[hits_Del27] = 37
	ITC$LL_Time[hits_Del27] = 28

hits_Del28<-grep('Del28.gif',ITC$stim)
	ITC$SS_Amount[hits_Del28] = 45
	ITC$SS_Time[hits_Del28] = 14
	ITC$LL_Amount[hits_Del28] = 67
	ITC$LL_Time[hits_Del28] = 42
	
hits_Del29<-grep('Del29.gif',ITC$stim)
	ITC$SS_Amount[hits_Del29] = 54
	ITC$SS_Time[hits_Del29] = 0
	ITC$LL_Amount[hits_Del29] = 80
	ITC$LL_Time[hits_Del29] = 3

hits_Del30<-grep('Del30.gif',ITC$stim)
	ITC$SS_Amount[hits_Del30] = 35
	ITC$SS_Time[hits_Del30] = 14
	ITC$LL_Amount[hits_Del30] = 53
	ITC$LL_Time[hits_Del30] = 17

hits_Del31<-grep('Del31.gif',ITC$stim)
	ITC$SS_Amount[hits_Del31] = 39
	ITC$SS_Time[hits_Del31] = 0
	ITC$LL_Amount[hits_Del31] = 31
	ITC$LL_Time[hits_Del31] = 28



# b) ACCEL
hits_Accel01<-grep('Accel01.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel01] = 41
	ITC$SS_Time[hits_Accel01] = 0
	ITC$LL_Amount[hits_Accel01] = 42
	ITC$LL_Time[hits_Accel01] = 14
	
hits_Accel02<-grep('Accel02.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel02] = 20
	ITC$SS_Time[hits_Accel02] = 14
	ITC$LL_Amount[hits_Accel02] = 21
	ITC$LL_Time[hits_Accel02] = 28

hits_Accel03<-grep('Accel03.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel03] = 28
	ITC$SS_Time[hits_Accel03] = 0
	ITC$LL_Amount[hits_Accel03] = 29
	ITC$LL_Time[hits_Accel03] = 28
	
hits_Accel04<-grep('Accel04.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel04] = 44
	ITC$SS_Time[hits_Accel04] = 14
	ITC$LL_Amount[hits_Accel04] = 45
	ITC$LL_Time[hits_Accel04] = 42

hits_Accel05<-grep('Accel05.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel05] = 46
	ITC$SS_Time[hits_Accel05] = 0
	ITC$LL_Amount[hits_Accel05] = 47
	ITC$LL_Time[hits_Accel05] = 3

hits_Accel06<-grep('Accel06.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel06] = 42
	ITC$SS_Time[hits_Accel06] = 14
	ITC$LL_Amount[hits_Accel06] = 43
	ITC$LL_Time[hits_Accel06] = 17
	
hits_Accel07<-grep('Accel07.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel07] = 46
	ITC$SS_Time[hits_Accel07] = 0
	ITC$LL_Amount[hits_Accel07] = 50
	ITC$LL_Time[hits_Accel07] = 14

hits_Accel08<-grep('Accel08.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel08] = 29
	ITC$SS_Time[hits_Accel08] = 14
	ITC$LL_Amount[hits_Accel08] = 32
	ITC$LL_Time[hits_Accel08] = 28
	
hits_Accel09<-grep('Accel09.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel09] = 24
	ITC$SS_Time[hits_Accel09] = 0
	ITC$LL_Amount[hits_Accel09] = 26
	ITC$LL_Time[hits_Accel09] = 28

hits_Accel10<-grep('Accel10.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel10] = 52
	ITC$SS_Time[hits_Accel10] = 14
	ITC$LL_Amount[hits_Accel10] = 58
	ITC$LL_Time[hits_Accel10] = 42

hits_Accel11<-grep('Accel11.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel11] = 33
	ITC$SS_Time[hits_Accel11] = 0
	ITC$LL_Amount[hits_Accel11] = 36
	ITC$LL_Time[hits_Accel11] = 3
	
hits_Accel12<-grep('Accel12.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel12] = 29
	ITC$SS_Time[hits_Accel12] = 14
	ITC$LL_Amount[hits_Accel12] = 32
	ITC$LL_Time[hits_Accel12] = 17

hits_Accel13<-grep('Accel13.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel13] = 19
	ITC$SS_Time[hits_Accel13] = 0
	ITC$LL_Amount[hits_Accel13] = 23
	ITC$LL_Time[hits_Accel13] = 14
	
hits_Accel14<-grep('Accel14.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel14] = 69
	ITC$SS_Time[hits_Accel14] = 14
	ITC$LL_Amount[hits_Accel14] = 83
	ITC$LL_Time[hits_Accel14] = 28

hits_Accel15<-grep('Accel15.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel15] = 62
	ITC$SS_Time[hits_Accel15] = 0
	ITC$LL_Amount[hits_Accel15] = 74 # this was incorrectly stated as 47 in an earlier version of this script!
	ITC$LL_Time[hits_Accel15] = 28

hits_Accel16<-grep('Accel16.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel16] = 16
	ITC$SS_Time[hits_Accel16] = 14
	ITC$LL_Amount[hits_Accel16] = 19
	ITC$LL_Time[hits_Accel16] = 42
	
hits_Accel17<-grep('Accel17.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel17] = 34
	ITC$SS_Time[hits_Accel17] = 0
	ITC$LL_Amount[hits_Accel17] = 41
	ITC$LL_Time[hits_Accel17] = 3

hits_Accel18<-grep('Accel18.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel18] = 56
	ITC$SS_Time[hits_Accel18] = 14
	ITC$LL_Amount[hits_Accel18] = 67
	ITC$LL_Time[hits_Accel18] = 17
	
hits_Accel19<-grep('Accel19.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel19] = 44
	ITC$SS_Time[hits_Accel19] = 0
	ITC$LL_Amount[hits_Accel19] = 57
	ITC$LL_Time[hits_Accel19] = 14

hits_Accel20<-grep('Accel20.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel20] = 33
	ITC$SS_Time[hits_Accel20] = 14
	ITC$LL_Amount[hits_Accel20] = 43
	ITC$LL_Time[hits_Accel20] = 28

hits_Accel21<-grep('Accel21.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel21] = 31
	ITC$SS_Time[hits_Accel21] = 0
	ITC$LL_Amount[hits_Accel21] = 40
	ITC$LL_Time[hits_Accel21] = 28
	
hits_Accel22<-grep('Accel22.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel22] = 56
	ITC$SS_Time[hits_Accel22] = 14
	ITC$LL_Amount[hits_Accel22] = 73
	ITC$LL_Time[hits_Accel22] = 42

hits_Accel23<-grep('Accel23.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel23] = 42
	ITC$SS_Time[hits_Accel23] = 0
	ITC$LL_Amount[hits_Accel23] = 55
	ITC$LL_Time[hits_Accel23] = 3
	
hits_Accel24<-grep('Accel24.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel24] = 16
	ITC$SS_Time[hits_Accel24] = 14
	ITC$LL_Amount[hits_Accel24] = 21
	ITC$LL_Time[hits_Accel24] = 17

hits_Accel25<-grep('Accel25.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel25] = 56
	ITC$SS_Time[hits_Accel25] = 0
	ITC$LL_Amount[hits_Accel25] = 85
	ITC$LL_Time[hits_Accel25] = 14

hits_Accel26<-grep('Accel26.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel26] = 22
	ITC$SS_Time[hits_Accel26] = 14
	ITC$LL_Amount[hits_Accel26] = 33
	ITC$LL_Time[hits_Accel26] = 28
	
hits_Accel27<-grep('Accel27.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel27] = 24
	ITC$SS_Time[hits_Accel27] = 0
	ITC$LL_Amount[hits_Accel27] = 36
	ITC$LL_Time[hits_Accel27] = 28

hits_Accel28<-grep('Accel28.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel28] = 45
	ITC$SS_Time[hits_Accel28] = 14
	ITC$LL_Amount[hits_Accel28] = 68
	ITC$LL_Time[hits_Accel28] = 42
	
hits_Accel29<-grep('Accel29.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel29] = 54
	ITC$SS_Time[hits_Accel29] = 0
	ITC$LL_Amount[hits_Accel29] = 81
	ITC$LL_Time[hits_Accel29] = 3

hits_Accel30<-grep('Accel30.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel30] = 36
	ITC$SS_Time[hits_Accel30] = 14
	ITC$LL_Amount[hits_Accel30] = 53
	ITC$LL_Time[hits_Accel30] = 17

hits_Accel31<-grep('Accel31.gif',ITC$stim)
	ITC$SS_Amount[hits_Accel31] = 42
	ITC$SS_Time[hits_Accel31] = 0
	ITC$LL_Amount[hits_Accel31] = 33
	ITC$LL_Time[hits_Accel31] = 28

#unique(ITC$SS_Amount)
#unique(ITC$LL_Amount)
#unique(ITC$SS_Time)
#unique(ITC$LL_Time)





# add variable that codes choice as SS=0, LL=1 (irrespective of frame)

ITC$choice_SS0_LL1 <- NA

# find delay and accel trials, respectively
delay_hits <- grep("dutch_delay/Del|dutch_practice/delay_practice",ITC$stim)
accel_hits <- grep("dutch_accel/Accel|dutch_practice/accel_practice",ITC$stim)

length(delay_hits) # 1056
length(accel_hits) # 0, good (for 2014)


# find Delay SS choices (i.e., q responses)
hits_Delay_SS_choice<-which(ITC$Frame == "delay" & ITC$resp == 'q')

# find Delay LL choices (i.e., p responses)
hits_Delay_LL_choice<-which(ITC$Frame == "delay" & ITC$resp == 'p')

length(hits_Delay_SS_choice) # 472
length(hits_Delay_LL_choice) # 584 --> good, pretty equally often

# recode them with 0 and 1 respectively
ITC$choice_SS0_LL1[hits_Delay_SS_choice] <- 0
ITC$choice_SS0_LL1[hits_Delay_LL_choice] <- 1




# find Accel SS choices (i.e., p responses)
hits_Accel_SS_choice<-which(ITC$Frame == "accelerate" & ITC$resp == 'p')

# find Accel LL choices (i.e., q responses)
hits_Accel_LL_choice<-which(ITC$Frame == "accelerate" & ITC$resp == 'q')

length(hits_Accel_SS_choice) # 0
length(hits_Accel_LL_choice) # 0

# recode them with 0 and 1 respectively
ITC$choice_SS0_LL1[hits_Accel_SS_choice] <- 0
ITC$choice_SS0_LL1[hits_Accel_LL_choice] <- 1


head(ITC)
tail(ITC)

#write a .csv (remember to change file = "...")

write.csv(ITC, file = "~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_preproc_22March2014a.csv", quote = FALSE)

#get rid of practice trials (blocknum == 0)

unique(droplevels(ITC$expname[which(ITC$blocknum == 0)])) # good, all practice

ITC_NoPractTrials <- droplevels(ITC[-which(ITC$blocknum == 0),])


unique(ITC_NoPractTrials$expname)
head(ITC_NoPractTrials)

nrow(ITC_NoPractTrials) #992
nrow(ITC) #1056 (1056-992) --> is 62; sounds about right

write.csv(ITC_NoPractTrials, file = "~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_preproc_NoPracticeTrials_22March2014a.csv", quote = FALSE)

# create reduced data frame

ITC_NoPractTrials_Red <- droplevels(subset(ITC_NoPractTrials, select = -c(ip, cur_hit, submitted, stim, pos)))

head(ITC_NoPractTrials_Red)

unique(ITC_NoPractTrials$blocknum) # 1

write.csv(ITC_NoPractTrials_Red, file = "~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_reduced_forBB_22March2014a.csv", quote = FALSE)

#ITC_NoPractTrials_Red <- read.csv("~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_reduced_forBB_22March2014a.csv")
head(ITC_NoPractTrials_Red)

unique(ITC_NoPractTrials_Red$LL_Time)
