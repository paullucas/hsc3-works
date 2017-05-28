sdr "/Users/paul/Producing/may9th-2017/a20s/" "f" [1..4]
sdr "/home/plll/Producing/may9th-2017/a20s/" "f" [1..4]

g
s1 <- c7t 1 0.07 0.3 10 1 15 40
s2 <- c7t 2 0.07 0.9 10 1 15 40
s3 <- c7t 2 0.2 1.2 8 1 5 40
s4 <- c7t 2 0.18 1.525 7 1 5 40
c s4 "fs" 1.7
c s4 "fs" 2.1
c s4 "fs" 0.8
c s1 "fs" 0.27
c s1 "fs" 0.26
c s2 "fs" 0.93
c s3 "fs" 1.245
s5 <- c6s 72 0.4 40
c s1 "fs" 4
c s2 "fs" 3
c s1 "g" 0
c s3 "fs" 0.1
c s4 "fs" 0.12
c s5 "g" 0
c s2 "g" 0
c s3 "g" 0
c s4 "g" 0  
f
