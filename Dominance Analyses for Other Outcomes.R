### ---
### Extra analyses: Dominance analyses for
### other predictors besides Adaptive Performance
### ---

### ---
### Rename other outcome variables
### ---

handlemerg <- NCO_Promotion_Study$HANDLINGEMERGENCIES
interperson <- NCO_Promotion_Study$INTERPERSONALADAPTABILITY
changecond <- NCO_Promotion_Study$UNPREDICTABLEWORKSIT
physicaladapt <- NCO_Promotion_Study$PHYSICALLYORIENTED
learnnew <- NCO_Promotion_Study$LEARNINGNEWTECH
handlestress <- NCO_Promotion_Study$HANDLINGWORKSTRESS
cultadapt <- NCO_Promotion_Study$CULTURALADAPTABILITY
probsolve <- NCO_Promotion_Study$SOLVINGPROBLEMSCREATIVELY

# DA of Competencies on Handling Emergencies #

userDat2 <- data.frame(posenviron,
                       getresults,
                       preplead,
                       devlead,
                       leadbyexp,
                       comms,
                       handlemerg)


userDat3 <- data.frame(
  sapply(
    userDat2,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test2 <- DW.accuracy(userDat3, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test2)

View(test2$avg.weights)


# 2. Competencies with Interperson #

userDat5 <- data.frame(posenviron,
                       getresults,
                       preplead,
                       devlead,
                       leadbyexp,
                       comms,
                       interperson)


userDat4 <- data.frame(
  sapply(
    userDat5,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test3 <- DW.accuracy(userDat4, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test3)

View(test3$avg.weights)

# 3. Competencies with changecond #

userDat7 <- data.frame(posenviron,
                       getresults,
                       preplead,
                       devlead,
                       leadbyexp,
                       comms,
                       changecond)


userDat6 <- data.frame(
  sapply(
    userDat7,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test4 <- DW.accuracy(userDat6, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test4)

View(test4$avg.weights)

# 4. Competencies with physicaladapt #

userDat9 <- data.frame(posenviron,
                       getresults,
                       preplead,
                       devlead,
                       leadbyexp,
                       comms,
                       physicaladapt)


userDat8 <- data.frame(
  sapply(
    userDat9,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test5 <- DW.accuracy(userDat8, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test5)

View(test5$avg.weights)



# 5. Competencies with learnnew #

userDat11 <- data.frame(posenviron,
                        getresults,
                        preplead,
                        devlead,
                        leadbyexp,
                        comms,
                        learnnew)


userDat10 <- data.frame(
  sapply(
    userDat11,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test6 <- DW.accuracy(userDat10, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test6)

View(test6$avg.weights)

# 6. Competencies with handlestress #

userDat13 <- data.frame(posenviron,
                        getresults,
                        preplead,
                        devlead,
                        leadbyexp,
                        comms,
                        handlestress)


userDat12 <- data.frame(
  sapply(
    userDat13,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test7 <- DW.accuracy(userDat12, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test7)

View(test7$avg.weights)


# 7. Competencies with handlestress #

userDat15 <- data.frame(posenviron,
                        getresults,
                        preplead,
                        devlead,
                        leadbyexp,
                        comms,
                        handlestress)


userDat14 <- data.frame(
  sapply(
    userDat15,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test8 <- DW.accuracy(userDat14, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test8)

View(test8$avg.weights)


# 8. Competencies with cultadapt #

userDat17 <- data.frame(posenviron,
                        getresults,
                        preplead,
                        devlead,
                        leadbyexp,
                        comms,
                        cultadapt)


userDat16 <- data.frame(
  sapply(
    userDat17,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test9 <- DW.accuracy(userDat16, 
                     iv.relia = c(.81,.81,.81,.81,.81,.81),
                     dv.relia = .77, 
                     iv.names = c("posenviron",
                                  "getresults",
                                  "preplead",
                                  "devlead",
                                  "leadbyexp",
                                  "comms"),
                     whichCor = 0, 
                     spurIV = T,
                     epsilon = F,
                     n.sims = 100)

print(test9)

View(test9$avg.weights)



# Competencies with probsolve #

userDat19 <- data.frame(posenviron,
                        getresults,
                        preplead,
                        devlead,
                        leadbyexp,
                        comms,
                        probsolve)


userDat18 <- data.frame(
  sapply(
    userDat19,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))





test10 <- DW.accuracy(userDat18, 
                      iv.relia = c(.81,.81,.81,.81,.81,.81),
                      dv.relia = .77, 
                      iv.names = c("posenviron",
                                   "getresults",
                                   "preplead",
                                   "devlead",
                                   "leadbyexp",
                                   "comms"),
                      whichCor = 0, 
                      spurIV = T,
                      epsilon = F,
                      n.sims = 100)

print(test10)

View(test10$avg.weights)