
abicot0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABICOT0.xlsx"
  )
)
abicot0$group <- "ABI"
abicot0$condition <- "co"
abicot0$time <- 0
dim(abicot0)
names(abicot0)

col_names <- c(
    "NUMERO"      , "CODICE"      , "ETA'"        , "SESSO"       , "SCOLARITA'"  , "LESIONE"     , "ANNI DL"    
  , "ADL"         , "IADL"        , "Dass-DEP"    , "Dass-ANX"    , "Dass-STRESS" , "Dass-TOT"    , "SDMT-GR"    
  , "SDMT-CR"     , "PSQI"        , "SWLS"        , "BMQP-GR"     , "BMQP-CR"     , "BMQF-GR"     , "BMQF-CR"    
  , "BREQP-GR"    , "BREQP-CR"    , "BREQF-GR"    , "BREQF-CR"    , "BSCQP-GR"    , "BSCQP-CR"    , "BSCQF-GR"   
  , "BSCQF-CR"    , "BDQP-GR"     , "BDQP-CR"     , "BDQF-GR"     , "BDQF-CR"     , "BIQP-GR"     , "BIQP-CR"    
  , "BIQF-GR"     , "BIQF-CR"     , "Panas-P"     , "Panas-N"     , "1AHD-F"      , "1AHD-D"      , "1AHD-I"     
  , "2AHD-F"      , "2AHD-D"      , "2AHD-I"      , "3AHD-F"      , "3AHD-D"      , "3AHD-I"      , "4AHD-F"     
  , "4AHD-D"      , "4AHD-I"      , "5AHD-F"      , "5AHD-D"      , "5AHD-I"      , "6AHD-F"      , "6AHD-D"     
  , "6AHD-I"      , "7AHD-F"      , "7AHD-D"      , "7AHD-I"      , "8AHD-F"      , "8AHD-D"      , "8AHD-I"     
  , "1AHF-F"      , "1AHF-D"      , "1AHF-I"      , "2AHF-F"      , "2AHF-D"      , "2AHF-I"      , "3AHF-F"     
  , "3AHF-D"      , "3AHF-I"      , "4AHF-F"      , "4AHF-D"      , "4AHF-I"      , "5AHF-F"      , "5AHF-D"     
  , "5AHF-I"      , "6AHF-F"      , "6AHF-D"      , "6AHF-I"      , "7AHF-F"      , "7AHFD"       , "7AHF-I"     
  , "8AHF-F"      , "8AHF-D"      , "8AHF-I"      , "9AHF-F"      , "9AHF-D"      , "9AHF-I"      , "AHP1"       
  , "AHP2"        , "group"       , "condition"   , "time"       
)

abicot2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABICOT2.xlsx"
  )
)
abicot2$group <- "ABI"
abicot2$condition <- "co"
abicot2$time <- 2
dim(abicot2)
names(abicot2) <- col_names


abispt0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABISPT0.xlsx"
  )
)
abispt0$group <- "ABI"
abispt0$condition <- "sp"
abispt0$time <- 0
dim(abispt0)
names(abispt0)
names(abispt0) <- col_names

abispt1 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABISPT1.xlsx"
  )
)
abispt1$group <- "ABI"
abispt1$condition <- "sp"
abispt1$time <- 1
dim(abispt1)
names(abispt1)


abispt2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABISPT2.xlsx"
  )
)
abispt2$group <- "ABI"
abispt2$condition <- "sp"
abispt2$time <- 2
dim(abispt2)
names(abispt2)
names(abispt2) <- col_names


abu <- bind_cols(
  abicot0, abicot2,
  abispt0, abispt1, abispt2
)






cgcot0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CGCOT0.xlsx"
  )
)
cgcot0$group <- "CG"
cgcot0$condition <- "co"
cgcot0$time <- 0
dim(cgcot0)
names(cgcot0)


cgcot1 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CGCOT1.xlsx"
  )
)
cgcot1$group <- "CG"
cgcot1$condition <- "co"
cgcot1$time <- 1
dim(cgcot1)
names(cgcot1)

cgcot2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CGCOT2.xlsx"
  )
)
cgcot2$group <- "CG"
cgcot2$condition <- "co"
cgcot2$time <- 2
dim(cgcot2)
names(cgcot2)

cgspt0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CGSPT0.xlsx"
  )
)
cgspt0$group <- "CG"
cgspt0$condition <- "sp"
cgspt0$time <- 0
dim(cgspt0)
names(cgspt0)


cgspt1 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CGSPT1.xlsx"
  )
)
cgspt1$group <- "CG"
cgspt1$condition <- "sp"
cgspt1$time <- 1
dim(cgspt1)
names(cgspt1)

cgspt2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CGSPT2.xlsx"
  )
)
cgspt2$group <- "CG"
cgspt2$condition <- "sp"
cgspt2$time <- 1
dim(cgspt2)
names(cgspt2)





thedat <- rbind(
  abicot0, abicot2, 
  abispt0, abispt1, abispt2,
  cgcot0, cgcot1, cgcot2,
  cgspt0, cgspt2, cgspt2
)





