using DataFrames

data = readtable("/home/john/stats_corner/fantasy_football/transpose2015.csv",header = false)

Pkg.update()
Pkg.add("JuMP")

using JuMP
Pkg.add("Cbc")
using Cbc
using MathProgBase
m2015 = Model(solver = CbcSolver())
@variable(m2015, x[1:3204], Bin)

mult18 = [(i % 18 == 0) for i in 1:size(x)[1]]

@objective(m2015,
Min,
sum(Array(data[:,[(i % 18 == 0) for i in 1:size(data)[2]]])*getindex(x,mult18)))

# Draft only 15 players (16 w/ HC)
@constraint(m2015, sum(getindex(x,mult18)) == roster_size)

target_ppw = 120

mult1  = [(i % 18 == 1) for i in 1:size(x)[1]]
mult2  = [(i % 18 == 2) for i in 1:size(x)[1]]
mult3  = [(i % 18 == 3) for i in 1:size(x)[1]]
mult4  = [(i % 18 == 4) for i in 1:size(x)[1]]
mult5  = [(i % 18 == 5) for i in 1:size(x)[1]]
mult6  = [(i % 18 == 6) for i in 1:size(x)[1]]
mult7  = [(i % 18 == 7) for i in 1:size(x)[1]]
mult8  = [(i % 18 == 8) for i in 1:size(x)[1]]
mult9  = [(i % 18 == 9) for i in 1:size(x)[1]]
mult10 = [(i % 18 == 10) for i in 1:size(x)[1]]
mult11 = [(i % 18 == 11) for i in 1:size(x)[1]]
mult12 = [(i % 18 == 12) for i in 1:size(x)[1]]
mult13 = [(i % 18 == 13) for i in 1:size(x)[1]]
mult14 = [(i % 18 == 14) for i in 1:size(x)[1]]
mult15 = [(i % 18 == 15) for i in 1:size(x)[1]]
mult16 = [(i % 18 == 16) for i in 1:size(x)[1]]

@constraint(m2015, sum(Array(data[:,[(i % 18 == 1)  for i in 1:size(data)[2]]])*getindex(x,mult1))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 2)  for i in 1:size(data)[2]]])*getindex(x,mult2))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 3)  for i in 1:size(data)[2]]])*getindex(x,mult3))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 4)  for i in 1:size(data)[2]]])*getindex(x,mult4))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 5)  for i in 1:size(data)[2]]])*getindex(x,mult5))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 6)  for i in 1:size(data)[2]]])*getindex(x,mult6))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 7)  for i in 1:size(data)[2]]])*getindex(x,mult7))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 8)  for i in 1:size(data)[2]]])*getindex(x,mult8))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 9)  for i in 1:size(data)[2]]])*getindex(x,mult9))  >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 10) for i in 1:size(data)[2]]])*getindex(x,mult10)) >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 11) for i in 1:size(data)[2]]])*getindex(x,mult11)) >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 12) for i in 1:size(data)[2]]])*getindex(x,mult12)) >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 13) for i in 1:size(data)[2]]])*getindex(x,mult13)) >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 14) for i in 1:size(data)[2]]])*getindex(x,mult14)) >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 15) for i in 1:size(data)[2]]])*getindex(x,mult15)) >= target_ppw)
@constraint(m2015, sum(Array(data[:,[(i % 18 == 16) for i in 1:size(data)[2]]])*getindex(x,mult16)) >= target_ppw)

for i in [getindex(1:3204,mult18)]
    for j in 2:17
        @constraint(m2015, x[i-j] .<= x[i])
    end
end

# Position limits by starting lineups

# But the total number of starters is 1 + 2 + 2 + 3 + 1 = 9
@constraint(m2015, sum(getindex(x,mult1)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult3)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult2)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult4)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult5)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult6)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult7)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult8)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult9)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult10)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult11)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult12)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult13)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult14)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult15)) <= starting_lineup)
@constraint(m2015, sum(getindex(x,mult16)) <= starting_lineup)


## Position Limits By Week
dst1  = [(i % 18 == 1)  for i in 1:(18*13)]
dst2  = [(i % 18 == 2)  for i in 1:(18*13)]
dst3  = [(i % 18 == 3)  for i in 1:(18*13)]
dst4  = [(i % 18 == 4)  for i in 1:(18*13)]
dst5  = [(i % 18 == 5)  for i in 1:(18*13)]
dst6  = [(i % 18 == 6)  for i in 1:(18*13)]
dst7  = [(i % 18 == 7)  for i in 1:(18*13)]
dst8  = [(i % 18 == 8)  for i in 1:(18*13)]
dst9  = [(i % 18 == 9)  for i in 1:(18*13)]
dst10 = [(i % 18 == 10) for i in 1:(18*13)]
dst11 = [(i % 18 == 11) for i in 1:(18*13)]
dst12 = [(i % 18 == 12) for i in 1:(18*13)]
dst13 = [(i % 18 == 13) for i in 1:(18*13)]
dst14 = [(i % 18 == 14) for i in 1:(18*13)]
dst15 = [(i % 18 == 15) for i in 1:(18*13)]
dst16 = [(i % 18 == 16) for i in 1:(18*13)]


dst_start = 1
dst_end = 13*18


@constraint(m2015, sum(getindex(x[1:(18*13)],dst1))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst2))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst3))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst4))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst5))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst6))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst7))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst8))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst8))  == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst10)) == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst11)) == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst12)) == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst13)) == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst14)) == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst15)) == 1)
@constraint(m2015, sum(getindex(x[1:(18*13)],dst16)) == 1)

k_start = 18*13 + 1
k_end = 18*13 + 18*12

k1  = [(i % 18 == 1)  for i in k_start:k_end]
k2  = [(i % 18 == 2)  for i in k_start:k_end]
k3  = [(i % 18 == 3)  for i in k_start:k_end]
k4  = [(i % 18 == 4)  for i in k_start:k_end]
k5  = [(i % 18 == 5)  for i in k_start:k_end]
k6  = [(i % 18 == 6)  for i in k_start:k_end]
k7  = [(i % 18 == 7)  for i in k_start:k_end]
k8  = [(i % 18 == 8)  for i in k_start:k_end]
k9  = [(i % 18 == 9)  for i in k_start:k_end]
k10 = [(i % 18 == 10) for i in k_start:k_end]
k11 = [(i % 18 == 11) for i in k_start:k_end]
k12 = [(i % 18 == 12) for i in k_start:k_end]
k13 = [(i % 18 == 13) for i in k_start:k_end]
k14 = [(i % 18 == 14) for i in k_start:k_end]
k15 = [(i % 18 == 15) for i in k_start:k_end]
k16 = [(i % 18 == 16) for i in k_start:k_end]



@constraint(m2015, sum(getindex(x[k_start:k_end],k1))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k2))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k3))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k4))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k5))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k6))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k7))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k8))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k9))  == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k10)) == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k11)) == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k12)) == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k13)) == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k14)) == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k15)) == 1)
@constraint(m2015, sum(getindex(x[k_start:k_end],k16)) == 1)

qb_start = 18*(13+12) + 1
qb_end = 18*(13+12) + 18*33

qb1  = [(i % 18 == 1)  for i in qb_start:qb_end]
qb2  = [(i % 18 == 2)  for i in qb_start:qb_end]
qb3  = [(i % 18 == 3)  for i in qb_start:qb_end]
qb4  = [(i % 18 == 4)  for i in qb_start:qb_end]
qb5  = [(i % 18 == 5)  for i in qb_start:qb_end]
qb6  = [(i % 18 == 6)  for i in qb_start:qb_end]
qb7  = [(i % 18 == 7)  for i in qb_start:qb_end]
qb8  = [(i % 18 == 8)  for i in qb_start:qb_end]
qb9  = [(i % 18 == 9)  for i in qb_start:qb_end]
qb10 = [(i % 18 == 10) for i in qb_start:qb_end]
qb11 = [(i % 18 == 11) for i in qb_start:qb_end]
qb12 = [(i % 18 == 12) for i in qb_start:qb_end]
qb13 = [(i % 18 == 13) for i in qb_start:qb_end]
qb14 = [(i % 18 == 14) for i in qb_start:qb_end]
qb15 = [(i % 18 == 15) for i in qb_start:qb_end]
qb16 = [(i % 18 == 16) for i in qb_start:qb_end]



@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb1))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb2))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb3))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb4))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb5))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb6))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb7))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb8))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb9))  <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb10)) <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb11)) <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb12)) <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb13)) <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb14)) <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb15)) <= 2)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb16)) <= 2)

rb_start = 18*(13+12+33) + 1
rb_end = 18*(13+12+33) + 18*51

rb1  = [(i % 18 == 1)  for i in rb_start:rb_end]
rb2  = [(i % 18 == 2)  for i in rb_start:rb_end]
rb3  = [(i % 18 == 3)  for i in rb_start:rb_end]
rb4  = [(i % 18 == 4)  for i in rb_start:rb_end]
rb5  = [(i % 18 == 5)  for i in rb_start:rb_end]
rb6  = [(i % 18 == 6)  for i in rb_start:rb_end]
rb7  = [(i % 18 == 7)  for i in rb_start:rb_end]
rb8  = [(i % 18 == 8)  for i in rb_start:rb_end]
rb9  = [(i % 18 == 9)  for i in rb_start:rb_end]
rb10 = [(i % 18 == 10) for i in rb_start:rb_end]
rb11 = [(i % 18 == 11) for i in rb_start:rb_end]
rb12 = [(i % 18 == 12) for i in rb_start:rb_end]
rb13 = [(i % 18 == 13) for i in rb_start:rb_end]
rb14 = [(i % 18 == 14) for i in rb_start:rb_end]
rb15 = [(i % 18 == 15) for i in rb_start:rb_end]
rb16 = [(i % 18 == 16) for i in rb_start:rb_end]



@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb1))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb2))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb3))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb4))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb5))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb6))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb7))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb8))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb9))  <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb10)) <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb11)) <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb12)) <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb13)) <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb14)) <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb15)) <= 3)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb16)) <= 3)

te_start = 18*(13+12+33+51) + 1
te_end = 18*(13+12+33+51) + 18*15

te1  = [(i % 18 == 1)  for i in te_start:te_end]
te2  = [(i % 18 == 2)  for i in te_start:te_end]
te3  = [(i % 18 == 3)  for i in te_start:te_end]
te4  = [(i % 18 == 4)  for i in te_start:te_end]
te5  = [(i % 18 == 5)  for i in te_start:te_end]
te6  = [(i % 18 == 6)  for i in te_start:te_end]
te7  = [(i % 18 == 7)  for i in te_start:te_end]
te8  = [(i % 18 == 8)  for i in te_start:te_end]
te9  = [(i % 18 == 9)  for i in te_start:te_end]
te10 = [(i % 18 == 10) for i in te_start:te_end]
te11 = [(i % 18 == 11) for i in te_start:te_end]
te12 = [(i % 18 == 12) for i in te_start:te_end]
te13 = [(i % 18 == 13) for i in te_start:te_end]
te14 = [(i % 18 == 14) for i in te_start:te_end]
te15 = [(i % 18 == 15) for i in te_start:te_end]
te16 = [(i % 18 == 16) for i in te_start:te_end]



@constraint(m2015, sum(getindex(x[te_start:te_end],te1))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te2))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te3))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te4))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te5))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te6))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te7))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te8))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te9))  <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te10)) <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te11)) <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te12)) <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te13)) <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te14)) <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te15)) <= 2)
@constraint(m2015, sum(getindex(x[te_start:te_end],te16)) <= 2)

wr_start = 18*(13+12+33+51+15) + 1
wr_end = 18*(13+12+33+51+15) + 18*54

wr1  = [(i % 18 == 1)  for i in wr_start:wr_end]
wr2  = [(i % 18 == 2)  for i in wr_start:wr_end]
wr3  = [(i % 18 == 3)  for i in wr_start:wr_end]
wr4  = [(i % 18 == 4)  for i in wr_start:wr_end]
wr5  = [(i % 18 == 5)  for i in wr_start:wr_end]
wr6  = [(i % 18 == 6)  for i in wr_start:wr_end]
wr7  = [(i % 18 == 7)  for i in wr_start:wr_end]
wr8  = [(i % 18 == 8)  for i in wr_start:wr_end]
wr9  = [(i % 18 == 9)  for i in wr_start:wr_end]
wr10 = [(i % 18 == 10) for i in wr_start:wr_end]
wr11 = [(i % 18 == 11) for i in wr_start:wr_end]
wr12 = [(i % 18 == 12) for i in wr_start:wr_end]
wr13 = [(i % 18 == 13) for i in wr_start:wr_end]
wr14 = [(i % 18 == 14) for i in wr_start:wr_end]
wr15 = [(i % 18 == 15) for i in wr_start:wr_end]
wr16 = [(i % 18 == 16) for i in wr_start:wr_end]



@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr1))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr2))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr3))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr4))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr5))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr6))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr7))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr8))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr9))  <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr10)) <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr11)) <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr12)) <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr13)) <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr14)) <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr15)) <= 4)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr16)) <= 4)

# 2 keepers
mult17 = [(i % 18 == 17) for i in 1:size(x)[1]]
@constraint(m2015, sum(getindex(x,mult17)) <= num_keepers)


dst18 = [(i % 18 == 0) for i in dst_start:dst_end]
k18   = [(i % 18 == 0) for i in k_start:k_end]
qb18  = [(i % 18 == 0) for i in qb_start:qb_end]
te18  = [(i % 18 == 0) for i in te_start:te_end]
wr18  = [(i % 18 == 0) for i in wr_start:wr_end]
rb18  = [(i % 18 == 0) for i in rb_start:rb_end]


@constraint(m2015, sum(getindex(x[dst_start:dst_end],dst18))  <= dst_draft_limit)
@constraint(m2015, sum(getindex(x[k_start:k_end],k18))  <= k_draft_limit)
@constraint(m2015, sum(getindex(x[qb_start:qb_end],qb18))  <= qb_draft_limit)
@constraint(m2015, sum(getindex(x[te_start:te_end],te18))  <= te_draft_limit)
@constraint(m2015, sum(getindex(x[wr_start:wr_end],wr18))  <= wr_draft_limit)
@constraint(m2015, sum(getindex(x[rb_start:rb_end],rb18))  <= rb_draft_limit)

solve(m2015)

x_sol = getvalue(x)


for j in 1:178
     if getindex(x_sol,mult18)[j] == 1
         println(j)
    end
end

lookup = readtable("/home/john/stats_corner/fantasy_football/lookup2015.csv",header = false)


for j in 1:178
     if getindex(x_sol,mult18)[j] == 1
         println(lookup[j,:])
    end
end


# Verify all the conditions are met
