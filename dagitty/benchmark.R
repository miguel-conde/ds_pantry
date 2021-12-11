library(tidyverse)

library(dagitty)

verisure_dag <- dagitty('dag {
bb="0,0,1,1"
audience [exposure,pos="0.058,0.426"]
booking [pos="0.611,0.519"]
channel [exposure,pos="0.346,0.236"]
daypart [exposure,pos="0.449,0.080"]
lead [pos="0.430,0.536"]
sales [outcome,pos="0.839,0.406"]
sessions [pos="0.275,0.538"]
weekday [exposure,pos="0.561,0.234"]
audience -> sessions
booking -> sales
channel -> audience
channel -> booking
channel -> daypart
channel -> lead
channel -> sales
channel -> sessions
daypart -> audience
lead -> booking
sessions -> lead
weekday -> audience
weekday -> booking
weekday -> daypart
weekday -> lead
weekday -> sales
weekday -> sessions
}
')

plot(verisure_dag)

dagitty::impliedConditionalIndependencies(verisure_dag, type = "missing.edge")
dagitty::impliedConditionalIndependencies(verisure_dag, type = "basis")
dagitty::impliedConditionalIndependencies(verisure_dag, type = "all.pairs")

simple_verisure_dag <- dagitty('dag {
bb="0,0,1,1"
audience [exposure,pos="0.058,0.426"]
channel [exposure,pos="0.346,0.236"]
daypart [exposure,pos="0.449,0.080"]
sales [outcome,pos="0.839,0.406"]
weekday [exposure,pos="0.561,0.234"]
audience -> sales
channel -> audience
channel -> daypart
channel -> sales
daypart -> audience
weekday -> audience
weekday -> daypart
weekday -> sales
}')

plot(simple_verisure_dag)

dagitty::impliedConditionalIndependencies(simple_verisure_dag, type = "missing.edge")
dagitty::impliedConditionalIndependencies(simple_verisure_dag, type = "basis")
dagitty::impliedConditionalIndependencies(simple_verisure_dag, type = "all.pairs")
