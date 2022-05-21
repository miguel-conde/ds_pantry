# Data Wrangling in R - Chapter 9 Date & Time 
#   - https://wrangle-r.rsquaredacademy.com/date-and-time-in-r.html
# R for Data Science - 16 Dates and Times
#   - lubridate: https://r4ds.had.co.nz/dates-and-times.html
# Comprehensive Date-Time Handling for R
#   - clock: https://www.tidyverse.org/blog/2021/03/clock-0-1-0/


# POSIX -------------------------------------------------------------------

# Lo que siempre se almacena es el número de segundos hasta 1970-01-01 GMT
as.POSIXct(0, origin = "1970-01-01 00:00:00") %>% as.numeric()
0 + as.numeric(as.POSIXct("1970-01-01", tz = "GMT"))
as.POSIXct(0, origin = "1990-01-01") %>% as.numeric()
0 + as.numeric(as.POSIXct("1990-01-01 00:00:00", tz = "GMT"))

# Al crear un objeto POSIX toma como origen la fecha origin (aqui 1970-01-01)
# EN LA ZONA HORARIA GMT
# pero le asocia la zona horaria del sistema si no decimos otra cosa
as.POSIXct(0, origin = "1970-01-01")
as.POSIXct(0, origin = "1970-01-01") %>% as.numeric()
as.POSIXct(0, origin = "1970-01-01") %>% attr("tzone")

# O la que le digamos:
as.POSIXct(0, origin = "1970-01-01", tz = "Europe/Madrid")
as.POSIXct(0, origin = "1970-01-01", tz = "Europe/Madrid") %>% as.numeric()
as.POSIXct(0, origin = "1970-01-01", tz = "Europe/Madrid") %>% attr("tzone")

# La tz es solo para 'imprimir', para expresar el número como una fecha en una zona u otra
# (Primero convierte el número a fecha-hora GMT a partir del origen 1970-01-01 GMT
# y luego la modifica para pasar de GMT a la tz)
as.POSIXct(0, origin = "1970-01-01", tz = "America/New_York")
as.POSIXct(0, origin = "1970-01-01", tz = "America/New_York") %>% as.numeric()
as.POSIXct(0, origin = "1970-01-01", tz = "America/New_York") %>% attr("tzone")

# Ojo a esto !!!!
as.POSIXct(-3600, origin = "1970-01-01 00:00:00", tz = "Europe/Madrid")
as.POSIXct(-3600, origin = "1970-01-01 00:00:00", tz = "Europe/Madrid") %>% base::as.Date()
as.POSIXct(-3600, origin = "1970-01-01 00:00:00", tz = "Europe/Madrid") %>% lubridate::as_date()


# Si paso un string no necesito origin, la guardará como el número de segundos
# desde 1970-01-01 00:00:00 GMT
as.POSIXct("2022-05-19 00:00:00", tz = "Europe/Madrid")
as.POSIXct("2022-05-19 00:00:00", tz = "Europe/Madrid") %>% base::as.Date()
as.POSIXct("2022-05-19 00:00:00", tz = "Europe/Madrid") %>% lubridate::as_date()

as.POSIXct("2022-05-19 00:00:00", tz = "Europe/Madrid") %>% hms::as_hms()

# CLOCK -------------------------------------------------------------------

library(clock)
library(nycflights13)
library(tidyverse)
library(lubridate)

# First steps -------------------------------------------------------------

# get_*(): Get a component
# 
# set_*(): Set a component
# 
# add_*(): Add a unit of time
# 
# date_*(): General date manipulation

flights

# date_build()
flights2 <- flights %>%
  select(year, month, day, dep_time, dep_delay) %>% 
  mutate(
    date = date_build(year, month, day), 
    .keep = "unused", 
    .before = 1
  )

flights2

mutate(flights2, year = get_year(date), month = get_month(date))

# date_group()
flights2 %>%
  mutate(date = date_group(date, "month")) %>%
  group_by(date) %>%
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop")

# Formatting date_format()) and parsing ( date_parse() and date_time_parse())


# Invalid dates -----------------------------------------------------------

# Lubridate:
mutate(flights2, date2 = date + months(1)) %>% 
  filter(date %in% as_date(c("2013-01-29", "2013-01-30")))

# Clock:
mutate(flights2, date2 = add_months(date, 1)) %>% 
  filter(date %in% as_date(c("2013-01-29", "2013-01-30")))

problems <- flights2 %>% 
  filter(date %in% as_date(c("2013-01-29", "2013-01-30"))) %>%
  select(date) %>% 
  distinct()

problems %>%
  mutate(
    date2 = add_months(date, 1, invalid = "previous"),
    date3 = add_months(date, 1, invalid = "next"),
    date4 = add_months(date, 1, invalid = "overflow"),
    date5 = add_months(date, 1, invalid = "NA")
  )


# Daylight saving time ----------------------------------------------------

flights_hm <- flights2 %>%
  select(date, dep_time) %>%
  mutate(
    hour = dep_time %/% 100L,
    minute = dep_time %% 100L,
    .keep = "unused"
  )

head(flights_hm, n = 2)

# We’d like to be able to add this time of day information to our date column. 
# This flight information was recorded in the America/New_York time zone, so our 
# resulting date-time should have that time zone as well. However, converting 
# Date -> POSIXct will always assume that Date starts as UTC, rather than being 
# naive to any time zones, and the result will use your system’s local time zone. 
# This can have unintended side effects:

# My local time zone is actually America/New_York.
# The conversion to POSIXct retains the underlying UTC instant, but
# the printed time changes unexpectedly, showing the equivalent time
# in the local time zone.
flights_hm %>%
  select(date) %>%
  mutate(
    datetime = as.POSIXct(date),
    datetime_utc = date_set_zone(datetime, "UTC")
  ) %>%
  head(n = 3)

# To get what we want, we need to convince the date column to “forget” that it 
# is UTC, then add on the America/New_York time zone. With clock, we’ll do this by 
# going through a new intermediate type called naive-time, a date-time type with a 
# yet-to-be-specified time zone. The ability to separate a date-time from its 
# associated time zone is one of clock’s most powerful features, which we’ll 
# explore more in the Time Points section below. For now, the important thing is 
# that this retains the printed time as we expected.
flights_dt <- flights_hm %>%
  mutate(
    datetime = as.POSIXct(as_naive_time(date), "America/New_York"),
    .keep = "unused",
    .before = 1
  )

flights_dt

flights_dt <- flights_dt %>%
  mutate(
    datetime = datetime %>%
      add_hours(hour) %>%
      add_minutes(minute),
    .keep = "unused"
  )

flights_dt


# Now assume that we want to add two days to this datetime column, again to 
# construct some forward looking variable.
flights_dt_lubridate <- flights_dt %>%
  # mutate(datetime = force_tz(datetime, "America/New_York")) %>% 
  filter((datetime %within% interval("2013-03-08 01:00:00", "2013-03-08 03:00:00", tz = "America/New_York"))) %>% 
  mutate(datetime2 = datetime + days(2))

# Now with clock:
flights_dt %>%
  # mutate(datetime = force_tz(datetime, "America/New_York")) %>%
  filter((datetime %within% 
            interval("2013-03-08 01:00:00", "2013-03-08 03:00:00", tz = "America/New_York"))) %>%
  mutate(datetime2 = add_days(datetime, 2))

flights_dt_lubridate[6,]

# As it turns out, in the America/New_York time zone, on 2013-03-10 the clocks 
# jumped forward from 01:59:59 -> 03:00:00, creating a daylight saving time gap, 
# and a nonexistent 2 o’clock hour. By adding two days, we’ve landed right in that 
# gap (at 02:23:00). With nonexistent times like this, lubridate silently returns 
# NA, while clock errors.

problem <- flights_dt_lubridate$datetime[6]
problem

# 02:23:00 -> 03:00:00
add_days(problem, 2, nonexistent = "roll-forward")

# 02:23:00 -> 01:59:59
add_days(problem, 2, nonexistent = "roll-backward")

# 02:23:00 -> 03:23:00
add_days(problem, 2, nonexistent = "shift-forward")

# 02:23:00 -> 01:23:00
add_days(problem, 2, nonexistent = "shift-backward")

# 02:23:00 -> NA
add_days(problem, 2, nonexistent = "NA")

# I recommend "roll-forward" or "roll-backward", as these retain the relative 
# ordering of datetime, an issue that you can read about here.

# There are another class of daylight saving time issues related to ambiguous 
# times. These generally result from daylight saving fallbacks, where your clock 
# might show two 1 AM hours. You resolve them in a similar way to what was done 
# with nonexistent times. If you’re interested, you can read more about ambiguous 
# times here.
