(* Date -- SML Basis Library *)

datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

datatype month = Jan | Feb | Mar | Apr | May | Jun
               | Jul | Aug | Sep | Oct | Nov | Dec

type date

exception Date

val date : {
             year   : int,              (* e.g. 1999                     *)
             month  : month,            (* Jan, Feb, ...                 *)
             day    : int,              (* 1-31                          *)
             hour   : int,              (* 0-23                          *)
             minute : int,              (* 0-59                          *)
             second : int,              (* 0-61, permitting leap seconds *)
             offset : Time.time option  (* time zone west of UTC         *)
           } -> date

val year       : date -> int
val month      : date -> month
val day        : date -> int
val hour       : date -> int
val minute     : date -> int
val second     : date -> int
val weekDay    : date -> weekday
val yearDay    : date -> int
val isDst      : date -> bool option
val offset     : date -> Time.time option

val compare    : date * date -> order

val toString   : date -> string
val fmt        : string -> date -> string
val fromString : string -> date option
val scan       : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader

val fromTimeLocal : Time.time -> date
val fromTimeUniv  : Time.time -> date
val toTime        : date -> Time.time
val localOffset   : unit -> Time.time

(* 
   These functions convert times to dates and vice versa, and format
   and scan dates.

   [date] is the type of points in time in a given time zone.  If the
   offset is NONE, then the date is in the local time zone.  If the
   offset is SOME t, then t is the offset of the main timezone
   (ignoring daylight savings time) west of UTC.
        When 0 hours <= t < 12 hours, the represented time is to the
   west of UTC and the local time is UTC-t.  
        When 12 hours <= t < 23 hours, the represented time is to the
   East of UTC and the local time is UTC+(24-t).

   [date { year, month, day, hour, minute, second, offset }] returns a
   canonical date value.  Seconds outside the range 0..59 are
   converted to the equivalent minutes and added to the minutes
   argument; leap seconds are ignored.  Similarly, excess minutes are
   converted to hours, hours to days, days to months, and months to
   years.  Then the weekday and day number in the year are computed.
   Leap years are assumed in accordance with the Gregorian calendar,
   for any year after year 0 A.D.

   If the offset is greater than one day (24 hours), then the excess
   days are added to the days, and the offset modulo 24 hours is used.

   [year dt] returns the year of dt, e.g. 1999.

   [month dt] returns the month of dt.

   [day dt] returns the day of dt

   [hour dt] returns the hour of dt.

   [minute dt] returns the minute of dt.

   [second dt] returns the second of dt.

   [weekDay dt] returns the weekday of dt.

   [yearDay dt] returns the number of the day in the year of dt.
   January 1 is day 0, and December 31 is day 364 (and 365 in leap years).

   [isDst dt] returns SOME(true) if daylight savings time is in effect
   at the date dt; returns SOME(false) if not; and returns NONE if
   this information is unavailable.

   [offset dt] returns NONE if the date dt is in the local time zone;
   returns SOME t where t is the offset west of UTC otherwise.  Thus
   SOME(Time.zeroTime) is UTC.

   [compare(dt1, dt2)] returns LESS, EQUAL, or GREATER, according as
   date dt1 precedes, equals, or follows dt2 in time.
   Lexicographically compares the dates.  Ignores timezone offset and
   DST.  Does not detect invalid dates.

   [toString dt] returns a 24 character string representing the date dt
   in the following format:   
                       Wed Mar  8 19:06:45 1995
   The result may be wrong if the date is not representable as a
   Time.time value.  Raises Date if dt is an invalid date.
   Corresponds to the ANSI C function `asctime'.

   [fmt fmtstr dt] formats the date dt according to the format string
   fmtstr.  The format string has the same meaning as with the ANSI C
   function `strftime'.  These ANSI C format codes should work on all
   platforms:

      %a  abbreviated weekday name (e.g. "Mon")
      %A  full weekday name (e.g. "Monday")
      %b  abbreviated month name (e.g. "Oct")
      %B  full month name (e.g. "October")
      %c  date and time (e.g. "Dec  2 06:55:15 1979")
      %d  day of month (01..31)
      %H  hour (00..23)
      %I  hour (01..12)
      %j  day of year (001..366)
      %m  month number (01..12)
      %M  minutes (00..59)
      %p  locale's equivalent of a.m./p.m.
      %S  seconds (00..61, allowing for leap seconds)
      %U  week number (00..53), with Sunday as the first day of week 01
      %w  day of week, with 0 representing Sunday (0..6)
      %W  week number (00..53), with Monday as the first day of week 01
      %x  locale's appropriate date representation
      %y  year of century (00..99)
      %Y  year including century (e.g. 1997)
      %Z  time zone name if it exists; otherwise the empty string
      %%  the percent character   

   Example: The current local date in ISO format (e.g. 1998-04-06) can
   be obtained by using: 
        fmt "%Y-%m-%d" (fromTimeLocal (Time.now ()))

   [fromString s] scans a 24-character date from the string s, after
   possible initial whitespace (blanks, tabs, newlines).  The format
   of the string must be as produced by toString.  The fields isDst
   and offset in the resulting date will be NONE.  No check of the
   consistency of the date (weekday, date in the month, ...) is
   performed. 

   [scan getc src] scans a 24-character date from the stream src,
   using the stream accessor getc.  Otherwise works as fromString.  In
   case of success, returns SOME(date, rst) where date is the scanned
   date and rst is the remainder of the stream; otherwise returns
   NONE.

   [fromTimeLocal t] returns the local date at (UTC) time t.  The
   resulting date will have offset = NONE.  The fields year, month,
   day, hour, minute, and second are as expected.  The resulting isDst
   may be NONE if the system cannot determine whether daylight savings
   time is in effect at the given time.  Corresponds to the ANSI C
   function `localtime'.

   [fromTimeUniv t] is similar to fromTime, but returns the UTC date
   at (UTC) time t.  The resulting date will have offset = SOME
   Time.zeroTime.  Corresponds to the ANSI C function `gmtime'.

   [toTime dt] returns the (UTC) time corresponding to the date dt.
   Uses the isDst time field if it is present (SOME _) and cannot be
   calculated from the given date.  May raise Date if the given date
   is invalid.  Raises Time.Time if the Date cannot be represented as
   a Time.time value.  At least the dates in the interval 1970-2030
   can be represented as Time.time values.  Corresponds to the ANSI C
   function `mktime'.

   [localOffset ()] is the local time zone offset west of UTC.  
   It holds that 0 hours <= localOffset () < 24 hours.
*)
