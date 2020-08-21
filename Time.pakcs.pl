%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Time:
%

'Time.getClockTime'('Time.CTime'(CTime)) :- currentClockTime(CTime).

'Time.prim_toCalendarTime'('Time.CTime'(ClockTime),
                       'Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,TZ)) :-
	clocktime2localtime(ClockTime,Year,Month,Day,Hour,Min,Sec,TZ).

'Time.prim_toUTCTime'('Time.CTime'(ClockTime),
                      'Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,0)) :-
	clocktime2utctime(ClockTime,Year,Month,Day,Hour,Min,Sec).

'Time.prim_toClockTime'('Time.CalendarTime'(Year,Month,Day,Hour,Min,Sec,TZ),
                        'Time.CTime'(CTime)) :-
	date2clocktime(Year,Month,Day,Hour,Min,Sec,TZ,CTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

