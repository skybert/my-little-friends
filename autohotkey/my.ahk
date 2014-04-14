; Replace caps with ctrl
Capslock::Ctrl

; Toggle maximize with Ctrl-shift-m
^+m::
   WinGet MX, MinMax, A

   If MX
        WinRestore A
   Else
	WinMaximize A
return
