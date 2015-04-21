; Replace caps with ctrl
Capslock::Ctrl

; Toggle maximize with Ctrl-shift-m. When in fullscreen, turn off window decorations
^+m::
   WinGet MX, MinMax, A

   If (MX) {
        WinSet, Style, +0xC00000, A
        WinRestore A
   }
   Else {
        WinSet, Style, -0xC00000, A
        WinMaximize A
   }
return
