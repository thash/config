#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
#SingleInstance ; allow executing multiple times
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


^h::
  Send, {Left down}
Return
^j::
  Send, {Down down}
Return
^k::
  Send, {Up down}
Return
^l::
  Send, {Right down}
Return

^e::
  Send, {End down}
Return

^a::
  Send, {Home down}
Return

^m::
  Send, {Enter down}
Return

^u::
  Send, {Backspace down}
Return

Enter::
  Send, {: down}
Return
+Enter::
  Send, {; down}
Return

; Muhenkan
LAlt::
  Send, {vk1Dsc07B down}
Return
; Henkan
RAlt::
  Send, {vk1Csc079 down}
Return

LWin::
  Send, {Alt down}
Return