#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
#SingleInstance ; allow executing multiple times
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


^h::Send,{Left down}
^j::Send,{Down down}
^k::Send,{Up down}
^l::Send,{Right down}
+^h::Send,{Shift down}{Left down}
+^j::Send,{Shift down}{Down down}
+^k::Send,{Shift down}{Up down}
+^l::Send,{Shift down}{Right down}

^e::Send,{End down}
^a::Send,{Home down}
+^e::Send,{Shift down}{End down}
+^a::Send,{Shift down}{Home down}

^m::Send,{Enter down}
^u::Send,{Backspace down}
^d::Send,{Delete down}

;; LAlt + h or l -> Alt + Tab window switch
LAlt & l::AltTab
LAlt & h::ShiftAltTab
;; LAlt -> Muhenkan
LAlt::Send,{vk1Dsc07B down}

;; RAlt -> Henkan
RAlt::Send,{vk1Csc079 down}

;; This could cause problem that locking the keys
; LWin::Send,{Alt down}
; LCtrl::Send,{Alt down}


;; GPD Pocket specific
RShift::Send,{: down}
+RShift::Send,{; down}
Del::Send,{- down}
+Del::Send,{_ down}
