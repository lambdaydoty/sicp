;MsgBox, %NewestFileName%
SetTitleMatchMode, 2
;RK_PATH  := "DrRacket"
;RK_TITLE := "Untitled - DrRacket ahk_exe DrRacket.exe"
LISP_PATH := "D:\Dropbox\02.Lambda\Lisp-Solutions\00-sicp"
LISP_TITLE:= "00-sicp ahk_exe explorer.exe"
Loop, %LISP_PATH%\*.*
{
	CheckFile_Start := A_LoopFileTimeCreated
	If (CheckFile_Start > CheckFile_End)
	{
		CheckFile_End := A_LoopFileTimeCreated
		NewestFileName := A_LoopFileName
	}
}
VI_PATH  := "gvim -c ""simalt ~x"" -c ""cd " . LISP_PATH . """ -c ""e .\" . NewestFileName . """ -c ""vsp"""
VI_TITLE := "ahk_exe gvim.exe"
Run             , %VI_PATH%     , 
WinWaitActive   , %VI_TITLE%    ,
Sleep           , 500
Run             , %LISP_PATH%     , 
WinWaitActive   , %LISP_TITLE%    ,
Sleep           , 500
;Run             , %RK_PATH%     , 
;WinWaitActive   , %RK_TITLE%    ,
;Sleep           , 500


;Run             , %PS_PATH%     ,
;WinWaitActive   , %PS_TITLE%    ,
;Sleep           , 4000
;Run             , %VI_PATH%     , 
;WinWaitActive   , %VI_TITLE%    ,
;Sleep           , 500
;Run             , %U3_PATH%     ,
;WinWaitActive   , %U3_TITLE%    ,
;Sleep           , 500
;Run             , %UV_PATH%     ,
;WinWaitActive   , %UV_TITLE%    ,
;Sleep           , 300
;Run             , %EX_PATH%     ,
;WinWaitActive   , %EX_TITLE%    ,
;Sleep           , 300
;Run             , %TG_PATH%     ,
;WinWaitActive   , %TG_TITLE%    ,
;Sleep           , 300

;Run, "C:\Users\jw\Desktop\Analyzer\putty\pietty0400b14.exe"
;WinWaitActive, PieTTY 連線設定,
;Sleep, 500
;SendInput {Tab 4}
;SendInput {Space}
;WinWaitActive, PieTTY,
;Sleep, 500
;SendInput {Space}
;WinWaitActive, PieTTY 連線設定,
;Sleep, 500
;SendInput {Tab 4}
;SendInput c
;SendInput {Enter}
