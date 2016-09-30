'..............................................................................
' Summary  Returns the Microsoft Scripting Engine version no.
' Copyright (c) 2004 by Altium Limited
'..............................................................................

'..............................................................................
sub GetScriptEngineInfo
   dim s

   s = ""  'Build string with necessary info.
   s = s & ScriptEngine() & " Version "
   s = s & ScriptEngineMajorVersion() & "."
   s = s & ScriptEngineMinorVersion() & "."
   s = s & ScriptEngineBuildVersion()

   showmessage(s)
end sub
'..............................................................................

'..............................................................................

