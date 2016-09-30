//..............................................................................
// Summary  Returns the Microsoft Scripting Engine version no.
// Copyright (c) 2004 by Altium Limited
//..............................................................................

//..............................................................................
function GetScriptEngineInfo(){
   var s;
   s = ""; // Build string with necessary info.
   s += ScriptEngine() + " Version ";
   s += ScriptEngineMajorVersion() + ".";
   s += ScriptEngineMinorVersion() + ".";
   s += ScriptEngineBuildVersion();
   
   showmessage(s);
}
//..............................................................................

//..............................................................................

