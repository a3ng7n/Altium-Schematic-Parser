//..............................................................................
// Summary  Demo of built in Date and Math objects in JScript...
//          Check Microsoft Developers Network for more information.
// Copyright (c) 2004 by Altium Limited
//..............................................................................

//..............................................................................
function DateDemo(){
   var d, s = "Today's date is: ";  //Declare variables.

   d = new Date();                  //Create Date object.
   s += (d.getMonth() + 1) + "/";   //Get month.
   s += d.getDate() + "/";          //Get day.
   s += d.getYear();                //Get year.
   showmessage(s);                  //Show date.
}
//..............................................................................

//..............................................................................
function testmathobj()
{
var result;

// returns the exponential value 2.71828...
result = Math.exp(1);
showmessage(result);
}
//..............................................................................

//..............................................................................

