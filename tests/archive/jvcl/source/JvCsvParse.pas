unit JvCsvParse;

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

Last Modified: 2003-04-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Internal pchar-manipulation functions required by TJvCsvDataSet data access component. 

Known Issues:
-----------------------------------------------------------------------------}

//--------------------------------------------------------------
// JvCsvParse.pas
//
// Useful extra functions for parsing strings using pascal,
// not present in your basic vanilla Pascal/Delphi standard
// libraries.
//
// MOST use PChars and char buffers, not the String type.
//
// These functions are used to implement the
// CsvDataSource component but are generally reuseable in
// any string parsing code.
//
//
// Original by Warren Postma.
//
// Donated to delphi jedi project.
//
//--------------------------------------------------------------

interface

uses Classes;

const MaxInitStrNum = 9;

{ String Class Functions - uses Delphi String objects instead of Pascal PChars }

{new 2003}
function StrSplit( const inString:String; const splitChar,quoteChar:Char; var OutStrings:Array of String;MaxSplit:Integer):Integer;


{ circa 1998-2001 classic functions }
function StrStrip(s:string):string; // Strip whitespace, carriage returns, linefeeds.
function GetString(var source : string;seperator:string):string; // Iteratively split off a piece of a string. Modifies original string.
function PadString( s:string; len:integer; padchar:char ):string;
//procedure Gibble(var S:string); // Deprecated. With a name like Gibble, are you surprised?
function BuildPathName( PathName, FileName:string):string;
function StrEatWhiteSpace(s:string):string;
function HexToAscii(s:string):string;
function AsciiToHex(s:string):string;
function StripQuotes( s1:String) : String;

{ TStringList helper functions }
function GetIntValueFromResultString( VarName : String; ResultStrings:TStringList;DefVal:Integer):Integer;
function GetValueFromResultString( VarName : String; ResultStrings:TStringList):String;



{ Pascal Low Level PChar Functions }
function ValidNumericLiteral(s1:PChar) : Boolean;
function ValidIntLiteral(s1:PChar) : Boolean;
function ValidHexLiteral(s1:PChar) : Boolean;
function HexPcharToInt(s1 : PChar) : Integer;
function ValidStringLiteral(s1:PChar) : Boolean;
function StripPCharQuotes( s1:PChar ) : String;

function ValidIdentifier( s1: PChar ) : Boolean;
function EndChar( x : char ) : Boolean;
procedure GetToken( s1,s2:PChar );
function IsExpressionKeyword(s1:PChar):Boolean;
function IsKeyword(s1:PChar):Boolean;
function ValidVarReference( s1: PChar):Boolean;
function GetParenthesis(s1,s2:Pchar):Boolean;
procedure GetVarReference( s1,s2,sidx : PChar );
procedure PcharEatWs(s1:PChar);



{ Debugging functions related to GetToken function. }
function GetTokenCount:Integer;
procedure ResetTokenCount;





implementation



uses SysUtils;

var
  TokenCount : integer;

{ Returns true for literals like '123.456', '78', or '-35.1231231' }
function ValidNumericLiteral(s1:PChar) : Boolean;
var
   l,x,x1:integer;
   DecimalFlag :Boolean;
begin
  l := strlen(s1);
  DecimalFlag := FALSE;
  x1 := 0;

  if (l <= 0) then begin
       result := FALSE;
       exit;
  end;
  
  { detect leading minus }
  if (s1[0] = '-') then
     Inc(x1); // skip the minus, as it's okay as a leading character
     
  { Detect a decimal number or integer number }
  for x := x1 to l-1 do begin
     if (s1[x] = '.') then begin
         if (DecimalFlag) then  begin
             result := FALSE; // two decimal places is invalid.
             exit;
         end;
         DecimalFlag := TRUE;
     end
     else
     if (s1[x] < '0') OR (s1[x] > '9') then begin
         result := FALSE;
         exit;
     end;
  end;
  result := TRUE;
end;

{ Returns true for integer literals only, like -35 or 199, but not
  for values like '123.45' }
function ValidIntLiteral(s1:PChar) : Boolean;
var
   l,x,x1:integer;

begin
  l := strlen(s1);
  x1 := 0;
  if (l <= 0) then begin
       result := FALSE;
       exit;
  end;
  { detect leading minus }
  if (s1[0] = '-') then
     Inc(x1); // skip the minus, as it's okay as a leading character

  { Detect a decimal number or integer number }
  for x := x1 to l-1 do begin
     if (s1[x] < '0') OR (s1[x] > '9') then begin
         result := FALSE;
         exit;
     end;
  end;
  result := TRUE;
end;

{ Returns true for integer literals only, like -35 or 199, but not
  for values like '123.45' }
function ValidHexLiteral(s1:PChar) : Boolean;
var
   l,x:integer;
begin
  l := strlen(s1);
//  x1 := 0;

  { detect hex code type indicator }
  if (l<2) OR (s1[0] <> '$') then begin
     result := FALSE;
     exit;
  end;

  { Detect hex digits }
  for x := 1 to l-2 do begin
     if ((s1[x] < '0') OR (s1[x] > '9'))
       AND ((s1[x] < 'a') OR (s1[x] > 'f'))
       AND ((s1[x] < 'A') OR (s1[x] > 'F'))
        then begin
         result := FALSE;
         exit;
     end;
  end;
  result := TRUE;
end;

function HexPcharToInt(s1 : PChar) : Integer;
var
  x,l:integer;
  digit,val:integer;
begin
 l := strlen(s1);
 if (l < 2) OR (l > 9) then
     raise Exception.create('HexStrToInt: Invalid hex literal');
 if (s1[0] <> '$') then
     raise Exception.create('HexStrToInt: Invalid hex literal');
 val := 0;
 for x := 1 to l-2 do begin
   val := val * 16; { shift right four bits at a time }
   if (s1[x] >= '0') AND (s1[x] <= '9') then
       digit := ord(s1[x])-ord('0')
   else if (s1[x] >= 'a') AND (s1[x] <= 'f') then
       digit := ( ord(s1[x])-ord('a') ) + 10
   else if (s1[x] >= 'A') AND (s1[x] <= 'F') then
       digit := ( ord(s1[x])-ord('A') ) + 10
   else
      raise Exception.create('HexStrToInt: Invalid hex literal');
   val := val + digit;
 end;
 result :=  val;
end;



function ValidStringLiteral(s1:PChar) : Boolean;
var
  l:integer;
begin
 l := strlen(s1);
 if (s1[0] = '"') AND (s1[l-1] = '"') then
   result := TRUE
 else
   result := FALSE;
end;

{ Strip quotes and return as a real Delphi String }
function StripQuotes( s1:String ) : String;
begin
  if ValidStringLiteral(PChar(s1)) then
     result := Copy(s1,2,Length(s1)-2)
  else
     result := s1;
end;


// This function is limited to 1 to 254 characters:
function StripPcharQuotes( s1:PChar ) : String;
var
  tempbuf: array [0..256] of char;
  l:Integer;
begin
   l := strlen(s1);
   if (l>255) then
      l := 255;
  if ValidStringLiteral(s1) then begin
     StrLCopy(tempbuf,s1+1,l-2 );
  end;
  result := String(tempbuf);
end;


{ Prevent confusion between expression-keywords and variable identifiers }
function IsExpressionKeyword(s1:PChar):Boolean;
begin
   if (StrIComp(s1,'AND')=0) then
       result := TRUE
   else if (StrIComp(s1,'OR')=0) then
       result := TRUE
   else if (StrIComp(s1,'XOR')=0) then
       result := TRUE
   else if (StrIComp(s1,'NOT')=0) then
       result := TRUE
   else if (StrIComp(s1,'DIV')=0) then
       result := TRUE
   else if (StrIComp(s1,'SHR')=0) then
       result := TRUE
   else if (StrIComp(s1,'SHL')=0) then
       result := TRUE
   else
       result := FALSE;
end;

function IsKeyword(s1:PChar):Boolean;
begin
 result :=    (StrIComp(s1,'SET')=0)
           OR (StrIComp(s1,'LET')=0)
           OR (StrIComp(s1,'DIM')=0)
           OR (StrIComp(s1,'ARRAYCOPY')=0)
           OR (StrIComp(s1,'STRCOPY')=0)
           OR (StrIComp(s1,'STRPAD')=0)
           OR (StrIComp(s1,'STRSTRIP')=0)
           OR (StrIComp(s1,'END')=0)
           OR (StrIComp(s1,'INC')=0)
           OR (StrIComp(s1,'DEC')=0)
           OR (StrIComp(s1,'PARAM')=0)
           OR (StrIComp(s1,'JUMP')=0)
           OR (StrIComp(s1,'SLEEP')=0) 
           OR (StrIComp(s1,'GOTO')=0)
           OR (StrIComp(s1,'IF')=0)
           OR (StrIComp(s1,'CALL')=0)
           OR (StrIComp(s1,'STOP')=0)
           OR (StrIComp(s1,'CONST')=0);
end;



{ ValidIdentifier:

  Valid identifier must start with a-z or A-Z or _, and can have alphanumeric or underscore(_)
  as subsequent characters, no spaces, punctuation, or other characters allowed.  Same rules as
  most programming languages, Cobol being one particularly nasty exception! <grin>

    --Warren.
}
function ValidIdentifier( s1: PChar ) : Boolean;
var
 x,y : integer;
 pass : Boolean;
begin

 Pass := TRUE;

 if IsExpressionKeyword(s1) then begin
     result := FALSE;
     exit;
 end;
 
 x := strlen(s1);
 if (x < 1) OR (x > 32) then begin
       result := FALSE;
       exit;
 end;
 
 if NOT ( ( (s1[0] >= 'a') AND (s1[0] <= 'z') ) OR
      ( (s1[0] >= 'A') AND (s1[0] <= 'Z') ) ) OR
      ( (s1[0] = '_' ) )

 then
        Pass := FALSE;

 if Pass AND (x > 1) then
    for y := 1 to x-1 do begin
      if NOT (( ( (s1[y] >= 'a') AND (s1[y] <= 'z') ) OR
           ( (s1[y] >= 'A') AND (s1[y] <= 'Z') ) OR
           ( (s1[y] >= '0') AND (s1[y] <= '9') ) ) OR
           ( (s1[y] = '_' ) ))
      then begin
          Pass := FALSE;
          result := Pass;
          exit;
      end;
    end;

  result := Pass;
end;



function EndChar( x : char ) : Boolean;
begin
 if    (x = ',')
    OR (x = ';')
    OR (x = ':')
    OR (x = '[')
    OR (x = ']')
    OR (x = '(')
    OR (x = ')')
    OR (x = '#')
    OR (x = '<')
    OR (x = '>')
    OR (x = '=')
    OR (x = '*')
    OR (x = '/')
    OR (x = '+')
    OR (x = Chr(0) ) then
       result := TRUE
    else
       result := FALSE;
end;


procedure GetToken( s1,s2:PChar );
var
 w,x,y : integer;
 InQuotes:Boolean;
begin
 x := 0;
 w := 0;

 { Empty in, Empty Out }
 if (strlen(s1)=0) then begin
     s2[0] := Chr(0);
     
 end;
     
 InQuotes := FALSE;

 { skip leading space }
 while (s1[x] = ' ') OR (s1[x] = Chr(9) ) do
     Inc(x);

 while TRUE do begin

   if EndChar(s1[x]) AND (NOT InQuotes) then begin
     { return punctuation one symbol at a time }
     if (w < 1) then begin
       s2[w] := s1[x];
       Inc(w);
       Inc(x);
     end;

     break;
   end;

   if (s1[x] = '"') then
      InQuotes := NOT InQuotes;

   { Break if space found and not in quotes }
   if (s1[x] = ' ') and (NOT InQuotes) then
         break
   else begin
            s2[w] := s1[x];
            Inc(w);
   end;
   
   Inc(x);

 end;
// s2[x] := Chr(0);

 { detect not-equal, less-than-or-equal and greater-than-or-equal operators }
 if (w = 1) then begin
  if (s2[0] = '<') AND (s1[x] = '>') then begin
     s2[w] := '>';
     Inc(x);
     Inc(w);   // char literal
   end else
   if (s2[0] = '<') AND (s1[x] = '=') then begin
     s2[w] := '=';
     Inc(x);
     Inc(w);
   end else
   if (s2[0] = '>') AND (s1[x] = '=') then begin
     s2[w] := '=';
     Inc(x);
     Inc(w);
   end;
 end;

 { remove token from initial buffer, move to second buffer }
 y := Integer(strlen(s1)) -  x; //Cardinal removes warning.
 if (y > 0) then
     StrLCopy( s1, s1+x, y) { copy remaining characters }
 else
     s1[0] := Chr(0); { just erase all of old string }

 s2[w] := Chr(0); { Terminate new string }
 Inc(TokenCount);
end;


function StrEatWhiteSpace(s:string):string;
var
  c : Array [0..1024] of char;
begin
  if length(s) > 1024 then begin
    result := s;
    exit;
  end;
  StrCopy(c, PChar(s) );
  PcharEatWs(c);
  result := string(c);
end;

{ strip whitespace from pchar - space or tab }
procedure PcharEatWs(s1:PChar);
var
  T,U,L:integer;
begin
  L := strlen(s1);
 // U := L;
  if (L<=0) then exit;
    { skip spaces starting at the beginning }
  for T := 0 to L do begin
     if (s1[T] <> ' ')and(s1[T] <> Chr(9)) then
           break;
  end;
   { skip spaces starting at the end }
  for U := L-1 downto T do begin
     if (s1[U] <> ' ')and(s1[U] <> Chr(9)) then
           break;
  end;
  if (T > 0) OR (U<L-1) then begin
     if (T > U) then begin // was T>=U (test me!)
         s1[0] := Chr(0);
     end else
         StrLCopy(s1,s1+T,(U-T)+1);
  end;
end;

function GetParenthesis(s1,s2:Pchar):Boolean;
var
   token,tempbuf:array [0..128] of char;
   brackets:integer;
begin
  { make temporary copy of s1, check for parenthesis }
  StrCopy(tempbuf,s1);
  GetToken(tempbuf,s2);
  if (strcomp(s2,'(')=0) then begin
       brackets := 1;
       s2[0] := Chr(0);
       repeat
          GetToken(tempbuf,token);
          if (strcomp(token,')')=0) then Dec(brackets);
          if (brackets > 0) then begin
             strcat(s2,token);
             strcat(s2,' ');
          end;
          if (strcomp(token,'(')=0) then Inc(brackets);
       until (strlen(s1)=0) OR (brackets = 0);
       if (brackets <> 0) then begin
             s2[0] := Chr(0);
             result := FALSE;
             exit;
       end;
       strcopy(s1,tempbuf); { remainder back into s1 }
       result := TRUE; { true }
  end else begin { not parenthesis }
     s2[0] := Chr(0);
     result := FALSE;
     exit;
  end;
end;


{ Gets a single token like ABC, or gets ABC[X] type reference if present }
procedure GetVarReference( s1,s2,sidx : PChar );
var
  tempbuf:array [0..128] of char;
  brackets:integer;
begin
   GetToken(s1,s2);
   sidx[0] := Chr(0);
   PCharEatWs(s1);
   if (s1[0]='[') then begin
        brackets := 0;
        repeat
         GetToken(s1,tempbuf);
         StrCat(sidx,tempbuf);
         if (StrComp(tempbuf,']')=0) then
               Dec(brackets);
         if (StrComp(tempbuf,'[')=0) then
               Inc(brackets);

         if (strlen(s1)=0) then
            break;
        until brackets <=0;

        { Remove outermost brackets }
        StrLCopy(sidx,sidx+1,strlen(sidx)-2);

   end;
end;




{ Expects ABC or ABC[X] type of reference }
function ValidVarReference( s1: PChar ):Boolean;
var
   len1:integer;
   tempbuf1,tempbuf2:array [0..128] of char;
begin
   StrCopy(s1,tempbuf1);
   GetToken(tempbuf1,tempbuf2);
   if strlen(tempbuf1) = 0 then begin
         result := ValidIdentifier(s1);
   end else begin
         len1 := strlen(tempbuf1);
         if (tempbuf1[0] = '[') AND ( tempbuf1[len1-1] = ']') then begin
               result := ValidIdentifier(s1);
         end else
               result := FALSE;
   end;
end;



{ debugging and performance tuning information }
function GetTokenCount:Integer;
begin
   result := TokenCount;
end;

procedure ResetTokenCount;
begin
   TokenCount := 0;
end;

function PadString( s:string; len:integer; padchar:char ):string;
var
  x:string;
begin
  x := s;
  while length(x) < len do begin
        x := x + padchar;
  end;
  result := x;
end;

{ Encoding function named in honor of Dennis Forbes' favourite word }
{procedure Gibble(var S:string);
var
 t,l,c1 : integer;
 lo,hi:byte;
 x:array[0..255] of char;
begin
 l := length(s);
 for t:= 0 to l-1 do begin
     c1 := Ord( s[t+1] );
     if ( c1  >= 32 ) AND (c1 <= 231) then begin
        c1 := c1 - 32;
        lo := (c1 MOD 25);
        hi := c1 div 25;
        lo := 24-lo;
        c1 := ( (hi*25)+lo ) +32;
        x[t] := Chr(c1);
     end else
        x[t] := Chr(c1);
 end;
 x[l] := Chr(0);
 s := String(x);
end;
 }

function BuildPathName( PathName, FileName:string):string;
var
 l:integer;
begin
  l := length(PathName);
  if (l = 0) then
         result := FileName
  else begin
       if (PathName[l] = '\') then begin
        result := PathName + FileName;
       end else begin
        result := PathName + '\' + FileName;
       end;
  end;
end;


function HexDigitVal(c:char):integer;
begin
 if (c >= '0') AND (c <= '9') then
    result := Ord(c)-Ord('0')
 else if (c >= 'a') AND (c <= 'f') then
    result := 10+Ord(c)-Ord('a')
 else if (c >= 'A') AND (c <= 'F') then
    result := 10+Ord(c)-Ord('A')
 else
    result := 0;
end;


function HexToAscii(s:string):string;
var
 t,y,L:integer;
 c:array[0..256] of char;
begin
 L := (length(s) div 2);
 for t := 0 to L-1 do begin
    y := (t*2)+1;
    c[t] := Char( HexDigitVal(s[y])*16 + HexDigitVal(s[y+1])  );
 end;
 c[L] := Chr(0);
 result := String(c);
end;

function AsciiToHex(s:string):string;
var
  t:integer;
  s2:string;
begin
 for t := 1 to length(s) do begin
   s2 := s2 + IntToHex(Ord(s[t]),2);
 end;
 result := s2;
end;



//-----------------------------------------------------------------------------
// GetIntValueFromResultString
//
// Retrieve an integer value from a result string, Formats that are valid
// include:
//
// VariableName : Value  - usual format for status results
// VariableName = Value  - usual format in ini files
// Label Name = Value    - labels names can contain spaces.
//-----------------------------------------------------------------------------
function GetIntValueFromResultString(VarName : String; ResultStrings:TStringList;DefVal:Integer):Integer;
var
   s : String;
begin
   s:= GetValueFromResultString(VarName,ResultStrings);
   result := StrToIntDef(s,DefVal);
end;

//-----------------------------------------------------------------------------
// GetValueFromResultString
//
// Retrieve a value from a result string, Formats that are valid include:
// VariableName : Value  - usual format for status results
// VariableName = Value  - usual format in ini files
// Label Name = Value    - labels names can contain spaces.
//-----------------------------------------------------------------------------
function GetValueFromResultString(VarName : String; ResultStrings:TStringList):String;
var
 label1,value1:String;
 len1,pos1,t,count:Integer;
begin

 if not Assigned(ResultStrings) then begin
    result := 'NIL';
    exit;
 end;

 count := ResultStrings.Count;
  for t := 0 to count-1 do begin
      len1 := Length(ResultStrings[t]);
      pos1 := Pos(':', ResultStrings[t]);
      if (pos1 = 0) then
            pos1 := Pos('=', ResultStrings[t]);
      // found a value to extract:
      if (pos1>0) then begin
         Label1 := Copy(ResultStrings[t],1,pos1-1);
         Value1 := Copy(ResultStrings[t],pos1+1,len1);

         if VarName = Label1 then begin // found it!
            result := Value1;
            exit;
         end;
      end;
  end;


end;

function StrStrip(s:string):string;
var
  len,i:integer;
begin
  len:=length(s);
  i:=1;
  while (len>=i) and ((s[i]=' ') or (s[i]=#9))  do
    i:=i+1;
  if (i>len) then begin
    result:='';
    exit;
  end;
  s:=Copy(s,i,len);
  len:=len-i+1;
  i:=len;
  while (i>0) and ((s[i]=' ') or (s[i]=#9)) do
    i:=i-1;
  result:=Copy(s,1,i);
end;

function GetString(var source : string;seperator:string):string;
var
  i,j,len:integer;
begin
  //source:=StrStrip(source);
  len:=length(source);
  i:=0;
  for j:=1 to len do
    if Pos(source[j],seperator)>0 then begin
      i:=j;
      break;
    end;
  if (i>0) then begin
    result:=StrStrip(Copy(source,1,i-1));
    source:=Copy(source,i+1,Length(source)-i);
    //Source:=StrStrip(source); //???
  end else begin
    result:=StrStrip(source);
    source:='';
  end;
end;

//------------------------------------------------------------------------------------------
// StrSplit
//   Given aString='Blah,Blah,Blah', splitChar=',', writes to OutStrings an Array
//   ie ( 'blah','blah','blah ) and returns the integer count of how many items are in
//   the resulting array, or -1 if more than MaxSplit items were found in the input 
//   string.
//
// XXX READ THESE NOTES! XXX
//
// XXX DOES NOT HANDLE QUOTING (YOU CAN'T HAVE A COMMA INSIDE QUOTES, AT LEAST NOT YET.) XXX
//
// XXX OutStrings array must be dimensioned to start at element ZERO,
//     if it starts at element 1, then you'll get exceptions XXX
//------------------------------------------------------------------------------------------
function StrSplit( const inString:String; const splitChar,quoteChar:Char; var OutStrings:Array of String;MaxSplit:Integer):Integer;
var
  t,Len,SplitCounter:Integer;
  Ch:Char;
  inQuotes:Boolean;
begin
   inQuotes := false;
   Len := Length(inString);
   for t := Low(OutStrings)  to High(OutStrings) do begin // clear array that is passed in!
        OutStrings[t] := ''; // Array
   end;

   SplitCounter := 0; // ALWAYS ASSUME THAT ZERO IS VALID IN THE OUTGOING ARRAY.

   for t := 1 to Len do begin
        Ch := inString[t];
        if (Ch = splitChar) and (not inQuotes) then begin
                Inc(SplitCounter);
                if SplitCounter>MaxSplit then begin
                        result := -1; // Error!
                        exit;
                end;
        end else begin
          OutStrings[SplitCounter] := OutStrings[SplitCounter] + ch;
          if (ch = quoteChar) then
              inQuotes := not inQuotes;
        end;
   end;
   Inc(SplitCounter);
   result := SplitCounter;
end;


initialization
   TokenCount := 0;

end.
