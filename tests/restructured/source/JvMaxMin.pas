{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMaxMin.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvMaxMin;

interface

function Max(A, B: Longint): Longint;
function Min(A, B: Longint): Longint;
function MaxInteger(const Values: array of Longint): Longint;
function MinInteger(const Values: array of Longint): Longint;
{$IFDEF Delphi4_Up}
function MaxInt64(const Values: array of int64): int64;
function MinInt64(const Values: array of int64): int64;
{$ENDIF}

function MaxFloat(const Values: array of Extended): Extended;
function MinFloat(const Values: array of Extended): Extended;
function MaxDateTime(const Values: array of TDateTime): TDateTime;
function MinDateTime(const Values: array of TDateTime): TDateTime;
{$IFDEF WIN32}
function MaxOf(const Values: array of Variant): Variant;
function MinOf(const Values: array of Variant): Variant;
{$ENDIF}

procedure SwapLong(var Int1, Int2: Longint);
procedure SwapInt(var Int1, Int2: Integer);
{$IFDEF Delphi4_Up}
procedure SwapInt64(var Int1, Int2: Int64);
{$ENDIF}

{$IFNDEF WIN32}
function MakeWord(A, B: Byte): Word;
{$ENDIF}

implementation

{$IFNDEF WIN32}
function MakeWord(A, B: Byte): Word;
begin
  Result := A or B shl 8;
end;
{$ENDIF}

procedure SwapInt(var Int1, Int2: Integer);
var
  I: Integer;
begin
  I := Int1; Int1 := Int2; Int2 := I;
end;

{$IFDEF Delphi4_Up}
procedure SwapInt64(var Int1, Int2: Int64);
var
  I: Int64;
begin
  I := Int1; Int1 := Int2; Int2 := I;
end;
{$ENDIF}

procedure SwapLong(var Int1, Int2: Longint);
var
  I: Longint;
begin
  I := Int1; Int1 := Int2; Int2 := I;
end;

function Max(A, B: Longint): Longint;
begin
  if A > B then Result := A
  else Result := B;
end;

function Min(A, B: Longint): Longint;
begin
  if A < B then Result := A
  else Result := B;
end;

function MaxInteger(const Values: array of Longint): Longint;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinInteger(const Values: array of Longint): Longint;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

{$IFDEF Delphi4_Up}

function MaxInt64(const Values: array of int64): int64; 
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinInt64(const Values: array of int64): int64;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

{$ENDIF Delphi4_Up}

function MaxFloat(const Values: array of Extended): Extended;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinFloat(const Values: array of Extended): Extended;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

function MaxDateTime(const Values: array of TDateTime): TDateTime;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

function MinDateTime(const Values: array of TDateTime): TDateTime;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

{$IFDEF WIN32}
function MaxOf(const Values: array of Variant): Variant;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinOf(const Values: array of Variant): Variant;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;
{$ENDIF WIN32}

end.
