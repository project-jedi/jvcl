{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimPIDlinker.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSimPIDLinker;

interface

uses
  Classes,
  JvSimPID;

type
  TJvSimPIDLinker = class(TComponent)
  private
    FIn1: TJvSimPID;
    FOut1: TJvSimPID;
    FIn2: TJvSimPID;
    FOut2: TJvSimPID;
    FIn3: TJvSimPID;
    FOut3: TJvSimPID;
    FIn4: TJvSimPID;
    FOut4: TJvSimPID;
    FIn5: TJvSimPID;
    FOut5: TJvSimPID;
    FIn6: TJvSimPID;
    FOut6: TJvSimPID;
    FIn7: TJvSimPID;
    FOut7: TJvSimPID;
    FIn8: TJvSimPID;
    FOut8: TJvSimPID;
  public
    procedure Execute;
  published
    property In1: TJvSimPID read FIn1 write FIn1;
    property Out1: TJvSimPID read FOut1 write FOut1;
    property In2: TJvSimPID read FIn2 write FIn2;
    property Out2: TJvSimPID read FOut2 write FOut2;
    property In3: TJvSimPID read FIn3 write FIn3;
    property Out3: TJvSimPID read FOut3 write FOut3;
    property In4: TJvSimPID read FIn4 write FIn4;
    property Out4: TJvSimPID read FOut4 write FOut4;
    property In5: TJvSimPID read FIn5 write FIn5;
    property Out5: TJvSimPID read FOut5 write FOut5;
    property In6: TJvSimPID read FIn6 write FIn6;
    property Out6: TJvSimPID read FOut6 write FOut6;
    property In7: TJvSimPID read FIn7 write FIn7;
    property Out7: TJvSimPID read FOut7 write FOut7;
    property In8: TJvSimPID read FIn8 write FIn8;
    property Out8: TJvSimPID read FOut8 write FOut8;
  end;

implementation

procedure TJvSimPIDLinker.Execute;
begin
  if Assigned(FIn1) and Assigned(FOut1) then
    FOut1.MV := FIn1.CV;
  if Assigned(FIn2) and Assigned(FOut2) then
    FOut2.MV := FIn2.CV;
  if Assigned(FIn3) and Assigned(FOut3) then
    FOut3.MV := FIn3.CV;
  if Assigned(FIn4) and Assigned(FOut4) then
    FOut4.MV := FIn4.CV;
  if Assigned(FIn5) and Assigned(FOut5) then
    FOut5.MV := FIn5.CV;
  if Assigned(FIn6) and Assigned(FOut6) then
    FOut6.MV := FIn6.CV;
  if Assigned(FIn7) and Assigned(FOut7) then
    FOut7.MV := FIn7.CV;
  if Assigned(FIn8) and Assigned(FOut8) then
    FOut8.MV := FIn8.CV;
end;

end.

