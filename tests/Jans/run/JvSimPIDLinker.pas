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
{$I JEDI.INC}
unit JvSimPIDLinker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvSimPID;

type
  TJvSimPIDLinker = class(TComponent)
  private
    FIn1: TJvSimPID;
    FOut1: TJvSimPID;
    FOut7: TJvSimPID;
    FOut6: TJvSimPID;
    FOut4: TJvSimPID;
    FIn2: TJvSimPID;
    FIn4: TJvSimPID;
    FOut2: TJvSimPID;
    FIn8: TJvSimPID;
    FOut8: TJvSimPID;
    FIn5: TJvSimPID;
    FOut3: TJvSimPID;
    FIn3: TJvSimPID;
    FIn7: TJvSimPID;
    FOut5: TJvSimPID;
    FIn6: TJvSimPID;
    procedure SetIn1(const Value: TJvSimPID);
    procedure SetOut1(const Value: TJvSimPID);
    procedure SetIn2(const Value: TJvSimPID);
    procedure SetIn3(const Value: TJvSimPID);
    procedure SetIn4(const Value: TJvSimPID);
    procedure SetIn5(const Value: TJvSimPID);
    procedure SetIn6(const Value: TJvSimPID);
    procedure SetIn7(const Value: TJvSimPID);
    procedure SetIn8(const Value: TJvSimPID);
    procedure SetOut2(const Value: TJvSimPID);
    procedure SetOut3(const Value: TJvSimPID);
    procedure SetOut4(const Value: TJvSimPID);
    procedure SetOut5(const Value: TJvSimPID);
    procedure SetOut6(const Value: TJvSimPID);
    procedure SetOut7(const Value: TJvSimPID);
    procedure SetOut8(const Value: TJvSimPID);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Execute;
  published
    { Published declarations }
    property In1: TJvSimPID read FIn1 write SetIn1;
    property Out1: TJvSimPID read FOut1 write SetOut1;
    property In2: TJvSimPID read FIn2 write SetIn2;
    property Out2: TJvSimPID read FOut2 write SetOut2;
    property In3: TJvSimPID read FIn3 write SetIn3;
    property Out3: TJvSimPID read FOut3 write SetOut3;
    property In4: TJvSimPID read FIn4 write SetIn4;
    property Out4: TJvSimPID read FOut4 write SetOut4;
    property In5: TJvSimPID read FIn5 write SetIn5;
    property Out5: TJvSimPID read FOut5 write SetOut5;
    property In6: TJvSimPID read FIn6 write SetIn6;
    property Out6: TJvSimPID read FOut6 write SetOut6;
    property In7: TJvSimPID read FIn7 write SetIn7;
    property Out7: TJvSimPID read FOut7 write SetOut7;
    property In8: TJvSimPID read FIn8 write SetIn8;
    property Out8: TJvSimPID read FOut8 write SetOut8;

  end;

implementation

{ TJvSimPIDLinker }

procedure TJvSimPIDLinker.Execute;
var
  value: extended;
begin
  if assigned(FIn1) then value := Fin1.CV;
  if assigned(FOut1) then FOut1.MV := value;
  if assigned(FIn2) then value := Fin2.CV;
  if assigned(FOut2) then FOut2.MV := value;
  if assigned(FIn3) then value := Fin3.CV;
  if assigned(FOut3) then FOut3.MV := value;
  if assigned(FIn4) then value := Fin4.CV;
  if assigned(FOut4) then FOut4.MV := value;
  if assigned(FIn5) then value := Fin5.CV;
  if assigned(FOut5) then FOut5.MV := value;
  if assigned(FIn6) then value := Fin6.CV;
  if assigned(FOut6) then FOut6.MV := value;
  if assigned(FIn7) then value := Fin7.CV;
  if assigned(FOut7) then FOut7.MV := value;
  if assigned(FIn8) then value := Fin8.CV;
  if assigned(FOut8) then FOut8.MV := value;

end;

procedure TJvSimPIDLinker.SetIn1(const Value: TJvSimPID);
begin
  FIn1 := Value;
end;

procedure TJvSimPIDLinker.SetIn2(const Value: TJvSimPID);
begin
  FIn2 := Value;
end;

procedure TJvSimPIDLinker.SetIn3(const Value: TJvSimPID);
begin
  FIn3 := Value;
end;

procedure TJvSimPIDLinker.SetIn4(const Value: TJvSimPID);
begin
  FIn4 := Value;
end;

procedure TJvSimPIDLinker.SetIn5(const Value: TJvSimPID);
begin
  FIn5 := Value;
end;

procedure TJvSimPIDLinker.SetIn6(const Value: TJvSimPID);
begin
  FIn6 := Value;
end;

procedure TJvSimPIDLinker.SetIn7(const Value: TJvSimPID);
begin
  FIn7 := Value;
end;

procedure TJvSimPIDLinker.SetIn8(const Value: TJvSimPID);
begin
  FIn8 := Value;
end;

procedure TJvSimPIDLinker.SetOut1(const Value: TJvSimPID);
begin
  FOut1 := Value;
end;

procedure TJvSimPIDLinker.SetOut2(const Value: TJvSimPID);
begin
  FOut2 := Value;
end;

procedure TJvSimPIDLinker.SetOut3(const Value: TJvSimPID);
begin
  FOut3 := Value;
end;

procedure TJvSimPIDLinker.SetOut4(const Value: TJvSimPID);
begin
  FOut4 := Value;
end;

procedure TJvSimPIDLinker.SetOut5(const Value: TJvSimPID);
begin
  FOut5 := Value;
end;

procedure TJvSimPIDLinker.SetOut6(const Value: TJvSimPID);
begin
  FOut6 := Value;
end;

procedure TJvSimPIDLinker.SetOut7(const Value: TJvSimPID);
begin
  FOut7 := Value;
end;

procedure TJvSimPIDLinker.SetOut8(const Value: TJvSimPID);
begin
  FOut8 := Value;
end;

end.
