{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvSimPIDlinker.PAS, released on 2002-06-15.

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
unit jvSimPIDLinker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,jvSimPID;

type
  TjvSimPIDLinker = class(TComponent)
  private
    FIn1: TjvSimPID;
    FOut1: TjvSimPID;
    FOut7: TjvSimPID;
    FOut6: TjvSimPID;
    FOut4: TjvSimPID;
    FIn2: TjvSimPID;
    FIn4: TjvSimPID;
    FOut2: TjvSimPID;
    FIn8: TjvSimPID;
    FOut8: TjvSimPID;
    FIn5: TjvSimPID;
    FOut3: TjvSimPID;
    FIn3: TjvSimPID;
    FIn7: TjvSimPID;
    FOut5: TjvSimPID;
    FIn6: TjvSimPID;
    procedure SetIn1(const Value: TjvSimPID);
    procedure SetOut1(const Value: TjvSimPID);
    procedure SetIn2(const Value: TjvSimPID);
    procedure SetIn3(const Value: TjvSimPID);
    procedure SetIn4(const Value: TjvSimPID);
    procedure SetIn5(const Value: TjvSimPID);
    procedure SetIn6(const Value: TjvSimPID);
    procedure SetIn7(const Value: TjvSimPID);
    procedure SetIn8(const Value: TjvSimPID);
    procedure SetOut2(const Value: TjvSimPID);
    procedure SetOut3(const Value: TjvSimPID);
    procedure SetOut4(const Value: TjvSimPID);
    procedure SetOut5(const Value: TjvSimPID);
    procedure SetOut6(const Value: TjvSimPID);
    procedure SetOut7(const Value: TjvSimPID);
    procedure SetOut8(const Value: TjvSimPID);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Execute;
  published
    { Published declarations }
    property In1:TjvSimPID read FIn1 write SetIn1;
    property Out1:TjvSimPID read FOut1 write SetOut1;
    property In2:TjvSimPID read FIn2 write SetIn2;
    property Out2:TjvSimPID read FOut2 write SetOut2;
    property In3:TjvSimPID read FIn3 write SetIn3;
    property Out3:TjvSimPID read FOut3 write SetOut3;
    property In4:TjvSimPID read FIn4 write SetIn4;
    property Out4:TjvSimPID read FOut4 write SetOut4;
    property In5:TjvSimPID read FIn5 write SetIn5;
    property Out5:TjvSimPID read FOut5 write SetOut5;
    property In6:TjvSimPID read FIn6 write SetIn6;
    property Out6:TjvSimPID read FOut6 write SetOut6;
    property In7:TjvSimPID read FIn7 write SetIn7;
    property Out7:TjvSimPID read FOut7 write SetOut7;
    property In8:TjvSimPID read FIn8 write SetIn8;
    property Out8:TjvSimPID read FOut8 write SetOut8;


  end;


implementation


{ TjvSimPIDLinker }

procedure TjvSimPIDLinker.Execute;
var value:extended;
begin
if assigned(FIn1) then value:=Fin1.CV ;
if assigned(FOut1) then FOut1.MV :=value;
if assigned(FIn2) then value:=Fin2.CV ;
if assigned(FOut2) then FOut2.MV :=value;
if assigned(FIn3) then value:=Fin3.CV ;
if assigned(FOut3) then FOut3.MV :=value;
if assigned(FIn4) then value:=Fin4.CV ;
if assigned(FOut4) then FOut4.MV :=value;
if assigned(FIn5) then value:=Fin5.CV ;
if assigned(FOut5) then FOut5.MV :=value;
if assigned(FIn6) then value:=Fin6.CV ;
if assigned(FOut6) then FOut6.MV :=value;
if assigned(FIn7) then value:=Fin7.CV ;
if assigned(FOut7) then FOut7.MV :=value;
if assigned(FIn8) then value:=Fin8.CV ;
if assigned(FOut8) then FOut8.MV :=value;

end;

procedure TjvSimPIDLinker.SetIn1(const Value: TjvSimPID);
begin
  FIn1 := Value;
end;

procedure TjvSimPIDLinker.SetIn2(const Value: TjvSimPID);
begin
  FIn2 := Value;
end;

procedure TjvSimPIDLinker.SetIn3(const Value: TjvSimPID);
begin
  FIn3 := Value;
end;

procedure TjvSimPIDLinker.SetIn4(const Value: TjvSimPID);
begin
  FIn4 := Value;
end;

procedure TjvSimPIDLinker.SetIn5(const Value: TjvSimPID);
begin
  FIn5 := Value;
end;

procedure TjvSimPIDLinker.SetIn6(const Value: TjvSimPID);
begin
  FIn6 := Value;
end;

procedure TjvSimPIDLinker.SetIn7(const Value: TjvSimPID);
begin
  FIn7 := Value;
end;

procedure TjvSimPIDLinker.SetIn8(const Value: TjvSimPID);
begin
  FIn8 := Value;
end;

procedure TjvSimPIDLinker.SetOut1(const Value: TjvSimPID);
begin
  FOut1 := Value;
end;

procedure TjvSimPIDLinker.SetOut2(const Value: TjvSimPID);
begin
  FOut2 := Value;
end;

procedure TjvSimPIDLinker.SetOut3(const Value: TjvSimPID);
begin
  FOut3 := Value;
end;

procedure TjvSimPIDLinker.SetOut4(const Value: TjvSimPID);
begin
  FOut4 := Value;
end;

procedure TjvSimPIDLinker.SetOut5(const Value: TjvSimPID);
begin
  FOut5 := Value;
end;

procedure TjvSimPIDLinker.SetOut6(const Value: TjvSimPID);
begin
  FOut6 := Value;
end;

procedure TjvSimPIDLinker.SetOut7(const Value: TjvSimPID);
begin
  FOut7 := Value;
end;

procedure TjvSimPIDLinker.SetOut8(const Value: TjvSimPID);
begin
  FOut8 := Value;
end;

end.
 