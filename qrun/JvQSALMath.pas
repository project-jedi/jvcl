{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSAL.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQSALMath;

interface

uses
  SysUtils, Classes, Math,
  JvQSAL, JvQTypes;

type
  TJvSALMath = class(TComponent)
  private
    FSal: TJvSAL;
  public
    procedure AddProcedures(ASal: TJvSAL);
    procedure XSin;
    procedure XArcSin;
    procedure XCos;
    procedure XArcCos;
    procedure XTan;
    procedure XArcTan;
    procedure XPi;
    procedure XExp;
    procedure XLn;
  end;

implementation

procedure TJvSALMath.AddProcedures(ASal: TJvSAL);
begin
  FSal := ASal;
  FSal.AddProcedure('sin', XSin, nil);
  FSal.AddProcedure('cos', XCos, nil);
  FSal.AddProcedure('tan', XTan, nil);
  // (rom) using XSin, XCos, XTan looks suspicious. Incomplete?
  FSal.AddProcedure('arcsin', XSin, nil);
  FSal.AddProcedure('arccos', XCos, nil);
  FSal.AddProcedure('arctan', XTan, nil);
  FSal.AddProcedure('pi', XPi, nil);
  // (rom) using XPi looks suspicious. Incomplete?
  FSal.AddProcedure('exp', XPi, nil);
  FSal.AddProcedure('ln', XPi, nil);
end;

procedure TJvSALMath.XSin;
var
  V1: Variant;
begin
  V1 := FSal.Pop;
  V1 := Sin(V1);
  FSal.Push(V1);
end;

procedure TJvSALMath.XCos;
var
  V1: Variant;
begin
  V1 := FSal.Pop;
  V1 := Cos(V1);
  FSal.Push(V1);
end;

procedure TJvSALMath.XTan;
var
  V1: Variant;
begin
  V1 := FSal.Pop;
  V1 := Tan(V1);
  FSal.Push(V1);
end;

procedure TJvSALMath.XPi;
begin
  FSal.Push(Pi);
end;

procedure TJvSALMath.XArcCos;
begin
  FSal.Push(ArcCos(FSal.Pop));
end;

procedure TJvSALMath.XArcSin;
begin
  FSal.Push(ArcSin(FSal.Pop));
end;

procedure TJvSALMath.XArcTan;
begin
  FSal.Push(ArcTan(FSal.Pop));
end;

procedure TJvSALMath.XExp;
begin
  FSal.Push(Exp(FSal.Pop));
end;

procedure TJvSALMath.XLn;
begin
  FSal.Push(Ln(FSal.Pop));
end;

end.
