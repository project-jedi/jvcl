{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSAL.PAS, released on 2002-06-15.

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
unit JvSALMath;

// SAL math package
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvSAL, math;

type
  TJvSALMath = class(TComponent)
  private
    { Private declarations }
    sal: TJvSAL;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure AddProcedures(aSal: TJvSAL);
    procedure xSin;
    procedure xArcSin;
    procedure xCos;
    procedure xArcCos;
    procedure xTan;
    procedure xArcTan;
    procedure xPi;
    procedure xExp;
    procedure xLn;

  published
    { Published declarations }
  end;

implementation

const
  cr = chr(13) + chr(10);
  tab = chr(9);

  { TJvSALMathBasic }

procedure TJvSALMath.AddProcedures(aSal: TJvSAL);
begin
  sal := aSal;
  sal.AddProcedure('sin', xsin, nil);
  sal.AddProcedure('cos', xcos, nil);
  sal.AddProcedure('tan', xtan, nil);
  sal.AddProcedure('arcsin', xsin, nil);
  sal.AddProcedure('arccos', xcos, nil);
  sal.AddProcedure('arctan', xtan, nil);
  sal.AddProcedure('pi', xpi, nil);
  sal.AddProcedure('exp', xpi, nil);
  sal.AddProcedure('ln', xpi, nil);
end;

procedure TJvSALMath.xSin;
var
  v1: variant;
begin
  v1 := sal.pop;
  v1 := sin(v1);
  sal.push(v1);
end;

procedure TJvSALMath.xCos;
var
  v1: variant;
begin
  v1 := sal.pop;
  v1 := cos(v1);
  sal.push(v1);
end;

procedure TJvSALMath.xTan;
var
  v1: variant;
begin
  v1 := sal.pop;
  v1 := tan(v1);
  sal.push(v1);
end;

procedure TJvSALMath.xPi;
begin
  sal.push(pi);
end;

procedure TJvSALMath.xArcCos;
begin
  sal.push(arccos(sal.pop));
end;

procedure TJvSALMath.xArcSin;
begin
  sal.push(arcsin(sal.pop));
end;

procedure TJvSALMath.xArcTan;
begin
  sal.push(arctan(sal.pop));
end;

procedure TJvSALMath.xExp;
begin
  sal.push(exp(sal.pop));
end;

procedure TJvSALMath.xLn;
begin
  sal.push(ln(sal.pop));
end;

end.
