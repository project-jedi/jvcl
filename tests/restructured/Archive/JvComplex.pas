{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComplex.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvComplex;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvComponent;

type
  TJvComplex = class(TJvComponent)
  private
    FReal: Single;
    FImage: Single;
  protected
  public
  published
    property Real: Single read FReal write FReal;
    property Imaginary: Single read FImage write FImage;
    function Add(co: array of TJvComplex): TJvComplex;
    function Mult(co: array of TJvComplex): TJvComplex;
  end;

implementation

{*****************************************************}

function TJvComplex.Add(co: array of TJvComplex): TJvComplex;
var
  // (rom) AAhhrrgghh !!!
  CoTyped: array[0..$FFF0 div SizeOf(TJvComplex)] of TJvComplex absolute co;
  i: Integer;
  res: TJvComplex;
begin
  // (rom) AAhhrrgghh !!!
  res := TJvComplex.Create(Self);
  res.FReal := 0;
  res.FImage := 0;
  for i := Low(Co) to High(co) do
  begin
    res.FReal := res.FReal + CoTyped[i].FReal;
    res.FImage := res.FImage + CoTyped[i].FImage;
  end;
  Result := res;
end;

{*****************************************************}

function TJvComplex.Mult(co: array of TJvComplex): TJvComplex;
var
  // (rom) AAhhrrgghh !!!
  CoTyped: array[0..$FFF0 div SizeOf(TJvComplex)] of TJvComplex absolute co;
  i: Integer;
  res: TJvComplex;
begin
  // (rom) AAhhrrgghh !!!
  res := TJvComplex.Create(Self);
  res.FReal := 0;
  res.FImage := 1;
  for i := Low(Co) to High(co) do
  begin
    res.FReal := res.FReal * CoTyped[i].FReal;
    res.FImage := res.FImage + CoTyped[i].FImage;
  end;
  Result := res;
end;

end.
