{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFloatEdit.PAS, released on 2001-02-28.

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

unit JvFloatEdit;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvTypes, JvEdit;

type
  TJvFloatEdit = class(TJvEdit)
  private
    FText: string;
    FOld: Extended;
    function GetValue: Extended;
    procedure SetValue(const Value: Extended);
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Text: string read FText;
    property Value: Extended read GetValue write SetValue;
  end;

implementation

{*****************************************************}

procedure TJvFloatEdit.CMEnter(var Msg: TCMEnter);
begin
  inherited;
  FOld := GetValue;
end;

{*****************************************************}

procedure TJvFloatEdit.CMExit(var Msg: TCMExit);
var
  st: string;
begin
  inherited;
  st := inherited Text;
  try
    StrToFloat(st);
  except
    SetValue(FOld);
  end;
end;

{*****************************************************}

constructor TJvFloatEdit.Create(AOwner: TComponent);
begin
  inherited;
  SetValue(0.0);
end;

{*****************************************************}

function TJvFloatEdit.GetValue: Extended;
var
  st: string;
begin
  st := inherited Text;
  try
    Result := StrToFloat(st);
  except
    Result := 0.0;
  end;
end;

{*****************************************************}

procedure TJvFloatEdit.SetValue(const Value: Extended);
var
  st: string;
begin
  st := FloatToStr(Value);
  inherited Text := st;
end;

end.
