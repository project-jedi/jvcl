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

{$I JVCL.INC}

unit JvFloatEdit;

interface

uses
  SysUtils, Classes, Controls,
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

constructor TJvFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetValue(0.0);
end;

procedure TJvFloatEdit.CMEnter(var Msg: TCMEnter);
begin
  inherited;
  FOld := GetValue;
end;

procedure TJvFloatEdit.CMExit(var Msg: TCMExit);
var
  St: string;
begin
  inherited;
  St := inherited Text;
  try
    StrToFloat(St);
  except
    SetValue(FOld);
  end;
end;

function TJvFloatEdit.GetValue: Extended;
var
  St: string;
begin
  St := inherited Text;
  try
    Result := StrToFloat(St);
  except
    Result := 0.0;
  end;
end;

procedure TJvFloatEdit.SetValue(const Value: Extended);
var
  St: string;
begin
  St := FloatToStr(Value);
  inherited Text := St;
end;

end.
