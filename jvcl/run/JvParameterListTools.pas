{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
withOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvParameterListTools;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

function ParameterListRadioGroupBox(SelectList: string;
  Caption: string = ''; Default: Integer = 0): string;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, SysUtils,
  JvParameterList, JvParameterListParameter, JvResources;

function ParameterListRadioGroupBox(SelectList: string;
  Caption: string = ''; Default: Integer = 0): string;
const
 cSelectionType = 'SelectionType';
var
  ParameterList: TJvParameterList;
  Parameter: TJvRadioGroupParameter;
  S: TStringList;
  I: Integer;

  procedure InsertParameter(ItemText: string);
  begin
    if Trim(ItemText) = '' then
      Exit;
    Parameter.ItemList.Add(ItemText);
    Parameter.Height := Parameter.Height + 15;
  end;

begin
  Result := '';
  if SelectList = '' then
    Exit;
  ParameterList := TJvParameterList.Create(nil);
  S := TStringList.Create;
  try
    ParameterList.Messages.Caption := RsSelectCaption;
    S.Text := SelectList;
    if S.Count = 1 then
    begin
      Result := S[0];
      Exit;
    end;
    Parameter := TJvRadioGroupParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := cSelectionType;
      Caption := Caption;
      ItemIndex := 0;
      Width := 200;
      Height := 30;
    end;
    for I := 0 to S.Count - 1 do
      InsertParameter(S[I]);
    Parameter.ItemIndex := Default;
    ParameterList.AddParameter(Parameter);
    if (Parameter.ItemIndex < 0) or
      (Parameter.ItemIndex >= Parameter.ItemList.Count) then
      Parameter.ItemIndex := 0;
    if Parameter.ItemList.Count = 1 then
      Result := Parameter.ItemList[0]
    else
    if ParameterList.ShowParameterDialog then
      Result :=
        TJvRadioGroupParameter(ParameterList.ParameterbyName(cSelectionType)).AsString;
  finally
    ParameterList.Free;
    S.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

