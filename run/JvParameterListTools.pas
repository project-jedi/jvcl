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


unit JvParameterListTools;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

function ParameterListRadioGroupBox (pSelectList : string;
                                 pCaption : string = '';
                                 pDefault : Integer = 0) : string;

implementation

Uses Classes, SysUtils, JvParameterList, JvParameterListParameter;

function ParameterListRadioGroupBox (pSelectList : string;
                                 pCaption : string = '';
                                 pDefault : Integer = 0) : string;
var
  ParameterList     : TJvParameterList;
  Parameter         : TJvRadioGroupParameter;
  s                 : tStringList;
  i                 : Integer;

  procedure InsertParameter(ItemText: string);
  begin
    if Trim(ItemText) = '' then
      Exit;
    Parameter.ItemList.Add(ItemText);
    Parameter.Height := Parameter.Height + 15;
  end;

begin
  Result := '';
  if pSelectList = '' then
    Exit;
  ParameterList := TJvParameterList.Create(nil);
  s := tStringList.Create;
  try
    ParameterList.Messages.Caption := 'Select ...';
    s.Text := pSelectList;
    if s.Count = 1 then
    begin
      Result := s [0];
      Exit;
    end;
    Parameter := TJvRadioGroupParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'SelectionType';
      Caption := pCaption;
      ItemIndex := 0;
      Width := 200;
      Height := 30;
    end;                                {*** with Parameter do ***}
    for i := 0 to s.Count -1 do
      InsertParameter(s[i]);
    Parameter.ItemIndex := pDefault;
    ParameterList.AddParameter(Parameter);
    if (Parameter.ItemIndex < 0) or
      (Parameter.ItemIndex >= Parameter.ItemList.Count) then
      Parameter.ItemIndex := 0;
    if Parameter.ItemList.Count = 1 then
      Result := Parameter.ItemList[0]
    else
      if ParameterList.ShowParameterDialog then
        Result :=
          TJvRadioGroupParameter(ParameterList.ParameterbyName('SelectionType')).AsString;
  finally
    ParameterList.Free;
    s.Free;
  end;
end;

end.
