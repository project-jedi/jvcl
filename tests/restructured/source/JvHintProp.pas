{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHintProp.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHintProp;

interface

uses {$IFDEF COMPILER6_UP}DesignIntf, VCLEditors{$ELSE}DsgnIntf{$ENDIF};


type

{ THintProperty }

  THintProperty = class(TCaptionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
{$IFDEF WIN32}
    function GetEditLimit: Integer; override;
{$ENDIF}
    procedure Edit; override;
  end;

implementation

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

uses SysUtils, Classes, {$IFDEF COMPILER3_UP} JvStrLEdit, {$ELSE} StrEdit, {$ENDIF}
  TypInfo, Forms, Controls, JvStrUtils;

function THintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{$IFDEF WIN32}
function THintProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength
  else Result := 1024;
end;
{$ENDIF}

procedure THintProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
  I, Cnt: Integer;
begin
  with TJvStrEditDlg.Create(Application) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else Caption := GetName;
    Temp := GetStrValue;
    Cnt := WordCount(Temp, [#13, #10]);
    for I := 1 to Cnt do
      Memo.Lines.Add(ExtractWord(I, Temp, [#13, #10]));
    Memo.MaxLength := GetEditLimit;
    UpdateStatus(nil);
    if ShowModal = mrOk then begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

end.
