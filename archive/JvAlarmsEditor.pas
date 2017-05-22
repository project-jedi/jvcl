{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlarmsEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAlarmsEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  JvAlarmsForm;

type
  TJvAlarmsEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

implementation

uses
  JvConsts, JvxDConst;

function TJvAlarmsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paSortList];
end;

function TJvAlarmsEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TJvAlarmsEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TJvAlarmsEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue(SJvEditorString);
end;

procedure TJvAlarmsEditor.Edit;
var
  Dlg: TFormAlarm;
  Res: TStringList;
begin
  Res := TStringList(GetOrdValue);
  Dlg := TFormAlarm.Create(Application);
  Dlg.LoadFromStr(Res);
  try
    Dlg.Tag := 1;
    Dlg.ShowModal;
    if Dlg.Tag = 0 then
    begin
      Res.Assign(Dlg.SetFromStr);
      SetOrdValue(Integer(Res));
    end;
  finally
    Dlg.Free;
  end;
end;

end.
