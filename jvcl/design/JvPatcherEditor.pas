{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPatcherEditor.PAS, released on 2001-02-28.

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

unit JvPatcherEditor;

interface

uses
  SysUtils, Classes, Controls, Forms,
  {$IFDEF COMPILER5}
  DsgnIntf,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ENDIF}
  JvFormPatch;

type
  TJvPatcherEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

implementation

uses
  JvxDConst;

function TJvPatcherEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paSortList];
end;

function TJvPatcherEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TJvPatcherEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TJvPatcherEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue(SJvEditorString);
end;

procedure TJvPatcherEditor.Edit;
var
  Dlg: TFoPatch;
  Res: TStringList;
begin
  Res := TStringList(GetOrdValue);
  Dlg := TFoPatch.Create(Application);
  Dlg.LoadFromStr(Res);
  try
    if Dlg.ShowModal = mrOk then
    begin
      Res.Assign(Dlg.SetFromStr);
      SetOrdValue(Integer(Res));
    end;
  finally
    Dlg.Free;
  end;
end;

end.

