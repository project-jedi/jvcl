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

The Original Code is: JvPatcherEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQPatcherEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QControls, QForms, 
  DesignEditors, DesignIntf, 
  JvQPatchForm;

type
  TJvPatcherEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

implementation

uses
  JvQDsgnConsts;

function TJvPatcherEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paSortList];
end;

procedure TJvPatcherEditor.Edit;
var
  Dlg: TPatchFrm;
  Res: TStringList;
begin
  Res := TStringList(GetOrdValue);
  Dlg := TPatchFrm.Create(Application);
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

procedure TJvPatcherEditor.SetValue(const Value: string);
begin
  inherited SetValue(Value);
  if Value = '' then
    TStrings(GetOrdValue).Clear;
end;

function TJvPatcherEditor.GetValue: string;
begin
  if TStrings(GetOrdValue).Count = 0 then
    Result := RsNone
  else
  if TStrings(GetOrdValue).Count > 4 then // first four items are filenames and file sizes
    Result := RsDiff
  else
    Result := RsEqual;
end;

end.

