{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppletEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppletEditor;

{$I jvcl.inc}
{$I vclonly.inc}

interface

uses
  Windows, Classes,
  {$IFDEF VCL}
  Controls, Forms, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QDialogs,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvWinDialogs;

type
  // (p3) changed to show the "friendly" names in a list: replaces
  // select value with CPL name
  TJvAppletNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TJvAppletIndexProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  SysUtils,
  JclSysInfo,
  JvJCLUtils, JvJVCLUtils;

var
  FApplets: TStringList = nil;

procedure Refresh;
var
  S: string;
begin
  if FApplets = nil then
    FApplets := TStringList.Create;
  FApplets.Clear;
  S := IncludeTrailingPathDelimiter(GetWindowsSystemFolder);
  GetControlPanelApplets(S, '*.cpl', FApplets, nil);
end;

//=== { TJvAppletNameProperty } ==============================================

function TJvAppletNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect, paRevertable];
end;

procedure TJvAppletNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  if FApplets = nil then
    Refresh;
  for I := 0 to FApplets.Count - 1 do
    Proc(FApplets.Names[I]);
end;

procedure TJvAppletNameProperty.SetValue(const Value: string);
var
  I: Integer;
  S: string;
begin
  if FApplets = nil then
    Refresh;
  I := FApplets.IndexOfName(Value);
  if I >= 0 then
  begin
    S := FApplets.Values[Value];
    inherited SetValue(Copy(S, 1, Pos(',', S) - 1));
  end
  else
    inherited SetValue(Value);
end;

//=== { TJvAppletIndexProperty } ==============================================

function TJvAppletIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

procedure TJvAppletIndexProperty.GetValues(Proc: TGetStrProc);
var
  I, J: Integer;
begin
  J := (GetComponent(0) as TJvAppletDialog).Count;
  for I := 0 to J - 1 do
    Proc(IntToStr(I));
end;

initialization

finalization
  FreeAndNil(FApplets);

end.

