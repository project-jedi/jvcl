{{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVHLEditEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JVHLEditEditor;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils, Windows, Controls,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvEditor, JvHLEditor, JvHLEditorPropertyForm, JvFormPlacement;

type
  TJvHLEdPropDlgEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TJvHLEditorColorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


implementation

uses
  JvAppStorage, JvAppIniStorage, JvDsgnConsts;

//=== { TJvHLEdPropDlgEditor } ===============================================

function TJvHLEdPropDlgEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvHLEdPropDlgEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := RsExecute
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvHLEdPropDlgEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

procedure TJvHLEdPropDlgEditor.Edit;
var
  PakName: string;
  NewRegAuto: TJvFormStorage;
  NewStore: TJvAppIniFileStorage;
  OldRegAuto: TJvFormStorage;
begin
  if (Component as TJvHLEdPropDlg).JvHLEditor <> nil then
    begin
      NewRegAuto := nil;
      NewStore := nil;
      try
        NewRegAuto := TJvFormStorage.Create(nil);
        NewStore := TJvAppIniFileStorage.Create(nil);
        NewRegAuto.AppStorage := NewStore;
        SetLength(PakName, MAX_PATH);
        SetLength(PakName, GetModuleFileName(hInstance, PChar(PakName), MAX_PATH));
        NewStore.Location := flCustom;
        NewStore.FileName := ExtractFilePath(PakName) + RsJvHLEdPropDlgIni;
        with Component as TJvHLEdPropDlg do
        begin
          OldRegAuto := Storage;
          try
            Storage := NewRegAuto;
            if Execute then
              Designer.Modified;
          finally
            Storage := OldRegAuto;
          end;
        end;
      finally
        NewRegAuto.Free;
        NewStore.Free;
      end;
    end
  else
    MessageBox(HWND_DESKTOP, PChar(RsHLEditorMsg), PChar(RsHLEditorMsgTitle), MB_OK + MB_ICONERROR);
end;

procedure TJvHLEditorColorProperty.Edit;
begin
  with TJvHLEdPropDlg.Create(nil) do
    try
      JvHLEditor := GetComponent(0) as TJvHLEditor;
      HighlighterCombo := False;
      ReadFrom := rfHLEditor;
      Pages := [epColors];
      if Execute then
        Designer.Modified;
    finally
      Free;
    end;
end;

function TJvHLEditorColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

end.
