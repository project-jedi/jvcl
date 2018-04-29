{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngineControlcxTreeList.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvControlActionsEngineCxTreeList;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Forms, Controls, Classes, DB,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  cxTL,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  JvControlActionsEngine, JvActionsEngine;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
type
  TJvControlActioncxTreeListEngine = class(TJvControlActionEngine)
  private
  protected
    procedure ExportTreeList(aTreeList: TcxCustomTreeList);
    function GetSupportedOperations: TJvControlActionOperations; override;
    function GetTreeList(AActionComponent: TComponent): TcxCustomTreeList;
  public
    function ExecuteOperation(const aOperation: TJvControlActionOperation; const aActionControl: TControl): Boolean; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
    function UpdateAction(Action: TBasicAction): boolean; override;
  end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  cxTLExportLink,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  Variants, SysUtils, Grids, Dialogs, JvControlActions;

//=== { TJvDatabaseActionDevExpcxTreeListControlEngine } =========================

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}

function TJvControlActioncxTreeListEngine.ExecuteOperation(const aOperation: TJvControlActionOperation; const
    aActionControl: TControl): Boolean;
begin
  Result := false;
  if Assigned(GetTreeList(aActionControl)) then
    Case aOperation of
      caoCollapse : GetTreeList(aActionControl).FullCollapse;
      caoExpand : GetTreeList(aActionControl).FullExpand;
      caoOptimizeColumns : GetTreeList(aActionControl).ApplyBestFit;
      caoCustomizeColumns : GetTreeList(aActionControl).Customizing.Visible := not GetTreeList(aActionControl).Customizing.Visible;
      caoExport : ExportTreeList(GetTreeList(aActionControl));
    End;
end;

procedure TJvControlActioncxTreeListEngine.ExportTreeList(aTreeList: TcxCustomTreeList);
var
  SaveDialog: TSaveDialog;
  Extension: String;
  FileName: String;
begin
  if not Assigned(aTreeList) then
    Exit;
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Name    := 'SaveDialog';
    SaveDialog.DefaultExt := 'XLSX';
    SaveDialog.Filter  := 'MS-Excel-Files (*.XLS;*.XLSX)|*.XLS;*.XLSX|XML-Files (*.XML)|*.HTM|HTML-Files (*.HTM)|*.HTM|Text-Files (*.TXT)|*.TXT|All Files (*.*)|*.*';
    SaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
    if SaveDialog.Execute then
      if SaveDialog.FileName <> '' then
      begin
        FileName := SaveDialog.Filename;
        Extension := Uppercase(ExtractFileExt(FileName));
        if Extension = '.XLS' then
          cxExportTLToExcel(Filename, aTreeList)
        else if Extension = '.XLSX' then
          cxExportTLToXLSX(Filename, aTreeList)
        else if Extension = 'XML' then
          cxExportTLToXML(Filename, aTreeList)
        else if (Extension = '.HTM') or (Extension = '.HTML') then
          cxExportTLToHTML(Filename, aTreeList)
        else
          cxExportTLToText(Filename, aTreeList);
      end;
  finally
    SaveDialog.Free;
  end;
end;

function TJvControlActioncxTreeListEngine.GetSupportedOperations: TJvControlActionOperations;
begin
  Result := [caoExport, caoCollapse, caoExpand, caoOptimizeColumns, caoCustomizeColumns];
end;

function TJvControlActioncxTreeListEngine.GetTreeList(AActionComponent: TComponent): TcxCustomTreeList;
begin
  if Assigned(AActionComponent) and (AActionComponent is TcxCustomTreeList) then
    Result := TcxCustomTreeList(AActionComponent)
  else
    Result := nil;
end;

function TJvControlActioncxTreeListEngine.SupportsComponent(aActionComponent: TComponent): Boolean;
begin
  Result := Assigned(GetTreeList(AActionComponent));
end;

function TJvControlActioncxTreeListEngine.UpdateAction(Action: TBasicAction): boolean;
begin
  Result := Inherited UpdateAction(Action);
  if Assigned(Action) and (Action is TJvControlBaseAction) and
    Assigned(GetTreeList(TJvControlBaseAction(Action).ActionComponent)) and (TJvControlBaseAction(Action).ControlOperation = caoCustomizeColumns) then
    TJvControlBaseAction(Action).SetChecked(GetTreeList(TJvControlBaseAction(Action).ActionComponent).Customizing.Visible);
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}

procedure InitActionEngineList;
begin
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  RegisterControlActionEngine(TJvControlActioncxTreeListEngine);
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
end;


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitActionEngineList;

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
