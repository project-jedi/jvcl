{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngineControlCxGrid.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvControlActionsEngineCxGrid;

{$I jvcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  Forms, Controls, Classes, DB,
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGridCustomTableView, cxDBData, cxGridCustomView, cxGrid, cxGridChartView,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  JvControlActionsEngine, JvActionsEngine;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
type
  TJvControlActioncxGridEngine = class(TJvControlActionEngine)
  private
    procedure ExportChartViewToImage(aExtension, aFileName: string; aChartView: TcxGridChartView);
  protected
    procedure ExportGrid(aGrid: TcxGrid);
    function GetGridTableView(AActionComponent: TComponent): TcxCustomGridTableView;
    function GetGridView(AActionComponent: TComponent): TcxCustomGridView;
    function GetGrid(AActionComponent: TComponent): TcxGrid;
    function GetSupportedOperations: TJvControlActionOperations; override;
  public
    function ExecuteOperation(const aOperation: TJvControlActionOperation; const aActionControl: TControl): Boolean; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
    function UpdateAction(Action: TBasicAction): boolean; override;
  end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile:
      '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGridDBDataDefinitions, cxControls,
  cxCustomData, cxGridExportLink,
  {$IFDEF DELPHI12_UP}
  pngimage, jpeg,
  {$ENDIf}
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  Graphics, Variants, SysUtils, Dialogs, JvControlActions;

//=== { TJvDatabaseActionDevExpCxGridControlEngine } =========================

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}

function TJvControlActioncxGridEngine.ExecuteOperation(const aOperation: TJvControlActionOperation; const
    aActionControl: TControl): Boolean;
begin
  Result := true;
  case aOperation of
    caoCollapse:
      if Assigned(GetGridTableView(aActionControl)) then
        GetGridTableView(aActionControl).Datacontroller.Groups.FullCollapse
      else
        Result := false;
    caoExpand:
      if Assigned(GetGridTableView(aActionControl)) then
        GetGridTableView(aActionControl).Datacontroller.Groups.FullExpand
      else
        Result := false;
    caoOptimizeColumns:
      if Assigned(GetGridTableView(aActionControl)) then
        GetGridTableView(aActionControl).ApplyBestFit
      else
        Result := false;
    caoExport:
      if Assigned(GetGridView(aActionControl)) then
        ExportGrid(GetGrid(aActionControl))
      else
        Result := false;
    caoCustomizeColumns:
      if Assigned(GetGridView(aActionControl)) then
        GetGridView(aActionControl).Controller.Customization := not GetGridView(aActionControl).Controller.Customization
      else
        Result := false;
  else
    Result := False
  end;
end;

procedure TJvControlActioncxGridEngine.ExportChartViewToImage(aExtension, aFileName: string; aChartView:
    TcxGridChartView);
var
  AGraphic: TGraphic;
  TmpGraphic: TGraphic;
begin
  if (aExtension = '.WMF') or (aExtension = '.EMF')  then
  begin
    AGraphic := AChartView.CreateImage(TMetaFile);
    TMetaFile(AGraphic).Enhanced := aExtension = '.EMF';
  end
  else
  AGraphic := AChartView.CreateImage(TBitmap);
  {$IFDEF DELPHI12_UP}
  if aExtension = '.PNG' then
    TMPGraphic := TPNGImage.Create
  else if aExtension = '.JPG' then
    TMPGraphic := TJPEGImage.Create
  else
    TMPGraphic := nil;
  if Assigned(TMPGraphic) then
  begin
    TMPGraphic.Assign(AGraphic);
    TMPGraphic.SaveToFile(aFileName);
    TMPGraphic.Free;
  end
  else
  {$ENDIF}
  AGraphic.SaveToFile(aFileName);
  AGraphic.Free;
end;

procedure TJvControlActioncxGridEngine.ExportGrid(aGrid: TcxGrid);
var
  SaveDialog: TSaveDialog;
  Extension: String;
  FileName: String;
begin
  if not Assigned(aGrid) then
    Exit;
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Name := 'SaveDialog';
    SaveDialog.DefaultExt := 'XLSX';
    SaveDialog.Filter :=
      'MS-Excel-Files (*.XLS;*.XLSX)|*.XLS;*.XLSX|XML-Files (*.XML)|*.XML|HTML-Files (*.HTM;*.HTML)|*.HTM;*.HTML|Text-Files (*.TXT)|*.TXT';
    if GetGridView(aGrid) is TcxGridChartView then
      {$IFDEF DELPHI12_UP}
      SaveDialog.Filter := SaveDialog.Filter+'|Image-Files (*.PNG;*.JPG;*.BMP)|*.PNG;*.JPG;*.BMP|Metafile-Graphics (*.WMF;*.EMF)|*.WMF;*.EMF';
      {$ELSE}
      SaveDialog.Filter := SaveDialog.Filter+'|Image-Files (*.BMP)|*.BMP|Metafile-Graphics (*.WMF;*.EMF)|*.WMF;*.EMF';
      {$ENDIF}
    SaveDialog.Filter := SaveDialog.Filter+'|All Files (*.*)|*.*';
    SaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
    if SaveDialog.Execute then
      if SaveDialog.FileName <> '' then
      begin
        FileName := SaveDialog.Filename;
        Extension := Uppercase(ExtractFileExt(FileName));
        if Extension = '.XLS' then
          ExportGridToExcel(Filename, aGrid)
        else if Extension = '.XLSX' then
          ExportGridToXLSX(Filename, aGrid)
        else if ((Extension = '.BMP') or (Extension = '.JPG') or (Extension = '.PNG') or
                 (Extension = '.WMF') or (Extension = '.EMF'))
            and (GetGridView(aGrid) is TcxGridChartView) then
          ExportChartViewToImage(Extension, Filename, TcxGridChartView(GetGridView(aGrid)))
        else if Extension = 'XML' then
          ExportGridToXML(Filename, aGrid)
        else if (Extension = '.HTM') or (Extension = '.HTML') then
          ExportGridToHTML(Filename, aGrid)
        else
          ExportGridToText(Filename, aGrid);
      end;
  finally
    SaveDialog.Free;
  end;
end;

function TJvControlActioncxGridEngine.GetGridTableView(AActionComponent: TComponent): TcxCustomGridTableView;
var GridView : TcxCustomGridView;
begin
  GridView := GetGridView(AActionComponent);
  if GridView is TcxCustomGridTableView then
    Result := TcxCustomGridTableView(GridView)
  else
    Result := nil;
end;

function TJvControlActioncxGridEngine.GetGridView(AActionComponent: TComponent): TcxCustomGridView;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TcxGridSite then
      Result := TcxGridSite(AActionComponent).GridView
    else
      if AActionComponent is TcxGrid then
        Result := TcxGrid(AActionComponent).ActiveView
      else
        Result := nil
  else
    Result := nil;
end;

function TJvControlActioncxGridEngine.GetGrid(AActionComponent: TComponent): TcxGrid;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TcxGridSite then
      Result := TcxGrid(TcxGridSite(AActionComponent).Container)
    else
      if AActionComponent is TcxGrid then
        Result := TcxGrid(AActionComponent)
      else
        Result := nil
  else
    Result := nil;
end;

function TJvControlActioncxGridEngine.GetSupportedOperations:
  TJvControlActionOperations;
begin
  Result := [caoCollapse, caoExpand, caoOptimizeColumns, caoExport, caoCustomizeColumns];
end;

function TJvControlActioncxGridEngine.SupportsComponent(aActionComponent:
  TComponent): Boolean;
begin
  Result := Assigned(GetGridView(AActionComponent));
end;

function TJvControlActioncxGridEngine.UpdateAction(Action: TBasicAction): boolean;
begin
  Result := Inherited UpdateAction(Action);
  if Assigned(Action) and (Action is TJvControlBaseAction) and
    Assigned(GetGridView(TJvControlBaseAction(Action).ActionComponent)) and (TJvControlBaseAction(Action).ControlOperation = caoCustomizeColumns) then
    TJvControlBaseAction(Action).SetChecked(GetGridView(TJvControlBaseAction(Action).ActionComponent).Controller.Customization);
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}

procedure InitActionEngineList;
begin
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  RegisterControlActionEngine(TJvControlActioncxGridEngine);
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
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
