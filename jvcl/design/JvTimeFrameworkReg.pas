{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeFrameworkReg.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTimeFrameworkReg;

// WARNING!!
// THIS FILE CANNOT BE INCLUDED IN A RUNTIME PACKAGE.  IF YOU INCLUDE
// THIS FILE IN A RUNTIME PACKAGE THE CODE FROM DsgnIntf WILL BE COMPILED
// INTO THE PACKAGE.  DsgnIntf IS *NOT* DESIGNATED AS REDISTRIBUTABLE BY
// BORLAND.

interface

uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  ColnEdit;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvTimeFrameworkReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvTimeFrameworkReg.dcr}
{$ENDIF LINUX}

type
  {$IFDEF COMPILER5}

  TJvTFGridLayoutCategory = class(TLayoutCategory)
  public
    class function Name: string; override;
  end;

  TJvTFCustomDrawCategory = class(TVisualCategory)
  public
    class function Name: string; override;
  end;

  {$ENDIF COMPILER5}

  TJvTFGlanceCellsProperty = class(TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

procedure Register;

implementation

uses
  Classes,
  {$IFDEF USEJVCL}
  JvDsgnConsts,
  {$ENDIF USEJVCL}
  JvTFGlance, JvTFGlanceTextViewer, JvTFMonths, JvTFWeeks, JvTFDays,
  JvTFAlarm, JvTFManager;
  
{$IFNDEF USEJVCL}
resourcestring
  RsPaletteTimeFramework = 'Jv TimeFrameWork';
  RsGridLayout = 'Grid Layout';
  RsCustomDraw = 'Custom Draw';
{$ENDIF USEJVCL}

//=== { TJvTFGridLayoutCategory } ============================================

{$IFDEF COMPILER5}
class function TJvTFGridLayoutCategory.Name: string;
begin
  Result := RsGridLayout;
end;
{$ENDIF COMPILER5}

//=== { TJvTFCustomDrawCategory } ============================================

{$IFDEF COMPILER5}
class function TJvTFCustomDrawCategory.Name: string;
begin
  Result := RsCustomDraw;
end;
{$ENDIF COMPILER5}

//=== { TJvTFGlanceCellsProperty } ===========================================

function TJvTFGlanceCellsProperty.GetColOptions: TColOptions;
begin
  Result := [];
end;

procedure Register;
begin
  RegisterComponents(RsPaletteTimeFramework, [TJvTFScheduleManager,
    TJvTFUniversalPrinter]);
//  RegisterPropertyEditor(TypeInfo(string), TJvTFControl, 'Version', TutfVersionEditor);
//  RegisterPropertyEditor(TypeInfo(string), TJvTFScheduleManager, 'Version', TutfVersionEditor);
  RegisterComponents(RsPaletteTimeFramework, [TJvTFGlanceTextViewer, TJvTFMonths,
    TJvTFWeeks, TJvTFAlarm]);
//  RegisterPropertyEditor(TypeInfo(TJvTFGlanceCells), '', 'Cells',
//    TJvTFGlanceCellsProperty);

  // register a nil property editor for now, so cells cannot be added,
  // deleted, or moved at design time... BAD THINGS HAPPEN
  RegisterPropertyEditor(TypeInfo(TJvTFGlanceCells), TJvTFMonths, 'Cells', nil);
  RegisterComponents(RsPaletteTimeFramework, [TJvTFDays, TJvTFDaysPrinter]);
  {$IFDEF COMPILER5}
  RegisterPropertiesInCategory(TVisualCategory, ['StateImages', 'CustomImages',
    'StateImageMap']);
  RegisterPropertiesInCategory(TDatabaseCategory, ['OnNeedAppts', 'OnPostAppt',
    'OnDeleteAppt', 'OnRefreshAppt', 'OnRefreshSched', 'OnRefreshAll',
    'OnPostApptQuery']);
  RegisterPropertiesInCategory(TJvTFCustomDrawCategory, ['OnDrawHeader',
    'OnDrawBody', 'OnDrawFooter']);
  RegisterPropertiesInCategory(TVisualCategory, ['ApptAttr', 'ApptBar',
    'ApptBuffer', 'ColTitleStyle', 'DateFormat', 'FancyRowHdrAttr',
    'GrabHandles', 'Granularity', 'HdrAttr', 'Options', 'RowHdrType',
    'SelApptAttr', 'SelCellAttr', 'SelFancyRowHdrAttr', 'SelHdrAttr',
    'Thresholds', 'TimeFormat', 'FormattedDesc', 'Grouping',
    'utfHintProps', 'GroupHdrAttr', 'SelGroupHdrAttr']);
  RegisterPropertiesInCategory(TJvTFGridLayoutCategory, ['AutoSizeCols',
    'ColHdrHeight', 'Cols', 'DefColWidth', 'Granularity', 'LeftCol',
    'MinColWidth', 'MinRowHeight', 'RowHdrWidth', 'RowHeight', 'Template',
    'TopRow', 'Grouping', 'GroupHdrHeight']);
  RegisterPropertiesInCategory(TLocaleCategory, ['DateFormat', 'TimeFormat']);
  RegisterPropertiesInCategory(TInputCategory, ['FocusedCol', 'FocusedRow',
    'Options']);
  RegisterPropertiesInCategory(TDragNDropCategory, ['GrabHandles', 'Options']);
  RegisterPropertiesInCategory(TJvTFGridLayoutCategory, ['OnDateChanged',
    'OnDateChanging', 'OnDeleteAppt', 'OnDeleteSchedule',
    'OnGranularityChanged', 'OnGranularityChanging', 'OnInsertAppt',
    'OnInsertSchedule', 'OnMoveCol', 'OnSizeCol', 'OnSizeColHdr', 'OnSizeRow',
    'OnSizeRowHdr', 'OnFocusedColChanged', 'OnFocusedRowChanged']);
  RegisterPropertiesInCategory(TInputCategory, ['OnDeleteAppt',
    'OnDeleteSchedule', 'OnFailEditor', 'OnInsertAppt', 'OnInsertSchedule',
    'OnQuickEntry', 'OnFocusedColChanged', 'OnFocusedRowChanged',
    'OnCreateQuickEntry']);
  RegisterPropertiesInCategory(TJvTFCustomDrawCategory, ['OnDrawAppt',
    'OnDrawColHdr', 'OnDrawCorner', 'OnDrawDataCell', 'OnDrawGrabHandle',
    'OnDrawMajorRowHdr', 'OnDrawMinorRowHdr', 'OnDrawRow', 'OnDrawRowHdr',
    'OnDrawApptBar', 'OnDrawGroupHdr', 'OnShadeCell']);
  RegisterPropertiesInCategory(TDragNDropCategory, ['OnSizeAppt', 'OnDropAppt']);
  RegisterPropertiesInCategory(TMiscellaneousCategory, ['OnSelectAppt',
    'OnSelectedAppt', 'OnSelectingAppt', 'OnSizeAppt', 'OnUpdateColTitles']);
  {$ENDIF COMPILER5}
end;

end.

