{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaCtlReg.pas, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Register custom useful controls

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvaCtlReg;

interface

procedure Register;

implementation

uses
  {$IFDEF COMPLIB_VCL}
  Windows,
  {$ENDIF COMPLIB_VCL}
  Classes, JvCtlConst, JvRegAuto, JvaDsgn, JvDsgnIntf,
  {$IFDEF COMPLIB_VCL}
  SysUtils, Controls,
  JvHooks, JvRegAutoEditor, JvHtControls, JvDlg,
  JvButtons, JvComponentPanel, JvScrollMax,
  JvEditor, JvHLEditor, JvHLEdPropDlg, JvaScrollText, JvHTHintEditor,
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPILER3_UP}
  JvIDEZoom,
  {$ENDIF COMPILER3_UP}
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories
  {$IFDEF COMPLIB_VCL}
  , VCLEditors
  {$ENDIF COMPLIB_VCL}
  {$ELSE}
  DsgnIntf
  {$ENDIF COMPILER6_UP}
  ;

{$R ractl.dcr}

{$IFDEF COMPLIB_VCL}
type

  TJvScrollMaxEditor = class(TComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
    procedure Edit; override;
  end;

  TJvHLEdPropDlgEditor = class(TComponentEditor)
  public
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
    procedure Edit; override;
  end;

  TJvHLEditorColorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TJvScrollMaxEditor }

function TJvScrollMaxEditor.GetVerbCount: integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvScrollMaxEditor.GetVerb(Index: integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Add Band'
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvScrollMaxEditor.ExecuteVerb(Index: integer);
begin
  if Index = GetVerbCount - 1 then
    Designer.CreateComponent(TJvScrollMaxBand, Component, 0, 0, 0, 50)
  else
    inherited ExecuteVerb(Index);
end;

procedure TJvScrollMaxEditor.Edit;
begin
  // We don't need to add band on double click
end;

{ TJvHLEdPropDlgEditor }

function TJvHLEdPropDlgEditor.GetVerbCount: integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvHLEdPropDlgEditor.GetVerb(Index: integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Execute'
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvHLEdPropDlgEditor.ExecuteVerb(Index: integer);
begin
  if Index = GetVerbCount - 1 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

procedure TJvHLEdPropDlgEditor.Edit;
var
  PakName: string;
  newRegAuto: TJvRegAuto;
  oldRegAuto: TJvRegAuto;
begin
  if (Component as TJvHLEdPropDlg).JvHLEditor <> nil then
    begin
      newRegAuto := TJvRegAuto.Create(nil);
      try
        newRegAuto.UseReg := False;
        newRegAuto.UseIni := True;
        newRegAuto.UseStr := False;
        SetLength(PakName, 260);
        SetLength(PakName, GetModuleFileName(hInstance, PChar(PakName), 260));
        newRegAuto.IniFile := ExtractFilePath(PakName) + 'JvHLEdPropDlg.ini';
        oldRegAuto := (Component as TJvHLEdPropDlg).RegAuto;
        (Component as TJvHLEdPropDlg).RegAuto := newRegAuto;
        if (Component as TJvHLEdPropDlg).Execute then
          Designer.Modified;
        (Component as TJvHLEdPropDlg).RegAuto := oldRegAuto;
      finally
        newRegAuto.Free;
      end;
    end
  else
    MessageBox(0, 'Please select "JvHLEditor" first', 'Cannot edit', MB_OK + MB_ICONERROR);
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

{$IFDEF COMPILER5}
type
  TJvEditorCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
  end;
  { TEditorCategory }

class function TJvEditorCategory.Name: string;
begin
  Result := 'Editor';
end;
{$ENDIF COMPILER5}

{$ENDIF COMPLIB_VCL}

const
  {$IFDEF MSWINDOWS}
  RALibTabName = 'JVCL-RA';
  {$ENDIF}
  {$IFDEF LINUX}
  RALibTabName = 'JVCL-RA';
  {$ENDIF}

procedure Register;
begin
  {$IFDEF COMPLIB_VCL}
  {JvEditor unit}
  RegisterComponents(RALibTabName, [TJvEditor]);
  {JvHLEditor unit}
  RegisterComponents(RALibTabName, [TJvHLEditor]);
  {JvHLEdPropDlg unit}
  RegisterComponents(RALibTabName, [TJvHLEdPropDlg]);
  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterPropertyEditor(TypeInfo(TJvColors), TJvHLEditor, 'Colors', TJvHLEditorColorProperty);
  {JvScrollMax unit}
  RegisterComponents(RALibTabName, [TJvScrollMax]);
  RegisterClass(TJvScrollMaxBand);
  RegisterComponentEditor(TJvScrollMax, TJvScrollMaxEditor);
  {JvaScrollText}
  RegisterComponents(RALibTabName, [TJvaScrollText]);
  {JvRegAuto unit}
  RegisterComponents(RALibTabName, [TJvRegAuto]);
  RegisterComponentEditor(TJvRegAuto, TJvRegAutoEditor);
  {JvHtControls unit}
  RegisterComponents(RALibTabName, [TJvhtListBox, TJvHTComboBox, TJvHTLabel]);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
  {JvButtons unit}
  RegisterComponents(RALibTabName, [TJvHTButton, TJvaCaptionButton]);
  {JvDlg unit}
  RegisterComponents(RALibTabName, [TJvProgressForm]);
  {JvComponentPanel unit}
  RegisterComponents(RALibTabName, [TJvComponentPanel]);
  {$ENDIF COMPLIB_VCL}

  {$IFDEF COMPLIB_CLX}
  {JvRegAuto unit}
  RegisterComponents(RALibTabName, [TJvRegAuto]);
  {$ENDIF COMPLIB_CLX}

  {Zoom unit}
  {$IFDEF COMPILER3_UP}
  RegisterZoom;
  {$ENDIF COMPILER3_UP}

  {$IFDEF COMPILER5}
  RegisterPropertiesInCategory(TJvEditorCategory, TJvCustomEditor,
    ['InsertMode', 'DoubleClickLine', 'Completion', 'SmartTab',
    'BackSpaceUnindents', 'AutoIndent', 'KeepTrailingBlanks', 'CursorBeyondEOF',
      'GutterColor', 'GutterWidth',
      'RightMarginVisible', 'RightMargin', 'RightMarginColor',

    'OnGetLineAttr', 'OnReservedWord', 'OnCompletionIdentifer',
      'OnCompletionDrawItem', 'OnCompletionMeasureItem', 'OnCompletionTemplate',
      'OnChange', 'OnChangeStatus', 'OnSelectionChange']);

  RegisterPropertiesInCategory(TVisualCategory, TJvCustomEditor,
    ['ScrollBars',
    'RightMarginVisible', 'RightMargin', 'RightMarginColor',
      'OnPaintGutter', 'OnScroll', 'OnConstrainedResize']);
  {$ENDIF}

  {$IFDEF COMPILER6_UP}
  {$IFDEF COMPLIB_VCL}
  RegisterPropertiesInCategory('Editor', TJvCustomEditor,
    ['InsertMode', 'DoubleClickLine', 'Completion', 'SmartTab',
    'BackSpaceUnindents', 'AutoIndent', 'KeepTrailingBlanks', 'CursorBeyondEOF',
      'GutterColor', 'GutterWidth',
      'RightMarginVisible', 'RightMargin', 'RightMarginColor',

    'OnGetLineAttr', 'OnReservedWord', 'OnCompletionIdentifer',
      'OnCompletionDrawItem', 'OnCompletionMeasureItem', 'OnCompletionTemplate',
      'OnChange', 'OnChangeStatus', 'OnSelectionChange']);

  RegisterPropertiesInCategory('Visual', TJvCustomEditor,
    ['ScrollBars',
    'RightMarginVisible', 'RightMargin', 'RightMarginColor',
      'OnPaintGutter', 'OnScroll', 'OnConstrainedResize']);
  {$ENDIF}
  {$ENDIF}
end;

end.

