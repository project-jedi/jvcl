{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaCtlReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Register Custom controls

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
  {$IFDEF COMPLIB_VCL}
  SysUtils, Controls,
  JvRegAutoEditor, JvHtControls, JvDlg,
  JvButtons, JvComponentPanel, JvScrollMax,
  JvEditor, JvHLEditor, JvHLEdPropDlg, JvaScrollText, JvHTHintEditor,
  {$ENDIF COMPLIB_VCL}
  JvIDEZoom,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$IFDEF COMPLIB_VCL}
  VCLEditors,
  {$ENDIF COMPLIB_VCL}
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  Classes,
  JvRegAuto, JvxDConst;

{$R ..\resources\ractl.dcr}

{$IFDEF COMPLIB_VCL}

resourcestring
  RS_JvHLEditorMsg = 'Please select "JvHLEditor" first';
  RS_JvHLEditorMsgTitle = 'Cannot edit';

type
  TJvScrollMaxEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

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

//=== TJvScrollMaxEditor =====================================================

function TJvScrollMaxEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvScrollMaxEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Add Band'
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvScrollMaxEditor.ExecuteVerb(Index: Integer);
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

//=== TJvHLEdPropDlgEditor ===================================================

function TJvHLEdPropDlgEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvHLEdPropDlgEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Execute'
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
  NewRegAuto: TJvRegAuto;
  OldRegAuto: TJvRegAuto;
begin
  if (Component as TJvHLEdPropDlg).JvHLEditor <> nil then
    begin
      NewRegAuto := TJvRegAuto.Create(nil);
      try
        NewRegAuto.UseReg := False;
        NewRegAuto.UseIni := True;
        NewRegAuto.UseStr := False;
        SetLength(PakName, MAX_PATH);
        SetLength(PakName, GetModuleFileName(hInstance, PChar(PakName), MAX_PATH));
        NewRegAuto.IniFile := ExtractFilePath(PakName) + srJvHLEdPropDlgIni;
        with Component as TJvHLEdPropDlg do
        begin
          OldRegAuto := RegAuto;
          RegAuto := NewRegAuto;
          if Execute then
            Designer.Modified;
          RegAuto := OldRegAuto;
        end;
      finally
        NewRegAuto.Free;
      end;
    end
  else
    MessageBox(0, PChar(RS_JvHLEditorMsg), PChar(RS_JvHLEditorMsgTitle), MB_OK + MB_ICONERROR);
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

//=== TJvEditorCategory ======================================================

type
  TJvEditorCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
  end;

class function TJvEditorCategory.Name: string;
begin
  Result := 'Editor';
end;

{$ENDIF COMPILER5}

{$ENDIF COMPLIB_VCL}

procedure Register;
begin
  {$IFDEF COMPLIB_VCL}
  {JvEditor unit}
  RegisterComponents(srJvCustomPalette, [TJvEditor]);
  {JvHLEditor unit}
  RegisterComponents(srJvCustomPalette, [TJvHLEditor]);
  {JvHLEdPropDlg unit}
  RegisterComponents(srJvCustomPalette, [TJvHLEdPropDlg]);
  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterPropertyEditor(TypeInfo(TJvColors), TJvHLEditor, 'Colors', TJvHLEditorColorProperty);
  {JvRegAuto unit}
  RegisterComponents(srJvCustomPalette, [TJvRegAuto]);
  RegisterComponentEditor(TJvRegAuto, TJvRegAutoEditor);

  {JvScrollMax unit}
  RegisterComponents(srJvCustomPalette, [TJvScrollMax]);
  RegisterClass(TJvScrollMaxBand);
  RegisterComponentEditor(TJvScrollMax, TJvScrollMaxEditor);
  {JvaScrollText}
  RegisterComponents(srJvAdditionalPalette, [TJvaScrollText]);

  {JvHtControls unit}
  RegisterComponents(srJvAdditionalPalette, [TJvhtListBox, TJvHTComboBox, TJvHTLabel]);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
  {JvButtons unit}
  RegisterComponents(srJvAdditionalPalette, [TJvHTButton]);
  RegisterComponents(srJvSystemPalette, [TJvaCaptionButton]);
  {JvDlg unit}
  RegisterComponents(srJvDialogsPalette, [TJvProgressForm]);
  {JvComponentPanel unit}
  RegisterComponents(srJvCustomPalette, [TJvComponentPanel]);
  {$ENDIF COMPLIB_VCL}

  {$IFDEF COMPLIB_CLX}
  {JvRegAuto unit}
  RegisterComponents(RALibTabName, [TJvRegAuto]);
  {$ENDIF COMPLIB_CLX}

  {Zoom unit}
  RegisterZoom;

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

