{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

{$I JEDI.INC}

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
{$IFDEF Delphi3_Up}
  JvIDEZoom,
{$ENDIF Delphi3_Up}
{$IFDEF Delphi6_Up}
  DesignIntf, DesignEditors, PropertyCategories
  {$IFDEF COMPLIB_VCL}
   , VCLEditors
  {$ENDIF COMPLIB_VCL}
{$ELSE}
  DsgnIntf
{$ENDIF Delphi6_Up}
;

{$R ractl.dcr}

{$IFDEF COMPLIB_VCL}
type

  TJvScrollMaxEditor = class(TComponentEditor)
  public
    function GetVerbCount : integer; override;
    function GetVerb(Index : integer) : string; override;
    procedure ExecuteVerb(Index : integer); override;
    procedure Edit; override;
  end;

  TJvHLEdPropDlgEditor = class(TComponentEditor)
  public
    function GetVerbCount : integer; override;
    function GetVerb(Index : integer) : string; override;
    procedure ExecuteVerb(Index : integer); override;
    procedure Edit; override;
  end;

  TJvHLEditorColorProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{ TJvScrollMaxEditor }
function TJvScrollMaxEditor.GetVerbCount : integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvScrollMaxEditor.GetVerb(Index : integer) : string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Add Band'
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvScrollMaxEditor.ExecuteVerb(Index : integer);
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
function TJvHLEdPropDlgEditor.GetVerbCount : integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvHLEdPropDlgEditor.GetVerb(Index : integer) : string;
begin
  if Index = GetVerbCount - 1 then
    Result := 'Execute'
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvHLEdPropDlgEditor.ExecuteVerb(Index : integer);
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

{$IFDEF Delphi5}
type
  TJvEditorCategory = class (TPropertyCategory)
  public
    class function Name: String; override;
  end;
{ TEditorCategory }


class function TJvEditorCategory.Name: String;
begin
  Result := 'Editor';
end;
{$ENDIF Delphi5}

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
  RegisterPropertyEditor(TypeInfo(TColors), TJvHLEditor, 'Colors', TJvHLEditorColorProperty);
 {JvScrollMax unit}
  RegisterComponents(RALibTabName, [TJvScrollMax]);
  RegisterClass(TJvScrollMaxBand);
  RegisterComponentEditor(TJvScrollMax, TJvScrollMaxEditor);
 {JvaScrollText}
  RegisterComponents(RALibTabName, [TJvaScrollText]);
 {JvRegAuto unit}
  RegisterComponents(RALibTabName, [TJvRegAuto]);
  RegisterComponentEditor(TJvRegAuto, TRegAutoEditor);
 {JvHtControls unit}
  RegisterComponents(RALibTabName, [TJvhtListBox, TJvHTComboBox, TJvHTLabel]);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
 {JvButtons unit}
  RegisterComponents (RALibTabName, [TJvHTButton, TJvaCaptionButton]);
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
 {$IFDEF Delphi3_Up}
  RegisterZoom;
 {$ENDIF Delphi3_Up}

 {$IFDEF Delphi5}
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

 {$IFDEF Delphi6_Up}
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
