{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaCtlReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
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
  {$IFDEF VCL}
  Windows,
  {$ENDIF VCL}
  {$IFDEF VCL}
  SysUtils, Controls,
  JvRegAutoEditor, JvHtControls, JvDlg,
  JvButtons, JvComponentPanel, JvScrollMax,
  JvEditor, JvHLEditor, JvHLEdPropDlg, JvaScrollText, JvHTHintEditor,
  {$ENDIF VCL}
  JvIDEZoom, JvHLEditEditor, JvScrollMaxEditor,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$IFDEF VCL}
  VCLEditors,
  {$ENDIF VCL}
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  Classes, JvRegAuto, JvxDConst;

{$R ..\resources\ractl.dcr}

{$IFDEF VCL}

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

{$ENDIF VCL}

procedure Register;
begin
  {$IFDEF VCL}
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
  {$ENDIF VCL}

  {$IFDEF VisualCLX}
  {JvRegAuto unit}
  RegisterComponents(RALibTabName, [TJvRegAuto]);
  {$ENDIF VisualCLX}

  {Zoom unit}
  RegisterZoom;

  {$IFDEF COMPILER5}
  RegisterPropertiesInCategory(TJvEditorCategory, TJvCustomEditor,
    ['InsertMode', 'DoubleClickLine', 'Completion', 'SmartTab',
     'BackSpaceUnindents', 'AutoIndent', 'KeepTrailingBlanks', 'CursorBeyondEOF',
     'GutterColor', 'GutterWidth',
     'RightMarginVisible', 'RightMargin', 'RightMarginColor',

     'OnGetLineAttr', 'OnReservedWord', 'OnCompletionIdentifier',
     'OnCompletionDrawItem', 'OnCompletionMeasureItem', 'OnCompletionTemplate',
     'OnChange', 'OnChangeStatus', 'OnSelectionChange']);

  RegisterPropertiesInCategory(TVisualCategory, TJvCustomEditor,
    ['ScrollBars',
     'RightMarginVisible', 'RightMargin', 'RightMarginColor',
     'OnPaintGutter', 'OnScroll', 'OnConstrainedResize']);
  {$ENDIF}

  {$IFDEF COMPILER6_UP}
  {$IFDEF VCL}
  RegisterPropertiesInCategory('Editor', TJvCustomEditor,
    ['InsertMode', 'DoubleClickLine', 'Completion', 'SmartTab',
     'BackSpaceUnindents', 'AutoIndent', 'KeepTrailingBlanks', 'CursorBeyondEOF',
     'GutterColor', 'GutterWidth',
     'RightMarginVisible', 'RightMargin', 'RightMarginColor',

     'OnGetLineAttr', 'OnReservedWord', 'OnCompletionIdentifier',
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

