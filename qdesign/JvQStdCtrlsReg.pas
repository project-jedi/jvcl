{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStdCtrlsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-03-22

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQStdCtrlsReg;

interface

procedure Register;

implementation

uses
  Classes,
  QControls, QImgList, QTypes,

  DesignEditors, DesignIntf,

  JvQDsgnConsts, JvQTypes, JvQWStrUtils,
  JvQEdit, JvQProgressBar, JvQTransparentPanel,
  
  JvQMaskEdit, JvQBaseEdits, JvQCalc, JvQToolEdit,
  JvQBevel, JvQCheckBox, JvQSpeedButton,
  JvQGroupBox, JvQHeaderControl,
  JvQImage, JvQRadioButton, JvQRadioGroup,
  JvQLabel, JvQStatusBar,
  JvQGauges, JvQTimeEdit, JvQStdDsgnEditors, JvQJCLUtils,
  JvQScrollBar, JvQShape, JvQControlBar, JvQGrids,
  JvQTabControl, JvQBitBtn, JvQPickDate, JvQStringGrid,
  JvQPanel, JvQImageList, JvQProgressEditor, JvQDsgnEditors;

{$IFDEF VCL}
{$R ..\Resources\JvStdCtrlsReg.dcr}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R ../Resources/JvStdCtrlsReg.dcr}
{$ENDIF VisualCLX}

procedure Register;
const
  BaseClass: TClass = TComponent;
  cText = 'Text';
  cOwnerDraw = 'OwnerDraw';
begin
  RegisterComponents(RsPaletteVisual, [TJvShape, TJvCalendar]);
  RegisterComponents(RsPaletteNonVisual, [TJvCalculator]);
  RegisterComponents(RsPaletteButton, [TJvBitBtn, TJvSpeedButton,
    TJvCheckBox, TJvRadioButton, TJvRadioGroup]);
  RegisterComponents(RsPaletteEdit, [TJvEdit,
    TJvMaskEdit, TJvCalcEdit, TJvComboEdit,
    TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit, TJvTimeSpin]);
  RegisterComponents(RsPaletteImageAnimator, [TJvImage, TJvImageList]);
  RegisterComponents(RsPaletteBarPanel, [TJvGauge, TJvTabControl, TJvControlBar,
    TJvGroupBox, TJvHeaderControl, TJvPanel, TJvBevel, TJvStatusBar,
    TJvTransparentPanel]);
  RegisterComponents(RsPaletteLabel, [TJvLabel]);
//  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvCustomLabel, 'ImageIndex',TJvDefaultImageIndexProperty);
  RegisterComponents(RsPaletteScrollerTracker, [TJvScrollBar]);
  RegisterComponents(RsPaletteListComboTree, [TJvDrawGrid, TJvStringGrid]);

  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, cText, nil);
//  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
//  RegisterPropertyEditor(TypeInfo(TWStringList), TJvArrayButton ,Hints', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, cText, TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', TJvHintProperty);
//  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvCheckItemsProperty);
//  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
//  RegisterPropertyEditor(TypeInfo(Boolean), TJvMainMenu, cOwnerDraw, nil);
//  RegisterPropertyEditor(TypeInfo(Boolean), TJvPopupMenu, cOwnerDraw, nil);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedButton, 'Caption', TJvHintProperty);
//  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvCustomLabel, 'ImageIndex',TJvDefaultImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TDate), nil, '', TJvDateExProperty);
  RegisterPropertyEditor(TypeInfo(TTime), nil, '', TJvTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TJvDateTimeExProperty);


end;

end.
