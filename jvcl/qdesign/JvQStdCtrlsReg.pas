{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

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

Last Modified: 2003-11-09

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
  Classes, QControls,
  QImgList, QTypes,

  DesignEditors, DesignIntf,

  JvQDsgnConsts, JvQTypes,
  JvQEdit, JvQProgressBar, JvQTransparentPanel,
  JvQMaskEdit, JvQBaseEdits, JvQCalc, JvQToolEdit,
  JvQBevel, JvQCheckBox, JvQSpeedButton,
  JvQGroupBox, JvQHeaderControl,
  JvQImage, {JvQLabel,} JvQRadioButton, JvQRadioGroup,
  JvQScrollBar, JvQShape, JvQControlBar,
  JvQTabControl, JvQBitBtn, JvQPickDate, JvQStringGrid,
  JvQPanel, JvQImageList, JvQProgressEditor, JvQDsgnEditors;

{$IFDEF MSWINDOWS}
{$R ..\resources\JvStdCtrlsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvStdCtrlsReg.dcr}
{$ENDIF LINUX}

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
    TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit]);
  RegisterComponents(RsPaletteImageAnimator, [TJvImage, TJvImageList]);
  RegisterComponents(RsPaletteBarPanel, [TJvTabControl, TJvControlBar,
    TJvGroupBox, TJvHeaderControl, TJvPanel, TJvBevel,
    TJvTransparentPanel]);
//  RegisterComponents(RsPaletteLabel, [TJvLabel]);
  RegisterComponents(RsPaletteScrollerTracker, [TJvScrollBar]);
  RegisterComponents(RsPaletteListComboTree, [TJvStringGrid]);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, cText, nil);
//  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, cText, TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', TJvHintProperty);
//  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedButton, 'Caption', TJvHintProperty);
//  RegisterPropertyEditor(TypeInfo(TWStringList), '', 'Hints', TJvHintProperty);
//  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvCustomLabel, 'ImageIndex',TJvDefaultImageIndexProperty);
end;

end.
