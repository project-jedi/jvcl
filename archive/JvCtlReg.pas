{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCtlReg;

interface

{ Register custom useful controls }

procedure Register;

implementation

uses
  Classes, SysUtils,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  TypInfo, Controls, Graphics, ExtCtrls, Tabs, Dialogs, Forms,
  DsnConst, ExtDlgs,
  JvRichEd,
  Menus, FiltEdit, StdCtrls,
  JvxDConst, JvxCtrls, JvGrids, JvCurrEdit, JvToolEdit, JvDateUtil,
  JvPickDate, JvSplit, JvxSlider, JvxClock, JvxAnimate, JvSpin,
  JvDsgnEditors, JvDice, JvSwitch, JvCheckItm, JvVCLUtils, JvColors, JvAniFile, JvGraph,
  {$IFDEF USE_JV_GIF}
  JvGIF, JvGIFCtrl,
  {$ENDIF}
  JvHints, JvExcptDlg,
  JvFileUtil, JvAnimatedEditor, JvProgressEditor;

procedure Register;
const
  cText = 'Text';
  cCaption = 'Caption';
  cHint = 'Hint';
const
  BaseClass: TClass = TComponent;
begin
  RegisterComponents(srJvCompositesPalette,
    [TJvComboEdit, TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit, TJvCalcEdit]);

//  RegisterComponents(srJvConvertPalette, [TJvxCurrencyEdit]);

  RegisterComponents(srJvControlsPalette, [TJvxCheckListBox, TJvxSplitter, TJvxSlider,
    TJvRichEdit,
    TJvxClock, TJvAnimatedImage, TJvDrawGrid, TJvSpeedButton,
    {$IFDEF USE_JV_GIF}
    TJvGIFAnimator,
    {$ENDIF}
    TJvSpinButton,
    TJvSwitch, TJvDice]);
  {$IFDEF BCB}
    RegisterComponents(ResStr(srSamplesPalette), [TScroller]);
  {$ELSE}
  RegisterComponents(ResStr(srSamplesPalette), [TScroller]);
  {$ENDIF BCB}

  RegisterNonActiveX([TJvCustomComboEdit, TJvCustomDateEdit, TJvCustomNumEdit,
    TJvFileDirEdit, TJvxCustomListBox, TJvRichEdit], axrComponentOnly);
  RegisterNonActiveX([TScroller], axrComponentOnly);

  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TJvWeekDayProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, cText, nil);
  RegisterPropertyEditor(TypeInfo(string), TJvFileDirEdit, cText, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomDateEdit, cText, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, cText, TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
  RegisterComponentEditor(TJvAnimatedImage, TJvAnimatedEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);

//  RegisterPropertyEditor(TypeInfo(TCaption), TJvxLabel, cCaption, THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedButton, cCaption, TJvHintProperty);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterPropertyEditor(TypeInfo(string), TMenuItem, cHint, TJvStringProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, cHint, TJvHintProperty);

  RegisterPropertyEditor(TypeInfo(TCaption), TLabel, cCaption, TJvHintProperty);

  RegisterPropertyEditor(TypeInfo(Integer), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(ShortInt), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(SmallInt), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Longint), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Word), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Byte), BaseClass, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Cardinal), BaseClass, '', TJvIntegerProperty);

  RegisterPropertyEditor(TypeInfo(Single), BaseClass, '', TJvFloatProperty);
  RegisterPropertyEditor(TypeInfo(Double), BaseClass, '', TJvFloatProperty);
  RegisterPropertyEditor(TypeInfo(Extended), BaseClass, '', TJvFloatProperty);
  RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TJvFloatProperty);
    {$IFNDEF DelphiPersonalEdition}
      RegisterComponentEditor(TPaintBox, TJvPaintBoxEditor);
      RegisterComponentEditor(TCustomImageList, TJvImageListEditor);
      RegisterComponentEditor(TImageList, TJvImageListEditor);
    {$ENDIF}
    RegisterJvColors;
  {$ENDIF}

end;

end.

