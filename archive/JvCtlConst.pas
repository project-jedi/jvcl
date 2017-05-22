{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCtlConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Language specific constant for English

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCtlConst;

interface

const
  {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'You can type property name here';
  sRegAutoEditorTreeHint       = 'Available properties';
  sRegAutoEditorListHint       = 'Stored properties';
  sRegAutoEditorBtnAddPropHint = 'Add/Remove property';
  sRegAutoEditorSort           = 'Sort';

  {JvEditor}
  JvEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

  {IParser}
  {$IFDEF DELPHI}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
  {$ENDIF DELPHI}
  {$IFDEF LINUX}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
  {$ENDIF BCB}

  {$IFDEF RAINTER}
  {RAInter}
  RAIIdSymbols      = ['0'..'9', 'A'..'Z', 'a'..'z',  '_'];
  RAIIdFirstSymbols = ['A'..'Z', 'a'..'z', '_'];
  {$ENDIF RAINTER}

  {$IFDEF COMPILER2}
  SScrollBarRange = 'Scrollbar value out of bounds';
  {$ENDIF}

  {JvDlg}
  SOk = 'OK';
  SCancel = 'Cancel';

  { Menu Designer }
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &SubMenu';

  SCantGetShortCut      = 'Target FileName for ShortCut %s not available';

  { RALib 1.23 }
  SPropertyNotExists    = 'Property "%s" does not exists';
  SInvalidPropertyType  = 'Property "%s" has invalid type';

  { RALib 1.55 }

  {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor Properties';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Colors';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayoutDefault = 'Default keymapping';
  SHLEdPropDlg_gbEditor = 'Editor options:';
  SHLEdPropDlg_cbAutoIndent = '&Auto indent mode';
  SHLEdPropDlg_cbSmartTab = 'S&mart tab';
  SHLEdPropDlg_cbBackspaceUnindents = 'Backspace &unindents';
  SHLEdPropDlg_cbGroupUndo = '&Group undo';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Cursor beyond &EOF';
  SHLEdPropDlg_cbUndoAfterSave = '&Undo after sa&ve';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Keep trailing blanks';
  SHLEdPropDlg_cbDoubleClickLine = '&Double click line';
  SHLEdPropDlg_cbSytaxHighlighting = 'Use &syntax highlight';
  SHLEdPropDlg_lblTabStops = '&Tab stops:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Color SpeedSettings for';
  SHLEdPropDlg_lblElement = '&Element:';
  SHLEdPropDlg_lblColor = '&Color:';
  SHLEdPropDlg_gbTextAttributes = 'Text attributes:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Use defaults for:';
  SHLEdPropDlg_cbBold = '&Bold';
  SHLEdPropDlg_cbItalic = '&Italic';
  SHLEdPropDlg_cbUnderline = '&Underline';
  SHLEdPropDlg_cbDefForeground = '&Foreground';
  SHLEdPropDlg_cbDefBackground = '&Background';
  SHLEdPropDlg_OptionCantBeChanged = 'This option can''t be changed. Sorry.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'JvHLEditor property is not assigned';
  SHLEdPropDlg_RegAutoNotAssigned = 'RegAuto property is not assigned';

implementation

end.
