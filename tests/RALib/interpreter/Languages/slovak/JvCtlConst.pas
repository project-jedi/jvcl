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

Contributor(s): Tibor Bednar (tiborbmt@hotmail.com)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for Slovak

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvCtlConst;

interface

const

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Tu zadajte n·zov vlastnosti';
  sRegAutoEditorTreeHint       = 'DostupnÈ vlastnosti';
  sRegAutoEditorListHint       = 'UloûenÈ vlastnosti';
  sRegAutoEditorBtnAddPropHint = 'Pridaù/odstr·niù vlastnosù';
  sRegAutoEditorSort           = 'Usporiadaù';

 {JvEditor}
  RAEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

{IParser}
 {$IFDEF Delphi}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF Delphi}
 {$IFDEF BCB}
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
  SScrollBarRange = 'Hodnota pos˙vaËa je mimo hranÌc';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = 'Cancel';

 { Menu Designer }
  SMDMenuDesigner       = 'N·vrh &menu';
  SMDInsertItem         = '&Vloûiù';
  SMDDeleteItem         = 'O&dstr·niù';
  SMDCreateSubmenuItem  = 'Vytvoriù &podmenu';

  SCantGetShortCut      = 'Cieæov˝ s˙bor odkazu %s je nedostupn˝';


 { RALib 1.23 } 
  SPropertyNotExists    = 'Vlastnosù "%s" neexistuje';
  SInvalidPropertyType  = 'Vlastnosù "%s" m· neplatn˝ typ';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor vlastnostÌ';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Farby';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'ätandardnÈ rozloûenie kl·ves';
  SHLEdPropDlg_gbEditor = 'Nastavenia editoru:';
  SHLEdPropDlg_cbAutoIndent = '&AutomatickÈ odsaÔovanie';
  SHLEdPropDlg_cbSmartTab = 'Inteli&gentn˝ tabul·tor';
  SHLEdPropDlg_cbBackspaceUnindents = 'Backspace r&uöÌ odsadenie';
  SHLEdPropDlg_cbGroupUndo = '&SkupinovÈ sp‰ù';
  SHLEdPropDlg_cbCursorBeyondEOF = '&Kurzor za koncom s˙boru';
  SHLEdPropDlg_cbUndoAfterSave = 'Umoûniù s&p‰ù po uloûenÌ';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Zachovaù koncovÈ &medzery';
  SHLEdPropDlg_cbDoubleClickLine = '&Dvojklik na riadku';
  SHLEdPropDlg_cbSytaxHighlighting = 'Zv˝razÚovanie &syntaxe';
  SHLEdPropDlg_lblTabStops = 'Zar·ûka &tabul·toru:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Farby SpeedSettings pre';
  SHLEdPropDlg_lblElement = '&Prvok:';
  SHLEdPropDlg_lblColor = '&Farba:';
  SHLEdPropDlg_gbTextAttributes = 'TextovÈ atrib˙ty:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Pouûiù ötandard pre:';
  SHLEdPropDlg_cbBold = '&TuËnÈ';
  SHLEdPropDlg_cbItalic = '&KurzÌva';
  SHLEdPropDlg_cbUnderline = '&PodËiarknutÈ';
  SHLEdPropDlg_cbDefForeground = 'P&opredie';
  SHLEdPropDlg_cbDefBackground = 'Poza&die';
  SHLEdPropDlg_OptionCantBeChanged = 'T·to voæba nemÙûe byù zmenen·.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Vlastnosù JvHLEditor nie je priraden·';
  SHLEdPropDlg_RegAutoNotAssigned = 'Vlastnosù RegAuto nie je priraden·';

implementation

end.
