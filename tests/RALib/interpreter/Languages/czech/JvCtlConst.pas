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

Contributor(s): Jaromir Solar (jarda@foresta.cz)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for Czech

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvCtlConst;

interface

const

 {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Nejprve musíte zadat jméno vlastnosti';
  sRegAutoEditorTreeHint       = 'Dostupné vlastnosti';
  sRegAutoEditorListHint       = 'Uložené vlastnosti';
  sRegAutoEditorBtnAddPropHint = 'Pøidat/odstranit vlastnost';
  sRegAutoEditorSort           = 'Setøídit';

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
  SScrollBarRange = 'Hodnota šoupátka je mimo hranice';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = 'Storno';

 { Menu Designer }
  SMDMenuDesigner       = 'Návrháø &menu';
  SMDInsertItem         = '&Vložit';
  SMDDeleteItem         = '&Smazat';
  SMDCreateSubmenuItem  = 'Vytvoøit &podmenu';

  SCantGetShortCut      = 'Soubor u zástupce %s není dostupný';


 { RALib 1.23 }
  SPropertyNotExists    = 'Vlastnost "%s" neexistuje';
  SInvalidPropertyType  = 'Vlastnost "%s" je špatného typu';

 { RALib 1.55 }

 {JvHLEdPropDlg}
  SHLEdPropDlg_Caption = 'Editor vlastností';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Barvy';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Pøednastavené mapování kláves';
  SHLEdPropDlg_gbEditor = 'Vlastnosti editoru:';
  SHLEdPropDlg_cbAutoIndent = '&Auto odsazovací mód';
  SHLEdPropDlg_cbSmartTab = 'Ch&ytrý tabulátor';
  SHLEdPropDlg_cbBackspaceUnindents = 'Odsazení pøi &Backspace';
  SHLEdPropDlg_cbGroupUndo = '&Skupinové zpìt';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Kurzor i za &konec souboru';
  SHLEdPropDlg_cbUndoAfterSave = 'Umožnit z&pìt po uložení';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Držet koncové mezery';
  SHLEdPropDlg_cbDoubleClickLine = '&Dvojklik na øádku';
  SHLEdPropDlg_cbSytaxHighlighting = 'Použij z&výraznìní syntaxe';
  SHLEdPropDlg_lblTabStops = 'Stop &tabulátoru na:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Barva SpeedSettings pro';
  SHLEdPropDlg_lblElement = '&Element:';
  SHLEdPropDlg_lblColor = '&Barva:';
  SHLEdPropDlg_gbTextAttributes = 'Textové atributy:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Použij pøednastavení pro:';
  SHLEdPropDlg_cbBold = '&Tuènì';
  SHLEdPropDlg_cbItalic = '&Kurzíva';
  SHLEdPropDlg_cbUnderline = '&Podtrženo';
  SHLEdPropDlg_cbDefForeground = 'P&opøedí';
  SHLEdPropDlg_cbDefBackground = 'Poza&dí';
  SHLEdPropDlg_OptionCantBeChanged = 'Tato možnost nemùže být zmìnìna.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'Vlastnost JvHLEditor není pøiøazena';
  SHLEdPropDlg_RegAutoNotAssigned = 'Vlastnost RegAuto není pøiøazena';

implementation

end.
