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

description : Language specific constants for Russian

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvCtlConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = 'Удалить %s ?';
  SDeleteNode2            = 'Удалить %s (вместе со всем содержимым) ?';
  SMasterFieldEmpty       = '"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField" must be integer type';
  SMoveToModeError        = 'Неверный режим перемещения RADBTreeNode';
  SDataSetNotActive       = 'DataSet not active';

   {RegAutoEditor}
  sRegAutoEditorEdtPropHint    = 'Имя свойства можно ввести прямо здесь';
  sRegAutoEditorTreeHint       = 'Доступные свойства';
  sRegAutoEditorListHint       = 'Список сохраняемых свойств';
  sRegAutoEditorBtnAddPropHint = 'Добавить/Удалить свойство';
  sRegAutoEditorSort           = 'Упорядочить';

 {JvEditor}
  RAEditorCompletionChars = #8+'_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁйцукенгшщзхъфывапролджэячсмитьбюё';

{IParser}
 {$IFDEF Delphi}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z', 'А'..'Я', 'а'..'я'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z', 'А'..'Я', 'а'..'я'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF Delphi}
 {$IFDEF BCB}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁйцукенгшщзхъфывапролджэячсмитьбюё';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮЁйцукенгшщзхъфывапролджэячсмитьбюё';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF BCB}

{$IFDEF COMPILER2}
  SScrollBarRange = 'значение Scrollbar вышло за допустимые пределы';
{$ENDIF}

 {JvDlg}
  SOk = 'OK';
  SCancel = 'Отмена';

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
  SHLEdPropDlg_Caption = 'Свойства: Редактор';
  SHLEdPropDlg_tsEditor = 'Редактор';
  SHLEdPropDlg_tsColors = 'Цвета';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Набор горячих клавиш';
  SHLEdPropDlg_cbKeyboardLayotDefault = 'Стандартный набор';
  SHLEdPropDlg_gbEditor = 'Параметры редактора:';
  SHLEdPropDlg_cbAutoIndent = '&Автоотступ';
  SHLEdPropDlg_cbSmartTab = '&Умный таб';
  SHLEdPropDlg_cbBackspaceUnindents = 'Забой &сдвигает назад';
  SHLEdPropDlg_cbGroupUndo = '&Групповая отмена';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Курсор за конец &файла';
  SHLEdPropDlg_cbUndoAfterSave = 'Отмена &после сохранения';
  SHLEdPropDlg_cbKeepTrailingBlanks = 'Сохранять &завершающие пробелы';
  SHLEdPropDlg_cbDoubleClickLine = '&Двойной клик выделяет строку';
  SHLEdPropDlg_cbSytaxHighlighting = 'Выделять &синтаксис';
  SHLEdPropDlg_lblTabStops = '&Табулостопы:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Цвета для';
  SHLEdPropDlg_lblElement = '&Элемент:';
  SHLEdPropDlg_lblColor = '&Цвет:';
  SHLEdPropDlg_gbTextAttributes = 'Атрибуты текста:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Стандартные:';
  SHLEdPropDlg_cbBold = '&Жирный';
  SHLEdPropDlg_cbItalic = '&Наклонный';
  SHLEdPropDlg_cbUnderline = '&Подчеркнутый';
  SHLEdPropDlg_cbDefForeground = '&Буквы';
  SHLEdPropDlg_cbDefBackground = '&Фон';
  SHLEdPropDlg_OptionCantBeChanged = 'Этот параметр менять нельзя. Извините.';
  SHLEdPropDlg_RAHLEditorNotAssigned = 'Свойство JvHLEditor не назначено';
  SHLEdPropDlg_RegAutoNotAssigned = 'Свойство RegAuto не назначено';

implementation

end.

