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

Contributor(s): Jaromir Solar (jarda@foresta.cz).

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Language specific constant for Czech

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDBConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = 'Smazat %s ?';
  SDeleteNode2            = 'Smazat %s (se všemi podøízenými uzly) ?';
  SMasterFieldEmpty       = 'Vlastnost "MasterField" musí být vyplnìna';
  SDetailFieldEmpty       = 'Vlastnost "DetailField" musí být vyplnìna';
  SItemFieldEmpty         = 'Vlastnost "ItemField" musí být vyplnìna';
  SMasterDetailFieldError = 'Vlastnosti "MasterField" a "DetailField" musí být stejného typu';
  SMasterFieldError       = 'Vlastnost "MasterField" musí být celé èíslo';
  SDetailFieldError       = 'Vlastnost "DetailField" musí být celé èíslo';
  SItemFieldError         = 'Vlastnost "ItemField" musí být øetìzec, datum nebo celé èíslo';
  SIconFieldError         = 'Vlastnost "IconField" musí být celé èíslo';
  SMoveToModeError        = 'Chybný pøesun uzlu v RADBTreeNode';
  SDataSetNotActive       = 'DataSet není aktivní';

implementation

end.
