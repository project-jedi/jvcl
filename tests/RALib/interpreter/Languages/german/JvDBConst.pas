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

Contributor(s): Suat IMAM-OGLU (simam@t-online.de)

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for German

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDBConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = '%s löschen?';                                            //'Delete %s ?';
  SDeleteNode2            = '%s löschen (mit allen untergeordneten Elementen)?';      //'Delete %s (with all children) ?';
  SMasterFieldEmpty       = '"MasterField" Eigenschaft muss eingegeben werden';        //'"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" Eigenschaft muss eingegeben werden';        //'"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" Eigenschaft muss eingegeben werden';          //'"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" und "DetailField" müssen vom selben Typ sein'; //'"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" muss vom Typ Integer sein';                 //'"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" muss vom Typ Integer sein';                 //'"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" muss vom Typ String, Date oder Integer sein'; //'"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField"  muss vom Typ Integer sein';                  //'"IconField" must be integer type';
  SMoveToModeError        = 'ungültiger Verschiebe-Modus für RADBTreeNode';           //'Invalid move mode for RADBTreeNode';
  SDataSetNotActive       = 'Datensatz nicht aktiv';                                  //'DataSet not active';

implementation

end.
