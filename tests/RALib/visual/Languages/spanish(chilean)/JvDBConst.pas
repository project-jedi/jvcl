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

description : Language specific constant for Spanish(chilean)

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDBConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = 'Borrar %s ?';
  SDeleteNode2            = 'Borrar %s (con todos los hijos) ?';
  SMasterFieldEmpty       = 'Propiedad "MasterField" debe ser llenada';
  SDetailFieldEmpty       = 'Propiedad "DetailField" debe ser llenada';
  SItemFieldEmpty         = 'Propiedad "ItemField" debe ser llenada';
  SMasterDetailFieldError = '"MasterField" y "DetailField" deben ser del mismo tipo';
  SMasterFieldError       = '"MasterField" debe ser de tipo integer';
  SDetailFieldError       = '"DetailField" debe ser de tipo integer';
  SItemFieldError         = '"ItemField" debe ser tipo string, date o integer';
  SIconFieldError         = '"IconField" debe ser de tipo integer';
  SMoveToModeError        = 'Modo de mover inválido para RADBTreeNode';
  SDataSetNotActive       = 'DataSet no activa';

implementation

end.
