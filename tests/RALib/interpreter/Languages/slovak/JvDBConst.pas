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

unit JvDBConst;

interface

const

 {TJvDBTreeView}
  SDeleteNode             = 'Odstr·niù %s?';
  SDeleteNode2            = 'Odstr·niù %s (so vöetk˝mi podriaden˝mi uzlami)?';
  SMasterFieldEmpty       = 'Vlastnosù "MasterField" musÌ byù vyplnen·';
  SDetailFieldEmpty       = 'Vlastnosù "DetailField" musÌ byù vyplnen·';
  SItemFieldEmpty         = 'Vlastnosù "ItemField" musÌ byù vyplnen·';
  SMasterDetailFieldError = 'Vlastnosti "MasterField" a "DetailField" musia byù rovnakÈho typu';
  SMasterFieldError       = 'Vlastnosù "MasterField" musÌ byù celÈ ËÌslo';
  SDetailFieldError       = 'Vlastnosù "DetailField" musÌ byù celÈ ËÌslo';
  SItemFieldError         = 'Vlastnosù "ItemField" musÌ byù reùazec, d·tum alebo celÈ ËÌslo';
  SIconFieldError         = 'Vlastnosù "IconField" musÌ byù celÈ ËÌslo';
  SMoveToModeError        = 'Neplatn˝ spÙsob presunu pre RADBTreeNode';
  SDataSetNotActive       = 'DataSet nie je aktÌvny';

implementation

end.
