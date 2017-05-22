{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBConst.PAS, released on 2002-07-04.

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

unit JvDBConst;

interface

const
  {TJvDBTreeView}
  SDeleteNode             = 'Delete %s ?';
  SDeleteNode2            = 'Delete %s (with all children) ?';
  SMasterFieldEmpty       = '"MasterField" property must be filled';
  SDetailFieldEmpty       = '"DetailField" property must be filled';
  SItemFieldEmpty         = '"ItemField" property must be filled';
  SMasterDetailFieldError = '"MasterField" and "DetailField" must be of same type';
  SMasterFieldError       = '"MasterField" must be integer type';
  SDetailFieldError       = '"DetailField" must be integer type';
  SItemFieldError         = '"ItemField" must be string, date or integer type';
  SIconFieldError         = '"IconField" must be integer type';
  SMoveToModeError        = 'Invalid move mode for RADBTreeNode';
  SDataSetNotActive       = 'DataSet not active';

implementation

end.
