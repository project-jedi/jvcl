{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderConsts.pas, released on --.

The Initial Developers of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-06-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderConsts;

interface

resourcestring
  SExtensibleIntObjDuplicateClass = 'Implementation of that class already exists.';
  SExtensibleIntObjCollectionExpected = 'Expected collection.';
  SExtensibleIntObjClassNameExpected = 'Missing ClassName property';
  SExtensibleIntObjInvalidClass = 'Invalid class type.';

  SDataItemRenderHasNoText = '(item doesn''t support the IJvDataItemText interface)';

  SDataProviderNeedsItemsImpl = 'Can''t create a data provider without an IJvDataItems implementation.';

implementation

end.
