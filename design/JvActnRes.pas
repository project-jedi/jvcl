{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvActnRes.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are:
All Rights Reserved.

Contributor(s):

Last Modified:

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description :

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvActnRes;

interface

uses
  SysUtils, Classes, ActnList, ImgList,
  {$IFNDEF COMPILER6_UP}
  Forms,
  {$ENDIF}
  Controls;

type
  TJvStandardActions = class(TDataModule)
    ImageList1: TImageList;
    ActionList1: TActionList;
  public
  end;

// (rom) unused
// var
//   JvStandardActions: TJvStandardActions;

implementation

{$R *.dfm}

end.
