{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStdToolbarDsgnFrame.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

Last Modified: 2003-07-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvStdToolbarDsgnFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ActnList, Menus, ImgList, ToolWin, ExtCtrls,
  JvBaseDsgnToolbarFrame;

type
  TfmeJvStdToolbarDesign = class(TfmeJvBaseToolbarDesign)
    aiAddItem: TAction;
    aiDeleteItem: TAction;
    aiDeleteSubItems: TAction;
    aiMoveUp: TAction;
    aiMoveDown: TAction;
    tbAddItem: TToolButton;
    tbDeleteItem: TToolButton;
    tbDeleteSubItems: TToolButton;
    tbDivider1: TToolButton;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
  private
  public
  end;

implementation

{$R *.dfm}

end.
