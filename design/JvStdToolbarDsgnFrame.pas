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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvStdToolbarDsgnFrame;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ComCtrls, ActnList,
  Menus, ImgList, ToolWin, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls, QActnList, QMenus,
  QImgList, QToolWin, QExtCtrls,
  {$ENDIF VisualCLX}
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
  public
  end;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

end.
