{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProviderToolbarFrame.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQProviderToolbarFrame;

interface

uses
  SysUtils, Classes,  
  QGraphics, QControls, QForms, QDialogs, Types, QStdCtrls, QActnList,
  QMenus, QImgList, QComCtrls, QToolWin, QExtCtrls, 
  JvQStdToolbarDsgnFrame;

type
  TfmeJvProviderToolbar = class(TfmeJvStdToolbarDesign)
    tbDivider2: TToolButton;
    pnlContexts: TPanel;
    cbContexts: TComboBox;
    procedure tbrToolbarResize(Sender: TObject);
    procedure pnlContextsResize(Sender: TObject);
  protected
    procedure ResizeContextsComboBox; dynamic;
  public
  end;

implementation



{$R *.xfm}


procedure TfmeJvProviderToolbar.ResizeContextsComboBox;
begin
  pnlContexts.Width := tbrToolbar.ClientWidth - pnlContexts.Left;
  UpdateToolbarSeparators;
end;

procedure TfmeJvProviderToolbar.tbrToolbarResize(Sender: TObject);
begin
  ResizeContextsComboBox;
end;

procedure TfmeJvProviderToolbar.pnlContextsResize(Sender: TObject);
begin
  cbContexts.Top := (pnlContexts.ClientHeight - cbContexts.Height) div 2;
end;

end.
