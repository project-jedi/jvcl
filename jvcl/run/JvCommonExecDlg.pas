{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCommonExecDlg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCommonExecDlg;

interface

uses
  {$IFDEF VCL}
  Windows,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QWindows,
  {$ENDIF VisualCLX}
  Classes,
  JvBaseDlg;

type
  TJvCommonExecDialog = class(TJvCommonDialogP)
  private
    FOwnerWindow: HWND;
  public
    constructor Create(AOwner: TComponent); override;
    property OwnerWindow: HWND read FOwnerWindow;
  end;

implementation

uses
  Controls, JvTypes;

constructor TJvCommonExecDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TWinControl then
    FOwnerWindow := (AOwner as TWinControl).Handle
  else
    FOwnerWindow := HWND_DESKTOP;
end;

end.

