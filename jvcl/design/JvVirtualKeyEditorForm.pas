{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppletEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvVirtualKeyEditorForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  JvVirtualKeySelectionFrame, JvComponent;

type
  TfrmJvVirtualKeyEditor = class(TJvForm)
    bbtOk: TBitBtn;
    bbtCancel: TBitBtn;
  private
    FEditingFrame: TJvVirtualKeySelectionFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property EditingFrame: TJvVirtualKeySelectionFrame read FEditingFrame;
  end;

implementation

{$R *.dfm}

constructor TfrmJvVirtualKeyEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditingFrame := TJvVirtualKeySelectionFrame.Create(Self);
  with FEditingFrame do
  begin
    Left := 4;
    Top := 8;
    Parent := Self;
    Visible := True;
  end;
end;

destructor TfrmJvVirtualKeyEditor.Destroy;
begin
  FEditingFrame.Free;
  inherited Destroy;
end;

end.
