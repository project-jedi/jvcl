{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOneSizeFitsAllButton.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-06-27

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvOneSizeFitsAllButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls,
  JVCLVer;

const
  CM_FORCESIZE = WM_USER + 777;

type
  TCmForceSize = record
    Msg: Cardinal;
    Sender: TWinControl;
    NewSize: TSmallPoint;
    Result: Longint;
  end;

  TJvOneSizeFitsAllButton = class(TButton)
  private
    FAboutJVCL: TJVCLAboutInfo;
    procedure CMForceSize(var Msg: TCMForceSize); message CM_FORCESIZE;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

procedure TJvOneSizeFitsAllButton.CMForceSize(var Msg: TCMForceSize);
begin
  // (p3) this is never called AFAICS
  with Msg do
    if Sender <> Self then
      with NewSize do
        inherited SetBounds(Left, Top, X, Y);
end;

procedure TJvOneSizeFitsAllButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Form: TCustomForm;
  Msg: TCMForceSize;
//  r: TRect;
begin
  inherited;
  Form := GetParentForm(Self);
  if Assigned(Form) then
  begin
    // (p3) what is this rect doing here?
    // r := Rect(aLeft, aTop, aWidth, aHeight);
    Msg.Msg := CM_FORCESIZE;
    Msg.Sender := Self;
    Msg.NewSize.x := AWidth;
    Msg.NewSize.y := AHeight;
    Form.Broadcast(Msg);
  end;
end;

end.

