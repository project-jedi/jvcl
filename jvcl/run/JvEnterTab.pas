{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEnterTab.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A unit that converts all Enter keypresses to Tab keypresses. }

unit JvEnterTab;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  JvComponent;

type
  TJvEnterAsTab = class(TJvGraphicControl)
  private
    FEnterAsTab: Boolean;
    FAllowDefault: Boolean;
    FBmp: TBitmap;
  protected
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property EnterAsTab: Boolean read FEnterAsTab write FEnterAsTab default true;
    property AllowDefault: Boolean read FAllowDefault write FAllowDefault default true;
  end;

implementation

uses
  Forms, StdCtrls;
  
{$R JvEnterAsTab.res}

constructor TJvEnterAsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoStdEvents, csFixedHeight, csFixedWidth];
  FEnterAsTab := True;
  FAllowDefault := True;
  if csDesigning in ComponentState then
  begin
    FBmp := TBitmap.Create;
    FBmp.LoadFromResourceName(hInstance, 'DESIGNENTERASTAB');
  end
  else
    Visible := False;
end;

destructor TJvEnterAsTab.Destroy;
begin
  FBmp.Free;
  inherited Destroy;
end;

procedure TJvEnterAsTab.CMDialogKey(var Msg: TCMDialogKey);
begin
  if (GetParentForm(Self).ActiveControl is TButtonControl) and (FAllowDefault) then
    inherited
  else
  if (Msg.CharCode = VK_RETURN) and FEnterAsTab then
  begin
    GetParentForm(Self).Perform(CM_DIALOGKEY, VK_TAB, 0);
    Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TJvEnterAsTab.Paint;
begin
  if not (csDesigning in ComponentState) then
    Exit;
  Canvas.Brush.Color := clBtnFace;
  inherited Canvas.BrushCopy(ClientRect, FBmp, ClientRect, clFuchsia);
end;

procedure TJvEnterAsTab.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, 28, 28);
end;

end.

