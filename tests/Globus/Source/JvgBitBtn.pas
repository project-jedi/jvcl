{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgBitBtn.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgBitBtn;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls, Buttons, Forms,
  JVCLVer,
  JvgTypes, JvgCommClasses, JvgUtils;

type
  TJvgBitBtn = class(TBitBtn)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FCanvas: TControlCanvas;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

constructor TJvgBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self; //...i can draw now! :)
  FHintColor := clInfoBk;
  FOver := False;
end;

destructor TJvgBitBtn.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvgBitBtn.CNDrawItem(var Msg: TWMDrawItem);
begin
  inherited;
  DrawItem(Msg.DrawItemStruct^);
end;

procedure TJvgBitBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown: Boolean;
  R: TRect;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;
  IsDown := DrawItemStruct.itemState and ODS_SELECTED <> 0;
  if (not FOver) and (not IsDown) then
    // (rom) using FCanvas now
    with FCanvas do
      if not Focused and not Default then
      begin
        Pen.Color := clBtnFace;
        MoveTo(R.Left + 1, R.Top + 1);
        LineTo(R.Right - 1, R.Top + 1);
        MoveTo(R.Left + 1, R.Top + 1);
        LineTo(R.Left + 1, R.Bottom - 1);

        Pen.Color := (Parent as TWinControl).Brush.Color;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
        MoveTo(R.Right - 1, R.Top);
        LineTo(R.Right - 1, R.Bottom);

        Pen.Color := clBtnShadow;
        MoveTo(R.Left - 2, R.Bottom - 2);
        LineTo(R.Right - 1, R.Bottom - 2);
        MoveTo(R.Right - 2, R.Top);
        LineTo(R.Right - 2, R.Bottom - 1);
      end
      else
      begin
        Brush.Color := clBtnFace;
        FrameRect(Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2));
      end;
  FCanvas.Handle := 0;
end;

procedure TJvgBitBtn.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvgBitBtn.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvgBitBtn.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

end.

