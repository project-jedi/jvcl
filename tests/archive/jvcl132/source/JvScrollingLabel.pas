{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollingLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvScrollingLabel;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, JvTypes, JVCLVer;

type
  TJvScrollingLabel = class(TCustomLabel)
  private
    FTimer: TTimer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FInterval: Cardinal;
    FText: string;
    FScrolling: Boolean;
    FScrollText: string;
    FNoGrap: Boolean;
    FDirection: TLabelDirection;
    FAboutJVCL: TJVCLAboutInfo;
    procedure FSetInterval(Value: Cardinal);
    procedure SetScrolling(Value: Boolean);
    procedure FSetScrollText(Value: string);
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure Scroll(Sender: TObject);
    procedure SetGrap(Value: Boolean);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Alignment;
    property AutoSize;
    property FocusControl;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentColor;
    property Color;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property Layout;
    property ShowAccelChar;
    property PopupMenu;
    property Transparent;
    property WordWrap;
    property Align;
    property Left;
    property Visible;
    property Top;
    property Height;
    property Width;
    property Cursor;
    property Enabled;
    property Hint;
    property ScrollInterval: Cardinal read FInterval write FSetInterval default 50;
    property Scrolling: Boolean read FScrolling write SetScrolling default True;
    property NoGrap: Boolean read FNoGrap write SetGrap default False;
    property Text: string read FScrollText write FSetScrollText;
    property ScrollDirection: TLabelDirection read FDirection write FDirection default sdLeftToRight;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnStartDrag;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

implementation

resourcestring
  // (rom) changed
  RC_UrlSite = 'http://delphi-jedi.org';

  {**************************************************}

constructor TJvScrollingLabel.Create(AOwner: TComponent);
begin
  FInterval := 50;
  FScrolling := True;
  FNoGrap := False;
  FScrollText := RC_UrlSite;
  FDirection := sdLeftToRight;
  inherited;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := Scroll;
  FTimer.Interval := FInterval;
  FTimer.Enabled := FScrolling;
  FText := RC_UrlSite;
  Caption := FScrollText;
  AutoSize := False;
end;

{**************************************************}

destructor TJvScrollingLabel.Destroy;
begin
  FTimer.Free;
  inherited;
end;

{**************************************************}

procedure TJvScrollingLabel.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvScrollingLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvScrollingLabel.MouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvScrollingLabel.MouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvScrollingLabel.FSetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
  FInterval := Value;
end;

{**************************************************}

procedure TJvScrollingLabel.Resize;
begin
  inherited;
  SetGrap(FNoGrap);
end;

{**************************************************}

procedure TJvScrollingLabel.SetScrolling(Value: Boolean);
begin
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := Value;
  FScrolling := Value;
end;

{**************************************************}

procedure TJvScrollingLabel.Scroll(Sender: TObject);
begin
  if csDesigning in ComponentState then
    Exit;
  if Length(FText) <> 0 then
  begin
    if FDirection = sdLeftToRight then
      FText := FText[Length(FText)] + Copy(FText, 1, Length(FText) - 1)
    else
      FText := Copy(FText, 2, Length(FText) - 1) + FText[1];
    Caption := FText;
  end;
end;

{**************************************************}

procedure TJvScrollingLabel.FSetScrollText(Value: string);
begin
  if Pos(#10, Value) <> 0 then
    Value := #10 + Value;
  Caption := Value;
  FScrollText := Value;
  FText := Value;
  SetGrap(FNoGrap);
end;

{**************************************************}

procedure TJvScrollingLabel.SetGrap(Value: Boolean);
var
  i, j: Integer;
begin
  FNoGrap := Value;
  if FNoGrap then
    FText := FScrolltext + ' '
  else
  begin
    FText := FScrollText;
    with TCanvas.Create do
    begin
      Handle := GetDC(HWND_DESKTOP);
      Font.Assign(Self.Font);
      j := 0;
      i := 0;
      while (j < Self.Width) and (i < 10000) do
      begin
        FText := FText + ' ';
        j := TextWidth(FText);
        Inc(i);
      end;
      ReleaseDC(HWND_DESKTOP, Handle);
    end;
  end;
end;

end.
