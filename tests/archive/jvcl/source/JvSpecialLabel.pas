{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpecialLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSpecialLabel;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, JVCLVer;

type
  TJvSpecialLabel = class(TCustomLabel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FText: string;
    FSleep: Integer;
    FAboutJVCL: TJVCLAboutInfo;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetText(Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
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
    property Text: string read FText write SetText;
    property SleepTime: Integer read FSleep write FSleep default 20;
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

{**************************************************}

constructor TJvSpecialLabel.Create(AOwner: TComponent);
begin
  inherited;
  FSleep := 20;
  FText := Caption;
  SetText(FText);
end;

{**************************************************}

procedure TJvSpecialLabel.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvSpecialLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvSpecialLabel.MouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvSpecialLabel.MouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvSpecialLabel.SetText(Value: string);
var
  i, j: Integer;
  cap: string;
begin
  Ftext := Value;
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    Caption := '';
    cap := '';
    for i := 1 to Length(Value) do
    begin
      for j := 10 to Ord(Value[i]) - 1 do
      begin
        Caption := cap + Char(j);
        Application.ProcessMessages;
        Sleep(FSleep);
      end;
      cap := cap + Value[i];
    end;
  end;
  Caption := Value;
end;

end.
