{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRealLabel.PAS, released on 2001-02-28.

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

unit JvRealLabel;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, JVCLVer;

type
  TJvRealLabel = class(TCustomLabel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FText: string;
    FSleep: Integer;
    FError: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetText(Value: string);
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
    property SleepTime: Integer read FSleep write FSleep default 100;
    property MakeErrors: Boolean read FError write FError default True;
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

constructor TJvRealLabel.Create(AOwner: TComponent);
begin
  inherited;
  FSleep := 100;
  FError := True;
  FText := Caption;
  SetText(Caption);
end;

{**************************************************}

procedure TJvRealLabel.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvRealLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;
{**************************************************}

procedure TJvRealLabel.MouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvRealLabel.MouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvRealLabel.SetText(Value: string);
var
  i, j: Integer;
  cap: string;
begin
  FText := Value;
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    Randomize;
    cap := '';
    for i := 1 to Length(Value) do
    begin
      Caption := Copy(Value, 1, i - 1);
      cap := Caption;
      j := Random(10);
      if (j = 7) and (FError) then
        cap := cap + Char(Ord(Value[i]) - Random(10))
      else
        cap := cap + Value[i];
      Caption := cap;
      Application.ProcessMessages;
      if (FError) and (j <> 7) then
        Sleep(FSleep)
      else
        Sleep(2 * FSleep);
    end;
  end;
  Caption := Value;
end;

end.
