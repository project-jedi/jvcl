{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressBar.PAS, released on 2001-02-28.

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

unit JvProgressBar;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ComCtrls,
  {$ELSE}
  QGraphics, QControls, QForms, QComCtrls,
  {$ENDIF VCL}
  JVCLVer;

type
  TJvProgressBar = class(TProgressBar)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FBarColor: TColor;
    procedure SetBarColor(const Value: TColor);
    {$IFDEF VCL}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    {$ENDIF VCL}
  protected
    {$IFDEF VCL}
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure ParentColorChanged; dynamic;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
    {$ENDIF VisualCLX}
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property BarColor: TColor read FBarColor write SetBarColor default clNavy;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

const
  PBM_SETBARCOLOR = WM_USER + 9; // lParam = bar color

constructor TJvProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  FBarColor := clNavy;
end;

{$IFDEF VCL}
procedure TJvProgressBar.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;
{$ENDIF VCL}

procedure TJvProgressBar.MouseEnter(AControl: TControl);
begin
  {$IFDEF VisualCLX}
  inherited MouseEnter(AControl);
  {$ENDIF VisualCLX}
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{$IFDEF VCL}
procedure TJvProgressBar.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;
{$ENDIF VCL}

procedure TJvProgressBar.MouseLeave(AControl: TControl);
begin
  {$IFDEF VisualCLX}
  inherited MouseLeave(AControl);
  {$ENDIF VisualCLX}
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFDEF VCL}
procedure TJvProgressBar.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  ParentColorChanged;
end;
{$ENDIF VCL}

procedure TJvProgressBar.ParentColorChanged;
begin
  {$IFDEF VisualCLX}
  inherited ParentColorChanged;
  {$ENDIF VisualCLX}
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvProgressBar.SetBarColor(const Value: TColor);
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    RecreateWnd;
  end;
end;

procedure TJvProgressBar.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FBarColor));
end;

end.

