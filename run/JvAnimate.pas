{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAnimate.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].
                André Snepvangers [asn@xs4all.nl]

Last Modified:
2003-01-19 - (asn) support for CLX

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAnimate;

interface

uses
  {$IFDEF VCL}
  Messages, Graphics, Controls, Forms, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QComCtrls,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  JvThemes, JVCLVer;

type
  TJvAnimate = class(TAnimate)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    {$IFDEF VCL}
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    {$ENDIF VCL}
  protected
    {$IFDEF VCL}
    procedure DoMouseEnter(var Msg: TMessage);
    procedure DoMouseLeave(var Msg: TMessage);
    procedure DoParentColorChange(var Msg: TMessage);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
    procedure DoMouseEnter;
    procedure DoMouseLeave;
    procedure DoParentColorChange;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnResize;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

implementation

constructor TJvAnimate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  ControlStyle := ControlStyle + [csAcceptsControls];
  IncludeThemeStyle(Self, [csParentBackground]);
end;

{$IFDEF VCL}
procedure TJvAnimate.DoParentColorChange(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.DoParentColorChange;
{$ENDIF VCL}
begin
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

{$IFDEF VCL}
procedure TJvAnimate.DoMouseEnter(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.DoMouseEnter;
{$ENDIF VCL}
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

{$IFDEF VCL}
procedure TJvAnimate.DoMouseLeave(var Msg: TMessage);
begin
{$ELSE}
procedure TJvAnimate.DoMouseLeave;
begin
//  inherited;
{$ENDIF VCL}
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

{$IFDEF VCL}
procedure TJvAnimate.CMParentColorChanged(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.ParentColorChanged;
{$ENDIF VCL}
begin
  inherited;
  {$IFDEF VCL}
  DoParentColorChange(Msg);
  {$ELSE}
  DoParentColorChange;
  {$ENDIF VCL}
end;

{**************************************************}

{$IFDEF VCL}
procedure TJvAnimate.MouseEnter(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.MouseEnter(AControl: TControl);
{$ENDIF VCL}
begin
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
  {$IFDEF VCL}
  DoMouseEnter(Msg);
  {$ELSE}
  DoMouseEnter;
  {$ENDIF VCL}
end;

{**************************************************}

{$IFDEF VCL}
procedure TJvAnimate.MouseLeave(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.MouseLeave(AControl: TControl);
{$ENDIF VCL}
begin
  Application.HintColor := FSaved;
  {$IFDEF VCL}
  DoMouseLeave(Msg);
  {$ELSE}
  DoMouseLeave;
  {$ENDIF VCL}
end;

end.
