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

{$I jvcl.inc}

unit JvAnimate;

interface

uses
{$IFDEF COMPLIB_VCL}
  Messages, Graphics, Controls, Forms, ComCtrls,
{$ENDIF}
{$IFDEF COMPLIB_CLX}
  QGraphics, QControls, QForms, QComCtrls,
{$ENDIF}
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
{$IFDEF COMPLIB_VCL}
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
{$ENDIF}
  protected
{$IFDEF COMPLIB_VCL}
    procedure DoMouseEnter(var Msg: TMessage);
    procedure DoMouseLeave(var Msg: TMessage);
    procedure DoParentColorChange(var Msg: TMessage);
{$ENDIF}
{$IFDEF COMPLIB_CLX}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
    procedure DoMouseEnter;
    procedure DoMouseLeave;
    procedure DoParentColorChange;
{$ENDIF}
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

{$IFDEF COMPLIB_VCL}
procedure TJvAnimate.DoParentColorChange(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.DoParentColorChange;
{$ENDIF}
begin
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

{$IFNDEF COMPLIB_CLX}
procedure TJvAnimate.DoMouseEnter(var Msg: TMessage);
begin
{$ELSE}
procedure TJvAnimate.DoMouseEnter;
begin
{$ENDIF}
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

{$IFNDEF COMPLIB_CLX}
procedure TJvAnimate.DoMouseLeave(var Msg: TMessage);
begin
{$ELSE}
procedure TJvAnimate.DoMouseLeave;
begin
//  inherited;
{$ENDIF}
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

{$IFNDEF COMPLIB_CLX}
procedure TJvAnimate.CMParentColorChanged(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.ParentColorChanged;
{$ENDIF}
begin
  inherited;
{$IFNDEF COMPLIB_CLX}
  DoParentColorChange(Msg);
{$ELSE}
  DoParentColorChange;
{$ENDIF}
end;

{**************************************************}

{$IFNDEF COMPLIB_CLX}
procedure TJvAnimate.MouseEnter(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.MouseEnter(AControl: TControl);
{$ENDIF}
begin
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
{$IFNDEF COMPLIB_CLX}
  DoMouseEnter(Msg);
{$ELSE}
  DoMouseEnter;
{$ENDIF}
end;

{**************************************************}

{$IFNDEF COMPLIB_CLX}
procedure TJvAnimate.MouseLeave(var Msg: TMessage);
{$ELSE}
procedure TJvAnimate.MouseLeave(AControl: TControl);
{$ENDIF}
begin
  Application.HintColor := FSaved;
{$IFNDEF COMPLIB_CLX}
  DoMouseLeave(Msg);
{$ELSE}
  DoMouseLeave;
{$ENDIF}
end;

end.
