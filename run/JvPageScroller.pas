{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageScroller.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvPageScroller;

interface

uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ComCtrls,
  {$ENDIF}
  {$IFDEF VisualCLX}
  Types, QGraphics, QControls, QForms, QComCtrls,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  JVCLVer, JvExComCtrls;

type
  TJvPageScroller = class(TJvExPageScroller)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FOnParentColorChanged: TNotifyEvent;
  protected
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure ParentColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  JvThemes;

constructor TJvPageScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  IncludeThemeStyle(Self, [csParentBackground]);
end;

procedure TJvPageScroller.MouseEnter(AControl: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    FOver := True;
    inherited MouseEnter(AControl);
  end;
end;

procedure TJvPageScroller.MouseLeave(AControl: TControl);
begin
  if FOver then
  begin
    FOver := False;
    Application.HintColor := FSaved;
    inherited MouseLeave(AControl);
  end;
end;

procedure TJvPageScroller.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

end.

