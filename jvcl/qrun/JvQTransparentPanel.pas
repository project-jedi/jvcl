{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113 dott 1101 att compuserve dott com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQTransparentPanel;

interface

uses
  Types, QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QExtCtrls, 
  Qt, 
  JvQPanel;

type
  TJvTransparentPanel = class(TJvPanel)
  private
    FBackground: TBitmap;
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
    procedure CaptureBackground;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

implementation

constructor TJvTransparentPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
end;

destructor TJvTransparentPanel.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

procedure TJvTransparentPanel.CaptureBackground;
var 
  SourceRect: TRect;
begin
  // (rom) check here to secure against misuse
  if Assigned(FBackground) then
    Exit;
  FBackground := TBitmap.Create;
  with FBackground do
  begin
    Width := ClientWidth;
    Height := ClientHeight;
  end;
  SourceRect.TopLeft := ClientToScreen(ClientRect.TopLeft);
  SourceRect.BottomRight := ClientToScreen(ClientRect.BottomRight);  
  QPixmap_grabWidget(FBackground.Handle, HWND_DESKTOP, SourceRect.Left, SourceRect.Top,
     (SourceRect.Right - SourceRect.Left), (SourceRect.Bottom - SourceRect.Top)); 
end;

procedure TJvTransparentPanel.Paint;
begin
  if csDesigning in ComponentState then
    inherited Paint;
    // would need to draw frame and optional caption here
  // do NOT call inherited, the control fills its client area if you do!
end;

procedure TJvTransparentPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Visible and HandleAllocated and not (csDesigning in ComponentState) then
  begin
    FBackground.Free;
    FBackground := nil;
    Hide;
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    Parent.Update;
    Show;
  end
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TJvTransparentPanel.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  if csDesigning in ComponentState then
    Result := inherited DoPaintBackground(Canvas, Param)
  else
  begin
    CaptureBackground;
    Canvas.Draw(0, 0, FBackground);
    Result := True;
  end;
end;

end.

