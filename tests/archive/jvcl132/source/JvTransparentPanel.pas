{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTransparentPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, JVCLVer;

type
  TJvTransparentPanel = class(TPanel)
  private
    { Private declarations }
    FBackground: TBitmap;
    FAboutJVCL: TJVCLAboutInfo;

    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd);
      message WM_ERASEBKGND;
  protected
    { Protected declarations }
    procedure CaptureBackground;
    procedure Paint; override;
  public
    { Public declarations }
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

{ TJvTransparentPanel }

procedure TJvTransparentPanel.CaptureBackground;
var
  canvas: TCanvas;
  dc: HDC;
  sourcerect: TRect;
begin
  FBackground := TBitmap.Create;
  with Fbackground do
  begin
    width := clientwidth;
    height := clientheight;
  end; { with }
  sourcerect.TopLeft := ClientToScreen(clientrect.TopLeft);
  sourcerect.BottomRight := ClientToScreen(clientrect.BottomRight);
  dc := CreateDC('DISPLAY', nil, nil, nil);
  try
    canvas := TCanvas.Create;
    try
      canvas.handle := dc;
      Fbackground.Canvas.CopyRect(clientrect, canvas, sourcerect);
    finally
      canvas.handle := 0;
      canvas.free;
    end; { finally }
  finally
    DeleteDC(dc);
  end; { finally }
end;

constructor TJvTransparentPanel.Create(aOwner: TComponent);
begin
  inherited;
  ControlStyle := controlStyle - [csSetCaption];
end;

destructor TJvTransparentPanel.Destroy;
begin
  FBackground.free;
  inherited;
end;

procedure TJvTransparentPanel.Paint;
begin
  if csDesigning in ComponentState then
    inherited
      // would need to draw frame and optional caption here
  // do NOT call inherited, the control fills its client area if you do!
end;

procedure TJvTransparentPanel.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  if Visible and HandleAllocated and not (csDesigning in ComponentState) then
  begin
    Fbackground.Free;
    Fbackground := nil;
    Hide;
    inherited;
    Parent.Update;
    Show;
  end
  else
    inherited;
end;

procedure TJvTransparentPanel.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  canvas: TCanvas;
begin
  if csDesigning in ComponentState then
    inherited
  else
  begin
    if not Assigned(FBackground) then
      Capturebackground;
    canvas := TCanvas.create;
    try
      canvas.handle := msg.DC;
      canvas.draw(0, 0, FBackground);
    finally
      canvas.handle := 0;
      canvas.free;
    end; { finally }
    msg.result := 1;
  end;
end;

end.
