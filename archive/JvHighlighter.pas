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

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHighlighter;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  JVCLVer;
  
type
  TJvHighlighter = class(TGraphicControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FFocusControl: TWinControl;
    FExtraBorder: Integer;
  protected
    procedure SetFocusControl(Value: TWinControl);
    procedure SetExtraBorder(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Color;
    property ExtraBorder: Integer read FExtraBorder write SetExtraBorder default 4;
    property Height default 30;
    property Width default 30;
  end;

implementation

constructor TJvHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FExtraBorder := 4;
  Width := 30;
  Height := 30;
  Color := clBlue;
end;

procedure TJvHighlighter.SetFocusControl(Value: TWinControl);
begin
  if Value = FFocusControl then
    Exit;
  Hide;
  if Value <> nil then
  begin
    FFocusControl := Value;
    Parent := Value.Parent;
    SetBounds(Value.Left - FExtraBorder,
      Value.Top - FExtraBorder,
      Value.Width + 2 * FExtraBorder,
      Value.Height + 2 * FExtraBorder);
    Show;
  end
  else
    Parent := nil;
end;

procedure TJvHighlighter.SetExtraBorder(Value: Integer);
begin
  if Value <> FExtraBorder then
  begin
    FExtraBorder := Value;
    Invalidate;
  end;
end;

procedure TJvHighlighter.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
end;

end.
