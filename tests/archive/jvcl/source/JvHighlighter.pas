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

{$I JVCL.INC}

unit JvHighlighter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JVCLVer;

type
  TJvHighlighter = class(TGraphicControl)
  private
    { Private declarations }
    FFocusControl: TWinControl;
    FExtraBorder: Integer;
    FAboutJVCL: TJVCLAboutInfo;
  protected
    { Protected declarations }
    procedure SetFocusControl(value: TWinControl);
    procedure SetExtraBorder(value: Integer);
  public
    { Public declarations }
    procedure Paint; override;
    constructor Create(aOwner: TComponent); override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Color;
    property ExtraBorder: Integer read FExtraBorder write SetExtraBorder
      default 4;
  end;

implementation

{+-------------------------
 | Methods of TJvHighlighter
 +------------------------}

procedure TJvHighlighter.SetFocusControl(value: TWinControl);
begin
  if value = FFocusControl then
    Exit;
  Hide;
  if value <> nil then
  begin
    FFocusControl := value;
    Parent := value.Parent;
    SetBounds(value.Left - FExtraBorder,
      value.Top - FExtraBorder,
      value.Width + 2 * FExtraBorder,
      value.Height + 2 * FExtraBorder);
    Show;
  end { If }
  else
    Parent := nil;
end; { TJvHighlighter.SetFocusControl }

procedure TJvHighlighter.SetExtraBorder(value: Integer);
begin
  if value <> FExtraBorder then
  begin
    FExtraBorder := value;
    Invalidate;
  end; { If }
end; { TJvHighlighter.SetExtraBorder }

procedure TJvHighlighter.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
end; { TJvHighlighter.Paint }

constructor TJvHighlighter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FExtraBorder := 4;
  Width := 30;
  Height := 30;
  Color := clBlue;
end; { TJvHighlighter.Create }

end.
