{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSplitter.PAS, released on 2001-02-28.

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

unit JvSplitter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, ExtCtrls, Controls,
  JVCLVer, JvExExtCtrls;

type
  TJvSplitter = class(TJvExSplitter)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    {$IFDEF JVCLThemesEnabled}
    function GetParentBackground: Boolean;
    {$ENDIF JVCLThemesEnabled}
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ParentColorChanged; override;
    {$IFDEF JVCLThemesEnabled}
    procedure SetParentBackground(Value: Boolean); virtual;
    procedure Paint; override;
    {$ENDIF JVCLThemesEnabled}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ShowHint;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    {$IFDEF JVCLThemesEnabled}
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground default True;
    {$ENDIF JVCLThemesEnabled}
  end;

implementation

uses
  JvThemes;

constructor TJvSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
  FHintColor := clInfoBk;
  FOver := False;
end;

procedure TJvSplitter.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvSplitter.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FOver := True;
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvSplitter.MouseLeave(Control: TControl);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
    inherited MouseLeave(Control);
  end;
end;

{$IFDEF JVCLThemesEnabled}

procedure TJvSplitter.Paint;
var
  Bmp: TBitmap;
  DC: THandle;
begin
  if ThemeServices.ThemesEnabled and ParentBackground then
  begin
//    DrawThemedBackground(Self, Canvas, ClientRect, Parent.Brush.Color);
    DC := Canvas.Handle;
    Bmp := TBitmap.Create;
    try
      Bmp.Width := ClientWidth;
      Bmp.Height := ClientHeight;
      Canvas.Handle := Bmp.Canvas.Handle;
      try
        inherited Paint;
      finally
        Canvas.Handle := DC;
      end;
      Bmp.Transparent := True;
      Bmp.TransparentColor := Color;
      Canvas.Draw(0, 0, Bmp);
    finally
      Bmp.Free;
    end;
  end
  else
    inherited Paint;
end;

function TJvSplitter.GetParentBackground: Boolean;
begin
  Result := csParentBackground in GetThemeStyle(Self);
end;

procedure TJvSplitter.SetParentBackground(Value: Boolean);
begin
  if Value <> GetParentBackground then
  begin
    if Value then
      IncludeThemeStyle(Self, [csParentBackground])
    else
      ExcludeThemeStyle(Self, [csParentBackground]);
  end;
end;

{$ENDIF JVCLThemesEnabled}

end.

