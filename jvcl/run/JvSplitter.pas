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

{$I JVCL.INC}

unit JvSplitter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, ExtCtrls, Controls,
  JVCLVer;

type
  TJvSplitter = class(TSplitter)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
  {$IFDEF JVCLThemesEnabled}
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
  {$ENDIF JVCLThemesEnabled}
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    {$IFDEF JVCLThemesEnabled}
    procedure Paint; override;
    {$ENDIF JVCLThemesEnabled}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ShowHint;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
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

procedure TJvSplitter.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvSplitter.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FOver := True;
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvSplitter.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
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

