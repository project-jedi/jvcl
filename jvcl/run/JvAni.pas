{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAni.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvAni;

interface

uses
  SysUtils, Classes, Consts,
  {$IFDEF VCL}
  Windows, Graphics, Forms, ExtCtrls, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QForms, QExtCtrls, QDialogs, Types,
  {$ENDIF QForms}
  JvTypes, JvAniFile;

type
  TJvAni = class(TGraphic)
  private
    FIconData: TJvAnimatedCursorImage;
    FIndex: Integer;
    FTimer: TTimer;
    procedure Clear;
    procedure SetIndex(const Value: Integer);
    function GetAnimated: Boolean;
    function GetAuthor: string;
    function GetTitle: string;
    function GetIconCount: Cardinal;
    function GetFrameCount: Cardinal;
    function GetCurrentIcon: TIcon;
    procedure SetAnimated(const Value: Boolean);
    procedure CalcDelay;
  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure Animate(Sender: TObject);
    procedure SetTransparent(Value: Boolean); override;
    function GetTransparent: Boolean; override;
    function GetDefaultRate: Longint;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    {$IFDEF VCL}
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE); override;
    procedure AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
      DecreaseColors, Vertical: Boolean);
    {$ENDIF VCL}
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    property Author: string read GetAuthor;
    property Title: string read GetTitle;
    property Icon: TIcon read GetCurrentIcon;
    property FrameCount: Cardinal read GetFrameCount;
    property IconCount: Cardinal read GetIconCount;
    property Index: Integer read FIndex write SetIndex;
    property Animated: Boolean read GetAnimated write SetAnimated;
    property DefaultRate: Longint read GetDefaultRate;
  end;

function LoadJvAnimatedCursorImageDialog: TJvAni;

implementation

uses
  JvConsts, JvResources;

constructor TJvAni.Create;
begin
  inherited Create;
  FIconData := TJvAnimatedCursorImage.Create;
  FIndex := -1;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer := Animate;
  FTimer.Enabled := False;
end;

destructor TJvAni.Destroy;
begin
  FTimer.Free;
  FIconData.Free;
  inherited Destroy;
end;

procedure TJvAni.Clear;
begin
  FIconData.Free;
  FIconData := TJvAnimatedCursorImage.Create;
  FIndex := -1;
  if not (csDestroying in Application.ComponentState) then
    Changed(Self);
end;

procedure TJvAni.Assign(Source: TPersistent);
var
  Stream: TStream;
begin
  if Source = nil then
    Clear
  else
  if Source is TJvAni then
  begin
    Stream := TMemoryStream.Create;
    TJvAni(Source).SaveToStream(Stream);
    Stream.Position := 0;
    LoadFromStream(Stream);
    Stream.Free;
    Animated := TJvAni(Source).Animated;
  end
  else
    inherited Assign(Source);
end;

procedure TJvAni.AssignTo(Dest: TPersistent);
begin
  FIconData.AssignTo(Dest);
end;

function TJvAni.GetEmpty: Boolean;
begin
  Result := (FrameCount = 0);
end;

procedure TJvAni.SetHeight(Value: Integer);
begin
  raise EInvalidGraphicOperation.Create(SChangeIconSize);
end;

procedure TJvAni.SetWidth(Value: Integer);
begin
  raise EInvalidGraphicOperation.Create(SChangeIconSize);
end;

function TJvAni.GetWidth: Integer;
begin
  Result := FIconData.Header.dwCX;
end;

function TJvAni.GetHeight: Integer;
begin
  Result := FIconData.Header.dwCY;
end;

{$IFDEF VCL}

procedure TJvAni.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
begin
  raise EInvalidGraphicOperation.Create(SIconToClipboard);
end;

procedure TJvAni.SaveToClipboardFormat(var Format: Word; var Data: THandle; var APalette: HPALETTE);
begin
  raise EInvalidGraphicOperation.Create(SIconToClipboard);
end;

procedure TJvAni.AssignToBitmap(Bitmap: TBitmap; BackColor: TColor;
  DecreaseColors, Vertical: Boolean);
begin
  FIconData.AssignToBitmap(Bitmap, Backcolor, DecreaseColors, Vertical);
end;

{$ENDIF VCL}

procedure TJvAni.LoadFromStream(Stream: TStream);
begin
  FIconData.LoadFromStream(Stream);
  if FIconData.FrameCount > 0 then
    Index := 0;
end;

procedure TJvAni.SaveToStream(Stream: TStream);
begin
  FIconData.SaveToStream(Stream);
end;

procedure TJvAni.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  FIconData.Draw(ACanvas, ARect);
end;

procedure TJvAni.SetIndex(const Value: Integer);
begin
  if (FrameCount > 0) and (Value >= 0) and
    (Cardinal(Value) < FrameCount) and (FIndex <> Value) then
  begin
    FIndex := Value;
    FIconData.Index := Value;
    Changed(Self);
  end;
end;

function TJvAni.GetAuthor: string;
begin
  Result := FIconData.Creator;
end;

function TJvAni.GetTitle: string;
begin
  Result := FIconData.Title;
end;

function TJvAni.GetIconCount: Cardinal;
begin
  Result := Cardinal(FIconData.IconCount);
end;

function TJvAni.GetFrameCount: Cardinal;
begin
  Result := Cardinal(FIconData.FrameCount);
end;

function TJvAni.GetCurrentIcon: TIcon;
begin
  Result := FIconData.Frames[Index].Icon;
end;

function TJvAni.GetAnimated: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TJvAni.GetDefaultRate: Longint;
begin
  Result := FIconData.DefaultRate;
end;

procedure TJvAni.SetAnimated(const Value: Boolean);
begin
  if Value <> FTimer.Enabled then
    FTimer.Enabled := Value;
end;

procedure TJvAni.Animate(Sender: TObject);
begin
  FTimer.Enabled := False;
  if FrameCount > 0 then
    Index := (Index + 1) mod Integer(FrameCount);
  CalcDelay;
  FTimer.Enabled := True;
end;

procedure TJvAni.CalcDelay;
begin
  if Index = -1 then
    Animated := False
  else
  begin
    FTimer.Interval := Cardinal(FIconData.Frames[Index].JiffRate) * (1000 div 60);
    if FTimer.Interval = 0 then
      FTimer.Interval := 100;
  end;
end;

procedure TJvAni.SetTransparent(Value: Boolean);
begin
  // Icons are always transparent so animations also
end;

function TJvAni.GetTransparent: Boolean;
begin
  Result := True;
end;

//=== TJvAnimatedCursorImage helper ==========================================

// (rom) created because JvAnimatedEditor.pas and JvIconListForm.pas contained the same code

function LoadJvAnimatedCursorImageDialog: TJvAni;
var
  CurDir: string;
begin
  Result := nil;
  CurDir := GetCurrentDir;
  with TOpenDialog.Create(Application) do
    try
      Options := [{$IFDEF VCL} ofHideReadOnly, {$ENDIF} ofFileMustExist];
      DefaultExt := RsAniExtension;
      Filter := RsAniCurFilter;
      if Execute then
      begin
        Result := TJvAni.Create;
        try
          Result.LoadFromFile(FileName);
        except
          FreeAndNil(Result);
          raise;
        end;
      end;
    finally
      Free;
      SetCurrentDir(CurDir);
    end;
end;

initialization
  Classes.RegisterClass(TJvAni);
  TPicture.RegisterFileFormat(RsAniExtension, RsAniFilterName, TJvAni);

finalization
  TPicture.UnregisterGraphicClass(TJvAni);

end.
