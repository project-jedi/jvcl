{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWinampLabel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvWinampLabel;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Messages, Graphics, Controls, StdCtrls,
  JvExStdCtrls, JvThread;

type
  TJvWinampThread = class(TJvPausableThread)
  protected
    procedure Draw;
    procedure Execute; override;
  public
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvWinampLabel = class(TJvExCustomLabel)
  private
    FBitmap: TBitmap;
    FPicture: TPicture;
    FTimer: TJvWinampThread;
    FScrollInterval: Cardinal;
    FActive: Boolean;
    FStretch: Boolean;
    FScrollTextBy: Integer;
    FCurPos: Integer;
    FWait: Integer;
    FWaiting: Boolean;
    FScale: Real;
    // (p3) renamed
    FText: TCaption;
    FCharHeight: Integer;
    FCharWidth: Integer;
    function GetScrollTextBy: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetPicture(Value: TPicture);
    procedure FillBitmap;
    procedure Activate;
    procedure Deactivate;
    procedure UpdatePos;
    procedure DoOnTimer(Sender: TObject);
    function GetCol(Ch: Char): Word;
    function GetRow(Ch: Char): Word;
    procedure SetText(const Value: TCaption);
  protected
    procedure ColorChanged; override;
    procedure Paint; override;
    // (rom) made protected property
    property CharHeight: Integer read FCharHeight;
    property CharWidth: Integer read FCharWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property Stretch: Boolean read FStretch write SetStretch;
    property ScrollTextBy: Integer read GetScrollTextBy write FScrollTextBy;
    property ScrollInterval: Cardinal read FScrollInterval write SetInterval;
    property WaitOnEnd: Integer read FWait write FWait;
    property Skin: TPicture read FPicture write SetPicture;
    property Color;
    property Text: TCaption read FText write SetText;
    property Align;
    property Alignment;
    property FocusControl;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property ShowHint;
    property ParentShowHint;
    property Layout;
    property Left;
    property Transparent;
    property PopupMenu;
    property Visible;
    property Top;
    property Height;
    property Width;
    property Cursor;
    property Enabled;
    property Hint;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnStartDrag;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvTypes, JvResources;

{$R JvWinampLabel.res}

const
  // (p3) fixed as suggested by Remko Bonte
  Row1: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ"@ ';
  Row2: string = '0123456789._:()-''!_+\/[]^&%.=$#';
  Row3: string = '���?* ';

//=== { TJvWinampThread } ====================================================

procedure TJvWinampThread.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self);
end;

procedure TJvWinampThread.Execute;
begin
  NameThread(ThreadName);
  // (rom) secure thread against exceptions
  try
    while not Terminated do
    begin
      EnterUnpauseableSection;
      try
        if Terminated then
          Exit;

        Synchronize(Draw);
      finally
        LeaveUnpauseableSection;
      end;
      Sleep(FDelay);
    end;
  except
  end;
end;

//=== { TJvWinampLabel } =====================================================

constructor TJvWinampLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := False;
  FScrollInterval := 100;
  FCharWidth := 5;
  FCharHeight := 6;
  FPicture := TPicture.Create;
  FPicture.Bitmap.LoadFromResourceName(HInstance, 'JvWinampLabelPICTURE');
  FBitmap := TBitmap.Create;
  with FBitmap do
  begin
    PixelFormat := pf24bit;
    Width := 10;
    Height := 10;
  end;
  FTimer := TJvWinampThread.Create(True);
  with FTimer do
  begin
    FreeOnTerminate := False;
    FDelay := FScrollInterval;
    FOnDraw := DoOnTimer;
  end;
  Width := 100;
  Height := CharHeight * 2;
  FActive := False;
  Activate;
  FStretch := True;
  FScrollTextBy := 2;
  FWait := 1000;
  Color := clBlack;
end;

destructor TJvWinampLabel.Destroy;
begin
  FTimer.Free;
  FBitmap.Free;
  FPicture.Free;
  inherited Destroy;
end;

function TJvWinampLabel.GetScrollTextBy: Integer;
begin
  Result := Abs(FScrollTextBy);
end;

procedure TJvWinampLabel.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  if (FPicture.Bitmap.Width <> 155) or (FPicture.Bitmap.Height <> 18) then
    raise EJVCLException.CreateRes(@RsEInvalidSkin);
  FText := '';
  Invalidate;
end;

procedure TJvWinampLabel.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
      Activate
    else
      Deactivate;
    FWaiting := False;
  end;
end;

procedure TJvWinampLabel.SetStretch(Value: Boolean);
var
  Rec: TRect;
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    Rec.Top := 0;
    Rec.Left := 0;
    Rec.Bottom := Height;
    Rec.Right := Width;
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rec);
    Repaint;
  end;
  if not FStretch then
    FScale := 1;
end;

procedure TJvWinampLabel.SetInterval(Value: Cardinal);
begin
  if Value <> FScrollInterval then
  begin
    FScrollInterval := Value;
    FTimer.FDelay := Value;
  end;
end;

procedure TJvWinampLabel.Activate;
begin
  FActive := True;
  if not (csDesigning in ComponentState) then
    FTimer.Paused := False;
  FTimer.FDelay := FScrollInterval;
  FWaiting := False;

  FCurPos := 0;
  FScrollTextBy := Abs(FScrollTextBy);
  FillBitmap;
end;

procedure TJvWinampLabel.Deactivate;
begin
  if not (csDesigning in ComponentState) then
    FTimer.Paused := True;
  FActive := False;
  Invalidate;
end;

procedure TJvWinampLabel.DoOnTimer(Sender: TObject);
begin
  // Must exit because we are "Synchronized" and our parent is already
  // partly destroyed. If we did not exit, we would get an AV.
  if csDestroying in ComponentState then
    Exit;

  if FWaiting then
  begin
    FTimer.FDelay := FScrollInterval;
    FWaiting := False;
  end;
  UpdatePos;
  Repaint;
end;




function TJvWinampLabel.GetCol(Ch: Char): Word;


var
  Index: Integer;
begin
  Ch := UpCase(Ch);
  Index := Pos(Ch, Row1);
  // (p3) Pos returns 0 on failure, not -1
  if Index = 0 then
    Index := Pos(Ch, Row2);
  if Index = 0 then
    Index := Pos(Ch, Row3);
  if Index = 0 then
    Result := GetCol(' ')
  else
    // (p3) fixed as suggested by Remko Bonte
    Result := (Index - 1) * CharWidth;
end;


function TJvWinampLabel.GetRow(Ch: Char): Word;


begin
  Ch := UpCase(Ch);
  Result := 0;
  if Pos(Ch, Row2) <> 0 then
    Result := CharHeight
  else
  if Pos(Ch, Row3) <> 0 then
    Result := 2 * CharHeight;
end;

procedure TJvWinampLabel.FillBitmap;
var
  Rec, SourceRect, DestRect: TRect;
  T: Word;
begin
  try
    with FBitmap do
    begin
      TransparentMode := tmAuto;
      if Text <> '' then
        Width := Length(Text) * CharWidth
      else
        Width := Self.Width;
      Height := CharHeight;
      if Width < Self.Width then
        Width := Self.Width;
      Rec.Top := 0;
      Rec.Left := 0;
      Rec.Bottom := Height;
      Rec.Right := Width;
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rec);
      if Self.Text <> '' then
        for T := 0 to Length(Text) - 1 do
        begin
          // (p3) fixed as suggested by Remko Bonte
          SourceRect := Bounds(GetCol(Text[T + 1]),
            GetRow(Text[T + 1]), CharWidth, CharHeight);
          DestRect := Bounds(T * CharWidth, 0, CharWidth, CharHeight);
          Canvas.CopyRect(DestRect, FPicture.Bitmap.Canvas, SourceRect);
        end;
    end;
  except
  end;
end;

procedure TJvWinampLabel.UpdatePos;
begin
  try
    if (Length(Text) * CharWidth) * FScale > Width then
    begin
      FCurPos := FCurPos + FScrollTextBy;
      if FCurPos <= 0 then
      begin
        FScrollTextBy := Abs(FScrollTextBy);
        if FWait <> 0 then
        begin
          FWaiting := True;
          FTimer.FDelay := FWait;
        end;
      end;
      if (Length(Text) * CharWidth - (FCurPos)) <= (Width / FScale) then
      begin
        FScrollTextBy := Abs(FScrollTextBy) * -1;
        if FWait <> 0 then
        begin
          FWaiting := True;
          FTimer.FDelay := FWait;
        end;
      end;
    end
    else
      FCurPos := 0;
  except
  end;
end;

procedure TJvWinampLabel.Paint;
var
  Rec: TRect;
begin
  try
    if not FStretch then
    begin
      Rec := ClientRect;
      Rec.Top := Rec.Top + CharHeight;
      Canvas.FillRect(Rec);
      if FActive then
        BitBlt(Canvas.Handle, 0, 0, Width, CharHeight, FBitmap.Canvas.Handle, FCurPos, 0, SRCCOPY)
      else
      begin
        Rec := ClientRect;
        Rec.Bottom := Rec.Bottom + CharHeight;
        Rec.Left := Rec.Left + (CharWidth * Length(Text));
        Canvas.FillRect(Rec);
        BitBlt(Canvas.Handle, 0, 0, Width, CharHeight, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end
    else
    begin
      FScale := Height / CharHeight;
      if FActive then
        StretchBlt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, FCurPos, 0, Round(Width / FScale),
          CharHeight, SRCCOPY)
      else
        StretchBlt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, Round(Width / FScale), CharHeight,
          SRCCOPY);
    end;
  except
  end;
end;



procedure TJvWinampLabel.SetText(const Value: TCaption);
var
  Rec: TRect;
begin
  if Value <> FText then
  begin
    FText := Value;
    FillBitmap;
    Rec.Top := 0;
    Rec.Left := 0;
    Rec.Bottom := Height;
    Rec.Right := Width;
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rec);
    FCurPos := 0;
    FScrollTextBy := Abs(FScrollTextBy);
  end;
end;

procedure TJvWinampLabel.ColorChanged;
begin
  FText := '';
  inherited ColorChanged;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
