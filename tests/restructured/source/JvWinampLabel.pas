{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWinampLabel.PAS, released on 2001-02-28.

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

unit JvWinampLabel;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, JVCLVer;

type
  TJvWinampThread = class(TThread)
  protected
    procedure Draw;
    procedure Execute; override;
  public
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
  end;

  TJvWinampLabel = class(TCustomLabel)
  private
    FBitmap: TBitmap;
    FPicture: TPicture;
    FTimer: TJvWinampThread;
    FInterval: Cardinal;
    FActive: Boolean;
    FStretch: Boolean;
    FScrollBy: Integer;
    FCurPos: Integer;
    FWait: Integer;
    FWaiting: Boolean;
    FColor: TColor;
    FScale: Real;
    // (p3) renamed
    FText: string;
    FAboutJVCL: TJVCLAboutInfo;
    function GetCol(Ch: Char): Word;
    procedure SetColor(Value: TColor);
    function GetScrollBy: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetPicture(Value: TPicture);
    procedure FillBitmap;
    procedure Activate;
    procedure Deactivate;
    procedure UpdatePos;
    procedure DoOnTimer(Sender: TObject);
    function GetRow(Ch: Char): Word;
    procedure SetText(Value: string);
  protected
    procedure paint; override;
  public
    CharWidth, CharHeight: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Active: Boolean read FActive write SetActive;
    property Stretch: Boolean read FStretch write SetStretch;
    property ScrollBy: Integer read GetScrollBy write FScrollBy;
    property ScrollInterval: Cardinal read FInterval write SetInterval;
    property WaitOnEnd: Integer read FWait write FWait;
    property Skin: TPicture read FPicture write SetPicture;
    property Color: TColor read FColor write SetColor;
    property Text: string read FText write SetText;
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

implementation
uses
  JvTypes;

const
  // (p3) fixed as suggested by Remko Bonte
  Row1: string[31] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ"@ ';
  Row2: string[31] = '0123456789._:()-''!_+\/[]^&%.=$#';
  Row3: string[31] = 'ÂÖÄ?* ';

resourcestring
  RC_InvalidSkin = 'Invalid skin';
  RC_WinampRC = 'WINAMP1';

{$R RES_WinampLabel.res}

  ///////////////////////////////////////////////////////////
  // TJvWinampThread
  ///////////////////////////////////////////////////////////

procedure TJvWinampThread.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self);
end;

{**************************************************}

procedure TJvWinampThread.Execute;
begin
  while not Terminated do
  begin
    // (rom) all other threads of this kind draw first then sleep
    Synchronize(Draw);
    Sleep(FDelay);
  end;
end;

///////////////////////////////////////////////////////////
// TJvWinampLabel
///////////////////////////////////////////////////////////

constructor TJvWinampLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := False;
  FInterval := 100;
  CharWidth := 5;
  CharHeight := 6;
  FPicture := TPicture.Create;
  FPicture.Bitmap.LoadFromResourceName(HInstance, RC_WinampRC);
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
    FDelay := FInterval;
    FOnDraw := DoOnTimer;
  end;
  Width := 100;
  Height := CharHeight * 2;
  FActive := False;
  Activate;
  FStretch := True;
  FScrollBy := 2;
  FWait := 1000;
  Color := clBlack;
end;

{**************************************************}

destructor TJvWinampLabel.Destroy;
begin
  Deactivate;
  FBitmap.Free;
  FPicture.Free;
  FTimer.Free;
  {  //-----------------
    FTimer.Terminate;
    while (not FTimer.Terminated) do
      Application.ProcessMessages;
    FTimer.Free;
    //-------------------}
  inherited Destroy;
end;

{**************************************************}

function TJvWinampLabel.GetScrollBy: Integer;
begin
  Result := Abs(FScrollBy);
end;

{**************************************************}

procedure TJvWinampLabel.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  if (FPicture.Bitmap.Width <> 155) or (FPicture.Bitmap.Height <> 18) then
    raise EJVCLException.Create(RC_InvalidSkin);
  FText := '';
  Invalidate;
end;

{**************************************************}

procedure TJvWinampLabel.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    FText := '';
    Invalidate;
  end;
end;

{**************************************************}

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

{**************************************************}

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
    Paint;
  end;
  if not FStretch then
    FScale := 1;
end;

{**************************************************}

procedure TJvWinampLabel.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    FTimer.FDelay := Value;
  end;
end;

{**************************************************}

procedure TJvWinampLabel.Activate;
begin
  FActive := True;
  if not (csDesigning in ComponentState) then
    FTimer.Resume;
  FTimer.FDelay := FInterval;
  FWaiting := False;

  FCurPos := 0;
  FScrollBy := Abs(FScrollBy);
  FillBitmap;
end;

{**************************************************}

procedure TJvWinampLabel.Deactivate;
begin
  if not (csDesigning in ComponentState) then
    FTimer.Suspend;
  FActive := False;
  Invalidate;
end;

{**************************************************}

procedure TJvWinampLabel.DoOnTimer(Sender: TObject);
begin
  if FWaiting then
  begin
    FTimer.FDelay := FInterval;
    FWaiting := False;
  end;
  UpdatePos;
  Paint;
end;

{**************************************************}

function TJvWinampLabel.GetCol(Ch: Char): Word;
var
  index: Integer;
begin
  Ch := UpCase(Ch);
  index := Pos(Ch, Row1);
  // (p3) Pos returns 0 on failure, not -1
  if index = 0 then
    index := Pos(Ch, Row2);
  if index = 0 then
    index := Pos(Ch, Row3);
  if index = 0 then
    Result := GetCol(' ')
  else
    // (p3) fixed as suggested by Remko Bonte
    Result := (index-1) * CharWidth
end;

{**************************************************}

function TJvWinampLabel.GetRow(Ch: Char): Word;
begin
  Ch := UpCase(Ch);
  Result := 0;
  if Pos(Ch, Row2) <> 0 then
    Result := CharHeight
  else if Pos(Ch, Row3) <> 0 then
    Result := 2 * CharHeight;
end;

{**************************************************}

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

{**************************************************}

procedure TJvWinampLabel.UpdatePos;
begin
  try
    if (Length(Text) * CharWidth) * FScale > Width then
    begin
      FCurPos := FCurPos + FScrollBy;
      if FCurPos <= 0 then
      begin
        FScrollBy := Abs(FScrollBy);
        if FWait <> 0 then
        begin
          FWaiting := True;
          FTimer.FDelay := FWait;
        end;
      end;
      if (Length(Text) * CharWidth - (FCurPos)) <= (Width / FScale) then
      begin
        FScrollBy := Abs(FScrollBy) * -1;
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

{**************************************************}

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
        BitBlt(Canvas.Handle, 0, 0, Width, CharHeight, FBitmap.Canvas.Handle, FCurPos, 0, srcCopy)
      else
      begin
        Rec := ClientRect;
        Rec.Bottom := Rec.Bottom + CharHeight;
        Rec.Left := rec.Left + (CharWidth * Length(Text));
        Canvas.FillRect(Rec);
        BitBlt(Canvas.Handle, 0, 0, Width, CharHeight, FBitmap.Canvas.Handle, 0, 0, srcCopy);
      end;
    end
    else
    begin
      FScale := Height / CharHeight;
      if FActive then
        StretchBlt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, FCurPos, 0, Round(Width / FScale),
          CharHeight, srcCopy)
      else
        StretchBlt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, Round(Width / FScale), CharHeight,
          srcCopy);
    end;
  except
  end;
end;

{**************************************************}

procedure TJvWinampLabel.SetText(Value: string);
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
    FScrollBy := Abs(FScrollBy);
  end;
end;

end.

