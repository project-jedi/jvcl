{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaret.PAS, released on 2003-02-15.

The Initial Developer of the Original Code is Joe Doe .
Portions created by Joe Doe are Copyright (C) 1999 Joe Doe.
Portions created by XXXX Corp. are Copyright (C) 1998, 1999 XXXX Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCaret;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  {$IFDEF VCL}
  Graphics, Controls,
  {$ENDIF}
  {$IFDEF VisualCLX}
  Qt, Types, QGraphics, QControls, QExtCtrls, QForms,
  {$ENDIF}
  SysUtils, Classes,
  JVCLVer, JvTypes;

type
  { A caret can be specified either by giving a bitmap that defines its shape
    or by defining the caret width and height. If a bitmap is specified the
    other properties are set to 0, if width or height are specified the
    bitmap is not used. A change to the caret at runtime will only have an
    immediate effect if the control has focus. }

  TJvCaret = class(TPersistent)
  private
    FCaretBitmap: TBitmap;
    FCaretWidth: Integer;
    FCaretHeight: Integer;
    FGrayCaret: Boolean;
    FCaretOwner: TWinControl;
    FUpdateCount: Integer;
    FOnChanged: TNotifyEvent;
    FCaretCreated: Boolean;
    procedure SetCaretBitmap(const Value: TBitmap);
    procedure SetCaretHeight(const Value: Integer);
    procedure SetCaretWidth(const Value: Integer);
    procedure SetGrayCaret(const Value: Boolean);
    procedure ReadBitmap(Stream: TStream);
    procedure WriteBitmap(Stream: TStream);
  protected
    procedure Changed; dynamic;
    function UsingBitmap: Boolean;
    function IsDefaultCaret: Boolean;
    property CaretOwner: TWinControl read FCaretOwner;
    property UpdateCount: Integer read FUpdateCount;
    property CaretCreated: Boolean read FCaretCreated;
  public
    constructor Create(Owner: TWinControl);
    destructor Destroy; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoCreateCaret;
    procedure DoDestroyCaret;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    { Note: streaming system does not deal properly with a published persistent
      property on another nested persistent. We use a pseudoproperty to save the
      bitmap. }
    property Bitmap: TBitmap read FCaretBitmap write SetCaretBitmap stored False;
    property Width: Integer read FCaretWidth write SetCaretWidth default 0;
    property Height: Integer read FCaretHeight write SetCaretHeight default 0;
    property Gray: Boolean read FGrayCaret write SetGrayCaret default False;
  end;

{$IFDEF VisualCLX}
function CreateCaret(Widget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean; overload;
function CreateCaret(Widget: QWidgetH; ColorCaret: Cardinal; Width, Height: Integer): Boolean; overload;
function GetCaretBlinkTime: Cardinal;
function SetCaretBlinkTime(MSeconds: Cardinal): Boolean;
function HideCaret(Widget: QWidgetH): Boolean;
function ShowCaret(Widget: QWidgetH): Boolean;
function SetCaretPos(X, Y: Integer): Boolean;
function GetCaretPos(var Pt: TPoint): Boolean;
function DestroyCaret: Boolean;
{$ENDIF VisualCLX}

implementation

uses
  JvJCLUtils, JvResources;

{$IFDEF VisualCLX}
type
  TEmulatedCaret = class(TObject)
  private
    FTimer: TTimer;
    FWndId: Cardinal;
    FWidget: QWidgetH;
    FPixmap: QPixmapH;
    FWidth, FHeight: Integer;
    FPos: TPoint;

    FVisible: Boolean;
    FShown: Boolean;
    FCritSect: TRTLCriticalSection;
    procedure SetPos(const Value: TPoint);
  protected
    procedure DoTimer(Sender: TObject);
    procedure DrawCaret; virtual;
    function CreateColorPixmap(Color: Cardinal): QPixmapH;
    procedure SetWidget(AWidget: QWidgetH);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    function CreateCaret(AWidget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean;
    function DestroyCaret: Boolean;

    function IsValid: Boolean;

    function Show(AWidget: QWidgetH): Boolean;
    function Hide: Boolean;

    property Timer: TTimer read FTimer;
    property Pos: TPoint read FPos write SetPos;
  end;

var
  GlobalCaret: TEmulatedCaret;

function CreateCaret(Widget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.CreateCaret(Widget, Pixmap, Width, Height);
  finally
    GlobalCaret.Unlock;
  end;
end;

function CreateCaret(Widget: QWidgetH; ColorCaret: Cardinal; Width, Height: Integer): Boolean;
begin
  Result := CreateCaret(Widget, QPixmapH(ColorCaret), Width, Height);
end;

function GetCaretBlinkTime: Cardinal;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.Timer.Interval;
  finally
    GlobalCaret.Unlock;
  end;
end;

function SetCaretBlinkTime(MSeconds: Cardinal): Boolean;
begin
  Result := True;
  GlobalCaret.Lock;
  try
    GlobalCaret.Timer.Interval := MSeconds;
  finally
    GlobalCaret.Unlock;
  end;
end;

function HideCaret(Widget: QWidgetH): Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.Hide;
  finally
    GlobalCaret.Unlock;
  end;
end;

function ShowCaret(Widget: QWidgetH): Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.Show(Widget);
  finally
    GlobalCaret.Unlock;
  end;
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := True;
  GlobalCaret.Lock;
  try
    GlobalCaret.Pos := Point(X, Y);
  finally
    GlobalCaret.Unlock;
  end;
end;

function GetCaretPos(var Pt: TPoint): Boolean;
begin
  Result := True;
  GlobalCaret.Lock;
  try
    Pt := GlobalCaret.Pos;
  finally
    GlobalCaret.Unlock;
  end;
end;

function DestroyCaret: Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.DestroyCaret;
  finally
    GlobalCaret.Unlock;
  end;
end;

{ TEmulatedCaret }

constructor TEmulatedCaret.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCritSect);

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
{$IFDEF MSWINDOWS}
  FTimer.Interval := Windows.GetCaretBlinkTime;
{$ELSE}
  FTimer.Interval := 800;
{$ENDIF}
  FTimer.OnTimer := DoTimer;
end;

destructor TEmulatedCaret.Destroy;
begin
  DestroyCaret;
  FTimer.Free;
  DeleteCriticalSection(FCritSect);
  inherited Destroy;
end;

function TEmulatedCaret.CreateCaret(AWidget: QWidgetH; Pixmap: QPixmapH;
  Width, Height: Integer): Boolean;
begin
  DestroyCaret;
  SetWidget(AWidget);
  FWidth := Width;
  FHeight := Height;
  if Cardinal(Pixmap) > $FFFF then
    FPixmap := QPixmap_create(Pixmap)
  else
    FPixmap := CreateColorPixmap(Integer(Pixmap));

  Result := IsValid;
end;

function TEmulatedCaret.DestroyCaret: Boolean;
begin
  Hide;
  if Assigned(FPixmap) then
    QPixmap_destroy(FPixmap);
  FWidget := nil;
  FPixmap := nil;
  FWidth := 0;
  FHeight := 0;
  Result := not IsValid;
end;

procedure TEmulatedCaret.DrawCaret;
var
  DestDev: QPaintDeviceH;
  R: TRect;
begin
  if IsValid then
  begin
    DestDev := QWidget_to_QPaintDevice(FWidget);
    R := Rect(0, 0, QPixmap_width(FPixmap), QPixmap_height(FPixmap));

    bitBlt(DestDev, @FPos, FPixmap, @R, RasterOp_XorROP);
    FShown := not FShown;
  end;
end;

function TEmulatedCaret.Show(AWidget: QWidgetH): Boolean;
begin
  if FWidget <> AWidget then
    Hide;
  SetWidget(AWidget);
  Result := IsValid;
  if Result then
  begin
    FVisible := True;
    FTimer.Enabled := True;
    DrawCaret;
  end;
end;

function TEmulatedCaret.Hide: Boolean;
begin
  Result := IsValid;
  if Result then
  begin
    FVisible := False;
    FTimer.Enabled := False;
    if FShown then
      DrawCaret;
    FShown := False;
  end;
end;

procedure TEmulatedCaret.SetPos(const Value: TPoint);
begin
  if FVisible then
  begin
    Hide;
    try
      FPos := Value;
    finally
      Show(FWidget);
    end;
  end
  else
    FPos := Value;
end;

procedure TEmulatedCaret.DoTimer(Sender: TObject);
begin
  DrawCaret;
end;

procedure TEmulatedCaret.Lock;
begin
  EnterCriticalSection(FCritSect);
end;

procedure TEmulatedCaret.Unlock;
begin
  LeaveCriticalSection(FCritSect);
end;

function TEmulatedCaret.CreateColorPixmap(Color: Cardinal): QPixmapH;
var
  C: QColorH;
begin
  if (FWidth <= 0) or (FHeight <= 0) then
    Result := nil
  else
  begin
    case Color of
      0: C := QColor(clWhite);
      1: C := QColor(clGray);
    else
      Result := nil;
      Exit;
    end;
    try
      Result := QPixmap_create(FWidth, FHeight, 32, QPixmapOptimization_MemoryOptim);
      try
        QPixmap_fill(Result, C);
      except
        QPixmap_destroy(Result);
        Result := nil;
      end;
    finally
      QColor_destroy(C);
    end;
  end;
end;

function TEmulatedCaret.IsValid: Boolean;
begin
  Result := (FWidget <> nil) and (FPixmap <> nil) and
            (QWidget_find(FWndId) <> nil);
end;

procedure TEmulatedCaret.SetWidget(AWidget: QWidgetH);
begin
  FWidget := AWidget;
  if FWidget <> nil then
    FWndId := QWidget_winId(FWidget)
  else
    FWndId := 0;
end;

{$ENDIF VisualCLX}


constructor TJvCaret.Create(Owner: TWinControl);
begin
  if not Assigned(Owner) then
    raise EJVCLException.CreateFmt(RsEInvalidCaretOwner, [ClassName]);
  inherited Create;
  FCaretOwner := Owner;
  FCaretBitmap := TBitmap.Create;
end;

destructor TJvCaret.Destroy;
begin
  DoDestroyCaret;
  FCaretBitmap.Free;
  inherited Destroy;
end;

procedure TJvCaret.Assign(Source: TPersistent);
begin
  if Source is TJvCaret then
  begin
    BeginUpdate;
    try
      FCaretWidth := TJvCaret(Source).Width;
      FCaretHeight := TJvCaret(Source).Height;
      FGrayCaret := TJvCaret(Source).Gray;
      Bitmap := TJvCaret(Source).Bitmap;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvCaret.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvCaret.Changed;
begin
  if Assigned(FOnChanged) and (FUpdateCount = 0) then
    FOnChanged(Self);
end;

function TJvCaret.UsingBitmap: Boolean;
begin
  Result := (Width = 0) and (Height = 0) and not Gray and not Bitmap.Empty;
end;

function TJvCaret.IsDefaultCaret: Boolean;
begin
  Result := (Width = 0) and (Height = 0) and not Gray and Bitmap.Empty;
end;

{$IFDEF VisualCLX}
type
  TOpenScrollingWidget = class(TScrollingWidget);
{$ENDIF VisualCLX}

procedure TJvCaret.DoCreateCaret;
const
  GrayHandles: array [Boolean] of THandle = (0, THandle(1));
var
  Handle: {$IFDEF VCL}HWND{$ENDIF}{$IFDEF VisualCLX}QWidgetH{$ENDIF};
begin
  if FCaretOwner.Focused and
    not (csDesigning in FCaretOwner.ComponentState) and not IsDefaultCaret then
  begin
  {$IFDEF VisualCLX}
    if FCaretOwner is TScrollingWidget then
      Handle := TOpenScrollingWidget(FCaretOwner).ViewportHandle
    else
      Handle := FCaretOwner.Handle;
  {$ENDIF}
    if UsingBitmap then
      OSCheck(CreateCaret(Handle, Bitmap.Handle, 0, 0))
    else
    { Gray carets seem to be unsupported on Win95 at least, so if the create
      failed for the gray caret, try again with a standard black caret }
    if not CreateCaret(Handle, GrayHandles[Gray], Width, Height) then
      OSCheck(CreateCaret(Handle, 0, Width, Height));
    FCaretCreated := True;
    ShowCaret(Handle);
  end;
end;

procedure TJvCaret.DoDestroyCaret;
begin
  if CaretCreated and FCaretOwner.Focused and not (csDesigning in FCaretOwner.ComponentState) and
    not IsDefaultCaret then
  begin
    if DestroyCaret then
      FCaretCreated := False;
  end;
end;

procedure TJvCaret.ReadBitmap(Stream: TStream);
begin
  FCaretBitmap.LoadFromStream(Stream);
end;

procedure TJvCaret.WriteBitmap(Stream: TStream);
begin
  FCaretBitmap.SaveToStream(Stream);
end;

procedure TJvCaret.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('CaretBitmap', ReadBitmap,
    WriteBitmap, not FCaretBitmap.Empty);
end;

procedure TJvCaret.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TJvCaret.SetCaretBitmap(const Value: TBitmap);
begin
  FCaretBitmap.Assign(Value);
  FCaretWidth := 0;
  FCaretHeight := 0;
  FGrayCaret := False;
  Changed;
end;

procedure TJvCaret.SetCaretHeight(const Value: Integer);
begin
  if FCaretHeight <> Value then
  begin
    FCaretHeight := Value;
    Changed;
  end;
end;

procedure TJvCaret.SetCaretWidth(const Value: Integer);
begin
  if FCaretWidth <> Value then
  begin
    FCaretWidth := Value;
    Changed;
  end;
end;

procedure TJvCaret.SetGrayCaret(const Value: Boolean);
begin
  if FGrayCaret <> Value then
  begin
    FGrayCaret := Value;
    Changed;
  end;
end;


{$IFDEF VisualCLX}
initialization
  GlobalCaret := TEmulatedCaret.Create;

finalization
  GlobalCaret.Free;
{$ENDIF VisualCLX}  

end.
