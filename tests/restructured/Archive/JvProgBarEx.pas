{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgBarEx.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Extended progressbar. Has smooth and vertical property. Also handles 32-bit
  integer values for the Min, Max and Position properties. Also includes a DB
  (read-only) version. New version has COlor and BarColor properties }

unit JvProgBarEx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, DB, DBCtrls, JVCLVer;

type
  PPBRange = ^TPBRange;
  TPBRange = packed record
    iLow: integer;
    iHigh: integer;
  end;

const
  PBM_SETRANGE32 = WM_USER + 6; // lParam = high, wParam = low
  PBM_GETRANGE = WM_USER + 7; // wParam = return (TRUE ? low : high). lParam = PPBRANGE or NULL
  PBM_GETPOS = WM_USER + 8;
  PBS_SMOOTH = $01;
  PBS_VERTICAL = $04;
  PBM_SETBARCOLOR = WM_USER + 9; // lParam = bar color
  PBM_SETBKCOLOR = $2001; // lParam = bkColor

type
  TJvProgressBar2 = class(TProgressBar)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FPosition: integer;
    FMin: integer;
    FMax: integer;
    FVertical: boolean;
    FSmooth: boolean;
    FBarColor: TColor;
    procedure SetVertical(Value: boolean);
    procedure SetSmooth(Value: boolean);
    procedure SetBarColor(Value: TColor);
    procedure SetMin(Val: integer);
    procedure SetMax(Val: integer);
    procedure SetPosition(Val: integer);
    function GetPosition: integer;
    procedure SetParams(AMin, AMax: integer);
    procedure RecreateAndSave;
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property BarColor: TColor read FBarColor write SetBarColor default clActiveCaption;
    property Color;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Position: integer read GetPosition write SetPosition default 0;
    property ParentColor;
    property Smooth: boolean read FSmooth write SetSmooth default false;
    property Vertical: boolean read FVertical write SetVertical default false;
  end;

  TJvDBProgressBar = class(TJvProgressBar)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    procedure SetDataField(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetField: TField;
  public
    procedure DataChange(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

uses
  Consts, CommCtrl;

{ TJvProgressBar2 }

constructor TJvProgressBar2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 100;
  FVertical := false;
  FSmooth := False;
  FBarColor := clActiveCaption;
  FPosition := 0;
end;

procedure TJvProgressBar2.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, PBM_SETRANGE32, FMin, FMax);
  SendMessage(Handle, PBM_SETPOS, FPosition, 0);
  SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FBarColor));
  //  SendMessage(Handle,PBM_SETBKCOLOR,0,ColorToRGB(FColor));
end;

procedure TJvProgressBar2.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FVertical then
    Params.Style := Params.Style or PBS_VERTICAL;
  if FSmooth then
    Params.Style := Params.Style or PBS_SMOOTH;
end;

procedure TJvProgressBar2.Loaded;
var
  Temp: integer;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) and FVertical then
  begin
    Temp := Width;
    Width := Height;
    Height := Temp;
  end;
end;

procedure TJvProgressBar2.RecreateAndSave;
var tmp: integer;
begin
  tmp := Position;
  RecreateWnd;
  Position := tmp;
end;

procedure TJvProgressBar2.SetVertical(Value: boolean);
begin
  if FVertical <> Value then
  begin
    FVertical := Value;
    SetBounds(Left, Top, Height, Width);
    RecreateAndSave;
  end;
end;

procedure TJvProgressBar2.SetSmooth(Value: boolean);
begin
  if FSmooth <> Value then
  begin
    FSmooth := Value;
    RecreateAndSave;
  end;
end;

procedure TJvProgressBar2.SetBarColor(Value: TColor);
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    RecreateAndSave;
  end;
end;

procedure TJvProgressBar2.SetParams(AMin, AMax: integer);
begin
  if AMax < AMin then
    raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
  if (FMin <> AMin) or (FMax <> AMax) then
  begin
    if HandleAllocated then
    begin
      SendMessage(Handle, PBM_SETRANGE32, AMin, AMax);
      if FMin > AMin then // since Windows sets Position when increase Min..
        SendMessage(Handle, PBM_SETPOS, AMin, 0); // set it back if decrease
    end;
    FMin := AMin;
    FMax := AMax;
  end;
end;

procedure TJvProgressBar2.SetMin(Val: integer);
begin
  SetParams(Val, FMax);
end;

procedure TJvProgressBar2.SetMax(Val: integer);
begin
  SetParams(FMin, Val);
end;

function TJvProgressBar2.GetPosition: integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, PBM_GETPOS, 0, 0)
  else
    Result := FPosition;
end;

procedure TJvProgressBar2.SetPosition(Val: integer);
begin
  if HandleAllocated then
    SendMessage(Handle, PBM_SETPOS, Val, 0);
  FPosition := Val;
end;

{ TJvDBProgressBar }

constructor TJvDBProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TJvDBProgressBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TJvDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBProgressBar.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TJvDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBProgressBar.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TJvDBProgressBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBProgressBar.DataChange(Sender: TObject);
begin
  if (FDataLink.Field <> nil) and (FDataLink.Field is TNumericField) then
    Position := FDataLink.Field.AsInteger
  else
    Position := Min;
end;

end.

