{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorSpaces.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorSpaces;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils,
  JvTypes;

type
  // (rom) What is that for? Better go for TPersistent.
  {$M+}

  TJvAxisIndex = (axIndex0, axIndex1, axIndex2);
  TJvColorID = type Byte;
  TJvFullColor = type Cardinal;
  TJvShortNameString = string[3];

  TJvColorSpace = class(TObject)
  private
    FID: TJvColorID;
  protected
    function GetAxisName(Index: TJvAxisIndex): string; virtual;
    function GetAxisMin(Index: TJvAxisIndex): Byte; virtual;
    function GetAxisMax(Index: TJvAxisIndex): Byte; virtual;
    function GetName: string; virtual;
    function GetShortName: TJvShortNameString; virtual;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; virtual;
  public
    constructor Create(ColorID: TJvColorID); reintroduce;
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; virtual;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; virtual;
    property ID: TJvColorID read FID;
    property Name: string read GetName;
    property ShortName: TJvShortNameString read GetShortName;
    property AxisName[Index: TJvAxisIndex]: string read GetAxisName;
    property AxisMin[Index: TJvAxisIndex]: Byte read GetAxisMin;
    property AxisMax[Index: TJvAxisIndex]: Byte read GetAxisMax;
    property AxisDefault[Index: TJvAxisIndex]: Byte read GetAxisDefault;
  end;

  TJvColorSpaceManager = class(TObject)
  private
    FColorSpaceList: TList;
    FPredefinedColors: TStringList;
    function GetColorSpaceCount: Integer;
    function GetColorSpaceIndex(Index: Integer): TJvColorSpace;
    procedure AddPredefinedColor(const S: string);
    function GetPredefinedColorCount: Integer;
    function GetPredefinedColorIndex(Index: Integer): string;
  protected
    function GetColorSpace(ID: TJvColorID): TJvColorSpace; virtual;
  public
    procedure RegisterColorSpace(NewColorSpace: TJvColorSpace);
    constructor Create; reintroduce;
    destructor Destroy; override;
    function ConvertToID(AColor: TJvFullColor; NewID: TJvColorID): TJvFullColor;
    function GetColorID(AColor: TJvFullColor): TJvColorID;

    property ColorSpace[ID: TJvColorID]: TJvColorSpace read GetColorSpace;
    property ColorSpaceIndex[Index: Integer]: TJvColorSpace read GetColorSpaceIndex;
    property ColorSpaceCount: Integer read GetColorSpaceCount;

    property PredefinedColorIndex[Index: Integer]: string read GetPredefinedColorIndex;
    property PredefinedColorCount: Integer read GetPredefinedColorCount;
  end;

  EJvColorSpaceError = class(EJVCLException);

  {$M-}

function ColorSpaceManager: TJvColorSpaceManager;
function GetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex): Byte;
function SetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex; NewValue: Byte): TJvFullColor;

const
  csRGB = TJvColorID(0);
  csPredefined = TJvColorID(128);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Graphics;

var
  GlobalColorSpaceManager: TJvColorSpaceManager = nil;

resourcestring
  RsUnnamedColorAxis = 'Unnamed Color Axis';
  RsUnnamedColorSpace = 'Unnamed Color Space';
  RsUCS = 'UCS';
  RsEColorSpaceNotFound = 'Color Space not found: %d';
  RsEColorSpaceAlreadyExists = 'Color Space Already exists [ID: %d, Name: %s]';

function ColorSpaceManager: TJvColorSpaceManager;
begin
  if GlobalColorSpaceManager = nil then
    GlobalColorSpaceManager := TJvColorSpaceManager.Create;
  Result := GlobalColorSpaceManager;
end;

function SetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex;
  NewValue: Byte): TJvFullColor;
begin
  case AAxis of
    axIndex0:
      AColor := (AColor and $FFFFFF00) or (NewValue shl 0);
    axIndex1:
      AColor := (AColor and $FFFF00FF) or (NewValue shl 8);
    axIndex2:
      AColor := (AColor and $FF00FFFF) or (NewValue shl 16);
  end;
  Result := AColor;
end;

function GetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex): Byte;
begin
  case AAxis of
    axIndex0:
      Result := (AColor and $000000FF) shr 0;
    axIndex1:
      Result := (AColor and $0000FF00) shr 8;
    axIndex2:
      Result := (AColor and $00FF0000) shr 16;
  else
    Result := 0;
  end;
end;

//=== { TJvColorSpace } ========================================================

function TJvColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := (AColor and $00FFFFFF) or (ID shl 24);
end;

function TJvColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := (AColor and $00FFFFFF) or (csRGB shl 24);
end;

constructor TJvColorSpace.Create(ColorID: TJvColorID);
begin
  inherited Create;
  FID := ColorID;
end;

function TJvColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := Low(Byte);
end;

function TJvColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := High(Byte);
end;

function TJvColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := Low(Byte);
end;

function TJvColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  Result := RsUnnamedColorAxis;
end;

function TJvColorSpace.GetName: string;
begin
  Result := RsUnnamedColorSpace;
end;

function TJvColorSpace.GetShortName: TJvShortNameString;
begin
  Result := RsUCS;
end;

//=== { TJvColorSpaceManager } =================================================

constructor TJvColorSpaceManager.Create;
begin
  inherited Create;
  FColorSpaceList := TList.Create;
  FPredefinedColors := TStringList.Create;
  GetColorValues(AddPredefinedColor);
end;

destructor TJvColorSpaceManager.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
    TJvColorSpace(FColorSpaceList.Items[Index]).Free;
  FColorSpaceList.Free;
  FPredefinedColors.Free;
  inherited Destroy;
end;

procedure TJvColorSpaceManager.AddPredefinedColor(const S: string);
begin
  FPredefinedColors.Add(S);
end;

function TJvColorSpaceManager.ConvertToID(AColor: TJvFullColor; NewID: TJvColorID): TJvFullColor;
var
  LColorID: TJvColorID;
begin
  LColorID := GetColorID(AColor);
  if LColorID = NewID then
    Result := AColor
  else
  begin
    if LColorID <> csRGB then
      AColor := ColorSpace[LColorID].ConvertToRGB(AColor);
    Result := ColorSpace[NewID].ConvertFromRGB(AColor);
  end;
end;

function TJvColorSpaceManager.GetColorID(AColor: TJvFullColor): TJvColorID;
var
  Index: Integer;
begin
  Result := TJvColorID(AColor shr 24);
  for Index := 0 to ColorSpaceCount - 1 do
    if ColorSpaceIndex[Index].ID = Result then
      Exit;
  Result := csPredefined;
end;

function TJvColorSpaceManager.GetColorSpace(ID: TJvColorID): TJvColorSpace;
var
  Index: Integer;
  LColorSpace: TJvColorSpace;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
  begin
    LColorSpace := TJvColorSpace(FColorSpaceList.Items[Index]);
    if LColorSpace.ID = ID then
    begin
      Result := LColorSpace;
      Exit;
    end;
  end;
  raise EJvColorSpaceError.CreateFmt(RsEColorSpaceNotFound, [ID]);
end;

function TJvColorSpaceManager.GetColorSpaceCount: Integer;
begin
  Result := FColorSpaceList.Count;
end;

function TJvColorSpaceManager.GetColorSpaceIndex(Index: Integer): TJvColorSpace;
begin
  Result := TJvColorSpace(FColorSpaceList.Items[Index]);
end;

function TJvColorSpaceManager.GetPredefinedColorCount: Integer;
begin
  Result := FPredefinedColors.Count;
end;

function TJvColorSpaceManager.GetPredefinedColorIndex(Index: Integer): string;
begin
  Result := FPredefinedColors.Strings[Index];
end;

procedure TJvColorSpaceManager.RegisterColorSpace(NewColorSpace: TJvColorSpace);
var
  Index: Integer;
  LColorSpace: TJvColorSpace;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
  begin
    LColorSpace := TJvColorSpace(FColorSpaceList.Items[Index]);
    if LColorSpace.ID = NewColorSpace.ID then
      with LColorSpace do
      begin
        EJvColorSpaceError.CreateFmt(RsEColorSpaceAlreadyExists, [ID, Name]);
        Exit;
      end;
  end;
  FColorSpaceList.Add(Pointer(NewColorSpace));
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GlobalColorSpaceManager);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

