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
  Classes, SysUtils;

type
  // (rom) What is that for? Better go for TPersistent.
  {$M+}

  TAxisIndex = (axIndex0, axIndex1, axIndex2);
  TColorID = type Byte;
  TFullColor = type Cardinal;
  ShortNameString = string[3];

  TColorSpace = class(TObject)
  private
    FID: TColorID;
  protected
    function GetAxisName(Index: TAxisIndex): string; virtual;
    function GetAxisMin(Index: TAxisIndex): Byte; virtual;
    function GetAxisMax(Index: TAxisIndex): Byte; virtual;
    function GetName: string; virtual;
    function GetShortName: ShortNameString; virtual;
    function GetAxisDefault(Index: TAxisIndex): Byte; virtual;
  public
    constructor Create(ColorID: TColorID); reintroduce;
    function ConvertFromRGB(AColor: TFullColor): TFullColor; virtual;
    function ConvertToRGB(AColor: TFullColor): TFullColor; virtual;
    property ID: TColorID read FID;
    property Name: string read GetName;
    property ShortName: ShortNameString read GetShortName;
    property AxisName[Index: TAxisIndex]: string read GetAxisName;
    property AxisMin[Index: TAxisIndex]: Byte read GetAxisMin;
    property AxisMax[Index: TAxisIndex]: Byte read GetAxisMax;
    property AxisDefault[Index: TAxisIndex]: Byte read GetAxisDefault;
  end;

  TColorSpaceManager = class(TObject)
  private
    FColorSpaceList: TList;
    FPredefinedColors: TStringList;
    function GetColorSpaceCount: Integer;
    function GetColorSpaceIndex(Index: Integer): TColorSpace;
    procedure AddPredefinedColor(const S: string);
    function GetPredefinedColorCount: Integer;
    function GetPredefinedColorIndex(Index: Integer): string;
  protected
    function GetColorSpace(ID: TColorID): TColorSpace; virtual;
  public
    procedure RegisterColorSpace(NewColorSpace: TColorSpace);
    constructor Create; reintroduce;
    destructor Destroy; override;
    function ConvertToID(AColor: TFullColor; NewID: TColorID): TFullColor;
    function GetColorID(AColor: TFullColor): TColorID;

    property ColorSpace[ID: TColorID]: TColorSpace read GetColorSpace;
    property ColorSpaceIndex[Index: Integer]: TColorSpace read GetColorSpaceIndex;
    property ColorSpaceCount: Integer read GetColorSpaceCount;

    property PredefinedColorIndex[Index: Integer]: string read GetPredefinedColorIndex;
    property PredefinedColorCount: Integer read GetPredefinedColorCount;
  end;

  EColorSpaceError = class(Exception);

  {$M-}

function ColorSpaceManager: TColorSpaceManager;
function GetAxisValue(AColor: TFullColor; AAxis: TAxisIndex): Byte;
function SetAxisValue(AColor: TFullColor; AAxis: TAxisIndex; NewValue: Byte): TFullColor;

const
  csRGB = TColorID(0);
  csPredefined = TColorID(128);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Graphics;

var
  GlobalColorSpaceManager: TColorSpaceManager = nil;

resourcestring
  RsUnnamedColorAxis = 'Unnamed Color Axis';
  RsUnnamedColorSpace = 'Unnamed Color Space';
  RsUCS = 'UCS';
  RsEColorSpaceNotFound = 'Color Space not found: %d';
  RsEColorSpaceAlreadyExists = 'Color Space Already exists [ID: %d, Name: %s]';

function ColorSpaceManager: TColorSpaceManager;
begin
  if GlobalColorSpaceManager = nil then
    GlobalColorSpaceManager := TColorSpaceManager.Create;
  Result := GlobalColorSpaceManager;
end;

function SetAxisValue(AColor: TFullColor; AAxis: TAxisIndex;
  NewValue: Byte): TFullColor;
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

function GetAxisValue(AColor: TFullColor; AAxis: TAxisIndex): Byte;
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

//=== { TColorSpace } ========================================================

function TColorSpace.ConvertFromRGB(AColor: TFullColor): TFullColor;
begin
  Result := (AColor and $00FFFFFF) or (ID shl 24);
end;

function TColorSpace.ConvertToRGB(AColor: TFullColor): TFullColor;
begin
  Result := (AColor and $00FFFFFF) or (csRGB shl 24);
end;

constructor TColorSpace.Create(ColorID: TColorID);
begin
  inherited Create;
  FID := ColorID;
end;

function TColorSpace.GetAxisDefault(Index: TAxisIndex): Byte;
begin
  Result := Low(Byte);
end;

function TColorSpace.GetAxisMax(Index: TAxisIndex): Byte;
begin
  Result := High(Byte);
end;

function TColorSpace.GetAxisMin(Index: TAxisIndex): Byte;
begin
  Result := Low(Byte);
end;

function TColorSpace.GetAxisName(Index: TAxisIndex): string;
begin
  Result := RsUnnamedColorAxis;
end;

function TColorSpace.GetName: string;
begin
  Result := RsUnnamedColorSpace;
end;

function TColorSpace.GetShortName: ShortNameString;
begin
  Result := RsUCS;
end;

//=== { TColorSpaceManager } =================================================

constructor TColorSpaceManager.Create;
begin
  inherited Create;
  FColorSpaceList := TList.Create;
  FPredefinedColors := TStringList.Create;
  GetColorValues(AddPredefinedColor);
end;

destructor TColorSpaceManager.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
    TColorSpace(FColorSpaceList.Items[Index]).Free;
  FColorSpaceList.Free;
  FPredefinedColors.Free;
  inherited Destroy;
end;

procedure TColorSpaceManager.AddPredefinedColor(const S: string);
begin
  FPredefinedColors.Add(S);
end;

function TColorSpaceManager.ConvertToID(AColor: TFullColor; NewID: TColorID): TFullColor;
var
  LColorID: TColorID;
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

function TColorSpaceManager.GetColorID(AColor: TFullColor): TColorID;
var
  Index: Integer;
begin
  Result := TColorID(AColor shr 24);
  for Index := 0 to ColorSpaceCount - 1 do
    if ColorSpaceIndex[Index].ID = Result then
      Exit;
  Result := csPredefined;
end;

function TColorSpaceManager.GetColorSpace(ID: TColorID): TColorSpace;
var
  Index: Integer;
  LColorSpace: TColorSpace;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
  begin
    LColorSpace := TColorSpace(FColorSpaceList.Items[Index]);
    if LColorSpace.ID = ID then
    begin
      Result := LColorSpace;
      Exit;
    end;
  end;
  raise EColorSpaceError.CreateFmt(RsEColorSpaceNotFound, [ID]);
end;

function TColorSpaceManager.GetColorSpaceCount: Integer;
begin
  Result := FColorSpaceList.Count;
end;

function TColorSpaceManager.GetColorSpaceIndex(Index: Integer): TColorSpace;
begin
  Result := TColorSpace(FColorSpaceList.Items[Index]);
end;

function TColorSpaceManager.GetPredefinedColorCount: Integer;
begin
  Result := FPredefinedColors.Count;
end;

function TColorSpaceManager.GetPredefinedColorIndex(Index: Integer): string;
begin
  Result := FPredefinedColors.Strings[Index];
end;

procedure TColorSpaceManager.RegisterColorSpace(NewColorSpace: TColorSpace);
var
  Index: Integer;
  LColorSpace: TColorSpace;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
  begin
    LColorSpace := TColorSpace(FColorSpaceList.Items[Index]);
    if LColorSpace.ID = NewColorSpace.ID then
      with LColorSpace do
      begin
        EColorSpaceError.CreateFmt(RsEColorSpaceAlreadyExists, [ID, Name]);
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

