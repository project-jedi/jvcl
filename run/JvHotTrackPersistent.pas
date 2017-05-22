{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvButtonPersistent.PAS, released on 2007-11-20.

The Initial Developer of the Original Code is dejoy den [dejoybbs att gmail dott com]
All Rights Reserved.

Contributor(s): dejoy.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvHotTrackPersistent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Graphics,
  JvTypes;

type
  TJvHotTrackOptionsClass = class of TJvHotTrackOptions;

  TJvHotTrackOptions = class(TJvPersistentProperty)
  private
    FEnabled: Boolean;
    FFrameVisible: Boolean;
    FColor: TColor;
    FFrameColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetEnabled(Value: Boolean);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameVisible(Value: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Color: TColor read FColor write SetColor default DefaultHotTrackColor;
    property FrameVisible: Boolean read FFrameVisible write SetFrameVisible default False;
    property FrameColor: TColor read FFrameColor write SetFrameColor default DefaultHotTrackFrameColor;
  end;

  { IJvHotTrack specifies whether Controls are highlighted when the mouse passes over them }
  IJvHotTrack = interface
    ['{8F1B40FB-D8E3-46FE-A7A3-21CE4B199A8F}']

    function GetHotTrack: Boolean;
    function GetHotTrackFont: TFont;
    function GetHotTrackFontOptions: TJvTrackFontOptions;
    function GetHotTrackOptions: TJvHotTrackOptions;

    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetHotTrackFontOptions(Value: TJvTrackFontOptions);
    procedure SetHotTrackOptions(Value: TJvHotTrackOptions);
    procedure Assign(Source: IJvHotTrack);

    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read GetHotTrackFontOptions write SetHotTrackFontOptions;
    property HotTrackOptions: TJvHotTrackOptions read GetHotTrackOptions write SetHotTrackOptions;
  end;

  TJvCustomHotTrackPersistent = class(TJvPersistentProperty, IJvHotTrack)
  private
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FHotTrackOptions:TJvHotTrackOptions;

    {IJvHotTrack}
    function GetHotTrack: Boolean;
    function GetHotTrackFont: TFont;
    function GetHotTrackFontOptions: TJvTrackFontOptions;
    function GetHotTrackOptions: TJvHotTrackOptions;
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetHotTrackFontOptions(Value: TJvTrackFontOptions);
    procedure SetHotTrackOptions(Value: TJvHotTrackOptions);

    procedure IJvHotTrack_Assign(Source: IJvHotTrack);
    procedure IJvHotTrack.Assign = IJvHotTrack_Assign;
  protected
    class function GetHotTrackOptionsClass: TJvHotTrackOptionsClass; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read GetHotTrackFontOptions write SetHotTrackFontOptions
      default DefaultTrackFontOptions;
    property HotTrackOptions: TJvHotTrackOptions read GetHotTrackOptions write SetHotTrackOptions;
  end;

  TJvHotTrackPersistent  = class(TJvCustomHotTrackPersistent)
  published
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    property HotTrackOptions;
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
  SysUtils;

//=== { TJvHotTrackOptions } ======================================

constructor TJvHotTrackOptions.Create(AOwner: TPersistent);
begin
  inherited ;
  FEnabled := False;
  FFrameVisible := False;
  FColor := DefaultHotTrackColor;
  FFrameColor := DefaultHotTrackFrameColor;
end;

procedure TJvHotTrackOptions.Assign(Source: TPersistent);
begin
  if Source is TJvHotTrackOptions then
  begin
    BeginUpdate;
    try
      Enabled := TJvHotTrackOptions(Source).Enabled;
      Color := TJvHotTrackOptions(Source).Color;
      FrameVisible := TJvHotTrackOptions(Source).FrameVisible;
      FrameColor := TJvHotTrackOptions(Source).FrameColor;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvHotTrackOptions.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    Changing;
    ChangingProperty('Color');
    FColor := Value;
    ChangedProperty('Color');
    Changed;
  end;
end;

procedure TJvHotTrackOptions.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    Changing;
    ChangingProperty('Enabled');
    FEnabled := Value;
    ChangedProperty('Enabled');
    Changed;
  end;
end;

procedure TJvHotTrackOptions.SetFrameVisible(Value: Boolean);
begin
  if FFrameVisible <> Value then
  begin
    Changing;
    ChangingProperty('FrameVisible');
    FFrameVisible := Value;
    ChangedProperty('FrameVisible');
    Changed;
  end;
end;

procedure TJvHotTrackOptions.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    Changing;
    ChangingProperty('FrameColor');
    FFrameColor := Value;
    ChangedProperty('FrameColor');
    Changed;
  end;
end;

//============================================================================

{ TJvCustomHotTrackPersistent }

constructor TJvCustomHotTrackPersistent.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FHotTrackOptions :=GetHotTrackOptionsClass.Create(Self);
end;

destructor TJvCustomHotTrackPersistent.Destroy;
begin
  FHotTrackFont.Free;
  FHotTrackOptions.Free;
  inherited Destroy;
end;

class function TJvCustomHotTrackPersistent.GetHotTrackOptionsClass: TJvHotTrackOptionsClass;
begin
  Result := TJvHotTrackOptions;
end;

procedure TJvCustomHotTrackPersistent.Assign(Source: TPersistent);
var
  Intf: IJvHotTrack;
begin
  if Supports(Source, IJvHotTrack, Intf) then
    IJvHotTrack(Self).Assign(Intf)
  else
    inherited Assign(Source);
end;

procedure TJvCustomHotTrackPersistent.AssignTo(Dest: TPersistent);
var
  Intf: IJvHotTrack;
begin
  if Supports(Dest, IJvHotTrack, Intf) then
    Intf.Assign(Self)
  else
    inherited AssignTo(Dest);
end;

procedure TJvCustomHotTrackPersistent.SetHotTrackFont(Value: TFont);
begin
  if (FHotTrackFont<>Value) and (Value <> nil) then
  begin
    Changing;
    ChangingProperty('HotTrackFont');
    FHotTrackFont.Assign(Value);
    ChangedProperty('HotTrackFont');
    Changed;
  end;
end;

procedure TJvCustomHotTrackPersistent.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    Changing;
    ChangingProperty('HotTrack');
    FHotTrack := Value;
    ChangedProperty('HotTrack');
    Changed;
  end;
end;

procedure TJvCustomHotTrackPersistent.SetHotTrackFontOptions(Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    Changing;
    ChangingProperty('HotTrackFontOptions');
    FHotTrackFontOptions := Value;
    ChangedProperty('HotTrackFontOptions');
    Changed;
  end;
end;

function TJvCustomHotTrackPersistent.GetHotTrack: Boolean;
begin
  Result := FHotTrack;
end;

function TJvCustomHotTrackPersistent.GetHotTrackFont: TFont;
begin
  Result := FHotTrackFont;
end;

function TJvCustomHotTrackPersistent.GetHotTrackFontOptions: TJvTrackFontOptions;
begin
  Result := FHotTrackFontOptions;
end;

function TJvCustomHotTrackPersistent.GetHotTrackOptions: TJvHotTrackOptions;
begin
  Result := FHotTrackOptions;
end;

procedure TJvCustomHotTrackPersistent.SetHotTrackOptions(Value: TJvHotTrackOptions);
begin
  if (FHotTrackOptions <> Value) and (Value <> nil) then
  begin
    Changing;
    ChangingProperty('HotTrackOptions');
    FHotTrackOptions.Assign(Value);
    ChangedProperty('HotTrackOptions');
    Changed;
  end;
end;

procedure TJvCustomHotTrackPersistent.IJvHotTrack_Assign(Source: IJvHotTrack);
begin
  if (Source <> nil) and (IJvHotTrack(Self) <> Source) then
  begin
    BeginUpdate;
    try
      HotTrack := Source.HotTrack;
      HotTrackFont := Source.HotTrackFont;
      HotTrackFontOptions := Source.HotTrackFontOptions;
      HotTrackOptions := Source.HotTrackOptions;
    finally
      EndUpdate;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
