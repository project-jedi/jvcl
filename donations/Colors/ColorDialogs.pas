{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorDialogs.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorDialogs;

{$I jvcl.inc}

interface

uses
  Classes, Graphics, Forms,
  ColorSpaces, ColorRotate;

type
  TFullColorDialogOption =
   (foFullOpen, foPreventExpand, foShowHelp,
    foAllowSpaceChange, foConvertToOriginalSpace,
    foShowNewPreview, foShowOldPreview,
    foShowPredefined, foAllowVariable,
    foNoneAndDefault, foShowApply);
  TFullColorDialogOptions = set of TFullColorDialogOption;

  TColorCircleDialogOption =
   (roFullOpen, roPreventExpand,
    roShowHelp, roAllowSpaceChange,
    roShowNewPreview, roShowOldPreview,
    roCommon, roRed, roGreen, roBlue,
    roShowSaturation, roDefaultRange,
    roShowApply);
  TColorCircleDialogOptions = set of TColorCircleDialogOption;

const
  DefaultFullColorDialogOptions =
   [foFullOpen, foAllowSpaceChange, foAllowVariable,
    foShowNewPreview, foShowOldPreview, foShowPredefined, foShowApply];

  DefaultColorCircleDialogOptions =
   [roFullOpen, roAllowSpaceChange,
    roShowNewPreview, roShowOldPreview,
    roCommon, roRed, roGreen, roBlue,
    roShowSaturation, roShowApply];

type
  TAxisType = (atCommon, atRed, atGreen, atBlue);

  TFullColorEvent = procedure(Sender: TObject; AFullColor: TFullColor) of object;

  TColorCircleEvent = procedure(Sender: TObject; AxisType: TAxisType;
    NewFullColor: TFullColor) of object;

  TFullColorDialog = class;
  TColorCircleDialog = class;
  TBaseFullColorForm = class;
  TBaseColorCircleForm = class;

  TFullColorDialog = class(TComponent)
  private
    FOptions: TFullColorDialogOptions;
    FTitle: string;
    FFullColor: TFullColor;
    FOnApply: TFullColorEvent;
    FForm: TBaseFullColorForm;
    FOnCloseQuery: TCloseQueryEvent;
    FOnShow: TNotifyEvent;
    FHelpContext: THelpContext;
    FOldColorSpace: TColorID;
    function GetFullColor: TFullColor;
    procedure SetFullColor(const Value: TFullColor);
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetOptions(const Value: TFullColorDialogOptions);
    procedure SetTitle(const Value: string);
    function GetColor: TColor;
  protected
    procedure FormApply(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    property OldColorSpace: TColorID read FOldColorSpace;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property Form: TBaseFullColorForm read FForm;
    property Color: TColor read GetColor;
  published
    // (rom) set default values
    property FullColor: TFullColor read GetFullColor write SetFullColor;
    property Options: TFullColorDialogOptions read FOptions write SetOptions default DefaultFullColorDialogOptions;
    property Title: string read FTitle write SetTitle;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext;
    property OnApply: TFullColorEvent read FOnApply write FOnApply;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

  TColorCircleDialog = class(TComponent)
  private
    FTitle: string;
    FForm: TBaseColorCircleForm;
    FHelpContext: THelpContext;
    FOnCloseQuery: TCloseQueryEvent;
    FOnShow: TNotifyEvent;
    FOptions: TColorCircleDialogOptions;
    FOnApply: TColorCircleEvent;
    FDelta: TColorDelta;
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetOptions(const Value: TColorCircleDialogOptions);
    procedure SetTitle(const Value: string);
    function GetDelta(const Index: TRotateColor): TAxisDelta;
    procedure SetDelta(const Index: TRotateColor; const Value: TAxisDelta);
  protected
    procedure FormApply(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property Form: TBaseColorCircleForm read FForm;
    property RedDelta: TAxisDelta index rcRed read GetDelta write SetDelta;
    property GreenDelta: TAxisDelta index rcGreen read GetDelta write SetDelta;
    property BlueDelta: TAxisDelta index rcBlue read GetDelta write SetDelta;
    property Delta: TColorDelta read FDelta;
  published
    // (rom) set default values
    property Options: TColorCircleDialogOptions read FOptions write SetOptions default DefaultColorCircleDialogOptions;
    property Title: string read FTitle write SetTitle;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext;
    property OnApply: TColorCircleEvent read FOnApply write FOnApply;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

  TBaseFullColorForm = class(TForm)
  private
    FFullColor: TFullColor;
    FOptions: TFullColorDialogOptions;
    FOnApply: TNotifyEvent;
  protected
    procedure SetOptions(const Value: TFullColorDialogOptions); virtual;
    procedure SetFullColor(const Value: TFullColor); virtual;
  public
    constructor Create(AOwner: TComponent; AFullColor: TFullColor;
      AOptions: TFullColorDialogOptions); reintroduce;
    property Options: TFullColorDialogOptions read FOptions write SetOptions;
    property FullColor: TFullColor read FFullColor write SetFullColor;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

  TFullColorFormClass = class of TBaseFullColorForm;

  TBaseColorCircleForm = class(TForm)
  private
    FDelta: TColorDelta;
    FOnApply: TNotifyEvent;
    FOptions: TColorCircleDialogOptions;
  protected
    procedure SetOptions(const Value: TColorCircleDialogOptions); virtual;
    procedure SetRedDelta(const Value: TAxisDelta); virtual;
    procedure SetGreenDelta(const Value: TAxisDelta); virtual;
    procedure SetBlueDelta(const Value: TAxisDelta); virtual;
    procedure SetDelta(const Value: TColorDelta); virtual;
  public
    property Options: TColorCircleDialogOptions read FOptions write SetOptions;
    property RedDelta: TAxisDelta read FDelta.AxisRed write SetRedDelta;
    property GreenDelta: TAxisDelta read FDelta.AxisGreen write SetGreenDelta;
    property BlueDelta: TAxisDelta read FDelta.AxisBlue write SetBlueDelta;
    property Delta: TColorDelta read FDelta write SetDelta;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

  TColorCircleBaseFormClass = class of TBaseColorCircleForm;

var
  FullColorFormClass: TFullColorFormClass;
  ColorCircleBaseFormClass: TColorCircleBaseFormClass;

resourcestring
  RsExpandedCaption = '<< &Hide';
  RsCollapsedCaption = '&Panels >>';

procedure Register;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Controls, SysUtils;

//=== { TFullColorDialog } ===================================================

constructor TFullColorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := DefaultFullColorDialogOptions;
end;

function TFullColorDialog.Execute: Boolean;
var
  NewColor: TFullColor;
begin
  Result := False;
  if (Form <> nil) or (FullColorFormClass = nil) then
    Exit;

  FOldColorSpace := ColorSpaceManager.GetColorID(FullColor);

  FForm := FullColorFormClass.Create(Application, FullColor, Options);
  with Form do
  begin
    if Title <> '' then
      Caption := Title;
    OnApply := FormApply;
    OnClose := FormClose;
    OnShow := FormShow;
    HelpContext := Self.HelpContext;

    Result := (ShowModal = mrOk);

    NewColor := FullColor;
  end;
  FreeAndNil(FForm);

  with ColorSpaceManager do
    if foConvertToOriginalSpace in Options then
      NewColor := ConvertToID(NewColor, OldColorSpace);
  FFullColor := NewColor;
end;

procedure TFullColorDialog.FormApply(Sender: TObject);
var
  Color: TFullColor;
begin
  if Assigned(FForm) then
  begin
    Color := Form.FullColor;
    if foConvertToOriginalSpace in Options then
      Color := ColorSpaceManager.ConvertToID(Color, OldColorSpace);
    if Assigned(FOnApply) then
      FOnApply(Self, Color);
  end;
end;

procedure TFullColorDialog.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Allow: Boolean;
begin
  Allow := True;

  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, Allow);

  if Allow then
    Action := caFree
  else
    Action := caNone;
end;

procedure TFullColorDialog.FormShow(Sender: TObject);
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TFullColorDialog.GetColor: TColor;
var
  ColorID: TColorID;
begin
  with ColorSpaceManager do
  begin
    ColorID := GetColorID(FullColor);
    if ColorID in [csRGB, csPredefined] then
      Result := FullColor
    else
      Result := ConvertToID(FullColor, csRGB);
  end;
end;

function TFullColorDialog.GetFullColor: TFullColor;
begin
  if Form <> nil then
    FFullColor := Form.FullColor;
  Result := FFullColor;
end;

procedure TFullColorDialog.SetFullColor(const Value: TFullColor);
begin
  FFullColor := Value;
  if Form <> nil then
    Form.FullColor := Value;
end;

procedure TFullColorDialog.SetHelpContext(const Value: THelpContext);
begin
  if FHelpContext <> Value then
  begin
    FHelpContext := Value;
    if Assigned(FForm) then
      Form.HelpContext := Value;
  end;
end;

procedure TFullColorDialog.SetOptions(const Value: TFullColorDialogOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if Assigned(FForm) then
      Form.Options := Value;
  end;
end;

procedure TFullColorDialog.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    if Assigned(FForm) then
      Form.Caption := Value;
  end;
end;

//=== { TBaseFullColorForm } =================================================

constructor TBaseFullColorForm.Create(AOwner: TComponent;
  AFullColor: TFullColor; AOptions: TFullColorDialogOptions);
begin
  inherited Create(AOwner);
  FOptions := AOptions;
  FFullColor := AFullColor;
end;

procedure TBaseFullColorForm.SetFullColor(const Value: TFullColor);
begin
  FFullColor := Value;
end;

procedure TBaseFullColorForm.SetOptions(const Value: TFullColorDialogOptions);
begin
  FOptions := Value;
end;

//=== { TBaseColorCircleForm } ===============================================

procedure TBaseColorCircleForm.SetBlueDelta(const Value: TAxisDelta);
begin
  FDelta.AxisBlue := Value;
end;

procedure TBaseColorCircleForm.SetDelta(const Value: TColorDelta);
begin
  FDelta := Value;
end;

procedure TBaseColorCircleForm.SetGreenDelta(const Value: TAxisDelta);
begin
  FDelta.AxisGreen := Value;
end;

procedure TBaseColorCircleForm.SetOptions(const Value: TColorCircleDialogOptions);
begin
  FOptions := Value;
end;

procedure TBaseColorCircleForm.SetRedDelta(const Value: TAxisDelta);
begin
  FDelta.AxisRed := Value;
end;

//=== { TColorCircleDialog } =================================================

constructor TColorCircleDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := DefaultColorCircleDialogOptions;
end;

function TColorCircleDialog.Execute: Boolean;
begin
  Result := False;
  if (Form <> nil) or (ColorCircleBaseFormClass = nil) then
    Exit;

  FForm := ColorCircleBaseFormClass.Create(Application);
  with Form do
  begin
    if Title <> '' then
      Caption := Title;
    Options := Self.Options;
    RedDelta := Self.RedDelta;
    GreenDelta := Self.GreenDelta;
    BlueDelta := Self.BlueDelta;
    OnApply := FormApply;
    OnClose := FormClose;
    OnShow := FormShow;
    HelpContext := Self.HelpContext;

    Result := (ShowModal = mrOk);

    Self.FDelta.AxisRed := RedDelta;
    Self.FDelta.AxisGreen := GreenDelta;
    Self.FDelta.AxisBlue := BlueDelta;
  end;
  FreeAndNil(FForm);
end;

procedure TColorCircleDialog.FormApply(Sender: TObject);
begin
end;

procedure TColorCircleDialog.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Allow: Boolean;
begin
  Allow := True;

  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, Allow);

  if Allow then
    Action := caFree
  else
    Action := caNone;
end;

procedure TColorCircleDialog.FormShow(Sender: TObject);
begin
end;

function TColorCircleDialog.GetDelta(const Index: TRotateColor): TAxisDelta;
var
  I: TAxisIndex;
begin
  for I := Low(Result) to High(Result) do
  begin
    Result[I].Value := 0;
    Result[I].SaturationMethod := smLoop;
  end;
end;

procedure TColorCircleDialog.SetDelta(const Index: TRotateColor;
  const Value: TAxisDelta);
begin
end;

procedure TColorCircleDialog.SetHelpContext(const Value: THelpContext);
begin
  FHelpContext := Value;
end;

procedure TColorCircleDialog.SetOptions(const Value: TColorCircleDialogOptions);
begin
  FOptions := Value;
end;

procedure TColorCircleDialog.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure Register;
begin
  RegisterComponents('Colors', [TFullColorDialog, TColorCircleDialog]);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

