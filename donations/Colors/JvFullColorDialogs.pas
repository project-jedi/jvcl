{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorDialogs.pas, released on 2004-09-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -                                               

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorDialogs;

{$I jvcl.inc}

interface

uses
  Classes, Graphics, Forms,
  JvFullColorSpaces, JvFullColorRotate;

type
  TJvFullColorDialogOption =
   (foFullOpen, foPreventExpand, foShowHelp,
    foAllowSpaceChange, foConvertToOriginalSpace,
    foShowNewPreview, foShowOldPreview,
    foShowPredefined, foAllowVariable,
    foNoneAndDefault, foShowApply);
  TJvFullColorDialogOptions = set of TJvFullColorDialogOption;

  TJvFullColorCircleDialogOption =
   (coFullOpen, coPreventExpand,
    coShowHelp, coAllowSpaceChange,
    coShowNewPreview, coShowOldPreview,
    coCommon, coRed, coGreen, coBlue,
    coShowSaturation, coDefaultRange,
    coShowApply);
  TJvFullColorCircleDialogOptions = set of TJvFullColorCircleDialogOption;

const
  JvDefaultFullColorDialogOptions =
   [foFullOpen, foAllowSpaceChange, foAllowVariable,
    foShowNewPreview, foShowOldPreview, foShowPredefined, foShowApply];

  JvDefaultColorCircleDialogOptions =
   [coFullOpen, coAllowSpaceChange,
    coShowNewPreview, coShowOldPreview,
    coCommon, coRed, coGreen, coBlue,
    coShowSaturation, coShowApply];

type
  TJvAxisType = (atCommon, atRed, atGreen, atBlue);

  TJvFullColorEvent = procedure(Sender: TObject; AFullColor: TJvFullColor) of object;

  TJvColorCircleEvent = procedure(Sender: TObject) of object;// AxisType: TJvAxisType;
//    NewFullColor: TJvFullColor) of object;

  TJvFullColorDialog = class;
  TJvFullColorCircleDialog = class;
  TJvBaseFullColorForm = class;
  TJvBaseColorCircleForm = class;

  TJvFullColorDialog = class(TComponent)
  private
    FOptions: TJvFullColorDialogOptions;
    FTitle: string;
    FFullColor: TJvFullColor;
    FOnApply: TJvFullColorEvent;
    FForm: TJvBaseFullColorForm;
    FOnCloseQuery: TCloseQueryEvent;
    FOnShow: TNotifyEvent;
    FHelpContext: THelpContext;
    FOldColorSpace: TJvFullColorSpaceID;
    function GetFullColor: TJvFullColor;
    procedure SetFullColor(const Value: TJvFullColor);
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetOptions(const Value: TJvFullColorDialogOptions);
    procedure SetTitle(const Value: string);
    function GetColor: TColor;
  protected
    procedure FormApply(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    property OldColorSpace: TJvFullColorSpaceID read FOldColorSpace;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property Form: TJvBaseFullColorForm read FForm;
    property Color: TColor read GetColor;
  published
    // (rom) set default values
    property FullColor: TJvFullColor read GetFullColor write SetFullColor;
    property Options: TJvFullColorDialogOptions read FOptions write SetOptions default JvDefaultFullColorDialogOptions;
    property Title: string read FTitle write SetTitle;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext;
    property OnApply: TJvFullColorEvent read FOnApply write FOnApply;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

  TJvFullColorCircleDialog = class(TComponent)
  private
    FTitle: string;
    FForm: TJvBaseColorCircleForm;
    FHelpContext: THelpContext;
    FOnCloseQuery: TCloseQueryEvent;
    FOnShow: TNotifyEvent;
    FOptions: TJvFullColorCircleDialogOptions;
    FOnApply: TJvColorCircleEvent;
    FDelta: TJvColorDelta;
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetOptions(const Value: TJvFullColorCircleDialogOptions);
    procedure SetTitle(const Value: string);
    procedure SetDelta(const Index: TJvRotateColor; const Value: TJvAxisDelta);
    procedure SetColorID(const Value: TJvFullColorSpaceID);
  protected
    procedure FormApply(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property Form: TJvBaseColorCircleForm read FForm;
    property RedDelta: TJvAxisDelta index rcRed read FDelta.AxisRed write SetDelta;
    property GreenDelta: TJvAxisDelta index rcGreen read FDelta.AxisGreen write SetDelta;
    property BlueDelta: TJvAxisDelta index rcBlue read FDelta.AxisBlue write SetDelta;
    property ColorID: TJvFullColorSpaceID read FDelta.ColorID write SetColorID;
    property Delta: TJvColorDelta read FDelta;
  published
    // (rom) set default values
    property Options: TJvFullColorCircleDialogOptions
      read FOptions write SetOptions default JvDefaultColorCircleDialogOptions;
    property Title: string read FTitle write SetTitle;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext;
    property OnApply: TJvColorCircleEvent read FOnApply write FOnApply;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

  TJvBaseFullColorForm = class(TForm)
  private
    FFullColor: TJvFullColor;
    FOptions: TJvFullColorDialogOptions;
    FOnApply: TNotifyEvent;
  protected
    procedure SetOptions(const Value: TJvFullColorDialogOptions); virtual;
    procedure SetFullColor(const Value: TJvFullColor); virtual;
  public
    constructor Create(AOwner: TComponent; AFullColor: TJvFullColor;
      AOptions: TJvFullColorDialogOptions); reintroduce;
    property Options: TJvFullColorDialogOptions read FOptions write SetOptions;
    property FullColor: TJvFullColor read FFullColor write SetFullColor;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

  TFullColorFormClass = class of TJvBaseFullColorForm;

  TJvBaseColorCircleForm = class(TForm)
  private
    FDelta: TJvColorDelta;
    FOnApply: TNotifyEvent;
    FOptions: TJvFullColorCircleDialogOptions;
  protected
    procedure SetOptions(const Value: TJvFullColorCircleDialogOptions); virtual;
    procedure SetRedDelta(const Value: TJvAxisDelta); virtual;
    procedure SetGreenDelta(const Value: TJvAxisDelta); virtual;
    procedure SetBlueDelta(const Value: TJvAxisDelta); virtual;
    procedure SetColorID(const Value: TJvFullColorSpaceID); virtual;
    procedure SetDelta(const Value: TJvColorDelta); virtual;
  public
    property Options: TJvFullColorCircleDialogOptions read FOptions write SetOptions;
    property RedDelta: TJvAxisDelta read FDelta.AxisRed write SetRedDelta;
    property GreenDelta: TJvAxisDelta read FDelta.AxisGreen write SetGreenDelta;
    property BlueDelta: TJvAxisDelta read FDelta.AxisBlue write SetBlueDelta;
    property ColorID: TJvFullColorSpaceID read FDelta.ColorID write SetColorID;
    property Delta: TJvColorDelta read FDelta write SetDelta;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

  TJvColorCircleBaseFormClass = class of TJvBaseColorCircleForm;

var
  FullColorFormClass: TFullColorFormClass;
  ColorCircleBaseFormClass: TJvColorCircleBaseFormClass;

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

//=== { TJvFullColorDialog } =================================================

constructor TJvFullColorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := JvDefaultFullColorDialogOptions;
  FFullColor := fclRGBWhite;
end;

function TJvFullColorDialog.Execute: Boolean;
var
  NewColor: TJvFullColor;
begin
  Result := False;
  if (Form <> nil) or (FullColorFormClass = nil) then
    Exit;

  FOldColorSpace := ColorSpaceManager.GetColorSpaceID(FullColor);

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

procedure TJvFullColorDialog.FormApply(Sender: TObject);
var
  Color: TJvFullColor;
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

procedure TJvFullColorDialog.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TJvFullColorDialog.FormShow(Sender: TObject);
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TJvFullColorDialog.GetColor: TColor;
begin
  Result := ColorSpaceManager.ConvertToColor(FullColor);
end;

function TJvFullColorDialog.GetFullColor: TJvFullColor;
begin
  if Form <> nil then
    FFullColor := Form.FullColor;
  Result := FFullColor;
end;

procedure TJvFullColorDialog.SetFullColor(const Value: TJvFullColor);
begin
  FFullColor := Value;
  if Form <> nil then
    Form.FullColor := Value;
end;

procedure TJvFullColorDialog.SetHelpContext(const Value: THelpContext);
begin
  if FHelpContext <> Value then
  begin
    FHelpContext := Value;
    if Assigned(FForm) then
      Form.HelpContext := Value;
  end;
end;

procedure TJvFullColorDialog.SetOptions(const Value: TJvFullColorDialogOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if Assigned(FForm) then
      Form.Options := Value;
  end;
end;

procedure TJvFullColorDialog.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    if Assigned(FForm) then
      Form.Caption := Value;
  end;
end;

//=== { TJvBaseFullColorForm } ===============================================

constructor TJvBaseFullColorForm.Create(AOwner: TComponent;
  AFullColor: TJvFullColor; AOptions: TJvFullColorDialogOptions);
begin
  inherited Create(AOwner);
  FOptions := AOptions;
  FFullColor := AFullColor;
end;

procedure TJvBaseFullColorForm.SetFullColor(const Value: TJvFullColor);
begin
  FFullColor := Value;
end;

procedure TJvBaseFullColorForm.SetOptions(const Value: TJvFullColorDialogOptions);
begin
  FOptions := Value;
end;

//=== { TJvBaseColorCircleForm } =============================================

procedure TJvBaseColorCircleForm.SetBlueDelta(const Value: TJvAxisDelta);
begin
  FDelta.AxisBlue := Value;
end;

procedure TJvBaseColorCircleForm.SetColorID(const Value: TJvFullColorSpaceID);
begin
  FDelta.ColorID := Value;
end;

procedure TJvBaseColorCircleForm.SetDelta(const Value: TJvColorDelta);
begin
  FDelta := Value;
end;

procedure TJvBaseColorCircleForm.SetGreenDelta(const Value: TJvAxisDelta);
begin
  FDelta.AxisGreen := Value;
end;

procedure TJvBaseColorCircleForm.SetOptions(const Value: TJvFullColorCircleDialogOptions);
begin
  FOptions := Value;
end;

procedure TJvBaseColorCircleForm.SetRedDelta(const Value: TJvAxisDelta);
begin
  FDelta.AxisRed := Value;
end;

//=== { TJvColorCircleDialog } ===============================================

constructor TJvFullColorCircleDialog.Create(AOwner: TComponent);
  procedure InitAxe (out Value: TJvAxisDelta);
  var
    Index: TJvAxisIndex;
  begin
    for Index := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    begin
      Value[Index].Value := 0;
      Value[Index].SaturationMethod := smLoop;
    end;
  end;
begin
  inherited Create(AOwner);
  FOptions := JvDefaultColorCircleDialogOptions;
  InitAxe(FDelta.AxisRed);
  InitAxe(FDelta.AxisGreen);
  InitAxe(FDelta.AxisBlue);
  FDelta.ColorID := csRGB;
end;

function TJvFullColorCircleDialog.Execute: Boolean;
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
    ColorID := Self.ColorID;
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
    Self.FDelta.ColorID := ColorID;
  end;
  FreeAndNil(FForm);
end;

procedure TJvFullColorCircleDialog.FormApply(Sender: TObject);
begin
  if FForm<>nil then
  begin
    FDelta.ColorID := FForm.ColorID;
    FDelta.AxisRed := FForm.RedDelta;
    FDelta.AxisGreen := FForm.GreenDelta;
    FDelta.AxisBlue := FForm.BlueDelta;
    if Assigned(FOnApply) then
      FOnApply(Self);
  end;
end;

procedure TJvFullColorCircleDialog.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Allow: Boolean;
begin
  if FForm<>nil then
  begin
    FDelta.ColorID := FForm.ColorID;
    FDelta.AxisRed := FForm.RedDelta;
    FDelta.AxisGreen := FForm.GreenDelta;
    FDelta.AxisBlue := FForm.BlueDelta;
    Allow := True;
    if Assigned(FOnCloseQuery) then
      FOnCloseQuery(Self, Allow);
    if Allow then
      Action := caFree
    else
      Action := caNone;
  end;
end;

procedure TJvFullColorCircleDialog.FormShow(Sender: TObject);
begin
  if FForm<>nil then
  begin
    FDelta.ColorID := FForm.ColorID;
    FDelta.AxisRed := FForm.RedDelta;
    FDelta.AxisGreen := FForm.GreenDelta;
    FDelta.AxisBlue := FForm.BlueDelta;
    if Assigned(FOnShow) then
      FOnShow(Self);
  end;
end;

procedure TJvFullColorCircleDialog.SetColorID(
  const Value: TJvFullColorSpaceID);
begin
  FDelta.ColorID := Value;
  if FForm <> nil then
    FForm.SetColorID(Value);
end;

procedure TJvFullColorCircleDialog.SetDelta(const Index: TJvRotateColor;
  const Value: TJvAxisDelta);
begin
  case Index of
    rcRed :
      begin
        FDelta.AxisRed := Value;
        if FForm <> nil then
          FForm.SetRedDelta(Value);
      end;
    rcGreen :
      begin
        FDelta.AxisGreen := Value;
        if FForm <> nil then
          FForm.SetGreenDelta(Value);
      end;
    rcBlue :
      begin
        FDelta.AxisBlue := Value;
        if FForm <> nil then
          FForm.SetBlueDelta(Value);
      end;
  end;
end;

procedure TJvFullColorCircleDialog.SetHelpContext(const Value: THelpContext);
begin
  FHelpContext := Value;
  if FForm <> nil then
    FForm.HelpContext := Value;
end;

procedure TJvFullColorCircleDialog.SetOptions(const Value: TJvFullColorCircleDialogOptions);
begin
  FOptions := Value;
  if FForm <> nil then
    FForm.Options := Value;
end;

procedure TJvFullColorCircleDialog.SetTitle(const Value: string);
begin
  FTitle := Value;
  if FForm <> nil then
    FForm.Caption := Value;
end;

procedure Register;
begin
  RegisterComponents('Jv Colors', [TJvFullColorDialog, TJvFullColorCircleDialog]);
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

