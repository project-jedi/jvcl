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

  TJvColorCircleEvent = procedure(Sender: TObject) of object;

  TJvFullColorDialog = class;
  TJvFullColorCircleDialog = class;

  TJvFullColorDialog = class(TComponent)
  private
    FOptions: TJvFullColorDialogOptions;
    FTitle: string;
    FFullColor: TJvFullColor;
    FOnApply: TJvFullColorEvent;
    FForm: TForm;
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
    property Form: TForm read FForm;
    property Color: TColor read GetColor;
  published
    property FullColor: TJvFullColor read GetFullColor write SetFullColor default fclRGBBlack;
    property Options: TJvFullColorDialogOptions read FOptions write SetOptions default JvDefaultFullColorDialogOptions;
    property Title: string read FTitle write SetTitle;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property OnApply: TJvFullColorEvent read FOnApply write FOnApply;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

  TJvFullColorCircleDialog = class(TComponent)
  private
    FTitle: string;
    FForm: TForm;
    FHelpContext: THelpContext;
    FOnCloseQuery: TCloseQueryEvent;
    FOnShow: TNotifyEvent;
    FOptions: TJvFullColorCircleDialogOptions;
    FOnApply: TJvColorCircleEvent;
    FDelta: TJvColorDelta;
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetOptions(const Value: TJvFullColorCircleDialogOptions);
    procedure SetTitle(const Value: string);
    procedure SetColorID(const Value: TJvFullColorSpaceID);
    procedure SetBlueDelta(const Value: TJvAxisDelta);
    procedure SetGreenDelta(const Value: TJvAxisDelta);
    procedure SetRedDelta(const Value: TJvAxisDelta);
    function GetRedDelta: TJvAxisDelta;
    function GetGreenDelta: TJvAxisDelta;
    function GetBlueDelta: TJvAxisDelta;
    function GetColorID: TJvFullColorSpaceID;
  protected
    procedure FormApply(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean;
    property Form: TForm read FForm;
    property RedDelta: TJvAxisDelta read GetRedDelta write SetRedDelta;
    property GreenDelta: TJvAxisDelta read GetGreenDelta write SetGreenDelta;
    property BlueDelta: TJvAxisDelta read GetBlueDelta write SetBlueDelta;
    property ColorID: TJvFullColorSpaceID read GetColorID write SetColorID;
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

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvResources,
  Controls, SysUtils, JvFullColorForm, JvFullColorCircleForm;

//=== { TJvFullColorDialog } =================================================

constructor TJvFullColorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := JvDefaultFullColorDialogOptions;
  FFullColor := fclRGBBlack;
end;

function TJvFullColorDialog.Execute: Boolean;
var
  NewColor: TJvFullColor;
begin
  FOldColorSpace := ColorSpaceManager.GetColorSpaceID(FullColor);

  FForm := TJvFullColorFrm.Create(Application, FFullColor, FOptions);
  with TJvFullColorFrm(Form) do
  begin
    if Title <> '' then
      Caption := FTitle;
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
    Color := TJvFullColorFrm(Form).FullColor;
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
    FFullColor := TJvFullColorFrm(Form).FullColor;
  Result := FFullColor;
end;

procedure TJvFullColorDialog.SetFullColor(const Value: TJvFullColor);
begin
  FFullColor := Value;
  if Form <> nil then
    TJvFullColorFrm(Form).FullColor := Value;
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
      TJvFullColorFrm(Form).Options := Value;
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

//=== { TJvColorCircleDialog } ===============================================

constructor TJvFullColorCircleDialog.Create(AOwner: TComponent);
  procedure InitAxe (Value: TJvAxisDelta);
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
  FDelta := TJvColorDelta.Create;
  
  InitAxe(FDelta.AxisRed);
  InitAxe(FDelta.AxisGreen);
  InitAxe(FDelta.AxisBlue);
  FDelta.ColorID := csRGB;
end;

destructor TJvFullColorCircleDialog.Destroy;
begin
  FDelta.Free;
  
  inherited;
end;

function TJvFullColorCircleDialog.Execute: Boolean;
begin
  FForm := TJvFullColorCircleFrm.Create(Application);
  with TJvFullColorCircleFrm(Form) do
  begin
    if Title <> '' then
      Caption := Title;
    Options := Self.Options;
    ColorID := Self.ColorID;
    RedDelta.Assign(Self.RedDelta);
    GreenDelta.Assign(Self.GreenDelta);
    BlueDelta.Assign(Self.BlueDelta);
    OnApply := FormApply;
    OnClose := FormClose;
    OnShow := FormShow;
    HelpContext := Self.HelpContext;

    Result := (ShowModal = mrOk);

    Self.FDelta.AxisRed.Assign(RedDelta);
    Self.FDelta.AxisGreen.Assign(GreenDelta);
    Self.FDelta.AxisBlue.Assign(BlueDelta);
    Self.FDelta.ColorID := ColorID;
  end;
  FreeAndNil(FForm);
end;

procedure TJvFullColorCircleDialog.FormApply(Sender: TObject);
begin
  if FForm<>nil then
  begin
    FDelta.ColorID := TJvFullColorCircleFrm(FForm).ColorID;
    FDelta.AxisRed := TJvFullColorCircleFrm(FForm).RedDelta;
    FDelta.AxisGreen := TJvFullColorCircleFrm(FForm).GreenDelta;
    FDelta.AxisBlue := TJvFullColorCircleFrm(FForm).BlueDelta;
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
    FDelta.ColorID := TJvFullColorCircleFrm(FForm).ColorID;
    FDelta.AxisRed := TJvFullColorCircleFrm(FForm).RedDelta;
    FDelta.AxisGreen := TJvFullColorCircleFrm(FForm).GreenDelta;
    FDelta.AxisBlue := TJvFullColorCircleFrm(FForm).BlueDelta;
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
    FDelta.ColorID := TJvFullColorCircleFrm(FForm).ColorID;
    FDelta.AxisRed := TJvFullColorCircleFrm(FForm).RedDelta;
    FDelta.AxisGreen := TJvFullColorCircleFrm(FForm).GreenDelta;
    FDelta.AxisBlue := TJvFullColorCircleFrm(FForm).BlueDelta;
    if Assigned(FOnShow) then
      FOnShow(Self);
  end;
end;

function TJvFullColorCircleDialog.GetBlueDelta: TJvAxisDelta;
begin
  Result := FDelta.AxisBlue;
end;

function TJvFullColorCircleDialog.GetColorID: TJvFullColorSpaceID;
begin
  Result := FDelta.ColorID;
end;

function TJvFullColorCircleDialog.GetGreenDelta: TJvAxisDelta;
begin
  Result := FDelta.AxisGreen;
end;

function TJvFullColorCircleDialog.GetRedDelta: TJvAxisDelta;
begin
  Result := FDelta.AxisRed;
end;

procedure TJvFullColorCircleDialog.SetBlueDelta(const Value: TJvAxisDelta);
begin
  FDelta.AxisBlue.Assign(Value);
  if FForm <> nil then
    TJvFullColorCircleFrm(FForm).BlueDelta := Value;
end;

procedure TJvFullColorCircleDialog.SetColorID(
  const Value: TJvFullColorSpaceID);
begin
  FDelta.ColorID := Value;
  if FForm <> nil then
    TJvFullColorCircleFrm(FForm).ColorID :=Value;
end;

procedure TJvFullColorCircleDialog.SetGreenDelta(
  const Value: TJvAxisDelta);
begin
  FDelta.AxisGreen.Assign(Value);
  if FForm <> nil then
    TJvFullColorCircleFrm(FForm).GreenDelta := Value;
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
    TJvFullColorCircleFrm(FForm).Options := Value;
end;

procedure TJvFullColorCircleDialog.SetRedDelta(const Value: TJvAxisDelta);
begin
  FDelta.AxisRed.Assign(Value);
  if FForm <> nil then
    TJvFullColorCircleFrm(FForm).RedDelta := Value;
end;

procedure TJvFullColorCircleDialog.SetTitle(const Value: string);
begin
  FTitle := Value;
  if FForm <> nil then
    FForm.Caption := Value;
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

