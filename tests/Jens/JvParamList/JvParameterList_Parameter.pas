{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens.fudickar@oratool.de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens.fudickar@oratool.de]

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvParameterList_Parameter;

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms,
  Controls, FileCtrl, Dialogs, ComCtrls,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvPanel, JvPropertyStore, JvParameterList, JvDynControlEngine, JvDSADialogs,
  JvDynControlEngine_Interface;

type
  TJvNoDataParameter = class (TJvBaseParameter)
  protected
    property AsString;
    property AsDouble;
    property AsInteger;
    property AsBoolean;
    property AsDate;
    property Required;
    property ReloadValuefromRegistry;
    property ReadOnly;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    function Validate(var AData: variant): boolean; override;
  end;

  TJvParameterLabelArrangeMode = (lamBefore, lamAbove);

  TJvBasePanelEditParameter = class (TJvBaseParameter)
  private
    FLabelControl: TControl;
    FFramePanel: TWinControl;
    FLabelArrangeMode: TJvParameterLabelArrangeMode;
    FLabelWidth: integer;
    FEditWidth: integer;
    FRightSpace: integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ArrangeLabelAndWinControlOnPanel; virtual;
    procedure CreateLabelControl(AParameterParent: TWinControl); virtual;
    procedure CreateFramePanel(AParameterParent: TWinControl); virtual;
    procedure CreateWinControl(AParameterParent: TWinControl); virtual; abstract;
    property LabelControl: TControl read FLabelControl write FLabelControl;
    property FramePanel: TWinControl read FFramePanel write FFramePanel;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure SetEnabled(Value: boolean); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property LabelArrangeMode: TJvParameterLabelArrangeMode read FLabelArrangeMode write FLabelArrangeMode;
    property LabelWidth: integer read FLabelWidth write FLabelWidth;
    property EditWidth: integer read FEditWidth write FEditWidth;
    property RightSpace: integer read FRightSpace write FRightSpace;
  end;

  TJvArrangeParameter = class (TJvNoDataParameter)
  private
    FArrangeSettings: TJvArrangeSettings;
  protected
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property Color;
  end;

  TJvPanelParameter = class (TJvArrangeParameter)
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelWidth: integer;
    FBorderStyle: TBorderStyle;
    FBorderWidth: integer;
  protected
    function GetParameterNameExt: string; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property BevelInner: TPanelBevel read FBevelInner write FBevelInner;
    property BevelOuter: TPanelBevel read FBevelOuter write FBevelOuter;
    property BevelWidth: integer read FBevelWidth write FBevelWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property BorderWidth: integer read FBorderWidth write FBorderWidth;
  end;

  TJvGroupBoxParameter = class (TJvArrangeParameter)
  protected
    function GetParameterNameExt: string; override;
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvImageParameter = class (TJvBasePanelEditParameter)
  private
    FAutoSize: boolean;
    FCenter: boolean;
    FIncrementalDisplay: boolean;
    FTransparent: boolean;
    FStretch: boolean;
    FPicture: TPicture;
  protected
    procedure SetPicture(Value: TPicture);
    procedure SetAutosize(Value: boolean);
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
//    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Center: boolean read FCenter write FCenter;
    property IncrementalDisplay: boolean read FIncrementalDisplay write FIncrementalDisplay;
    property Transparent: boolean read FTransparent write FTransparent;
    property Stretch: boolean read FStretch write FStretch;
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TJvLabelParameter = class (TJvNoDataParameter)
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvCheckboxParameter = class (TJvBaseParameter)
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvEditParameter = class (TJvBasePanelEditParameter)
  private
    FEditMask: string;
    FPasswordChar: char;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property EditMask: string read FEditMask write FEditMask;
    property PasswordChar: char read FPasswordChar write FPasswordChar;
  end;

  TJvNumberEditorType = (netEdit, netSpin, netCalculate);

  TJvNumberEditParameter = class (TJvEditParameter)
  private
    FEditorType: TJvNumberEditorType;
  protected
  public
    procedure Assign(Source: TPersistent); override;
  published
    property EditorType: TJvNumberEditorType read FEditorType write FEditorType;
  end;

  TJvIntegerEditParameter = class (TJvNumberEditParameter)
  private
    FMinValue: integer;
    FMaxValue: integer;
    fIncrement: integer;
  protected
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: variant): boolean; override;
  published
    property Increment: integer read FIncrement write FIncrement;
    property MinValue: integer read FMinValue write FMinValue;
    property MaxValue: integer read FMaxValue write FMaxValue;
  end;

  TJvDoubleEditParameter = class (TJvNumberEditParameter)
  private
    FMinValue: double;
    FMaxValue: double;
    FIncrement: integer;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: variant): boolean; override;
  published
    property Increment: integer read FIncrement write FIncrement;
    property MinValue: double read FMinValue write FMinValue;
    property MaxValue: double read FMaxValue write FMaxValue;
  end;

  TJvFileNameParameter = class (TJvBasePanelEditParameter)
  private
    FDefaultExt: string;
    FFilter: string;
    FFilterIndex: integer;
    FInitialDir: string;
    FDialogOptions: TOpenOptions;
    FDialogTitle: string;
    FDialogKind: TJvDynControlFileNameDialogKind;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: variant): boolean; override;
  published
    property FileName: string read GetAsString write SetAsString;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property Filter: string read FFilter write FFilter;
    property FilterIndex: integer read FFilterIndex write FFilterIndex;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogKind: TJvDynControlFileNameDialogKind read FDialogKind write FDialogKind;
  end;

  TJvDirectoryParameter = class (TJvBasePanelEditParameter)
  private
    FInitialDir: string;
    FDialogTitle: string;
    FDialogOptions: TSelectDirOpts;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: variant): boolean; override;
  published
    property Directory: string read GetAsString write SetAsString;
    property InitialDir: string read FInitialDir write FInitialDir;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DialogOptions: TSelectDirOpts read FDialogOptions write FDialogOptions;
  end;

  TJvListParameter = class (TJvBasePanelEditParameter)
  private
    FItemList: TStringList;
    FItemIndex: integer;
    FSorted: boolean;
  protected
    procedure SetItemList(Value: TStringList);
    procedure SetItemIndex(Value: integer);
    procedure SetAsString(Value: string); override;
    procedure SetAsInteger(Value: integer); override;
    function GetAsInteger: integer; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SearchItemIndex(Search: string);
    procedure GetData; override;
    procedure SetData; override;
  published
    property ItemList: TStringList read FItemList write SetItemList;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Sorted: boolean read FSorted write FSorted;
  end;

  TJvRadioGroupParameter = class (TJvListParameter)
  private
    FColumns: integer;
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property Columns: integer read FColumns write FColumns;
  end;

  TJvComboBoxParameterStyle = (cpsListEdit, cpsListFixed);

  TJvComboBoxParameter = class (TJvListParameter)
  private
    FSorted: boolean;
    FNewEntriesAllowed: boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: boolean read FSorted write FSorted;
    property NewEntriesAllowed: boolean read FNewEntriesAllowed write FNewEntriesAllowed;
  end;

  TJvListBoxParameter = class (TJvListParameter)
  private
    FSorted: boolean;
  protected
    function GetParameterNameExt: string; override;
    function GetWinControlData: variant; override;
    procedure SetWinControlData(Value: variant); override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: boolean read FSorted write FSorted;
  end;

  TJvDateTimeParameter = class (TJvBasePanelEditParameter)
  private
    FCalAlignment: TDTCalAlignment;
    FDateFormat: TDTDateFormat;
    FDateMode: TDTDateMode;
    FKind: TDateTimeKind;
    FMaxDate: TDate;
    FMinDate: TDate;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CalAlignment: TDTCalAlignment read FCalAlignment write FCalAlignment;
    property DateFormat: TDTDateFormat read FDateFormat write FDateFormat;
    property DateMode: TDTDateMode read FDateMode write FDateMode;
    property Kind: TDateTimeKind read FKind write FKind;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property MinDate: TDate read FMinDate write FMinDate;
  end;

  TJvMemoParameter = class (TJvBasePanelEditParameter)
  private
    FWordWrap: boolean;
    FWantTabs: boolean;
    FWantReturns: boolean;
    FScrollBars: TScrollStyle;
    FFontName: string;
  protected
    function GetParameterNameExt: string; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControl(AParameterParent: TWinControl); override;
  published
    property WordWrap: boolean read FWordWrap write FWordWrap;
    property WantTabs: boolean read FWantTabs write FWantTabs;
    property WantReturns: boolean read FWantReturns write FWantReturns;
    property ScrollBars: TScrollStyle read FScrollBars write FScrollBars;
    property FontName: string read FFontName write FFontName;
  end;

implementation

resourcestring
  SErrParameterMustBeEntered      = 'Parameter %s must be entered!';
  SErrParameterIsNotAValidNumber  = 'Parameter %s: %s is not a valid number value!';
  SErrParameterMustBeBetween      = 'Parameter %s: %s must be between %s and %s!';
  SErrParameterFileDoesNotExist   = 'Parameter %s: The file "%s" does not exist!';
  SErrParameterFileExistOverwrite = 'Parameter %s: The file "%s" exists! Overwrite?';
  SErrParameterDirectoryNotExist  = 'Parameter %s: The directory "%s" does not exist!';

//=== TJvNoDataParameter =====================================================

constructor TJvNoDataParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ReloadValuefromRegistry := false;
end;

function TJvNoDataParameter.Validate(var AData: variant): boolean;
begin
  Result := true;
end;

//=== TJvBasePanelEditParameter ==============================================

constructor TJvBasePanelEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FLabelArrangeMode := lamAbove;
  FLabelWidth := 0;
  FEditWidth  := 0;
  FRightSpace := 0;
end;

procedure TJvBasePanelEditParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  CreateFramePanel(ParameterParent);
  CreateWinControl(FramePanel);
  CreateLabelControl(FramePanel);
  ArrangeLabelAndWinControlOnPanel;
end;

procedure TJvBasePanelEditParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FFramePanel) and (Operation = opRemove) then
    FFramePanel := nil;
  if (AComponent = FLabelControl) and (Operation = opRemove) then
    FLabelControl := nil;
end;

procedure TJvBasePanelEditParameter.CreateFramePanel(AParameterParent: TWinControl);
begin
  FramePanel := DynControlEngine.CreatePanelControl(Self, AParameterParent, GetParameterName + 'Panel', '', alNone);
  if FramePanel is TPanel then
    with TPanel(FramePanel) do
    begin
      BevelInner := bvNone;
      BevelOuter := bvNone;
    end;
  if TabOrder >= 0 then
    FramePanel.TabOrder := TabOrder;
  if Width > 0 then
    FramePanel.Width := Width;
  if Height > 0 then
    FramePanel.Height := Height;
  FramePanel.Visible := true;
end;

procedure TJvBasePanelEditParameter.CreateLabelControl(AParameterParent: TWinControl);
begin
  if Caption = '' then
    Exit;
  LabelControl := DynControlEngine.CreateLabelControl(Self, AParameterParent, GetParameterName + 'Label', Caption, WinControl);
  LabelControl.Visible := true;
  LabelControl.Enabled := Enabled;
  LabelControl.Parent := AParameterParent;
end;

procedure TJvBasePanelEditParameter.ArrangeLabelAndWinControlOnPanel;
begin
  if not Assigned(FramePanel) or not Assigned(WinControl) then
    Exit;
  if (LabelArrangeMode = lamBefore) and not Assigned(LabelControl) then
    LabelArrangeMode := lamAbove;

  if not Assigned(LabelControl) then
  begin
    WinControl.Top  := 0;
    WinControl.Left := 0;
    if FramePanel.Height > 0 then
      FramePanel.Height := WinControl.Height
    else
      WinControl.Height := FramePanel.Height;
    if EditWidth > 0 then
    begin
      WinControl.Width := EditWidth;
      if FramePanel.Width <= 0 then
        FramePanel.Width := WinControl.Width;
    end
    else if RightSpace > 0 then
      if FramePanel.Width > 0 then
        WinControl.Width := FramePanel.Width - RightSpace
      else
      begin
        FramePanel.Width := WinControl.Width;
        WinControl.Width := WinControl.Width - RightSpace;
      end
    else if FramePanel.Width > 0 then
      WinControl.Width := FramePanel.Width
    else
      FramePanel.Width := WinControl.Width;
    Exit;
  end
  else
  begin
    LabelControl.Top  := 0;
    LabelControl.Left := 0;
  end;
  if (LabelArrangeMode = lamAbove) or not Assigned(LabelControl) then
  begin
    if Assigned(LabelControl) then
      WinControl.Top := LabelControl.Height + 2
    else
      WinControl.Top := 0;
    WinControl.Left := 0;

    if EditWidth > 0 then
    begin
      WinControl.Width := EditWidth;
      if FramePanel.Width <= 0 then
        FramePanel.Width := WinControl.Width;
    end
    else if RightSpace > 0 then
      if FramePanel.Width > 0 then
        WinControl.Width := FramePanel.Width - RightSpace
      else
      begin
        FramePanel.Width := WinControl.Width;
        WinControl.Width := WinControl.Width - RightSpace;
      end
    else if FramePanel.Width > 0 then
      WinControl.Width := FramePanel.Width
    else
      FramePanel.Width := WinControl.Width;
    if Assigned(LabelControl) then
      LabelControl.Width := FramePanel.Width;

    if Height > 0 then
      if Assigned(LabelControl) then
        WinControl.Height := Height - (LabelControl.Height + 3)
      else
        WinControl.Height := Height
    else if Assigned(LabelControl) then
      FramePanel.Height := WinControl.Height + LabelControl.Height + 3
    else
      FramePanel.Height := WinControl.Height;
  end
  else
  begin
    if LabelWidth > 0 then
      LabelControl.Width := LabelWidth;
 //    ELSE
 //      LabelControl.Width :=
    WinControl.Top := LabelControl.Top;
    WinControl.Left := LabelControl.Left + LabelControl.Width + 4;
    if FramePanel.Height > 0 then
      WinControl.Height := FramePanel.Height
    else
      FramePanel.Height := WinControl.Height;
    LabelControl.Top := WinControl.Top + Round((WinControl.Height - LabelControl.Height) / 2);
    if EditWidth > 0 then
    begin
      WinControl.Width := EditWidth;
      if FramePanel.Width <= 0 then
        FramePanel.Width := WinControl.Width + LabelControl.Width + 3;
    end
    else
    begin
      if FramePanel.Width > 0 then
        WinControl.Width := FramePanel.Width - (LabelControl.Width + 3)
      else
        FramePanel.Width := WinControl.Width + LabelControl.Width + 3;
    end;
  end;
end;

procedure TJvBasePanelEditParameter.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if Assigned(LabelControl) then
    LabelControl.Enabled := Value;
end;

procedure TJvBasePanelEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  LabelArrangeMode := TJvBasePanelEditParameter(Source).LabelArrangeMode;
  LabelWidth := TJvBasePanelEditParameter(Source).LabelWidth;
  EditWidth  := TJvBasePanelEditParameter(Source).EditWidth;
  RightSpace := TJvBasePanelEditParameter(Source).RightSpace;
end;

//=== TJvLabelParameter ======================================================

procedure TJvLabelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateStaticTextControl(Self, ParameterParent, GetParameterName, Caption);
end;


//=== TJvImageParameter ======================================================

constructor TJvImageParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FPicture     := TPicture.Create;
  FAutoSize    := false;
  FCenter      := false;
  FIncrementalDisplay := false;
  FStretch     := false;
  FTransparent := false;
end;

destructor TJvImageParameter.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvImageParameter.SetPicture(Value: TPicture);
var
  ITmpImage: IJvDynControlImage;
begin
  FPicture.Assign(Value);
 //  if Assigned(WinControl) then
 //    if Supports(WinControl, IJvDynControlImage, ITmpImage) then
 //      with ITmpImage do
 //        ControlSetPicture(Value);
end;

procedure TJvImageParameter.SetAutosize(Value: boolean);
begin
  FAutosize := Value;
end;

procedure TJvImageParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Picture     := TJvImageParameter(Source).Picture;
//  AutoSize := TJvImageParameter(Source).AutoSize;
  Center      := TJvImageParameter(Source).Center;
  IncrementalDisplay := TJvImageParameter(Source).IncrementalDisplay;
  Stretch     := TJvImageParameter(Source).Stretch;
  Transparent := TJvImageParameter(Source).Transparent;
end;

function TJvImageParameter.GetParameterNameExt: string;
begin
  Result := 'Image';
end;

procedure TJvImageParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpImage: IJvDynControlImage;
begin
  WinControl := DynControlEngine.CreateImageControl(Self, AParameterParent, GetParameterName);
  if Supports(WinControl, IJvDynControlImage, ITmpImage) then
    with ITmpImage do
    begin
      ControlSetPicture(Picture);
//      ControlSetAutoSize(AutoSize);
      ControlSetIncrementalDisplay(IncrementalDisplay);
      ControlSetCenter(Center);
      ControlSetStretch(Stretch);
      ControlSetTransparent(Transparent);
    end;
end;

//=== TJvArrangeParameter ====================================================

constructor TJvArrangeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FArrangeSettings := TJvArrangeSettings.Create(nil);
end;

destructor TJvArrangeParameter.Destroy;
begin
  FArrangeSettings.Free;
  inherited Destroy;
end;

procedure TJvArrangeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TJvArrangeParameter.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
end;


//=== TJvPanelParameter ======================================================

constructor TJvPanelParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  bevelInner  := bvNone;
  bevelOuter  := bvNone;
  BevelWidth  := 1;
  BorderStyle := bsNone;
  BorderWidth := 0;
end;

procedure TJvPanelParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  bevelInner := TJvPanelParameter(Source).bevelInner;
  bevelOuter := TJvPanelParameter(Source).bevelOuter;
end;

function TJvPanelParameter.GetParameterNameExt: string;
begin
  Result := 'Panel';
end;

procedure TJvPanelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  ITmpPanel: IJvDynControlPanel;
begin
  WinControl := DynControlEngine.CreatePanelControl(Self, ParameterParent, GetParameterName, Caption, alNone);
  if Supports(WinControl, IJvDynControlPanel, ITmpPanel) then
    ITmpPanel.ControlSetBorder(BevelInner, BevelOuter, BevelWidth, BorderStyle, BorderWidth);
end;

//=== TJvGroupBoxParameter ===================================================

function TJvGroupBoxParameter.GetParameterNameExt: string;
begin
  Result := 'GroupBoxPanel';
end;

procedure TJvGroupBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  GroupBox: TWinControl;
  Panel:    TJvPanel;
begin
  GroupBox := DynControlEngine.CreateGroupBoxControl(Self, ParameterParent, GetParameterName, Caption);
  if Width > 0 then
    GroupBox.Width := Width;
  if Height > 0 then
    GroupBox.Height := Height;
  Panel := TJvPanel.Create(ParameterParent.Owner);
  WinControl    := Panel;
  Panel.Name    := GetParameterName;
  Panel.ArrangeSettings := ArrangeSettings;
  Panel.Bevelinner := bvNone;
  Panel.BevelOuter := bvNone;
  Panel.Parent  := GroupBox;
  Panel.Align   := alClient;
  Panel.Visible := true;
  Panel.Caption := '';
  Panel.Color   := Color;
end;

//=== TJvListParameter =======================================================

constructor TJvListParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FItemList := TStringList.Create;
  Sorted    := false;
end;

destructor TJvListParameter.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;

procedure TJvListParameter.SetAsString(Value: string);
var
  I: integer;
begin
  I := ItemList.IndexOf(Value);
  if (I >= 0) and (I < ItemList.Count) then
    ItemIndex := I;
end;

procedure TJvListParameter.SetAsInteger(Value: integer);
begin
  if (Value >= 0) and (Value < ItemList.Count) then
    ItemIndex := Value;
end;

function TJvListParameter.GetAsInteger: integer;
begin
  Result := ItemIndex;
end;

procedure TJvListParameter.SetItemList(Value: TStringList);
begin
  FItemList.Assign(Value);
  if Assigned(Value) then
    if (ItemIndex >= 0) and (ItemIndex < ItemList.Count) then
      AsVariant := ItemList[ItemIndex];
end;

procedure TJvListParameter.SetItemIndex(Value: integer);
begin
  if Value >= ItemList.Count then
    FItemIndex := ItemList.Count - 1
  else
    FItemIndex := Value;
  if (Value >= 0) and (Value < ItemList.Count) then
    AsVariant := ItemList[Value];
end;

procedure TJvListParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  ItemList.Assign(TJvListParameter(Source).Itemlist);
  ItemIndex := TJvListParameter(Source).ItemIndex;
  Sorted    := TJvListParameter(Source).Sorted;
end;

procedure TJvListParameter.SearchItemIndex(Search: string);
var
  I: integer;
begin
  FItemIndex := -1;
  for I := 0 to ItemList.Count - 1 do
    if Search = ItemList.Strings[I] then
    begin
      FItemIndex := I;
      Break;
    end;
end;

procedure TJvListParameter.GetData;
begin
  inherited GetData;
  if Assigned(WinControl) then
    ItemIndex := ItemList.IndexOf(AsString)
  else
    ItemIndex := -1;
end;

procedure TJvListParameter.SetData;
begin
  inherited SetData;
 //  IF Assigned (
 //  IF Assigned (WinControl) THEN
 //    ItemList.IndexOf (AsString) := ItemIndex;
end;

//=== TJvRadioGroupParameter =================================================

procedure TJvRadioGroupParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Columns := TJvRadioGroupParameter(Source).Columns;
end;

procedure TJvRadioGroupParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  ITmpRadioGroup: IJvDynControlRadioGroup;
begin
  WinControl := DynControlEngine.CreateRadioGroupControl(Self, ParameterParent, GetParameterName, Caption, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlRadioGroup, ITmpRadioGroup) then
    ITmpRadioGroup.ControlSetColumns(Columns);
end;

//=== TJvCheckBoxParameter ===================================================

procedure TJvCheckBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateCheckBoxControl(Self, ParameterParent, GetParameterName, Caption);
  JvDynControl.ControlSetOnClick(HandleEnableDisable);
end;

//=== TJvComboBoxParameter ===================================================

procedure TJvComboBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Sorted := TJvComboBoxParameter(Source).Sorted;
  NewEntriesAllowed := TJvComboBoxParameter(Source).NewEntriesAllowed;
end;

function TJvComboBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ComboBox';
end;

procedure TJvComboBoxParameter.GetData;
begin
  Value := Null;
  if Assigned(WinControl) then
    Value := WinControlData;
end;

procedure TJvComboBoxParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := Value;
end;

procedure TJvComboBoxParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpComboBox: IJvDynControlComboBox;
  ITmpItems:    IJvDynControlItems;
begin
  WinControl := DynControlEngine.CreateComboBoxControl(Self, AParameterParent, GetParameterName, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlComboBox, ITmpComboBox) then
    ITmpComboBox.ControlSetNewEntriesAllowed(NewEntriesAllowed);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

//=== TJvListBoxParameter ====================================================

constructor TJvListBoxParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
end;

destructor TJvListBoxParameter.Destroy;
begin
  inherited Destroy;
end;

procedure TJvListBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Sorted := TJvListBoxParameter(Source).Sorted;
end;

function TJvListBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ListBox';
end;

procedure TJvListBoxParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpItems: IJvDynControlItems;
begin
  WinControl := DynControlEngine.CreateListBoxControl(Self, AParameterParent, GetParameterName, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;

function TJvListBoxParameter.GetWinControlData: variant;
begin
  Result := inherited GetWinControlData;
end;

procedure TJvListBoxParameter.SetWinControlData(Value: variant);
begin
  inherited SetWinControlData(Value);
end;

//=== TJvDateTimeParameter ===================================================

procedure TJvDateTimeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  CalAlignment := TJvDateTimeParameter(Source).CalAlignment;
  DateFormat := TJvDateTimeParameter(Source).DateFormat;
  DateMode := TJvDateTimeParameter(Source).DateMode;
  Kind    := TJvDateTimeParameter(Source).Kind;
  MaxDate := TJvDateTimeParameter(Source).maxDate;
  MinDate := TJvDateTimeParameter(Source).MinDate;
end;

function TJvDateTimeParameter.GetParameterNameExt: string;
begin
  Result := 'DateTime';
end;

procedure TJvDateTimeParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateDateTimeControl(Self, AParameterParent, GetParameterName);
 //  DateTime.CalAlignment:= CalAlignment;
 //  DateTime.DateFormat := DateFormat;
 //  DateTime.DateMode:= DateMode;
 //  DateTime.Kind:= Kind;
 //  DateTime.MaxDate:= maxDate;
 //  DateTime.MinDate:= MinDate;
end;

//=== TJvEditParameter =======================================================

constructor TJvEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FPasswordChar := #0;
  FEditMask     := '';
  FLabelWidth   := 0;
  FEditWidth    := 0;
  FLabelArrangeMode := lamAbove;
  FRightSpace   := 0;
end;

procedure TJvEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  EditMask     := TJvEditParameter(Source).EditMask;
  PasswordChar := TJvEditParameter(Source).PasswordChar;
  LabelWidth   := TJvEditParameter(Source).LabelWidth;
  EditWidth    := TJvEditParameter(Source).EditWidth;
  LabelArrangeMode := TJvEditParameter(Source).LabelArrangeMode;
  RightSpace   := TJvEditParameter(Source).RightSpace;
end;

function TJvEditParameter.GetParameterNameExt: string;
begin
  Result := 'MaskEdit';
end;

procedure TJvEditParameter.CreateWinControl(AParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
 //  MaskEdit.PasswordChar := PasswordChar;
 //  MaskEdit.EditMask := EditMask;
 //  MaskEdit.EditText := AsString;
end;

//=== TJvNumberEditParameter ================================================

procedure TJvNumberEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  EditorType := TJvNumberEditParameter(Source).EditorType;
end;

//=== TJvIntegerEditParameter ================================================

constructor TJvIntegerEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required  := true;
  MinValue  := Low(integer);
  MaxValue  := High(integer);
  Increment := 10;
end;

procedure TJvIntegerEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpSpin: IJvDynControlSpin;
begin
  if (EditorType = netCalculate) and DynControlEngine.IsControlTypeRegistered(jctCalculateEdit) then
    WinControl := DynControlEngine.CreateCalculateControl(Self, AParameterParent, GetParameterName)
  else if (EditorType = netSpin) and DynControlEngine.IsControlTypeRegistered(jctSpinEdit) then
    WinControl := DynControlEngine.CreateSpinControl(Self, AParameterParent, GetParameterName)
  else
    WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlSpin, ITmpSpin) then
    with ITmpSpin do
    begin
      ControlSetIncrement(Increment);
      ControlSetMinValue(MinValue);
      ControlSetMaxValue(MaxValue);
      ControlSetUseForInteger(true);
    end;
 //  MaskEdit.PasswordChar := PasswordChar;
 //  MaskEdit.EditMask := EditMask;
 //  MaskEdit.EditText := AsString;
end;

procedure TJvIntegerEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  MinValue := TJvIntegerEditParameter(Source).MinValue;
  MaxValue := TJvIntegerEditParameter(Source).MaxValue;
end;

function TJvIntegerEditParameter.Validate(var AData: variant): boolean;
var
  I: integer;
begin
  Result := not Enabled;
  if Result then
    Exit;
  if VarIsNull(AData) or (AData = '') then
    if Required then
    begin
      JvDSADialogs.MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end
    else
    begin
      Result := true;
      Exit;
    end;
  try
    I := AData;
  except
    JvDSADialogs.MessageDlg(Format(SErrParameterIsNotAValidNumber, [Caption, AData]), mtError, [mbOK], 0);
    Exit;
  end;
  if (I < MinValue) or (I > MaxValue) then
    JvDSADialogs.MessageDlg(Format(SErrParameterMustBeBetween, [Caption, AData, IntToStr(MinValue), IntToStr(MaxValue)]), mtError,
      [mbOK], 0)
  else
    Result := true;
end;

//=== TJvDoubleEditParameter =================================================

constructor TJvDoubleEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required  := true;
  MinValue  := -1E38;
  MaxValue  := 1E38;
  Increment := 100;
end;

procedure TJvDoubleEditParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpSpin: IJvDynControlSpin;
begin
  WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  if (EditorType = netCalculate) and DynControlEngine.IsControlTypeRegistered(jctCalculateEdit) then
    WinControl := DynControlEngine.CreateCalculateControl(Self, AParameterParent, GetParameterName)
  else if (EditorType = netSpin) and DynControlEngine.IsControlTypeRegistered(jctSpinEdit) then
    WinControl := DynControlEngine.CreateSpinControl(Self, AParameterParent, GetParameterName)
  else
    WinControl := DynControlEngine.CreateEditControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlSpin, ITmpSpin) then
    with ITmpSpin do
    begin
      ControlSetIncrement(Increment);
      ControlSetMinValue(MinValue);
      ControlSetMaxValue(MaxValue);
      ControlSetUseForInteger(false);
    end;
 //  MaskEdit.PasswordChar := PasswordChar;
 //  MaskEdit.EditMask := EditMask;
 //  MaskEdit.EditText := AsString;
end;


procedure TJvDoubleEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  MinValue := TJvDoubleEditParameter(Source).MinValue;
  MaxValue := TJvDoubleEditParameter(Source).MaxValue;
end;

function TJvDoubleEditParameter.Validate(var AData: variant): boolean;
var
  D: double;
begin
  if not Enabled then
  begin
    Result := true;
    Exit;
  end;
  Result := false;
  if VarIsNull(AData) then
  begin
    if Required then
      JvDSADialogs.MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0)
    else
      Result := true;
    Exit;
  end;
  try
    D := AData;
  except
    JvDSADialogs.MessageDlg(Format(SErrParameterIsNotAValidNumber, [Caption, AData]), mtError, [mbOK], 0);
    Exit;
  end;
  if (D < MinValue) or (D > MaxValue) then
    JvDSADialogs.MessageDlg(Format(SErrParameterMustBeBetween, [Caption, AData, FloatToStr(MinValue), FloatToStr(MaxValue)]), mtError,
      [mbOK], 0)
  else
    Result := true;
end;

//=== TJvFileNameParameter ===================================================

procedure TJvFileNameParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  DialogKind  := TJvFileNameParameter(Source).DialogKind;
  DefaultExt  := TJvFileNameParameter(Source).DefaultExt;
  Filter      := TJvFileNameParameter(Source).Filter;
  FilterIndex := TJvFileNameParameter(Source).FilterIndex;
  InitialDir  := TJvFileNameParameter(Source).InitialDir;
  DialogOptions := TJvFileNameParameter(Source).DialogOptions;
  DialogTitle := TJvFileNameParameter(Source).DialogTitle;
end;

function TJvFileNameParameter.GetParameterNameExt: string;
begin
  Result := 'FileNameEdit';
end;

procedure TJvFileNameParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpControlFileName: IJvDynControlFileName;
begin
  WinControl := DynControlEngine.CreateFileNameControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlFileName, ITmpControlFileName) then
    with ITmpControlFileName do
    begin
      ControlSetDialogKind(DialogKind);
      ControlSetDefaultExt(DefaultExt);
      ControlSetFilter(Filter);
      ControlSetFilterIndex(FilterIndex);
      ControlSetInitialDir(InitialDir);
      ControlSetDialogOptions(DialogOptions);
      ControlSetDialogTitle(DialogTitle);
    end;
end;

function TJvFileNameParameter.Validate(var AData: variant): boolean;
begin
  Result := not Enabled;
  if Result then
    Exit;
  AData := Trim(AData);
  if AData = DefaultExt then
    AData := '';
  if Required then
    if AData = '' then
    begin
      JvDSADialogs.MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end;
  if AData <> '' then
    if ExtractFileExt(AData) = '' then
      if DefaultExt <> '' then
        if DefaultExt[1] = '.' then
          AData := AData + DefaultExt
        else
          AData := AData + '.' + DefaultExt;
  if ofFileMustExist in DialogOptions then
    if not FileExists(AData) then
    begin
      JvDSADialogs.MessageDlg(Format(SErrParameterFileDoesNotExist, [Caption, AData]), mtError, [mbOK], 0);
      Exit;
    end;
  if ofOverwritePrompt in DialogOptions then
    if FileExists(AData) then
      if JvDSADialogs.MessageDlg(Format(SErrParameterFileExistOverwrite, [Caption, AData]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
  if ofPathMustExist in DialogOptions then
    if ExtractFilePath(AData) <> '' then
      if not DirectoryExists(ExtractFilePath(AData)) then
      begin
        JvDSADialogs.MessageDlg(Format(SErrParameterDirectoryNotExist, [Caption, ExtractFilePath(AData)]), mtError, [mbOK], 0);
        Exit;
      end;
  Result := true;
end;

//=== TJvDirectoryParameter ==================================================

procedure TJvDirectoryParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  InitialDir    := TJvDirectoryParameter(Source).InitialDir;
  DialogOptions := TJvDirectoryParameter(Source).DialogOptions;
  DialogTitle   := TJvDirectoryParameter(Source).DialogTitle;
end;

function TJvDirectoryParameter.GetParameterNameExt: string;
begin
  Result := 'DirectoryEdit';
end;

procedure TJvDirectoryParameter.CreateWinControl(AParameterParent: TWinControl);
var
  ITmpControlDirectory: IJvDynControlDirectory;
begin
  WinControl := DynControlEngine.CreateDirectoryControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlDirectory, ITmpControlDirectory) then
    with ITmpControlDirectory do
    begin
      ControlSetDialogTitle(DialogTitle);
      ControlSetDialogOptions(DialogOptions);
      ControlSetInitialDir(InitialDir);
    end;
end;

function TJvDirectoryParameter.Validate(var AData: variant): boolean;
begin
  Result := not Enabled;
  if Result then
    Exit;
  AData := Trim(AData);
  if Required then
    if AData = '' then
    begin
      JvDSADialogs.MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end;
  if not DirectoryExists(AData) then
    if not (sdAllowCreate in DialogOptions) then
    begin
      JvDSADialogs.MessageDlg(Format(SErrParameterDirectoryNotExist, [Caption, AData]), mtError, [mbOK], 0);
      Exit;
    end;
  Result := true;
end;

///=== TJvMemoParameter ======================================================

constructor TJvMemoParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ScrollBars  := ssNone;
  WantTabs    := false;
  WantReturns := true;
  WordWrap    := false;
end;

destructor TJvMemoParameter.Destroy;
begin
  inherited Destroy;
end;

procedure TJvMemoParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

function TJvMemoParameter.GetParameterNameExt: string;
begin
  Result := 'Memo';
end;

procedure TJvMemoParameter.CreateWinControl(AParameterParent: TWinControl);
var
  Memo: TWinControl;
begin
  WinControl := DynControlEngine.CreateMemoControl(Self, AParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  with IJvDynControlMemo(IntfCast(WinControl, IJvDynControlMemo)) do
  begin
    ControlSetWantTabs(WantTabs);
    ControlSetWantReturns(WantReturns);
    ControlSetWordWrap(WordWrap);
    ControlSetScrollbars(Scrollbars);
  end;
end;

procedure TJvMemoParameter.GetData;
begin
  inherited GetData;
end;

procedure TJvMemoParameter.SetData;
begin
  inherited SetData;
end;

end.
