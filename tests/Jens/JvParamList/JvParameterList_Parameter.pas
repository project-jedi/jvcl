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

uses Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms, Controls,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  FileCtrl, Dialogs, ComCtrls,
  JvPanel, JvPropertyStore, JvParameterList, jvDynControlEngine,
  JvDynControlEngine_Interface;

type
  TJvNoDataParameter = class (tJvBaseParameter)
  private
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
    function Validate(var fData: variant): boolean; override;
  end;

  tJvParameterLabelArrangeMode = (lamBefore, lamAbove);

  TJvBasePanelEditParameter = class (tJvBaseParameter)
  private
    fLabelControl: tControl;
    fFramePanel: tWinControl;
    fLabelArrangeMode: tJvParameterLabelArrangeMode;
    fLabelWidth: integer;
    fEditWidth: integer;
    fRightSpace: integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ArrangeLabelAndWinControlOnPanel; virtual;
    procedure CreateLabelControl(aParameterParent: TWinControl); virtual;
    procedure CreateFramePanel(aParameterParent: TWinControl); virtual;
    procedure CreateWinControl(aParameterParent: TWinControl); virtual; abstract;
    property LabelControl: TControl Read fLabelControl Write fLabelControl;
    property FramePanel: tWinControl Read fFramePanel Write fFramePanel;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure SetEnabled(Value: boolean); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property LabelArrangeMode: tJvParameterLabelArrangeMode Read fLabelArrangeMode Write fLabelArrangeMode;
    property LabelWidth: integer Read fLabelWidth Write fLabelWidth;
    property EditWidth: integer Read fEditWidth Write fEditWidth;
    property RightSpace: integer Read fRightSpace Write fRightSpace;
  end;

  TJvArrangeParameter = class (TJvNoDataParameter)
  private
    fArrangeSettings: tJvArrangeSettings;
  protected
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ArrangeSettings: TJvArrangeSettings Read FArrangeSettings Write SetArrangeSettings;
    property Color;
  end;

  TJvPanelParameter = class (TJvArrangeParameter)
  private
    fBevelInner: TPanelBevel;
    fBevelOuter: TPanelBevel;
    fBevelWidth: integer;
    fBorderStyle: TBorderStyle;
    fBorderWidth: integer;
  protected
    function GetParameterNameExt: string; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property BevelInner: TPanelBevel Read fBevelInner Write fBevelInner;
    property BevelOuter: TPanelBevel Read fBevelOuter Write fBevelOuter;
    property BevelWidth: integer Read fBevelWidth Write fBevelWidth;
    property BorderStyle: TBorderStyle Read fBorderStyle Write fBorderStyle;
    property BorderWidth: integer Read fBorderWidth Write fBorderWidth;
  end;

  TJvGroupBoxParameter = class (TJvArrangeParameter)
  private
  protected
    function GetParameterNameExt: string; override;
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvImageParameter = class (TJvBasePanelEditParameter)
  private
    fAutoSize: boolean;
    fCenter: boolean;
    fIncrementalDisplay: boolean;
    fTransparent: boolean;
    fStretch: boolean;
    fPicture: TPicture;
  protected
    procedure SetPicture(Value: TPicture);
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize: boolean Read fAutoSize Write fAutoSize;
    property Center: boolean Read fCenter Write fCenter;
    property IncrementalDisplay: boolean Read fIncrementalDisplay Write fIncrementalDisplay;
    property Transparent: boolean Read fTransparent Write fTransparent;
    property Stretch: boolean Read fStretch Write fStretch;
    property Picture: TPicture Read fPicture Write SetPicture;
  end;

  TJvLabelParameter = class (TJvNoDataParameter)
  private
  protected
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvCheckboxParameter = class (tJvBaseParameter)
  private
  protected
  public
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  end;

  TJvEditParameter = class (TJvBasePanelEditParameter)
  private
    fEditMask: string;
    fPasswordChar: char;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;

  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property EditMask: string Read fEditMask Write fEditMask;
    property PasswordChar: char Read fPasswordChar Write fPasswordChar;
  end;

  TJvIntegerEditParameter = class (TJvEditParameter)
  private
    fMinValue: integer;
    fMaxValue: integer;
  protected
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var fData: variant): boolean; override;
  published
    property MinValue: integer Read fMinValue Write fMinValue;
    property MaxValue: integer Read fMaxValue Write fMaxValue;
  end;

  TJvDoubleEditParameter = class (TJvEditParameter)
  private
    fMinValue: double;
    fMaxValue: double;
  protected
  public
    constructor Create(AParameterList: TJvParameterList); override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var fData: variant): boolean; override;
  published
    property MinValue: double Read fMinValue Write fMinValue;
    property MaxValue: double Read fMaxValue Write fMaxValue;
  end;


  TJvFileNameParameter = class (TJvBasePanelEditParameter)
  private
    fDefaultExt: string;
    fFilter: string;
    fFilterIndex: integer;
    fInitialDir: string;
    fDialogOptions: TOpenOptions;
    fDialogTitle: string;
    fDialogKind: TJvDynControlFileNameDialogKind;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
    function Validate(var fData: variant): boolean; override;
  published
    property FileName: string Read GetAsString Write SetAsString;
    property DefaultExt: string Read fDefaultExt Write fDefaultExt;
    property Filter: string Read fFilter Write fFilter;
    property FilterIndex: integer Read fFilterIndex Write fFilterIndex;
    property InitialDir: string Read fInitialDir Write fInitialDir;
    property DialogOptions: TOpenOptions Read fDialogOptions Write fDialogOptions;
    property DialogTitle: string Read fDialogTitle Write fDialogTitle;
    property DialogKind: TJvDynControlFileNameDialogKind Read fDialogKind Write fDialogKind;
  end;

  TJvDirectoryParameter = class (TJvBasePanelEditParameter)
  private
    fInitialDir: string;
    fDialogTitle: string;
    fDialogOptions: TSelectDirOpts;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
    function Validate(var fData: variant): boolean; override;
  published
    property Directory: string Read GetAsString Write SetAsString;
    property InitialDir: string Read fInitialDir Write fInitialDir;
    property DialogTitle: string Read fDialogTitle Write fDialogTitle;
    property DialogOptions: TSelectDirOpts Read fDialogOptions Write fDialogOptions;
  end;


  TJvListParameter = class (TJvBasePanelEditParameter)
  private
    fItemList: TStringList;
    fItemIndex: integer;
    fSorted: boolean;
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
    property ItemList: TStringList Read fItemList Write SetItemList;
    property ItemIndex: integer Read fItemIndex Write SetItemIndex;
    property Sorted: boolean Read fSorted Write fSorted;
  end;

  TJvRadioGroupParameter = class (TJvListParameter)
  private
    fColumns: integer;
  protected
  public
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); override;
  published
    property Columns: integer Read fColumns Write fColumns;
  end;

  TJvComboBoxParameterStyle = (cpsListEdit, cpsListFixed);

  TJvComboBoxParameter = class (TJvListParameter)
  private
    fSorted: boolean;
    fNewEntriesAllowed: boolean;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;

  public
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: boolean Read fSorted Write fSorted;
    property NewEntriesAllowed: boolean Read fNewEntriesAllowed Write fNewEntriesAllowed;
  end;

  TJvListBoxParameter = class (TJvListParameter)
  private
    fsorted: boolean;
  protected
    function GetParameterNameExt: string; override;
    function GetWinControlData: variant; override;
    procedure SetWinControlData(Value: variant); override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;

  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Sorted: boolean Read fSorted Write fSorted;
  end;

  TJvDateTimeParameter = class (TJvBasePanelEditParameter)
  private
    fCalAlignment: TDTCalAlignment;
    fDateFormat: TDTDateFormat;
    fDateMode: TDTDateMode;
    fKind: TDateTimeKind;
    fMaxDate: TDate;
    fMinDate: TDate;
  protected
    function GetParameterNameExt: string; override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CalAlignment: TDTCalAlignment Read fCalAlignment Write fCalAlignment;
    property DateFormat: TDTDateFormat Read fDateFormat Write fDateFormat;
    property DateMode: TDTDateMode Read fDateMode Write fDateMode;
    property Kind: TDateTimeKind Read fKind Write fKind;
    property MaxDate: TDate Read fMaxDate Write fMaxDate;
    property MinDate: TDate Read fMinDate Write fMinDate;
  end;

  TJvMemoParameter = class (TJvBasePanelEditParameter)
  private
    fWordWrap: boolean;
    fWantTabs: boolean;
    fWantReturns: boolean;
    fScrollBars: TScrollStyle;
    fFontName: string;
  protected
    function GetParameterNameExt: string; override;
  public
    constructor Create(AParameterList: TJvParameterList); override;
    destructor Destroy; override;
    procedure GetData; override;
    procedure SetData; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWinControl(aParameterParent: TWinControl); override;
  published
    property WordWrap: boolean Read fWordWrap Write fWordWrap;
    property WantTabs: boolean Read fWantTabs Write fWantTabs;
    property WantReturns: boolean Read fWantReturns Write fWantReturns;
    property ScrollBars: TScrollStyle Read fScrollBars Write fScrollBars;
    property FontName: string Read fFontName Write fFontName;
  end;


implementation

resourcestring
  ErrParameterMustBeEntered      = 'Parameter %s must be entered!';
  ErrParameterIsNotAValidNumber  = 'Parameter %s : %s is not a valid number-value!';
  ErrParameterMustBeBetween      = 'Parameter %s : %s must be between %s and %s!';
  ErrParameterFileDoesNotExist   = 'Parameter %s : The file "%s" does not exist!';
  ErrParameterFileExistOverwrite = 'Parameter %s : The file "%s" exists! Overwrite ?';
  ErrParameterDirectoryNotExist  = 'Parameter %s : The directory "%s" does not exist!';

 {*****************************************************************************}
 {* TJvNoDataParameter                                                           *}
 {*****************************************************************************}

constructor TJvNoDataParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  ReloadValuefromRegistry := false;
end;   {*** Constructor TJvNoDataParameter.Create ***}

function TJvNoDataParameter.Validate(var fData: variant): boolean;
begin
  Result := true;
end;   {*** Function TJvNoDataParameter.Validate ***}


 {*****************************************************************************}
 {* TJvBasePanelEditParameter                                                           *}
 {*****************************************************************************}

constructor TJvBasePanelEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  fLabelArrangeMode := lamAbove;
  fLabelWidth := 0;
  fEditWidth  := 0;
  fRightSpace := 0;
end;   {*** Constructor TJvNoDataParameter.Create ***}

procedure TJvBasePanelEditParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  CreateFramePanel(ParameterParent);

  CreateWinControl(FramePanel);

  CreateLabelControl(FramePanel);

  ArrangeLabelAndWinControlOnPanel;
end;   {*** Procedure TJvBasePanelEditParameter.CreateWinControlOnParent ***}

procedure TJvBasePanelEditParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = fFramePanel) and (Operation = opRemove) then
    fFramePanel := nil;
  if (AComponent = fLabelControl) and (Operation = opRemove) then
    fLabelControl := nil;
end;   {*** procedure tBaseOraToolForm.Notification ***}

procedure TJvBasePanelEditParameter.CreateFramePanel(aParameterParent: TWinControl);
begin
  FramePanel := DynControlEngine.CreatePanelControl(Self, aParameterParent, GetParameterName + 'Panel', '', alNone);
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

procedure TJvBasePanelEditParameter.CreateLabelControl(aParameterParent: TWinControl);
begin
  if Caption = '' then
    Exit;
  LabelControl := DynControlEngine.CreateLabelControl(Self, aParameterParent, GetParameterName + 'Label', Caption, WinControl);
  LabelControl.Visible := true;
  LabelControl.Enabled := Enabled;
  LabelControl.Parent := aParameterParent;
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
    LabelControl.Top := WinControl.Top + ROUND((WinControl.Height - LabelControl.Height) / 2);
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

 {*****************************************************************************}
 {* TJvLabelParameter                                                           *}
 {*****************************************************************************}

procedure TJvLabelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateStaticTextControl(Self, ParameterParent, GetParameterName, Caption);
end;   {*** Procedure TJvLabelParameter.CreateWinControlOnParent ***}

 {*****************************************************************************}
 {* TJvImageParameter                                                           *}
 {*****************************************************************************}

constructor TJvImageParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  fPicture    := TPicture.Create;
  AutoSize    := false;
  Center      := false;
  IncrementalDisplay := false;
  Stretch     := false;
  Transparent := false;
end;   {*** Constructor TJvImageParameter.Create ***}

destructor TJvImageParameter.Destroy;
begin
  fPicture.Free;
  inherited Destroy;
end;   {*** Destructor TJvImageParameter.Destroy ***}

procedure TJvImageParameter.SetPicture(Value: TPicture);
begin
  fPicture.Assign(Value);
end;  {*** procedure TJvImageParameter.SetPicture ***}

procedure TJvImageParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Picture     := TJvImageParameter(Source).Picture;
  AutoSize    := TJvImageParameter(Source).AutoSize;
  Center      := TJvImageParameter(Source).Center;
  IncrementalDisplay := TJvImageParameter(Source).IncrementalDisplay;
  Stretch     := TJvImageParameter(Source).Stretch;
  Transparent := TJvImageParameter(Source).Transparent;
end;   {*** Procedure TJvImageParameter.Assign ***}

function TJvImageParameter.GetParameterNameExt: string;
begin
  Result := 'Image';
end;   {*** function TJvImageParameter.GetParameterNameExt ****}

procedure TJvImageParameter.CreateWinControl(aParameterParent: TWinControl);
var
  ITmpImage: IJvDynControlImage;
begin
  WinControl := DynControlEngine.CreateImageControl(Self, aParameterParent, GetParameterName);
  if Supports(WinControl, IJvDynControlImage, ITmpImage) then
    with ITmpImage do
    begin
      ControlSetPicture(Picture);
      ControlSetAutoSize(AutoSize);
      ControlSetIncrementalDisplay(IncrementalDisplay);
      ControlSetCenter(Center);
      ControlSetStretch(Stretch);
      ControlSetTransparent(Transparent);
    end;
end;

 {*****************************************************************************}
 {* TJvArrangeParameter                                                           *}
 {*****************************************************************************}

constructor TJvArrangeParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  fArrangeSettings := tJvArrangeSettings.Create(nil);
end;   {*** Constructor TJvArrangeParameter.Create ***}

destructor TJvArrangeParameter.Destroy;
begin
  fArrangeSettings.Free;
  inherited Destroy;
end;   {*** destructor TJvArrangeParameter.Destroy ***}

procedure TJvArrangeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;   {*** Procedure TJvArrangeParameter.Assign ***}

procedure TJvArrangeParameter.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  fArrangeSettings.Assign(Value);
end;   {*** Procedure TJvArrangeParameter.SetArrangeSettings ***}


 {*****************************************************************************}
 {* TJvPanelParameter                                                           *}
 {*****************************************************************************}

constructor TJvPanelParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  bevelInner  := bvNone;
  bevelOuter  := bvNone;
  BevelWidth  := 1;
  BorderStyle := bsNone;
  BorderWidth := 0;
end;   {*** Constructor TJvPanelParameter.Create ***}

procedure TJvPanelParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  bevelInner := TJvPanelParameter(Source).bevelInner;
  bevelOuter := TJvPanelParameter(Source).bevelOuter;
end;   {*** Procedure TJvPanelParameter.Assign ***}

function TJvPanelParameter.GetParameterNameExt: string;
begin
  Result := 'Panel';
end;   {*** function TJvPanelParameter.GetParameterNameExt ****}

procedure TJvPanelParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  iTmpPanel: IJvDynControlPanel;
begin
  WinControl := DynControlEngine.CreatePanelControl(Self, ParameterParent, GetParameterName, Caption, alNone);
  if Supports(WinControl, IJvDynControlPanel, iTmpPanel) then
    iTmpPanel.ControlSetBorder(BevelInner, BevelOuter, BevelWidth, BorderStyle, BorderWidth);
end;   {*** Procedure TJvPanelParameter.CreateWinControlOnParent ***}

 {*****************************************************************************}
 {* TJvGroupBoxParameter                                                        *}
 {*****************************************************************************}

function TJvGroupBoxParameter.GetParameterNameExt: string;
begin
  Result := 'GroupBoxPanel';
end;   {*** function TJvGroupBoxParameter.GetParameterNameExt ****}


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
  Panel := tJvPanel.Create(ParameterParent.Owner);
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
end;   {*** Procedure TJvGroupBoxParameter.CreateWinControlOnParent ***}


 {*****************************************************************************}
 {* TJvListParameter                                                            *}
 {*****************************************************************************}

constructor TJvListParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  fItemList := TStringList.Create;
  Sorted    := false;
end;

destructor TJvListParameter.Destroy;
begin
  fItemList.Free;
  inherited Destroy;
end;

procedure TJvListParameter.SetAsString(Value: string);
var
  i: integer;
begin
  i := ItemList.IndexOf(Value);
  if (i >= 0) and (i < ItemList.Count) then
    ItemIndex := i;
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
  fItemList.Assign(Value);
  if not Assigned(Value) then
    Exit;
  if (ItemIndex >= 0) and (ItemIndex < ItemList.Count) then
    AsVariant := ItemList[ItemIndex];
end;   {*** Procedure tJvBaseParameter.SetAsDate ***}

procedure TJvListParameter.SetItemIndex(Value: integer);
begin
  if Value >= ItemList.Count then
    fItemIndex := ItemList.Count - 1
  else
    fItemIndex := Value;
  if (Value >= 0) and (Value < ItemList.Count) then
    AsVariant := ItemList[Value];
end;   {*** Procedure tJvBaseParameter.SetAsDate ***}

procedure TJvListParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  ItemList.Assign(TJvListParameter(Source).Itemlist);
  ItemIndex := TJvListParameter(Source).ItemIndex;
  Sorted    := TJvListParameter(Source).Sorted;
end;   {*** Procedure TJvListParameter.SetAsDate ***}

procedure TJvListParameter.SearchItemIndex(Search: string);
var
  i: integer;
begin
  fItemIndex := -1;
  for i := 0 to ItemList.Count - 1 do
    if Search = ItemList.Strings[i] then
    begin
      fItemIndex := i;
      break;
    end;   {*** IF Search = ItemList.Strings[i] THEN ***}
end;   {*** Procedure TJvListParameter.SearchItemIndex ***}

procedure TJvListParameter.GetData;
begin
  inherited GetData;
  if Assigned(WinControl) then
    ItemIndex := ItemList.IndexOf(AsString)
  else
    ItemIndex := -1;
end;   {*** procedure TJvListParameter.GetData ***}

procedure TJvListParameter.SetData;
begin
  inherited SetData;
 //  IF Assigned (
 //  IF Assigned (WinControl) THEN
 //    ItemList.IndexOf (AsString) := ItemIndex;
end;   {*** procedure TJvListParameter.SetData ***}


 {*****************************************************************************}
 {* TJvRadioGroupParameter                                                      *}
 {*****************************************************************************}


procedure TJvRadioGroupParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Columns := TJvRadioGroupParameter(Source).Columns;
end;   {*** Procedure TJvRadioGroupParameter.SetAsDate ***}

procedure TJvRadioGroupParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
var
  iTmpRadioGroup: IJvDynControlRadioGroup;
begin
  WinControl := DynControlEngine.CreateRadioGroupControl(Self, ParameterParent, GetParameterName, Caption, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlRadioGroup, iTmpRadioGroup) then
    iTmpRadioGroup.ControlSetColumns(Columns);
end;   {*** Procedure TJvRadioGroupParameter.CreateWinControlOnParent ***}


 {*****************************************************************************}
 {* TJvCheckBoxParameter                                                           *}
 {*****************************************************************************}

procedure TJvCheckBoxParameter.CreateWinControlOnParent(ParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateCheckBoxControl(Self, ParameterParent, GetParameterName, Caption);
  JvDynControl.ControlSetOnClick(HandleEnableDisable);
end;   {*** Procedure TJvCheckBoxParameter.CreateWinControlOnParent ***}


 {*****************************************************************************}
 {* TJvComboBoxParameter                                                      *}
 {*****************************************************************************}

procedure TJvComboBoxParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Sorted := TJvComboBoxParameter(Source).Sorted;
  NewEntriesAllowed := TJvComboBoxParameter(Source).NewEntriesAllowed;
end;   {*** Procedure TJvComboBoxParameter.SetAsDate ***}

function TJvComboBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ComboBox';
end;   {*** function TJvComboBoxParameter.GetParameterNameExt ****}

procedure TJvComboBoxParameter.GetData;
begin
  Value := NULL;
  if Assigned(WinControl) then
    Value := WinControlData;
end;

procedure TJvComboBoxParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := Value;
end;

procedure TJvComboBoxParameter.CreateWinControl(aParameterParent: TWinControl);
var
  ITmpComboBox: IJvDynControlComboBox;
  ITmpItems:    IJvDynControlItems;
begin
  WinControl := DynControlEngine.CreateComboBoxControl(Self, aParameterParent, GetParameterName, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlComboBox, ITmpComboBox) then
    ITmpComboBox.ControlSetNewEntriesAllowed(NewEntriesAllowed);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);

end;

 {*****************************************************************************}
 {* TJvListBoxParameter                                                      *}
 {*****************************************************************************}

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
end;   {*** Procedure TJvListBoxParameter.SetAsDate ***}

function TJvListBoxParameter.GetParameterNameExt: string;
begin
  Result := 'ListBox';
end;   {*** function TJvListBoxParameter.GetParameterNameExt ****}

procedure TJvListBoxParameter.CreateWinControl(aParameterParent: TWinControl);
var
  ITmpItems: IJvDynControlItems;
begin
  WinControl := DynControlEngine.CreateListBoxControl(Self, aParameterParent, GetParameterName, ItemList);
  JvDynControlData.ControlSetOnChange(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlItems, ITmpItems) then
    ITmpItems.ControlSetSorted(Sorted);
end;


function TJvListBoxParameter.GetWinControlData: variant;
var
  i: integer;
begin
  Result := inherited GetWinControlData;
end;   {*** function TJvListBoxParameter.GetWinControlData ***}

procedure TJvListBoxParameter.SetWinControlData(Value: variant);
begin
  inherited SetWinControlData(Value);
end;   {*** procedure TJvListBoxParameter.SetWinControlData ***}


 {*****************************************************************************}
 {* TJvDateTimeParameter                                                      *}
 {*****************************************************************************}

procedure TJvDateTimeParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  CalAlignment := TJvDateTimeParameter(Source).CalAlignment;
  DateFormat := TJvDateTimeParameter(Source).DateFormat;
  DateMode := TJvDateTimeParameter(Source).DateMode;
  Kind    := TJvDateTimeParameter(Source).Kind;
  MaxDate := TJvDateTimeParameter(Source).maxDate;
  MinDate := TJvDateTimeParameter(Source).MinDate;
end;   {*** Procedure TJvDateTimeParameter.SetAsDate ***}

function TJvDateTimeParameter.GetParameterNameExt: string;
begin
  Result := 'DateTime';
end;   {*** function TJvDateTimeParameter.GetParameterNameExt ****}


procedure TJvDateTimeParameter.CreateWinControl(aParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateDateTimeControl(Self, aParameterParent, GetParameterName);
 //  DateTime.CalAlignment:= CalAlignment;
 //  DateTime.DateFormat := DateFormat;
 //  DateTime.DateMode:= DateMode;
 //  DateTime.Kind:= Kind;
 //  DateTime.MaxDate:= maxDate;
 //  DateTime.MinDate:= MinDate;
end;



 {*****************************************************************************}
 {* TJvEditParameter                                                            *}
 {*****************************************************************************}
constructor TJvEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  fPasswordChar := #0;
  fEditMask     := '';
  fLabelWidth   := 0;
  fEditWidth    := 0;
  fLabelArrangeMode := lamAbove;
  fRightSpace   := 0;
end;   {*** Constructor tJvBaseParameter.Create ***}

procedure TJvEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  EditMask     := TJvEditParameter(Source).EditMask;
  PasswordChar := TJvEditParameter(Source).PasswordChar;
  LabelWidth   := TJvEditParameter(Source).LabelWidth;
  EditWidth    := TJvEditParameter(Source).EditWidth;
  LabelArrangeMode := TJvEditParameter(Source).LabelArrangeMode;
  RightSpace   := TJvEditParameter(Source).RightSpace;
end;   {*** Procedure TJvEditParameter.SetAsDate ***}

function TJvEditParameter.GetParameterNameExt: string;
begin
  Result := 'MaskEdit';
end;   {*** function TJvEditParameter.GetParameterNameExt ****}


procedure TJvEditParameter.CreateWinControl(aParameterParent: TWinControl);
begin
  WinControl := DynControlEngine.CreateEditControl(Self, aParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
 //  MaskEdit.PasswordChar := PasswordChar;
 //  MaskEdit.EditMask := EditMask;
 //  MaskEdit.EditText := AsString;
end;



 {*****************************************************************************}
 {* TJvIntegerEditParameter                                                            *}
 {*****************************************************************************}

constructor TJvIntegerEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := true;
  MinValue := low(integer);
  MaxValue := High(integer);
end;

procedure TJvIntegerEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  MinValue := TJvIntegerEditParameter(Source).MinValue;
  MaxValue := TJvIntegerEditParameter(Source).MaxValue;
end;   {*** procedure TJvIntegerEditParameter.Assign***}

function TJvIntegerEditParameter.Validate(var fData: variant): boolean;
var
  i: integer;
begin
  if not Enabled then
  begin
    Result := true;
    Exit;
  end;   {*** IF NOT Enabled THEN ***}
  Result := false;
  if VarIsNull(fData) or (fData = '') then
    if Required then
    begin
      MessageDlg(Format(ErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      exit;
    end   {*** IF Required AND (s = '') THEN ***}
    else
    begin
      Result := true;
      Exit;
    end;
  try
    i := fData;
  except
    MessageDlg(Format(ErrParameterIsNotAValidNumber, [Caption, fData]), mtError, [mbOK], 0);
    exit;
  end;
  if (i < MinValue) or (i > MaxValue) then
    MessageDlg(Format(ErrParameterMustBeBetween, [Caption, fData, IntToStr(MinValue), IntToStr(MaxValue)]), mtError, [mbOK], 0)
  else
    Result := true;
end;   {*** function TJvIntegerEditParameter.Validate ***}

 {*****************************************************************************}
 {* TJvDoubleEditParameter                                                            *}
 {*****************************************************************************}

constructor TJvDoubleEditParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  Required := true;
  MinValue := -1E10;
  MaxValue := 1E10;
end;

procedure TJvDoubleEditParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  MinValue := TJvDoubleEditParameter(Source).MinValue;
  MaxValue := TJvDoubleEditParameter(Source).MaxValue;
end;   {*** procedure TJvDoubleEditParameter.Assign***}

function TJvDoubleEditParameter.Validate(var fData: variant): boolean;
var
  i: double;
begin
  if not Enabled then
  begin
    Result := true;
    Exit;
  end;   {*** IF NOT Enabled THEN ***}
  Result := false;
  if VarIsNull(fData) then
    if Required then
    begin
      MessageDlg(Format(ErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      exit;
    end   {*** IF Required AND (s = '') THEN ***}
    else
    begin
      Result := true;
      Exit;
    end;
  try
    i := fData;
  except
    MessageDlg(Format(ErrParameterIsNotAValidNumber, [Caption, fData]), mtError, [mbOK], 0);
    exit;
  end;
  if (i < MinValue) or (i > MaxValue) then
    MessageDlg(Format(ErrParameterMustBeBetween, [Caption, fData, FloatToStr(MinValue), FloatToStr(MaxValue)]), mtError, [mbOK], 0)
  else
    Result := true;
end;   {*** function TJvDoubleEditParameter.Validate ***}


 {*****************************************************************************}
 {* tJvFileNameParameter                                                            *}
 {*****************************************************************************}

procedure tJvFileNameParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  DialogKind  := tJvFileNameParameter(Source).DialogKind;
  DefaultExt  := tJvFileNameParameter(Source).DefaultExt;
  Filter      := tJvFileNameParameter(Source).Filter;
  FilterIndex := tJvFileNameParameter(Source).FilterIndex;
  InitialDir  := tJvFileNameParameter(Source).InitialDir;
  DialogOptions := tJvFileNameParameter(Source).DialogOptions;
  DialogTitle := tJvFileNameParameter(Source).DialogTitle;
end;   {*** Procedure tJvFileNameParameter.SetAsDate ***}

function tJvFileNameParameter.GetParameterNameExt: string;
begin
  Result := 'FileNameEdit';
end;   {*** function tJvFileNameParameter.GetParameterNameExt ****}

procedure tJvFileNameParameter.CreateWinControl(aParameterParent: TWinControl);
var
  ITmpControlFileName: IJvDynControlFileName;
begin
  WinControl := DynControlEngine.CreateFileNameControl(Self, aParameterParent, GetParameterName);
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

function tJvFileNameParameter.Validate(var fData: variant): boolean;
begin
  if not Enabled then
  begin
    Result := true;
    Exit;
  end;   {*** IF NOT Enabled THEN ***}
  Result := false;
  fData  := trim(fData);
  if fData = DefaultExt then
    fData := '';
  if Required then
    if fData = '' then
    begin
      MessageDlg(Format(ErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end;   {*** IF NOT Result THEN ***}
  if fData <> '' then
    if ExtractFileExt(fData) = '' then
      if DefaultExt <> '' then
        if DefaultExt[1] = '.' then
          fData := fData + DefaultExt
        else
          fData := fData + '.' + DefaultExt;
  if ofFileMustExist in DialogOptions then
    if not FileExists(fData) then
    begin
      MessageDlg(Format(ErrParameterFileDoesNotExist, [Caption, fData]), mtError, [mbOK], 0);
      Exit;
    end;   {*** IF NOT Result THEN ***}
  if ofOverwritePrompt in DialogOptions then
    if FileExists(fData) then
    begin
      if MessageDlg(Format(ErrParameterFileExistOverwrite, [Caption, fData]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
    end;   {*** IF NOT Result THEN ***}
  if ofPathMustExist in DialogOptions then
  begin
    if ExtractFilePath(fData) <> '' then
      if not DirectoryExists(ExtractFilePath(fData)) then
      begin
        MessageDlg(Format(ErrParameterDirectoryNotExist, [Caption, ExtractFilePath(fData)]), mtError, [mbOK], 0);
        Exit;
      end;   {*** IF NOT Result THEN ***}
  end;   {*** IF ofPathMustExist THEN ***}
  Result := true;
end;  {*** function tJvFileNameParameter.Validate ***}

 {*****************************************************************************}
 {* tJvDirectoryParameter                                                            *}
 {*****************************************************************************}

procedure tJvDirectoryParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  InitialDir    := tJvDirectoryParameter(Source).InitialDir;
  DialogOptions := tJvDirectoryParameter(Source).DialogOptions;
  DialogTitle   := tJvDirectoryParameter(Source).DialogTitle;
end;   {*** Procedure tJvDirectoryParameter.SetAsDate ***}

function tJvDirectoryParameter.GetParameterNameExt: string;
begin
  Result := 'DirectoryEdit';
end;   {*** function tJvDirectoryParameter.GetParameterNameExt ****}

procedure tJvDirectoryParameter.CreateWinControl(aParameterParent: TWinControl);
var
  ITmpControlDirectory: IJvDynControlDirectory;
begin
  WinControl := DynControlEngine.CreateDirectoryControl(Self, aParameterParent, GetParameterName);
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
  if Supports(WinControl, IJvDynControlDirectory, ITmpControlDirectory) then
    with ITmpControlDirectory do
    begin
      ControlSetDialogTitle(DialogTitle);
      ControlSetDialogOptions(DialogOptions);
      ControlSetInitialDir(InitialDir);
    end;
end;

function tJvDirectoryParameter.Validate(var fData: variant): boolean;
begin
  if not Enabled then
  begin
    Result := true;
    Exit;
  end;   {*** IF NOT Enabled THEN ***}
  Result := false;
  fData  := trim(fData);
  if Required then
    if fData = '' then
    begin
      MessageDlg(Format(ErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
      Exit;
    end;   {*** IF NOT Result THEN ***}
  if not DirectoryExists(fData) then
    if not (sdAllowCreate in DialogOptions) then
    begin
      MessageDlg(Format(ErrParameterDirectoryNotExist, [Caption, ExtractFilePath(fData)]), mtError, [mbOK], 0);
      Exit;
    end;
  Result := true;
end;  {*** function tJvDirectoryParameter.Validate ***}


 {*****************************************************************************}
 {* TJvMemoParameter                                                            *}
 {*****************************************************************************}

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
end;   {*** Procedure TJvMemoParameter.SetAsDate ***}

function TJvMemoParameter.GetParameterNameExt: string;
begin
  Result := 'Memo';
end;   {*** function TJvMemoParameter.GetParameterNameExt ****}


procedure TJvMemoParameter.CreateWinControl(aParameterParent: TWinControl);
var
  Memo: tWinControl;
begin
  WinControl := DynControlEngine.CreateMemoControl(Self, aParameterParent, GetParameterName);
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
end;   {*** procedure TJvMemoParameter.GetData ***}

procedure TJvMemoParameter.SetData;
begin
  inherited SetData;
end;   {*** procedure TJvMemoParameter.GetData ***}


end.
