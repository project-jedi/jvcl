{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOfficeColorPanel.PAS, released on 2004-02-26.

The Initial Developer of the Original Code is dejoy [dejoy att ynl dott gov dott cn]
All Rights Reserved.

Contributor(s): dejoy.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  a Color panel look like Microsoft office Color picker,make to customable Highly.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvOfficeColorPanel;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Graphics, Controls, Forms, Buttons, Dialogs,ExtCtrls,
  JvTypes, JvHotTrackPersistent, JvExControls, JvSpeedButton,
  JvPanel;

const
  Tag_DefaultColorCaption = 0;
  Tag_CustomColorCaption = 1;
  Tag_NoneColorCaption = 2;
  Tag_DefaultColorHint = 3;
  Tag_CustomColorHint = 4;
  Tag_NoneColorHint = 5;
  Tag_ShowColorsHint = 10;
  Tag_ShowAddInHint = 12;
  Tag_ShowStandardColors = 15;
  Tag_ShowSystemColors = 16;
  Tag_ShowUserColors = 17;
  Tag_HoldCustomColor = 18;

  Tag_ButtonHeight = 20;
  Tag_ButtonWidth = 21;
  Tag_ColorSize = 22;
  Tag_ColorSpace = 23;
  Tag_ColorSpaceTop = 24;
  Tag_ColorSpaceBottom = 25;
  Tag_TopMargin = 26;
  Tag_BottomMargin = 27;
  Tag_HorizontalMargin = 28;
  Tag_RightClickSelect = 29;
  Tag_SelectIfPopup = 30;

  MinColorSize = 18;
  MinColorSpace = 0;
  MinColorSpaceTop = 4;
  MinColorSpaceBottom = 4;
  MinTopMargin = 4;
  MinBottomMargin = 4;
  MinHorizontalMargin = 4;
  MinButtonHeight = MinColorSize + 7;
  MinButtonWidth = 23;

  PrimaryGroupIndex = 1;
  LineColorCount = 8;
  MaxLineRow = 8;
  MaxSectColorCount = LineColorCount * MaxLineRow;

type
  TJvOfficeColorPanelClass = class of TJvCustomOfficeColorPanel;

  TJvColorSpeedButton = class(TJvSpeedButton)
  private
    FDrawColor: TColor;
    FDisabledDrawColor: TColor;
    FCanDrawInnerFrame: Boolean;
    FCanDrawGlyph: Boolean;
    FCanDrawColorQuad: Boolean;
    FColorQuadLayOut: TJvColorQuadLayOut;
    FOnEnabledChanged: TNotifyEvent;
    procedure SetDrawColor(const Value: TColor);
    procedure SetDisabledDrawColor(const Value: TColor);
    procedure SetEdgeWidth(const Value: Integer);
    procedure SetFColorQuadLayOut(const Value: TJvColorQuadLayOut);
    procedure SetCanDrawInnerFrame(const Value: Boolean);
    procedure SetCanDrawGlyph(const Value: Boolean);
    procedure SetCanDrawColorQuad(const Value: Boolean);
  protected
    FEdgeWidth: Integer;
    procedure Paint; override;
    procedure EnabledChanged; override;
    function GetEdgeWidth: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
    property CanDrawColorQuad: Boolean read FCanDrawColorQuad write SetCanDrawColorQuad default True;
    property CanDrawGlyph: Boolean read FCanDrawGlyph write SetCanDrawGlyph default True;
    property CanDrawInnerFrame: Boolean read FCanDrawInnerFrame write SetCanDrawInnerFrame default False;
    property ColorQuadLayOut: TJvColorQuadLayOut read FColorQuadLayOut write SetFColorQuadLayOut default cqlClient;
    property DrawColor: TColor read FDrawColor write SetDrawColor default clDefault;
    property DisabledDrawColor: TColor read FDisabledDrawColor write SetDisabledDrawColor default clGray;
    property EdgeWidth: Integer read GetEdgeWidth write SetEdgeWidth;
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
  end;

  TJvOfficeColorDrawer = class(TJvColorSpeedButton)
  public
    constructor Create(AOwner: TComponent); override;
    property CanDrawGlyph default False;
  end;

  // (ahuser) TJvColorDialog is not registered as component
  TJvOfficeColorDialog = class(TColorDialog)
  published
    property OnShow;
    property OnClose;
  end;

  TJvOfficeColorPanelProperties = class(TJvPersistentProperty)
  private
    FShowNoneColor: Boolean;
    FShowDefaultColor: Boolean;
    FShowCustomColor: Boolean;
    FDefaultColorColor: TColor;
    FNoneColorColor: TColor;
    FShowColorsHint: Boolean;
    FShowAddInHint: Boolean;

    FDefaultColorCaption: string;
    FCustomColorCaption: string;
    FNoneColorCaption: string;
    FDefaultColorHint: string;
    FCustomColorHint: string;
    FNoneColorHint: string;

    FTopMargin: Integer;
    FColorSpaceBottom: Integer;
    FHorizontalMargin: Integer;
    FColorSpace: Integer;
    FColorSpaceTop: Integer;
    FButtonHeight: Integer;
    FColorSize: Integer;
    FBottomMargin: Integer;

    FRightClickSelect: Boolean;
    FSelectIfPopup: Boolean;
    FCustomColorFont: TFont;
    FDefaultColorFont: TFont;
    FNoneColorFont: TFont;
    FShowStandardColors: Boolean;
    FShowSystemColors: Boolean;
    FShowUserColors: Boolean;
    FHoldCustomColor: Boolean;
    FFilerTag:string;
    procedure SetIntegerValue(const Index, Value: Integer);
    procedure SetStringValue(const Index: Integer; const Value: string);
    procedure SetBooleanValue(const Index: Integer; const Value: Boolean);
    procedure SetColorValue(const Index: Integer; const Value: TColor);
    procedure SetFontValue(const Index: Integer; const Value: TFont);
    procedure OnFontChange(Sender: TObject);
    procedure ReadData(Reader: TReader);
  protected
    procedure CreateDefaultText; virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ShowStandardColors: Boolean index Tag_ShowStandardColors read FShowStandardColors write SetBooleanValue default True;
  published
    property ShowNoneColor: Boolean index Tag_NoneColorCaption read FShowNoneColor write SetBooleanValue default False;
    property ShowDefaultColor: Boolean index Tag_DefaultColorCaption read FShowDefaultColor write SetBooleanValue default True;
    property ShowCustomColor: Boolean index Tag_CustomColorCaption read FShowCustomColor write SetBooleanValue default True;
    property ShowAddInHint: Boolean index Tag_ShowAddInHint read FShowAddInHint write SetBooleanValue default True;
    property ShowColorsHint: Boolean index Tag_ShowColorsHint read FShowColorsHint write SetBooleanValue default True;
    property ShowSystemColors: Boolean index Tag_ShowSystemColors read FShowSystemColors write SetBooleanValue default False;
    property ShowUserColors: Boolean index Tag_ShowUserColors read FShowUserColors write SetBooleanValue default False;
    property HoldCustomColor: Boolean index Tag_HoldCustomColor read FHoldCustomColor write SetBooleanValue default False;

    property NoneColorCaption: string index Tag_NoneColorCaption read FNoneColorCaption write SetStringValue;
    property DefaultColorCaption: string index Tag_DefaultColorCaption read FDefaultColorCaption write SetStringValue;
    property CustomColorCaption: string index Tag_CustomColorCaption read FCustomColorCaption write SetStringValue;
    property NoneColorHint: string index Tag_NoneColorHint read FNoneColorHint write SetStringValue;
    property DefaultColorHint: string index Tag_DefaultColorHint read FDefaultColorHint write SetStringValue;
    property CustomColorHint: string index Tag_CustomColorHint read FCustomColorHint write SetStringValue;
    property NoneColorColor: TColor index Tag_NoneColorCaption read FNoneColorColor write SetColorValue default clNone;
    property DefaultColorColor: TColor index Tag_DefaultColorCaption read FDefaultColorColor write SetColorValue default clDefault;

    property NoneColorFont: TFont index Tag_NoneColorCaption read FNoneColorFont write SetFontValue;
    property DefaultColorFont: TFont index Tag_DefaultColorCaption read FDefaultColorFont write SetFontValue;
    property CustomColorFont: TFont index Tag_CustomColorCaption read FCustomColorFont write SetFontValue;

    property TopMargin: Integer index Tag_TopMargin read FTopMargin write SetIntegerValue default MinTopMargin;
    property BottomMargin: Integer index Tag_BottomMargin read FBottomMargin write SetIntegerValue default MinBottomMargin;
    property HorizontalMargin: Integer index Tag_HorizontalMargin read FHorizontalMargin write SetIntegerValue default MinHorizontalMargin;
    property ColorSpace: Integer index Tag_ColorSpace read FColorSpace write SetIntegerValue default MinColorSpace;
    property ColorSpaceTop: Integer index Tag_ColorSpaceTop read FColorSpaceTop write SetIntegerValue default MinColorSpaceTop;
    property ColorSpaceBottom: Integer index Tag_ColorSpaceBottom read FColorSpaceBottom write SetIntegerValue default MinColorSpaceBottom;
    property ColorSize: Integer index Tag_ColorSize read FColorSize write SetIntegerValue default MinColorSize;
    property ButtonHeight: Integer index Tag_ButtonHeight read FButtonHeight write SetIntegerValue default MinButtonHeight;

    property RightClickSelect: Boolean index Tag_RightClickSelect read FRightClickSelect write SetBooleanValue default False;
    property SelectIfPopup: Boolean index Tag_SelectIfPopup read FSelectIfPopup write SetBooleanValue default False;
  end;

  TJvCustomOfficeColorPanel = class(TJvCustomArrangePanel, IJvHotTrack)
  private
    FStandardColors: TStringList;
    FSystemColors: TStringList;
    FUserColors: TStringList;
    FStandardColorDrawers: TList;
    FSystemColorDrawers: TList;
    FUserColorDrawers: TList;
    FAddInControls: TList;
    FButtonNoneColor: TJvColorSpeedButton;
    FButtonDefaultColor: TJvColorSpeedButton;
    FButtonCustomColor: TJvColorSpeedButton;
    FCustomColorDrawer: TJvOfficeColorDrawer;
    FPriorCheckedButton: TJvColorSpeedButton;
    FDividerLine1: TControl;
    FDividerLine2: TControl;
    FDividerLine3: TControl;
    FProperties: TJvOfficeColorPanelProperties;
    FColorDialog: TJvOfficeColorDialog;
    FSelectedColor: TColor;
    FBackgroundColor: TColor;
    FInited: Boolean;
    FNeedReDrawDownState: Boolean;
    //in SetSeletedColor,is need to refresh button down state.
    FInRearrangeControls: Boolean;
    FOnColorChange: TNotifyEvent;
    FOnHoldCustomColor: TJvHoldCustomColorEvent;
    FOnColorButtonClick: TNotifyEvent;
    FOnShowOwnerColorDialog: TNotifyEvent;
    FOnGetAddInControlSiteInfo: TJvGetAddInControlSiteInfoEvent;
    FClickColorType: TJvClickColorType;
    FHotTrackOptions: TJvHotTrackOptions;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    FColorDialogOptions: TColorDialogOptions;
    procedure SetColorDialogOptions(const Value: TColorDialogOptions);
    procedure SetSelectedColor(const Value: TColor);
    function GetColorDlgCustomColors: TStrings;
    procedure SetColorDlgCustomColors(const Value: TStrings);
    procedure SetProperties(const Value: TJvOfficeColorPanelProperties);
    function GetControlBackgroundColor: TColor;
    procedure SetControlBackgroundColor(const Value: TColor);

    {IJvHotTrack}
    function GetHotTrack: Boolean;
    function GetHotTrackFont: TFont;
    function GetHotTrackFontOptions: TJvTrackFontOptions;
    function GetHotTrackOptions: TJvHotTrackOptions;
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackOptions(Value: TJvHotTrackOptions);
    procedure SetHotTrackFont(Value: TFont);
    procedure SetHotTrackFontOptions(Value: TJvTrackFontOptions);
    procedure IJvHotTrack_Assign(Source: IJvHotTrack);
    procedure IJvHotTrack.Assign = IJvHotTrack_Assign;

    procedure SetStandardColors(const Value: TStringList);
    procedure SetSystemColors(const Value: TStringList);
    procedure SetUserColors(const Value: TStringList);
    procedure RedirectToColorButtonClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure DoColorDrawersEnabledChange(Sender: TObject);
    procedure DoHotTrackOptionsChanged(Sender: TObject);
    procedure DoLoadedUserColors(Sender: TObject);
    procedure DoHoldedCustomColor(Sender: TObject;AColor: TColor);
  protected
    procedure DoColorButtonClick(Sender: TObject); virtual;
    procedure DoSelectedColorChange(Sender: TObject); virtual;
    procedure DoGetAddInControlSiteInfo(Sender: TControl; var ASiteInfo:
      TJvAddInControlSiteInfo); virtual;
    procedure DoPropertiesChanged(Sender: TObject; const PropName: string); virtual;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure ShowHintChanged; override;
    function CreateStandardColors(ColorList: TStrings): Integer; virtual;
    function CreateSystemColors(ColorList: TStrings): Integer; virtual;

    { If you wnat to create published color list by default,override this procedure.
      If you want to change ColorList,make sure call strings.beginupdate before change,
      and call strings.EndUpdate after changed. }
    function CreateUserColors(ColorList: TStrings): Integer; virtual;
    procedure CreateColorDrawersByColors(DrawersList: TList; ColorsList: TStringList; AVisible: Boolean);
    procedure SetEnabled( Value: Boolean); override;

    // Don't change the following list, the result might unpredictability.
    property StandardColorDrawers: TList read FStandardColorDrawers;
    property SystemColorDrawers: TList read FSystemColorDrawers;
    property UserColorDrawers: TList read FUserColorDrawers;
    property AddInControls: TList read FAddInControls;

    property ButtonNoneColor: TJvColorSpeedButton read FButtonNoneColor;
    property ButtonDefaultColor: TJvColorSpeedButton read FButtonDefaultColor;
    property ButtonCustomColor: TJvColorSpeedButton read FButtonCustomColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateStandardColorDrawers; virtual;
    procedure CreateSystemColorDrawers; virtual;
    procedure CreateUserColorDrawers; virtual;
    procedure RearrangeControls; virtual;
    procedure RefreshControls;

    property ColorDialog: TJvOfficeColorDialog read FColorDialog write FColorDialog;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clBlack;
    property BackColor: TColor read GetControlBackgroundColor write SetControlBackgroundColor default clBtnFace;

    //overried property
    property Color: TColor read GetControlBackgroundColor write SetControlBackgroundColor;

    property ClickColorType: TJvClickColorType read FClickColorType;

    {  //Each custom color is represented as a string of the form ColorValue=ColorDescription.
         For example, the following string sets the  custom color list.
           clBlack = Black
           $00003333 = Olive Green
         if set ColorDescription to cUseDefaultColorHint('($)'),the ColorDescription will be put to ColorToPrettyName(ColorValue) at runtime,
         such as:
           'clBlack = ($)' will be put to 'clBlack =  Black' at runtime;
           '$00113333 = ($)' will be put to '$00113333 = $00113333' at runtime;

         Up to MaxSectColorCount custom colors  can be set.

        // If you changed Colors list manual,also you must call CreateXXXColorButtons and RearrangeControls to refresh manual,
          or call RefreshControls to refresh.
        // The StandardColors and SystemColors did't published by default,so it's not stroed in DFM, you just only change the value at runtime,
        if you want stroed StandardColors and SystemColors,create a new control descended from TJvOfficeColorPanel and
        put them to published section.

        //if you want to change ColorList,make sure call strings.beginupdate before change,
        //and call strings.EndUpdate after changed.
    }
    property StandardColors: TStringList read FStandardColors write SetStandardColors;
    property SystemColors: TStringList read FSystemColors write SetSystemColors;
    property UserColors: TStringList read FUserColors write SetUserColors;

    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont;
    property HotTrackFontOptions: TJvTrackFontOptions read GetHotTrackFontOptions write SetHotTrackFontOptions default DefaultTrackFontOptions;
    property HotTrackOptions: TJvHotTrackOptions read GetHotTrackOptions write SetHotTrackOptions;
    property Properties: TJvOfficeColorPanelProperties read FProperties write SetProperties;

    property ColorDlgCustomColors: TStrings read GetColorDlgCustomColors write SetColorDlgCustomColors;
    property ColorDialogOptions: TColorDialogOptions read FColorDialogOptions write SetColorDialogOptions default [];

    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnColorButtonClick: TNotifyEvent read FOnColorButtonClick write FOnColorButtonClick;
    property OnHoldCustomColor: TJvHoldCustomColorEvent read FOnHoldCustomColor write FOnHoldCustomColor;
    property OnShowOwnerColorDialog: TNotifyEvent read FOnShowOwnerColorDialog write FOnShowOwnerColorDialog;
    property OnGetAddInControlSiteInfo: TJvGetAddInControlSiteInfoEvent read FOnGetAddInControlSiteInfo write FOnGetAddInControlSiteInfo;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvOfficeColorPanel = class(TJvCustomOfficeColorPanel)
  private
    FFilerTag: string;
    procedure ReadData(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    // UserColors must be load fist on read from the DFM file.
    property UserColors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property FlatBorder;
    property FlatBorderColor;
    property BackColor;
    property SelectedColor;
    property ColorDlgCustomColors;
    property HotTrack;
    property HotTrackFont;
    property HotTrackFontOptions;
    property HotTrackOptions;
    property ColorDialogOptions;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnEndDock;
    property OnGetSiteInfo;

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnStartDrag;

    property Properties;
    property OnColorChange;
    property OnColorButtonClick;

    { If OnShowOwnerColorDialog not nil,the default ColorDialog will not show,
      so you can show coustom ColorDialog yourself. }
    property OnShowOwnerColorDialog;
    property OnClick;
  end;

  TJvOfficePanelDividerLine = class(TBevel)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Shape default bsTopLine;
  end;

var
  JvDividerLineClass: TControlClass = TJvOfficePanelDividerLine;

function GetTopOwner(AComponent: TComponent): TComponent;
function JvReaderReadColor(Reader: TReader):TColor;
function JvReaderReadColorDialogOptions(Reader: TReader):TColorDialogOptions;
procedure JvReaderReadStrings(Reader: TReader;Strings:TStrings);

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
  JvJCLUtils, JvResources, Math;

const
  cNoneColorCaption = 'NoneColorCaption';
  cNoneColorColor = 'NoneColorColor';
  cNoneColorFont = 'NoneColorFont';
  cNoneColorHint = 'NoneColorHint';
  cDefaultColorCaption = 'DefaultColorCaption';
  cDefaultColorColor = 'DefaultColorColor';
  cDefaultColorFont = 'DefaultColorFont';
  cDefaultColorHint = 'DefaultColorHint';
  cCustomColorCaption = 'CustomColorCaption';
  cCustomColorFont = 'CustomColorFont';
  cCustomColorHint = 'CustomColorHint';
  cShowNoneColor = 'ShowNoneColor';
  cShowDefaultColor = 'ShowDefaultColor';
  cShowCustomColor = 'ShowCustomColor';
  cShowAddInHint = 'ShowAddInHint';
  cShowColorsHint = 'ShowColorsHint';
  cShowStandardColors = 'ShowStandardColors';
  cShowSystemColors = 'ShowSystemColors';
  cShowUserColors = 'ShowUserColors';
  cHoldCustomColor = 'HoldCustomColor';
  cRightClickSelect = 'RightClickSelect';
  cSelectIfPopup = 'SelectIfPopup';

  cUseDefaultColorHint = '($)';

type
  TControlAccessProtected = class(TControl);

{ TJvOfficePanelDividerLine }

constructor TJvOfficePanelDividerLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Shape := bsTopLine;
end;

//=== { TJvOfficeColorPanelProperties } ======================================

constructor TJvOfficeColorPanelProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FShowNoneColor := False;
  FShowDefaultColor := True;
  FShowCustomColor := True;
  FShowColorsHint := True;
  FShowAddInHint := True;
  FShowStandardColors := True;
  FShowSystemColors := False;
  FShowUserColors := False;
  FHoldCustomColor := False;
  FNoneColorColor := clNone;
  FDefaultColorColor := clDefault;

  FDefaultColorFont := TFont.Create;
  FDefaultColorFont.OnChange := OnFontChange;
  FNoneColorFont := TFont.Create;
  FNoneColorFont.OnChange := OnFontChange;
  FCustomColorFont := TFont.Create;
  FCustomColorFont.OnChange := OnFontChange;

  FHorizontalMargin := MinHorizontalMargin;
  FTopMargin := MinTopMargin;
  FBottomMargin := MinBottomMargin;
  FButtonHeight := MinButtonHeight;
  FColorSpace := MinColorSpace;
  FColorSpaceTop := MinColorSpaceTop;
  FColorSpaceBottom := MinColorSpaceBottom;
  FColorSize := MinColorSize;

  FRightClickSelect := False;
  FSelectIfPopup := False;
end;

destructor TJvOfficeColorPanelProperties.Destroy;
begin
  FreeAndNil(FNoneColorFont);
  FreeAndNil(FDefaultColorFont);
  FreeAndNil(FCustomColorFont);
  inherited Destroy;
end;

procedure TJvOfficeColorPanelProperties.Assign(Source: TPersistent);
begin
  if Source is TJvOfficeColorPanelProperties then
  begin
    BeginUpdate;
    try
      with TJvOfficeColorPanelProperties(Source) do
      begin
        Self.ShowNoneColor := ShowNoneColor;
        Self.ShowDefaultColor := ShowDefaultColor;
        Self.ShowCustomColor := ShowCustomColor;
        Self.ShowStandardColors := ShowStandardColors;
        Self.ShowSystemColors := ShowSystemColors;
        Self.ShowUserColors := ShowUserColors;
        Self.ShowAddInHint := ShowAddInHint;
        Self.ShowColorsHint := ShowColorsHint;
        Self.HoldCustomColor := HoldCustomColor;

        Self.TopMargin := TopMargin;
        Self.BottomMargin := BottomMargin;
        Self.HorizontalMargin := HorizontalMargin;
        Self.ButtonHeight := ButtonHeight;
        Self.ColorSpace := ColorSpace;
        Self.ColorSpaceTop := ColorSpaceTop;
        Self.ColorSpaceBottom := ColorSpaceBottom;
        Self.ColorSize := ColorSize;

        Self.NoneColorCaption := NoneColorCaption;
        Self.DefaultColorCaption := DefaultColorCaption;
        Self.CustomColorCaption := CustomColorCaption;
        Self.NoneColorHint := NoneColorHint;
        Self.DefaultColorHint := DefaultColorHint;
        Self.CustomColorHint := CustomColorHint;
        Self.NoneColorColor := NoneColorColor;
        Self.DefaultColorColor := DefaultColorColor;
        Self.NoneColorFont.Assign(NoneColorFont);
        Self.DefaultColorFont.Assign(DefaultColorFont);
        Self.CustomColorFont.Assign(CustomColorFont);

        Self.RightClickSelect := RightClickSelect;
        Self.SelectIfPopup := SelectIfPopup;
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

const
  cShowAutoButton = 'ShowAutoButton';
  cShowOtherButton = 'ShowOtherButton';
  cShowColorHint ='ShowColorHint';
  cAutoCaption ='AutoCaption';
  cOtherCaption ='OtherCaption';
  cAutoHint ='AutoHint';
  cOtherHint = 'OtherHint';

procedure TJvOfficeColorPanelProperties.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);;
  { For backwards compatibility }
  BeginUpdate;
  try
    FFilerTag := cShowAutoButton;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
    FFilerTag := cShowOtherButton;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
    FFilerTag := cShowColorHint;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
    FFilerTag := cAutoCaption;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
    FFilerTag := cOtherCaption;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
    FFilerTag := cAutoHint;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
    FFilerTag := cOtherHint;
    Filer.DefineProperty(FFilerTag, ReadData, nil, False);
  finally
    EndUpdate;
  end;
end;

procedure TJvOfficeColorPanelProperties.ReadData(Reader: TReader);
begin
  if SameText(FFilerTag, cShowAutoButton) then
    ShowDefaultColor := Reader.ReadBoolean
  else
  if SameText(FFilerTag, cShowOtherButton) then
    ShowCustomColor := Reader.ReadBoolean
  else
  if SameText(FFilerTag, cShowColorHint) then
    ShowColorsHint := Reader.ReadBoolean
  else
  if SameText(FFilerTag, cAutoCaption) then
    DefaultColorCaption := Reader.ReadString
  else
  if SameText(FFilerTag, cOtherCaption) then
    CustomColorCaption := Reader.ReadString
  else
  if SameText(FFilerTag, cAutoHint) then
    DefaultColorHint := Reader.ReadString
  else
  if SameText(FFilerTag, cOtherHint) then
    CustomColorHint := Reader.ReadString;
end;

procedure TJvOfficeColorPanelProperties.CreateDefaultText;
begin
  BeginUpdate;
  try
    NoneColorCaption := RsNoneColorCaption;
    DefaultColorCaption := RsDefaultColorCaption;
    CustomColorCaption := RsCustomColorCaption;
    DefaultColorHint := RsDefaultColorCaption;
    NoneColorHint := RsNoneColorCaption;
    CustomColorHint := RsCustomColorCaption;
  finally
    EndUpdate;
  end;
end;

procedure TJvOfficeColorPanelProperties.SetIntegerValue(const Index, Value:
  Integer);
var
  MeasureItem: PInteger;
  MeasureConst: Integer;
  LName: string;
begin
  case Index of
    Tag_TopMargin:
      begin
        MeasureItem := @FTopMargin;
        MeasureConst := MinTopMargin;
        LName := 'TopMargin';
      end;
    Tag_BottomMargin:
      begin
        MeasureItem := @FBottomMargin;
        MeasureConst := MinBottomMargin;
        LName := 'BottomMargin';
      end;
    Tag_HorizontalMargin:
      begin
        MeasureItem := @FHorizontalMargin;
        MeasureConst := MinHorizontalMargin;
        LName := 'HorizontalMargin';
      end;
    Tag_ColorSpace:
      begin
        MeasureItem := @FColorSpace;
        MeasureConst := MinColorSpace;
        LName := 'ColorSpace';
      end;
    Tag_ColorSpaceTop:
      begin
        MeasureItem := @FColorSpaceTop;
        MeasureConst := MinColorSpaceTop;
        LName := 'ColorSpaceTop';
      end;
    Tag_ColorSpaceBottom:
      begin
        MeasureItem := @FColorSpaceBottom;
        MeasureConst := MinColorSpaceBottom;
        LName := 'ColorSpaceBottom';
      end;
    Tag_ColorSize:
      begin
        MeasureItem := @FColorSize;
        MeasureConst := MinColorSize;
        LName := 'ColorSize';
      end;
    Tag_ButtonHeight:
      begin
        MeasureItem := @FButtonHeight;
        MeasureConst := MinButtonHeight;
        LName := 'ButtonHeight';
      end;
  else
    Exit;
  end;
  if MeasureItem^ = Value then
    Exit;

  Changing;
  ChangingProperty(LName);
  MeasureItem^ := Value;
  if MeasureItem^ < MeasureConst then
    MeasureItem^ := MeasureConst;
  ChangedProperty(LName);
  Changed;
end;

procedure TJvOfficeColorPanelProperties.SetStringValue(const Index: Integer;
  const Value: string);
var
  Prop: PString;
  Name: string;
begin
  case Index of
    Tag_NoneColorCaption:
      begin
        Prop := @FNoneColorCaption;
        Name := cNoneColorCaption;
      end;
    Tag_DefaultColorCaption:
      begin
        Prop := @FDefaultColorCaption;
        Name := cDefaultColorCaption;
      end;
    Tag_CustomColorCaption:
      begin
        Prop := @FCustomColorCaption;
        Name := cCustomColorCaption;
      end;
    Tag_NoneColorHint:
      begin
        Prop := @FNoneColorHint;
        Name := cNoneColorHint;
      end;
    Tag_DefaultColorHint:
      begin
        Prop := @FDefaultColorHint;
        Name := cDefaultColorHint;
      end;
    Tag_CustomColorHint:
      begin
        Prop := @FCustomColorHint;
        Name := cCustomColorHint;
      end;
  else
    Exit;
  end;

  if (Prop^ <> Value) then
  begin
    Changing;
    ChangingProperty(Name);
    Prop^ := Value;
    ChangedProperty(Name);
    Changed;
  end;
end;

procedure TJvOfficeColorPanelProperties.SetBooleanValue(const Index: Integer;
  const Value: Boolean);
var
  Prop: PBoolean;
  Name: string;
begin
  case Index of
    Tag_NoneColorCaption:
      begin
        Prop := @FShowNoneColor;
        Name := cShowNoneColor;
      end;
    Tag_DefaultColorCaption:
      begin
        Prop := @FShowDefaultColor;
        Name := cShowDefaultColor;
      end;
    Tag_CustomColorCaption:
      begin
        Prop := @FShowCustomColor;
        Name := cShowCustomColor;
      end;
    Tag_ShowAddInHint:
      begin
        Prop := @FShowAddInHint;
        Name := cShowAddInHint;
      end;
    Tag_ShowColorsHint:
      begin
        Prop := @FShowColorsHint;
        Name := cShowColorsHint;
      end;
    Tag_ShowStandardColors:
      begin
        Prop := @FShowStandardColors;
        Name := cShowStandardColors;
      end;
    Tag_ShowSystemColors:
      begin
        Prop := @FShowSystemColors;
        Name := cShowSystemColors;
      end;
    Tag_ShowUserColors:
      begin
        Prop := @FShowUserColors;
        Name := cShowUserColors;
      end;
    Tag_RightClickSelect:
      begin
        Prop := @FRightClickSelect;
        Name := cRightClickSelect;
      end;
    Tag_SelectIfPopup:
      begin
        Prop := @FSelectIfPopup;
        Name := cSelectIfPopup;
      end;
    Tag_HoldCustomColor:
      begin
        Prop := @HoldCustomColor;
        Name := cHoldCustomColor;
      end;
  else
    Exit;
  end;

  if (Prop^ <> Value) then
  begin
    Changing;
    ChangingProperty(Name);
    Prop^ := Value;
    ChangedProperty(Name);
    Changed;
  end;
end;

procedure TJvOfficeColorPanelProperties.SetColorValue(const Index: Integer;
  const Value: TColor);
var
  Prop: PColor;
  Name: string;
begin
  case Index of
    Tag_NoneColorCaption:
      begin
        Prop := @FNoneColorColor;
        Name := cNoneColorColor;
      end;
    Tag_DefaultColorCaption:
      begin
        Prop := @FDefaultColorColor;
        Name := cDefaultColorColor;
      end;
  else
    Exit;
  end;

  if (Prop^ <> Value) then
  begin
    Changing;
    ChangingProperty(Name);
    Prop^ := Value;
    ChangedProperty(Name);
    Changed;
  end;
end;

procedure TJvOfficeColorPanelProperties.SetFontValue(const Index: Integer;
  const Value: TFont);
var
  Prop: TFont;
  Name: string;
begin
  case Index of
    Tag_NoneColorCaption:
      begin
        Prop := FNoneColorFont;
        Name := cNoneColorFont;
      end;
    Tag_DefaultColorCaption:
      begin
        Prop := FDefaultColorFont;
        Name := cDefaultColorFont;
      end;
    Tag_CustomColorCaption:
      begin
        Prop := FCustomColorFont;
        Name := cCustomColorFont;
      end;
  else
    Exit;
  end;

  if (Prop <> Value) then
  begin
    Changing;
    ChangingProperty(Name);
    Prop.Assign(Value);
    ChangedProperty(Name);
    Changed;
  end;
end;

procedure TJvOfficeColorPanelProperties.OnFontChange(Sender: TObject);
begin
  if Sender = FNoneColorFont then
    ChangedProperty(cNoneColorFont)
  else
  if Sender = FDefaultColorFont then
    ChangedProperty(cDefaultColorFont)
  else
  if Sender = FCustomColorFont then
    ChangedProperty(cCustomColorFont);
end;

//=== { TJvColorSpeedButton } ================================================

constructor TJvColorSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawColor := clDefault;
  FDisabledDrawColor := clGray;
  FEdgeWidth := 4;
  FColorQuadLayOut := cqlClient;
  FCanDrawColorQuad := True;
  FCanDrawGlyph := True;
  FCanDrawInnerFrame := False;
end;

function TJvColorSpeedButton.GetEdgeWidth: Integer;
begin
  Result := Height div 5;
end;

procedure TJvColorSpeedButton.Paint;
var
  B, X, Y: Integer;
  PaintColor: TColor;
begin
  inherited Paint;

  if Enabled then
    PaintColor := DrawColor
  else
    PaintColor := DisabledDrawColor;
  if EdgeWidth >= 0 then
    B := EdgeWidth
  else
    B := Height div 5;
  with Canvas do
  begin
    if CanDrawGlyph and (not Glyph.Empty) then
    begin
      Glyph.Transparent := True;
      X := (Width div 2) - 9 + Integer(FState in [TJvButtonState(bsDown)]);
      Y := (Height div 2) + 4 + Integer(FState in [TJvButtonState(bsDown)]);
      if CanDrawColorQuad then
      begin
        Pen.Color := PaintColor;
        Brush.Color := PaintColor;
        Brush.Style := bsSolid;
        Rectangle(X, Y, X + 17, Y + 4);
      end;
    end
    else
    begin
      if CanDrawInnerFrame then
      begin
        Pen.Color := clGray;
        Brush.Style := bsClear;
        Rectangle(B - 1, B - 1, Width - (B - 2), Height - (B - 2));
      end;
      if CanDrawColorQuad then
      begin
        Pen.Color := clGray;
        Brush.Color := PaintColor;
        Brush.Style := bsSolid;
        case ColorQuadLayOut of
          cqlLeft:
            Rectangle(B + 1, B + 1, Height, Height - B);
          cqlRight:
            Rectangle(Width - Height + 1, B + 1, Width - B, Height - B);
          cqlClient:
            Rectangle(B, B, Width - B, Height - B);
        end;
      end;
    end;
  end;
end;

procedure TJvColorSpeedButton.EnabledChanged;
begin
  inherited EnabledChanged;
  if Assigned(OnEnabledChanged) then
    OnEnabledChanged(Self);
end;

procedure TJvColorSpeedButton.SetDrawColor(const Value: TColor);
begin
  if FDrawColor <> Value then
  begin
    FDrawColor := Value;
    Invalidate;
  end;
end;

procedure TJvColorSpeedButton.SetDisabledDrawColor(const Value: TColor);
begin
  if FDisabledDrawColor <> Value then
  begin
    FDisabledDrawColor := Value;
    Invalidate;
  end;
end;

procedure TJvColorSpeedButton.SetCanDrawColorQuad(const Value: Boolean);
begin
  if CanDrawColorQuad <> Value then
  begin
    FCanDrawColorQuad := Value;
    Invalidate;
  end;
end;

procedure TJvColorSpeedButton.SetCanDrawGlyph(const Value: Boolean);
begin
  if FCanDrawGlyph <> Value then
  begin
    FCanDrawGlyph := Value;
    Invalidate;
  end;
end;

procedure TJvColorSpeedButton.SetCanDrawInnerFrame(const Value: Boolean);
begin
  if FCanDrawInnerFrame <> Value then
  begin
    FCanDrawInnerFrame := Value;
    Invalidate;
  end;
end;

procedure TJvColorSpeedButton.SetEdgeWidth(const Value: Integer);
begin
  if FEdgeWidth <> Value then
  begin
    FEdgeWidth := Value;
    Invalidate;
  end;
end;

procedure TJvColorSpeedButton.SetFColorQuadLayOut(
  const Value: TJvColorQuadLayOut);
begin
  if FColorQuadLayOut <> Value then
  begin
    FColorQuadLayOut := Value;
    Invalidate;
  end;
end;

//if you directed set the Visible property,the control visible state never changed in design mode.
//That can be hide control in design mode.

procedure SetControlVisible(Ctl: TControl; AVisible: Boolean);
begin
  if Ctl.Visible = AVisible then
    Exit;

  if csDesigning in Ctl.ComponentState then
  begin
    with Ctl do
      if AVisible then
        ControlStyle := ControlStyle - [csNoDesignVisible]
      else
        ControlStyle := ControlStyle + [csNoDesignVisible];
  end;

  Ctl.Visible := AVisible;
end;

function GetTopOwner(AComponent: TComponent): TComponent;
begin
  Result := AComponent;
  while (AComponent <> nil) and
        (AComponent.ComponentState * [csReading, csLoading] = []) do
    AComponent := AComponent.Owner;

  if AComponent <> nil then
    Result := AComponent;
end;

function JvReaderReadColor(Reader: TReader):TColor;
begin
  with Reader do
  begin
    if NextValue = vaIdent then
      Result := StringToColor(ReadIdent)
    else
      Result := ReadInteger;
  end;
end;


function JvReaderReadColorDialogOptions(Reader: TReader):TColorDialogOptions;
var
  EnumName: string;

  procedure SkipSetBody;
  begin
    while Reader.ReadStr <> '' do
      {nothing};
  end;

begin
  try
    if Reader.ReadValue <> vaSet then
      Exit;
    Result := [];
    while True do
    begin
      EnumName := Reader.ReadStr;
      if EnumName = '' then
        Break;
      if SameText(EnumName, 'cdFullOpen') then
        Include(Result, cdFullOpen)
      else
      if SameText(EnumName, 'cdPreventFullOpen') then
        Include(Result, cdPreventFullOpen)
      else
      if SameText(EnumName, 'cdShowHelp') then
        Include(Result, cdShowHelp)
      else
      if SameText(EnumName, 'cdSolidColor') then
        Include(Result, cdSolidColor)
      else
      if SameText(EnumName, 'cdAnyColor') then
        Include(Result, cdAnyColor);
    end;
  except
    SkipSetBody;
  end;
end;


procedure JvReaderReadStrings(Reader: TReader;Strings:TStrings);
begin
  Reader.ReadListBegin;
  with Strings do
  begin
    BeginUpdate;
    try
      Clear;
      while not Reader.EndOfList do
        Add(Reader.ReadString);
    finally
      EndUpdate;
    end;
  end;
  Reader.ReadListEnd;
end;

//=== { TJvOfficeColorDrawer } ===============================================

constructor TJvOfficeColorDrawer.Create(AOwner: TComponent);
begin
  inherited;
  FCanDrawGlyph := True;
end;

type
  TJvDefaultColorSpeendButton = class(TJvColorSpeedButton)
  public
    function HintShow(var HintInfo: THintInfo): Boolean; override;
  end;

//=== { TJvDefaultColorSpeendButton } ========================================

function TJvDefaultColorSpeendButton.HintShow(var HintInfo: THintInfo): Boolean;
var
  ColorQuadRect:TRect;
  B: Integer;
begin
  Result := inherited HintShow(HintInfo);
  if CanDrawColorQuad then
  begin
    if EdgeWidth >= 0 then
      B := EdgeWidth
    else
      B := Height div 5;
    case ColorQuadLayOut of
      cqlLeft:
        ColorQuadRect:= Rect(B + 1, B + 1, Height, Height - B);
      cqlRight:
        ColorQuadRect:= Rect(Width - Height + 1, B + 1, Width - B, Height - B);
      cqlClient:
        ColorQuadRect:= Rect(B, B, Width - B, Height - B);
    end;
    if PtInRect(ColorQuadRect,ScreenToClient(Mouse.CursorPos)) then
      HintInfo.HintStr := ColorToPrettyName(DrawColor);
  end;
end;

//=== { TJvCustomOfficeColorPanel } ==========================================

constructor TJvCustomOfficeColorPanel.Create(AOwner: TComponent);
var
  TopOwner: TComponent;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FInited := False;
  FNeedReDrawDownState := True;
  FBackgroundColor := clBtnFace;
  FHotTrack := False;
  FHotTrackFont := TFont.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
  FHotTrackOptions := TJvHotTrackOptions.Create(Self);
  FHotTrackOptions.OnChanged := DoHotTrackOptionsChanged;

  FStandardColors := TStringList.Create;
  FSystemColors := TStringList.Create;
  FUserColors := TStringList.Create;
  FStandardColorDrawers := TList.Create;
  FSystemColorDrawers := TList.Create;
  FUserColorDrawers := TList.Create;
  FAddInControls := TList.Create;

  FColorDialogOptions := [];
  FClickColorType := cctNone;

  FProperties := TJvOfficeColorPanelProperties.Create(Self);
  FProperties.OnChangedProperty := DoPropertiesChanged;
  FSelectedColor := FProperties.DefaultColorColor;

  FButtonNoneColor := TJvColorSpeedButton.Create(Self);
  with FButtonNoneColor do
  begin
    Parent := Self;
    GroupIndex := PrimaryGroupIndex;
    Tag := FStandardColors.Count + 1;
    DrawColor := FProperties.NoneColorColor;
    Flat := True;
    CanDrawColorQuad := False;
    CanDrawGlyph := False;
    CanDrawInnerFrame := False;
    OnClick := DoColorButtonClick;
    OnEnabledChanged := DoColorDrawersEnabledChange;
  end;
  FAddInControls.Add(FButtonNoneColor);

  FButtonDefaultColor := TJvDefaultColorSpeendButton.Create(Self);
  with FButtonDefaultColor do
  begin
    Parent := Self;
    GroupIndex := PrimaryGroupIndex;
    Tag := FButtonNoneColor.Tag + 1;
    Down := True;
    DrawColor := FProperties.DefaultColorColor;
    Flat := True;
    CanDrawColorQuad := True;
    CanDrawGlyph := False;
    CanDrawInnerFrame := True;
    ColorQuadLayOut := cqlLeft;
    OnClick := DoColorButtonClick;
    OnMouseUp := RedirectToColorButtonClick;
    OnEnabledChanged := DoColorDrawersEnabledChange;
  end;
  FAddInControls.Add(FButtonDefaultColor);
  FPriorCheckedButton := FButtonDefaultColor;

  FButtonCustomColor := TJvColorSpeedButton.Create(Self);
  with FButtonCustomColor do
  begin
    Parent := Self;
    GroupIndex := PrimaryGroupIndex;
    Tag := FButtonDefaultColor.Tag + 1;
    DrawColor := clDefault;
    Flat := True;
    CanDrawColorQuad := False;
    CanDrawGlyph := False;
    CanDrawInnerFrame := True;
    OnClick := DoColorButtonClick;
    OnMouseUp := RedirectToColorButtonClick;
    OnEnabledChanged := DoColorDrawersEnabledChange;
  end;
  FAddInControls.Add(FButtonCustomColor);

  FCustomColorDrawer := TJvOfficeColorDrawer.Create(Self);
  with FCustomColorDrawer do
  begin
    Parent := Self;
    GroupIndex := PrimaryGroupIndex;
    Tag := FButtonCustomColor.Tag + 1;
    Flat := True;
    DrawColor := clDefault;
    Hint := ColorToString(DrawColor);
    CanDrawColorQuad := True;
    CanDrawGlyph := False;
    CanDrawInnerFrame := False;
    ColorQuadLayOut := cqlClient;
    OnClick := DoColorButtonClick;
    OnMouseUp := RedirectToColorButtonClick;
    OnEnabledChanged := DoColorDrawersEnabledChange;
  end;
  FAddInControls.Add(FCustomColorDrawer);

  FDividerLine1 := JvDividerLineClass.Create(Self);
  with FDividerLine1 do
  begin
    Parent := Self;
    Visible := False;
    Height := 2;
  end;
  FDividerLine2 := JvDividerLineClass.Create(Self);
  with FDividerLine2 do
  begin
    Parent := Self;
    Visible := False;
    Height := 2;
  end;
  FDividerLine3 := JvDividerLineClass.Create(Self);
  with FDividerLine3 do
  begin
    Parent := Self;
    Visible := False;
    Height := 2;
  end;

  FColorDialog := TJvOfficeColorDialog.Create(Self);
  FColorDialog.Options := FColorDialogOptions;

  FStandardColors.BeginUpdate;
  try
    CreateStandardColors(FStandardColors);
  finally
    FStandardColors.EndUpdate;
  end;
  FSystemColors.BeginUpdate;
  try
    CreateSystemColors(FSystemColors);
  finally
    FSystemColors.EndUpdate;
  end;

  TopOwner := GetTopOwner(Self);
  // make sure that if this is not loading from DFM file or stream.
  if (TopOwner <> nil) and (TopOwner.ComponentState * [csReading, csLoading] = [])
    then
  begin
    FUserColors.BeginUpdate;
    try
      CreateUserColors(FUserColors);
    finally
      FUserColors.EndUpdate;
    end;

    CreateUserColorDrawers;
    FProperties.CreateDefaultText;
  end
  else //When loaded UserColors,Call CreateUserColorDrawers at once in DoLoadedUserColors
    FUserColors.OnChange := DoLoadedUserColors;

  CreateStandardColorDrawers;
  CreateSystemColorDrawers;

  FInited := True;
end;

destructor TJvCustomOfficeColorPanel.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FHotTrackFont);
  FreeAndNil(FHotTrackOptions);
  FreeAndNil(FStandardColors);
  FreeAndNil(FSystemColors);
  FreeAndNil(FUserColors);
  FreeAndNil(FStandardColorDrawers);
  FreeAndNil(FSystemColorDrawers);
  FreeAndNil(FUserColorDrawers);
  FreeAndNil(FAddInControls);
  inherited Destroy;
end;

function TJvCustomOfficeColorPanel.CreateStandardColors(ColorList: TStrings): Integer;
var
  Index: Integer;
begin
  Result := 0;
  if ColorList <> nil then
  begin
    ColorList.BeginUpdate;
    ColorList.Clear;
    for Index := Low(StandardColorValues) to High(StandardColorValues) do
      ColorList.Values[
        ColorToString(StandardColorValues[Index].Value)] :=
        StandardColorValues[Index].Description;
    ColorList.EndUpdate;
    Result := ColorList.Count;
  end;
end;

function TJvCustomOfficeColorPanel.CreateSystemColors(ColorList: TStrings): Integer;
var
  Index: Integer;
begin
  Result := 0;
  if ColorList <> nil then
  begin
    ColorList.BeginUpdate;
    ColorList.Clear;
    for Index := Low(SysColorValues) to High(SysColorValues) do
      ColorList.Values[
        ColorToString(SysColorValues[Index].Value)] :=
        SysColorValues[Index].Description;
    Result := ColorList.Count;
    ColorList.EndUpdate;
  end;
end;

function TJvCustomOfficeColorPanel.CreateUserColors(ColorList: TStrings): Integer;
begin
  Result := 0;
end;

procedure TJvCustomOfficeColorPanel.CreateColorDrawersByColors(DrawersList: TList;
  ColorsList: TStringList; AVisible: Boolean);
var
  I, ValidColCount: Integer;
  Drawer: TJvOfficeColorDrawer;
  Value, NewHint: string;
  ExceptionRaised: Boolean;
begin
  DeleteEmptyLines(ColorsList);
  if ColorsList.Count < 1 then
    Exit;

  //ignore the record that more then MaxSectColorCount .
  ValidColCount := Min(ColorsList.Count, MaxSectColorCount);
  if DrawersList.Count > ValidColCount then
  begin
    for I := ValidColCount to DrawersList.Count - 1 do
      SetControlVisible(TControl(DrawersList[I]), False);
  end
  else
    while DrawersList.Count < ValidColCount do
    begin
      Drawer := TJvOfficeColorDrawer.Create(Self);
      with Drawer do
      begin
        CanDrawColorQuad := True;
        CanDrawGlyph := False;
        CanDrawInnerFrame := False;
        ColorQuadLayOut := cqlClient;
      end;
      DrawersList.Add(Drawer);
    end;

  for I := 0 to ValidColCount - 1 do
  begin
    ExceptionRaised := False;
    Drawer := TJvOfficeColorDrawer(DrawersList[I]);
    with Drawer do
    begin
      Parent := Self;
      GroupIndex := PrimaryGroupIndex;
      Tag := I;
      Flat := True;
      try
        Value := Trim(ColorsList.Names[I]);
        DrawColor := StringToColor(Value);
      except
        on EConvertError do
        begin
          DrawColor := clDefault;
          DisabledDrawColor := Self.BackColor;
          ExceptionRaised := True;
        end;
      end;

      if ExceptionRaised then
      begin
        Drawer.Enabled := False;
        SetControlVisible(Drawer, False);
      end
      else
        SetControlVisible(Drawer, AVisible);
      {$IFDEF COMPILER7_UP}
      NewHint := ColorsList.ValueFromIndex[I];
      {$ELSE}
      NewHint := ColorsList.Values[ColorsList.Names[I]];
      {$ENDIF}
      if SameText(NewHint, cUseDefaultColorHint) then
        NewHint := ColorToPrettyName(DrawColor);
      Hint := NewHint;
      Transparent := False;
      Color := Self.BackColor;
      DisabledDrawColor := Self.BackColor;
      HotTrack := Self.HotTrack;
      HotTrackFont := Self.HotTrackFont;
      HotTrackFontOptions := Self.HotTrackFontOptions;
      HotTrackOptions := Self.HotTrackOptions;

      OnClick := DoColorButtonClick;
      OnEnabledChanged := DoColorDrawersEnabledChange;
      OnMouseUp := RedirectToColorButtonClick;
    end;
  end;
end;

procedure TJvCustomOfficeColorPanel.CreateStandardColorDrawers;
begin
  CreateColorDrawersByColors(FStandardColorDrawers, FStandardColors, Properties.ShowStandardColors);
end;

procedure TJvCustomOfficeColorPanel.CreateSystemColorDrawers;
begin
  CreateColorDrawersByColors(FSystemColorDrawers, FSystemColors, Properties.ShowSystemColors);
end;

procedure TJvCustomOfficeColorPanel.CreateUserColorDrawers;
begin
  CreateColorDrawersByColors(FUserColorDrawers, FUserColors, Properties.ShowUserColors);
end;

procedure TJvCustomOfficeColorPanel.RearrangeControls;
var
  I, CtrlHeight: Integer;
  ValidColCount: Integer;
  TempTopInc: Integer;
  StdColBtnsTop, StdColBtnsLeft: Integer;
  SysColBtnsTop, SysColBtnsLeft: Integer;
  UserColBtnsTop, UserColBtnsLeft: Integer;
  Control, BottomControl: TControl; // the bottom most Button
  CaclButtonWidth: Integer;
  HotTrackIntf: IJvHotTrack;
  R: TRect;
  lSiteInfo: TJvAddInControlSiteInfo;
begin
  if (not FInited) or (Parent = nil) or FInRearrangeControls then
    Exit;
  DisableAlign;
  BottomControl := nil;
  TempTopInc := 0;
  FInRearrangeControls := True;

  with Properties do
  begin
    CaclButtonWidth := LineColorCount * (ColorSize + ColorSpace) - ColorSpace;
    StdColBtnsTop := BorderWidth;
    StdColBtnsLeft :=BorderWidth + HorizontalMargin;

    //set None Color Button Bounds
    //do't direct set value to Visible property.
    SetControlVisible(FButtonNoneColor, ShowNoneColor);
    if FButtonNoneColor.Visible then
    begin
      FButtonNoneColor.Caption := NoneColorCaption;
      FButtonNoneColor.SetBounds(HorizontalMargin + BorderWidth,BorderWidth +  TopMargin + TempTopInc,
        CaclButtonWidth, ButtonHeight);
      StdColBtnsTop := FButtonNoneColor.Top + FButtonNoneColor.Height;
      StdColBtnsLeft := FButtonNoneColor.Left;
      TempTopInc := BorderWidth + TopMargin + FButtonNoneColor.Height;
      BottomControl := FButtonNoneColor;
    end;

    //set Default Color Button Bounds
    SetControlVisible(FButtonDefaultColor, ShowDefaultColor);
    if FButtonDefaultColor.Visible then
    begin
      FButtonDefaultColor.Caption := DefaultColorCaption;
      FButtonDefaultColor.SetBounds(StdColBtnsLeft, TopMargin + TempTopInc,
        CaclButtonWidth, ButtonHeight);
      StdColBtnsTop := FButtonDefaultColor.Top + FButtonDefaultColor.Height;
      StdColBtnsLeft := FButtonDefaultColor.Left;
      BottomControl := FButtonDefaultColor;
    end;

    //set Standard Colors drawer position
    ValidColCount := Min(FStandardColors.Count, MaxSectColorCount);
    if FStandardColorDrawers.Count < ValidColCount then
      CreateStandardColorDrawers
    else
    if FStandardColorDrawers.Count > ValidColCount then
      for I := ValidColCount to FStandardColorDrawers.Count - 1 do
        SetControlVisible(TControl(FStandardColorDrawers[I]), False);

    for I := 0 to ValidColCount - 1 do
    begin
      Control := TJvColorSpeedButton(FStandardColorDrawers[I]);

      SetControlVisible(Control, ShowStandardColors);
      if Control.Visible then
      begin
        Control.SetBounds(StdColBtnsLeft + (I mod LineColorCount) * (ColorSpace
          + ColorSize),
          StdColBtnsTop + ColorSpaceTop + (I div LineColorCount) * (ColorSpace
          + ColorSize),
          ColorSize, ColorSize);
        BottomControl := Control;
      end;
    end;

    SysColBtnsLeft := StdColBtnsLeft;
    UserColBtnsLeft := StdColBtnsLeft;
    if not Properties.ShowStandardColors then
      SysColBtnsTop := StdColBtnsTop
    else
      SysColBtnsTop := BottomControl.Top + BottomControl.Height;

    //System colors

    ValidColCount := Min(FSystemColors.Count, MaxSectColorCount);
    if FSystemColorDrawers.Count < ValidColCount then
      CreateSystemColorDrawers
    else
    if FSystemColorDrawers.Count > ValidColCount then
      for I := ValidColCount to FSystemColorDrawers.Count - 1 do
        SetControlVisible(TControl(FSystemColorDrawers[I]), False);

    //set Divider Line position up System Colors
    SetControlVisible(FDividerLine1, (ShowSystemColors and (FSystemColorDrawers.Count > 0)) and
      ShowStandardColors);
    if FDividerLine1.Visible then
      with FDividerLine1 do
      begin
        SetBounds(StdColBtnsLeft, SysColBtnsTop + ColorSpaceBottom, CaclButtonWidth, Height);
        SysColBtnsTop := Top + Height;
      end;

    //set system Colors drawer position
    for I := 0 to ValidColCount - 1 do
    begin
      Control := TControl(FSystemColorDrawers[I]);

      SetControlVisible(Control, ShowSystemColors);
      if Control.Visible then
      begin
        Control.SetBounds(SysColBtnsLeft + (I mod LineColorCount) * (ColorSpace + ColorSize),
          SysColBtnsTop + ColorSpaceTop + (I div LineColorCount) * (ColorSpace + ColorSize),
          ColorSize, ColorSize);
        BottomControl := Control;
      end;
    end;

    if not Properties.ShowSystemColors then
      UserColBtnsTop := SysColBtnsTop
    else
      UserColBtnsTop := BottomControl.Top + BottomControl.Height;

    // User colors
    ValidColCount := Min(FUserColors.Count, MaxSectColorCount);
    if FUserColorDrawers.Count < ValidColCount then
      CreateUserColorDrawers
    else
    if FUserColorDrawers.Count > ValidColCount then
      for I := ValidColCount to FUserColorDrawers.Count - 1 do
        SetControlVisible(TControl(FUserColorDrawers[I]), False);

    // set Divider Line position up user Colors
    SetControlVisible(FDividerLine2,
      (ShowUserColors and (FUserColorDrawers.Count > 0)) and
      (ShowSystemColors or ShowStandardColors));
    if FDividerLine2.Visible then
      with FDividerLine2 do
      begin
        SetBounds(StdColBtnsLeft, UserColBtnsTop + ColorSpaceBottom,
          CaclButtonWidth, Height);
        UserColBtnsTop := Top + Height;
      end;

    // set User Colors drawer position
    for I := 0 to ValidColCount - 1 do
    begin
      Control := TControl(FUserColorDrawers[I]);

      SetControlVisible(Control, ShowUserColors);
      if Control.Visible then
      begin
        Control.SetBounds(UserColBtnsLeft + (I mod LineColorCount) *
          (ColorSpace + ColorSize),
          UserColBtnsTop + ColorSpaceTop + (I div LineColorCount) * (ColorSpace + ColorSize),
          ColorSize, ColorSize);
        BottomControl := Control;
      end;
    end;

    //set Divider Line position up custom color button
    if BottomControl = nil then
      TempTopInc := TopMargin
    else
      TempTopInc := BottomControl.Top + BottomControl.Height +
        ColorSpaceBottom;

    SetControlVisible(FDividerLine3,
      (ShowCustomColor and
      (ShowSystemColors or ShowStandardColors or ShowUserColors)) );
    if FDividerLine3.Visible then
      with FDividerLine3 do
      begin
        SetBounds(StdColBtnsLeft, TempTopInc, CaclButtonWidth, Height);
        BottomControl := FDividerLine3;
      end;

    //set Custom color button Bounds
    if BottomControl = nil then
      TempTopInc := TopMargin
    else
      TempTopInc := BottomControl.Top + BottomControl.Height +
        ColorSpaceBottom;

    SetControlVisible(FCustomColorDrawer, ShowCustomColor);
    SetControlVisible(FButtonCustomColor, ShowCustomColor);
    with FCustomColorDrawer do
      if Visible then
      begin
        CtrlHeight := ButtonHeight;
        SetBounds(StdColBtnsLeft + CaclButtonWidth - CtrlHeight, TempTopInc,
          CtrlHeight, CtrlHeight);
        BottomControl := FCustomColorDrawer;
      end;
    with FButtonCustomColor do
      if Visible then
      begin
        TJvSpeedButton(FButtonCustomColor).Caption := CustomColorCaption;
        CtrlHeight := ButtonHeight;
        SetBounds(StdColBtnsLeft, TempTopInc,
          CaclButtonWidth - FCustomColorDrawer.Width - ColorSpace, CtrlHeight);
        BottomControl := FButtonCustomColor;
      end;

    //set Other Add-in Controls Bounds
    for I := 0 to FAddInControls.Count - 1 do
    begin
      Control := FAddInControls[I];
      if (Control = FButtonNoneColor) or (Control = FButtonDefaultColor) or
         (Control = FButtonCustomColor) or (Control = FCustomColorDrawer) then
        Continue;

      if BottomControl = nil then
        TempTopInc := TopMargin
      else
        TempTopInc := BottomControl.Top + BottomControl.Height + ColorSpaceBottom;

      with Control do
      begin
        if Visible then
        begin
          Parent := Self;
          CtrlHeight := Height;
          if Control is TJvCustomSpeedButton then
            with TJvSpeedButton(Control) do
            begin
              Flat := True;
              Color := Self.BackColor;
              Enabled := Self.Enabled;
            end;

          if Supports(Control, IJvHotTrack, HotTrackIntf) then
            with HotTrackIntf do
            begin
              HotTrack := Self.HotTrack;
              HotTrackFont := Self.HotTrackFont;
              HotTrackFontOptions := Self.HotTrackFontOptions;
              HotTrackOptions := Self.HotTrackOptions;
            end;
          R := Rect(StdColBtnsLeft, TempTopInc,
            StdColBtnsLeft + CaclButtonWidth, TempTopInc + CtrlHeight);
          lSiteInfo.AddInControl:= Control;
          lSiteInfo.BoundsRect := R;
          DoGetAddInControlSiteInfo(Self, lSiteInfo);
          BoundsRect := lSiteInfo.BoundsRect;

          BottomControl := Control;
        end;
      end;
    end;

    //Set panel size
    Width := (HorizontalMargin + BorderWidth) * 2 + ColorSize * LineColorCount +
      ColorSpace * (LineColorCount - 1);
    if BottomControl = nil then
      Height := 0
    else
      Height := BottomControl.Top + BottomControl.Height + BottomMargin  + BorderWidth;
  end;
  EnableAlign;

  FInRearrangeControls := False;
end;

procedure TJvCustomOfficeColorPanel.RefreshControls;
begin
  if FInRearrangeControls then
    Exit;
  with Properties do
  begin
    if ShowStandardColors then
      CreateStandardColorDrawers;
    if ShowSystemColors then
      CreateSystemColorDrawers;
    if ShowUserColors then
      CreateUserColorDrawers;
    if ShowStandardColors or ShowSystemColors or ShowUserColors then
      RearrangeControls;
  end;
end;

procedure TJvCustomOfficeColorPanel.DoColorButtonClick(Sender: TObject);
var
  LastColor: TColor;

  procedure HoldCustomColor(AColor: TColor);
  var
    StrColor: string;
  begin
    if ClickColorType = cctCustomColor then
    begin
      StrColor := ColorToString(AColor);
      if (FButtonCustomColor.DrawColor <> AColor) then
      begin //refresh CustomColorDrawer
        FButtonCustomColor.DrawColor := AColor;
        FCustomColorDrawer.DrawColor := AColor;
        FCustomColorDrawer.Hint := ColorToPrettyName(AColor);
      end;

      if Properties.HoldCustomColor and
        ((FButtonCustomColor.DrawColor <> AColor) or (UserColors.IndexOfName(StrColor) = -1)) then
      begin
        UserColors.BeginUpdate;
        try
          UserColors.Values[StrColor] := cUseDefaultColorHint;
        finally
          UserColors.EndUpdate;
        end;
        CreateUserColorDrawers;
        RearrangeControls;
        DoHoldedCustomColor(Self,AColor);
      end;
    end;
  end;

begin
  if Sender is TJvColorSpeedButton then
  begin
    if Sender = FButtonNoneColor then
      FClickColorType := cctNoneColor
    else
    if Sender = FButtonDefaultColor then
      FClickColorType := cctDefaultColor
    else
    if Sender = FButtonCustomColor then
      FClickColorType := cctCustomColor
    else
    if FAddInControls.IndexOf(Sender) <> -1 then
      FClickColorType := cctAddInControl
    else
      FClickColorType := cctColors;
  end
  else
    FClickColorType := cctNone;

  FNeedReDrawDownState := False;
  try
    if Assigned(FOnColorButtonClick) then
      FOnColorButtonClick(Sender);

    if FClickColorType <> cctNone then
    begin
      if (FClickColorType = cctCustomColor) then
      begin
        LastColor := SelectedColor;
        if Assigned(FOnShowOwnerColorDialog) then
        begin
          FOnShowOwnerColorDialog(Sender);
          if LastColor = SelectedColor then //never changed SelectedColor
          begin
            if (FPriorCheckedButton <> nil) then
              FPriorCheckedButton.Down := True;
          end
          else
            HoldCustomColor(SelectedColor);
        end
        else
        begin
          FColorDialog.Options := FColorDialogOptions;
          FColorDialog.Color := SelectedColor{FCustomColorDrawer.DrawColor};
          if FColorDialog.Execute then
          begin
            HoldCustomColor(FColorDialog.Color);
            SelectedColor := FColorDialog.Color;
          end
          else
          if FPriorCheckedButton <> nil then //cancel the color dialog selection, restore the Prior Checked Button down
             FPriorCheckedButton.Down := True;
        end;
        // the PriorCheckedButton have not been assign.
        if ((FPriorCheckedButton <> nil) and (not FPriorCheckedButton.Down)) or
          // if the PriorCheckedButton is ButtonCustom,Set CustomColorDrawer.Down to true for ever.
          (FPriorCheckedButton = FButtonCustomColor) then
          FCustomColorDrawer.Down := True;
      end
      else
        SelectedColor := TJvColorSpeedButton(Sender).DrawColor;
    end;
  finally
    FNeedReDrawDownState := True;
  end;
  if Sender is TJvCustomSpeedButton then
    // the PriorCheckedButton have not been assign.
    if (FPriorCheckedButton <> nil) and (not FPriorCheckedButton.Down) then
      FPriorCheckedButton := TJvColorSpeedButton(Sender);

  FClickColorType := cctNone;
end;

procedure TJvCustomOfficeColorPanel.DoSelectedColorChange(Sender: TObject);
begin
  if Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

procedure TJvCustomOfficeColorPanel.DoGetAddInControlSiteInfo(Sender: TControl;
  var ASiteInfo: TJvAddInControlSiteInfo);
begin
  if Assigned(FOnGetAddInControlSiteInfo) then
    FOnGetAddInControlSiteInfo(Sender, ASiteInfo);
end;

function TJvCustomOfficeColorPanel.GetControlBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
end;

procedure TJvCustomOfficeColorPanel.SetControlBackgroundColor(const Value: TColor);
var
  I: Integer;
begin
  if FBackgroundColor <> Value then
  begin
    inherited Color := Value;
    FBackgroundColor := Value;
    for I := 0 to FStandardColorDrawers.Count - 1 do
      with TJvColorSpeedButton(FStandardColorDrawers[I]) do
      begin
        Color := Value;
        DisabledDrawColor := Value;
      end;
    for I := 0 to FSystemColorDrawers.Count - 1 do
      with TJvColorSpeedButton(FSystemColorDrawers[I]) do
      begin
        Color := Value;
        DisabledDrawColor := Value;
      end;
    for I := 0 to FUserColorDrawers.Count - 1 do
      with TJvColorSpeedButton(FUserColorDrawers[I]) do
      begin
        Color := Value;
        DisabledDrawColor := Value;
      end;
    for I := 0 to FAddInControls.Count - 1 do
    begin
      TControlAccessProtected(FAddInControls[I]).Color := Value;
      if TObject(FAddInControls[I]) is TJvColorSpeedButton then
        TJvColorSpeedButton(FAddInControls[I]).DisabledDrawColor := Value;
    end;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetSelectedColor(const Value: TColor);

  function FindColorButton(Buttons: TList; ValidColCount: Integer): TJvColorSpeedButton;
  var
    I: Integer;
  begin
    for I := 0 to ValidColCount - 1 do
    begin
      Result := TJvColorSpeedButton(Buttons[I]);
      if Result.DrawColor = Value then
        Exit;
    end;
    Result := nil;
  end;

var
  Button: TJvColorSpeedButton;
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    if FNeedReDrawDownState then
    begin
      if FButtonDefaultColor.DrawColor = Value then
      begin
        FButtonDefaultColor.Down := True;
        if FPriorCheckedButton = nil then
          FPriorCheckedButton := FButtonDefaultColor;
      end
      else
      begin
        FButtonDefaultColor.Down := False;
        Button := FindColorButton(FStandardColorDrawers, Min(FStandardColorDrawers.Count, MaxSectColorCount));
        if Button <> nil then
          Button.Down := True // all other buttons automatically switch to False due to their GroupIndex
        else
        begin
          Button := FindColorButton(FSystemColorDrawers, Min(FSystemColorDrawers.Count, MaxSectColorCount));
          if Button <> nil then
            Button.Down := True // all other buttons automatically switch to False due to their GroupIndex
          else
          begin
            Button := FindColorButton(FUserColorDrawers, Min(FUserColorDrawers.Count, MaxSectColorCount));
            if Button <> nil then
              Button.Down := True // all other buttons automatically switch to False due to their GroupIndex
          end;
        end;
        if (FPriorCheckedButton = nil) and (Button <> nil) then
          FPriorCheckedButton := Button;
      end;
    end;

    DoSelectedColorChange(Self);
  end;
end;

function TJvCustomOfficeColorPanel.GetHotTrack: Boolean;
begin
  Result := FHotTrack;
end;

function TJvCustomOfficeColorPanel.GetHotTrackFont: TFont;
begin
  Result := FHotTrackFont;
end;

function TJvCustomOfficeColorPanel.GetHotTrackFontOptions: TJvTrackFontOptions;
begin
  Result := FHotTrackFontOptions;
end;

function TJvCustomOfficeColorPanel.GetHotTrackOptions: TJvHotTrackOptions;
begin
  Result := FHotTrackOptions;
end;

procedure TJvCustomOfficeColorPanel.SetHotTrack(Value: Boolean);
var
  I: Integer;
  HotTrackIntf: IJvHotTrack;
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    for I := 0 to FStandardColorDrawers.Count - 1 do
      TJvColorSpeedButton(FStandardColorDrawers[I]).HotTrack := Value;
    for I := 0 to FSystemColorDrawers.Count - 1 do
      TJvColorSpeedButton(FSystemColorDrawers[I]).HotTrack := Value;
    for I := 0 to FUserColorDrawers.Count - 1 do
      TJvColorSpeedButton(FUserColorDrawers[I]).HotTrack := Value;
    for I := 0 to FAddInControls.Count - 1 do
      if Supports(TObject(FAddInControls[I]), IJvHotTrack, HotTrackIntf) then
        HotTrackIntf.HotTrack := Value;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetHotTrackFont(Value: TFont);
var
  I: Integer;
  HotTrackIntf: IJvHotTrack;
begin
  if (FHotTrackFont <> Value) and (Value <> nil) then
  begin
    FHotTrackFont.Assign(Value);
    for I := 0 to FStandardColorDrawers.Count - 1 do
      TJvColorSpeedButton(FStandardColorDrawers[I]).HotTrackFont := Value;
    for I := 0 to FSystemColorDrawers.Count - 1 do
      TJvColorSpeedButton(FSystemColorDrawers[I]).HotTrackFont := Value;
    for I := 0 to FAddInControls.Count - 1 do
      if Supports(TObject(FAddInControls[I]), IJvHotTrack, HotTrackIntf) then
        HotTrackIntf.HotTrackFont := Value;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetHotTrackFontOptions(Value: TJvTrackFontOptions);
var
  I: Integer;
  HotTrackIntf: IJvHotTrack;
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    for I := 0 to FStandardColorDrawers.Count - 1 do
      TJvColorSpeedButton(FStandardColorDrawers[I]).HotTrackFontOptions :=
        Value;
    for I := 0 to FSystemColorDrawers.Count - 1 do
      TJvColorSpeedButton(FSystemColorDrawers[I]).HotTrackFontOptions := Value;
    for I := 0 to FAddInControls.Count - 1 do
      if Supports(TObject(FAddInControls[I]), IJvHotTrack, HotTrackIntf) then
        HotTrackIntf.HotTrackFontOptions := Value;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetHotTrackOptions(Value: TJvHotTrackOptions);
var
  I: Integer;
  HotTrackIntf: IJvHotTrack;
begin
  if (FHotTrackOptions <> Value) and (Value <> nil) then
  begin
    FHotTrackOptions.Assign(Value);
    for I := 0 to FStandardColorDrawers.Count - 1 do
      TJvColorSpeedButton(FStandardColorDrawers[I]).HotTrackOptions := Value;
    for I := 0 to FSystemColorDrawers.Count - 1 do
      TJvColorSpeedButton(FSystemColorDrawers[I]).HotTrackOptions := Value;
    for I := 0 to FUserColorDrawers.Count - 1 do
      TJvColorSpeedButton(FUserColorDrawers[I]).HotTrackOptions := Value;

    for I := 0 to FAddInControls.Count - 1 do
      if Supports(TObject(FAddInControls[I]), IJvHotTrack, HotTrackIntf) then
        HotTrackIntf.HotTrackOptions := Value;
  end;
end;

procedure TJvCustomOfficeColorPanel.IJvHotTrack_Assign(
  Source: IJvHotTrack);
begin
  if (Source <> nil) and (IJvHotTrack(Self) <> Source) then
  begin
    HotTrack := Source.HotTrack;
    HotTrackFont :=Source.HotTrackFont;
    HotTrackFontOptions := Source.HotTrackFontOptions;
    HotTrackOptions := Source.HotTrackOptions;
  end;
end;

procedure TJvCustomOfficeColorPanel.DoHotTrackOptionsChanged(Sender: TObject);
var
  I: Integer;
  HotTrackIntf: IJvHotTrack;
  DrawInnerFrame: Boolean;
begin
  DrawInnerFrame := not HotTrackOptions.Enabled;
  FButtonDefaultColor.CanDrawInnerFrame := DrawInnerFrame;
  FButtonCustomColor.CanDrawInnerFrame := DrawInnerFrame;

  for I := 0 to FStandardColorDrawers.Count - 1 do
    TJvColorSpeedButton(FStandardColorDrawers[I]).HotTrackOptions :=
      FHotTrackOptions;
  for I := 0 to FSystemColorDrawers.Count - 1 do
    TJvColorSpeedButton(FSystemColorDrawers[I]).HotTrackOptions :=
      FHotTrackOptions;
  for I := 0 to FUserColorDrawers.Count - 1 do
    TJvColorSpeedButton(FUserColorDrawers[I]).HotTrackOptions :=
      FHotTrackOptions;
  for I := 0 to FAddInControls.Count - 1 do
    if Supports(TObject(FAddInControls[I]), IJvHotTrack, HotTrackIntf) then
      HotTrackIntf.HotTrackOptions := FHotTrackOptions;
end;

function TJvCustomOfficeColorPanel.GetColorDlgCustomColors: TStrings;
begin
  Result := FColorDialog.CustomColors;
end;

procedure TJvCustomOfficeColorPanel.SetColorDlgCustomColors(const Value: TStrings);
begin
  FColorDialog.CustomColors.Assign(Value);
end;

procedure TJvCustomOfficeColorPanel.SetStandardColors(const Value: TStringList);
begin
  if (FStandardColors <> Value) and (Value <> nil) then
  begin
    FStandardColors.Assign(Value);
    RefreshControls;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetSystemColors(const Value: TStringList);
begin
  if (FSystemColors <> Value) and (Value <> nil) then
  begin
    FSystemColors.Assign(Value);
    RefreshControls;
  end;
end;

procedure TJvCustomOfficeColorPanel.SetUserColors(const Value: TStringList);
begin
  if (FUserColors <> Value) and (Value <> nil) then
  begin
    FUserColors.Assign(Value);
    RefreshControls;
  end;
end;

procedure TJvCustomOfficeColorPanel.DoColorDrawersEnabledChange(Sender: TObject);
begin
  if Sender is TJvColorSpeedButton then
    TJvColorSpeedButton(Sender).DisabledDrawColor := BackColor;
end;

procedure TJvCustomOfficeColorPanel.DoLoadedUserColors(Sender: TObject);
begin
  FUserColors.OnChange := nil; // Run once time only.
  CreateUserColorDrawers;
end;

procedure TJvCustomOfficeColorPanel.DoHoldedCustomColor(Sender: TObject;AColor: TColor);
begin
  if Assigned(FOnHoldCustomColor) then
    FOnHoldCustomColor(Self,AColor);
end;

procedure TJvCustomOfficeColorPanel.Loaded;
begin
  inherited Loaded;
  FUserColors.OnChange := nil;
end;

procedure TJvCustomOfficeColorPanel.Resize;
begin
  inherited Resize;
  if not FInRearrangeControls then
    RearrangeControls;
end;

procedure TJvCustomOfficeColorPanel.SetEnabled(Value: Boolean);
var
  I: Integer;
begin
  inherited SetEnabled(Value);
  for I := 0 to FStandardColorDrawers.Count - 1 do
    TControl(FStandardColorDrawers[I]).Enabled := Value;
  for I := 0 to FSystemColorDrawers.Count - 1 do
    TControl(FSystemColorDrawers[I]).Enabled := Value;
  for I := 0 to FUserColorDrawers.Count - 1 do
    TControl(FUserColorDrawers[I]).Enabled := Value;
  for I := 0 to FAddInControls.Count - 1 do
    TControl(FAddInControls[I]).Enabled := Value;
end;

procedure TJvCustomOfficeColorPanel.ShowHintChanged;
var
  I: Integer;
begin
  inherited ShowHintChanged;
  for I := 0 to FStandardColorDrawers.Count - 1 do
    TControl(FStandardColorDrawers[I]).ShowHint := ShowHint;
  for I := 0 to FSystemColorDrawers.Count - 1 do
    TControl(FSystemColorDrawers[I]).ShowHint := ShowHint;
  for I := 0 to FUserColorDrawers.Count - 1 do
    TControl(FUserColorDrawers[I]).ShowHint := ShowHint;
  for I := 0 to FAddInControls.Count - 1 do
    TControl(FAddInControls[I]).ShowHint := ShowHint;
end;



procedure TJvCustomOfficeColorPanel.SetColorDialogOptions(const Value: TColorDialogOptions);
begin
  FColorDialogOptions := Value;
end;

procedure TJvCustomOfficeColorPanel.CreateWnd;
begin
  inherited CreateWnd;
  RearrangeControls;
end;





procedure TJvCustomOfficeColorPanel.SetProperties(const Value: TJvOfficeColorPanelProperties);
begin
  if (FProperties <> Value) and (Value <> nil) then
    FProperties.Assign(Value);
end;

procedure TJvCustomOfficeColorPanel.DoPropertiesChanged(Sender: TObject;
  const PropName: string);
var
  RealignFlag: Boolean;
  I: Integer;
begin
  { The initial value is do't Adjust size,if you wan't adjust size when a property changed,
    add a sentence like following to do nothing to skip notify. }
  RealignFlag := False;

  if SameText(PropName, cNoneColorCaption) then
    FButtonNoneColor.Caption := Properties.NoneColorCaption
  else
  if SameText(PropName, cDefaultColorCaption) then
    FButtonDefaultColor.Caption := Properties.DefaultColorCaption
  else
  if SameText(PropName, cCustomColorCaption) then
    FButtonCustomColor.Caption := Properties.CustomColorCaption
  else
  if SameText(PropName, cNoneColorHint) then
    FButtonNoneColor.Hint := Properties.NoneColorHint
  else
  if SameText(PropName, cDefaultColorHint) then
    FButtonDefaultColor.Hint := Properties.DefaultColorHint
  else
  if SameText(PropName, cCustomColorHint) then
    FButtonCustomColor.Hint := Properties.CustomColorHint
  else
  if SameText(PropName, cNoneColorColor) then
    FButtonNoneColor.DrawColor := Properties.NoneColorColor
  else
  if SameText(PropName, cDefaultColorColor) then
    FButtonDefaultColor.DrawColor := Properties.DefaultColorColor
  else
  if SameText(PropName, cNoneColorFont) then
    FButtonNoneColor.Font := Properties.NoneColorFont
  else
  if SameText(PropName, cDefaultColorFont) then
    FButtonDefaultColor.Font := Properties.DefaultColorFont
  else
  if SameText(PropName, cCustomColorFont) then
    FButtonCustomColor.Font := Properties.CustomColorFont
  else
  if SameText(PropName, cShowAddInHint) then
  begin
    FButtonNoneColor.ShowHint := Properties.ShowAddInHint;
    FButtonDefaultColor.ShowHint := Properties.ShowAddInHint;
    FButtonCustomColor.ShowHint := Properties.ShowAddInHint;
  end
  else
  if SameText(PropName, cShowColorsHint) then
  begin
    for I := 0 to FStandardColorDrawers.Count - 1 do
      TControl(FStandardColorDrawers[I]).ShowHint := Properties.ShowColorsHint;
    for I := 0 to FSystemColorDrawers.Count - 1 do
      TControl(FSystemColorDrawers[I]).ShowHint := Properties.ShowColorsHint;
    for I := 0 to FUserColorDrawers.Count - 1 do
      TControl(FUserColorDrawers[I]).ShowHint := Properties.ShowColorsHint;
  end
  else
  if SameText(PropName, cRightClickSelect) or
    SameText(PropName, cHoldCustomColor) or
    SameText(PropName, cRightClickSelect) then
  else // Other property change will adjust size by default.
    RealignFlag := True;

  if RealignFlag then
    RearrangeControls;
end;

procedure TJvCustomOfficeColorPanel.RedirectToColorButtonClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Parent: TWinControl;
begin
  if Assigned(OnMouseUp) then
    OnMouseUp(Sender, Button, Shift, X, Y);

  // If any of the possible parents has a popup menu, we let it
  // run, and do not select the button (hence the exit), unless
  // the properties tell us to select anyway
  Parent := Self;
  while Assigned(Parent) do
    if Assigned(TControlAccessProtected(Parent).PopupMenu) then
    begin
      if not Properties.SelectIfPopup then
        Exit;
    end
    else
      Parent := Parent.Parent;

  // if the user asked not to right click select, we stop here
  if not Properties.RightClickSelect then
    Exit;

  if Button = mbRight then
    DoColorButtonClick(Sender);
end;

{ TJvOfficeColorPanel }

procedure TJvOfficeColorPanel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { For backwards compatibility }
  FFilerTag := 'Color';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
  FFilerTag := 'Flat';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
  FFilerTag := 'CustomColors';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
  FFilerTag := 'Options';
  Filer.DefineProperty(FFilerTag, ReadData, nil, False);
end;

procedure TJvOfficeColorPanel.ReadData(Reader: TReader);
begin
  if SameText(FFilerTag, 'Color') then
    SelectedColor := JvReaderReadColor(Reader)
  else
  if SameText(FFilerTag, 'Flat') then
    FlatBorder := Reader.ReadBoolean
  else
  if SameText(FFilerTag, 'CustomColors') then
    JvReaderReadStrings(Reader,ColorDlgCustomColors)
  else
  if SameText(FFilerTag, 'Options') then
    ColorDialogOptions := JvReaderReadColorDialogOptions(Reader)
  ;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

