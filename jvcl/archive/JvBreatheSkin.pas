{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBreatheSkin.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBreatheSkin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Registry, IniFiles, jpeg, Menus, JvBehaviorLabel,
  JvSlider, JvLabel, JvPcx, JvImage, JvTypes, JVCLVer;

type
  TJvBreatheLabel = class(TPersistent)
  private
    FLabel: TJvLabel;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetColor: TColor;
    function GetFont: TFont;
    function GetTransparent: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetTransparent(const Value: Boolean);
    function GetHint: string;
    function GetShowHint: Boolean;
    procedure SetHint(const Value: string);
    procedure SetShowHint(const Value: Boolean);
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
  public
    constructor Create(AOwner: TComponent);
  published
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Caption: string read FCaption write SetCaption;
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
  end;

  TJvBreatheScrollLabel = class(TPersistent)
  private
    FLabel: TJvBehaviorLabel;
    FHeight: Integer;
    FTop: Integer;
    FWidth: Integer;
    FLeft: Integer;
    FCaption: string;
    function GetColor: TColor;
    function GetDirection: TJvLabelScrollDirection;
    function GetFont: TFont;
    function GetHint: string;
    function GetInterval: Cardinal;
//    function GetNoGrap: Boolean;
    function GetScrolling: Boolean;
    function GetShowHint: Boolean;
    function GetTransparent: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetDirection(const Value: TJvLabelScrollDirection);
    procedure SetFont(const Value: TFont);
    procedure SetHeight(const Value: Integer);
    procedure SetHint(const Value: string);
    procedure SetInterval(const Value: Cardinal);
    procedure SetLeft(const Value: Integer);
//    procedure SetNoGrap(const Value: Boolean);
    procedure SetScrolling(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
    procedure SetTop(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create(AOwner: TComponent);
  published
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Caption: string read FCaption write SetCaption;
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property Scrolling: Boolean read GetScrolling write SetScrolling;
    property ScrollDirection: TJvLabelScrollDirection read GetDirection write SetDirection;
    property ScrollInterval: Cardinal read GetInterval write SetInterval;
//    property NoGrap: Boolean read GetNoGrap write SetNoGrap;
  end;

  TJvBreatheButton = class(TPersistent)
  private
    FButton: TJvImage;
    function GetEnabled: Boolean;
    function GetHint: string;
    function GetShowHint: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetShowHint(const Value: Boolean);
    function GetMouseDown: TMouseEvent;
    function GetMouseMove: TMouseMoveEvent;
    function GetOnClick: TNotifyEvent;
    function GetOnDblClick: TNotifyEvent;
    function GetPopupMenu: TPopupMenu;
    function GetVisible: Boolean;
    procedure SetMouseDown(const Value: TMouseEvent);
    procedure SetMouveMove(const Value: TMouseMoveEvent);
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnDblClick(const Value: TNotifyEvent);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetVisible(const Value: Boolean);
    function GetMouseUp: TMouseEvent;
    procedure SetMouseUp(const Value: TMouseEvent);
  public
    constructor Create(AOwner: TComponent);
  published
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnMouseMove: TMouseMoveEvent read GetMouseMove write SetMouveMove;
    property OnMouseDown: TMouseEvent read GetMouseDown write SetMouseDown;
    property OnMouseUp: TMouseEvent read GetMouseUp write SetMouseUp;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
  end;

  TJvBreatheButtons = class(TPersistent)
  private
    FPlaylist: TJvBreatheButton;
    FPrev: TJvBreatheButton;
    FMove: TJvBreatheButton;
    FPlay: TJvBreatheButton;
    FNext: TJvBreatheButton;
    FExit: TJvBreatheButton;
    FOptions: TJvBreatheButton;
    FOpen: TJvBreatheButton;
    FPause: TJvBreatheButton;
    FId3: TJvBreatheButton;
    FForward: TJvBreatheButton;
    FRewind: TJvBreatheButton;
    FStop: TJvBreatheButton;
    FMinimize: TJvBreatheButton;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Play: TJvBreatheButton read FPlay write FPlay;
    property Stop: TJvBreatheButton read FStop write FStop;
    property Pause: TJvBreatheButton read FPause write FPause;
    property Next: TJvBreatheButton read FNext write FNext;
    property Previous: TJvBreatheButton read FPrev write FPrev;
    property Open: TJvBreatheButton read FOpen write FOpen;
    property Exit: TJvBreatheButton read FExit write FExit;
    property Move: TJvBreatheButton read FMove write FMove;
    property Minimize: TJvBreatheButton read FMinimize write FMinimize;
    property Id3: TJvBreatheButton read FId3 write FId3;
    // (rom) Forward is not a good name.
    property Forward: TJvBreatheButton read FForward write FForward;
    property Rewind: TJvBreatheButton read FRewind write FRewind;
    property Options: TJvBreatheButton read FOptions write FOptions;
    property Playlist: TJvBreatheButton read FPlaylist write FPlaylist;
  end;

  TJvBreatheLabels = class(TPersistent)
  private
    FStatus: TJvBreatheLabel;
    FBitRate: TJvBreatheLabel;
    FTime: TJvBreatheLabel;
    FFrequency: TJvBreatheLabel;
    FSongName: TJvBreatheScrollLabel;
    FTotalInfo: TJvBreatheLabel;
    FTotalTime: TJvBreatheLabel;
    FLayer: TJvBreatheLabel;
    FVersion: TJvBreatheLabel;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Status: TJvBreatheLabel read FStatus write FStatus;
    property CurrentTime: TJvBreatheLabel read FTime write FTime;
    property Layer: TJvBreatheLabel read FLayer write FLayer;
    property BitRate: TJvBreatheLabel read FBitRate write FBitRate;
    property Frequency: TJvBreatheLabel read FFrequency write FFrequency;
    property Version: TJvBreatheLabel read FVersion write FVersion;
    property SongName: TJvBreatheScrollLabel read FSongName write FSongName;
    property TotalTime: TJvBreatheLabel read FTotalTime write FTotalTime;
    property TotalInfo: TJvBreatheLabel read FTotalInfo write FTotalInfo;
  end;

  TJvBreatheOption = class(TPersistent)
  private
    FAutoSize: Boolean;
    FOnChange: TNotifyEvent;
    FAutoMove: Boolean;
    FAutoRegion: Boolean;
    FEasyMove: Boolean;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetAutoMove(const Value: Boolean);
    procedure SetAutoRegion(const Value: Boolean);
    procedure SetEasyMove(const Value: Boolean);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent);
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property AutoMove: Boolean read FAutoMove write SetAutoMove default True;
    property AutoRegion: Boolean read FAutoRegion write SetAutoRegion default False;
    property EasyMove: Boolean read FEasyMove write SetEasyMove default False;
  end;

  TJvBreatheVolume = class(TPersistent)
  private
    FSlider: TJvSlider;
    function GetMaximum: Integer;
    function GetPosition: Integer;
    procedure SetMaximum(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    function GetHint: string;
    function GetShow: Boolean;
    procedure SetHint(const Value: string);
    procedure SetShow(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Maximum: Integer read GetMaximum write SetMaximum;
    property Position: Integer read GetPosition write SetPosition;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShow write SetShow;
  end;

  TJvBreathePosition = class(TPersistent)
  private
    FSlider: TJvSlider;
    function GetMaximum: Integer;
    function GetPosition: Integer;
    procedure SetMaximum(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    function GetHint: string;
    procedure SetHint(const Value: string);
    function GetShow: Boolean;
    procedure SetShow(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Maximum: Integer read GetMaximum write SetMaximum;
    property Position: Integer read GetPosition write SetPosition;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShow write SetShow;
  end;

  TJvBreatheSkin = class(TWinControl)
  private
    // Internal data
    FImg: TImage;
    FSkin: TFileName;
    FBOptions: TJvBreatheOption;
    FLabels: TJvBreatheLabels;
    FTAbout: TStringList;
    FLastX: Integer;
    FLastY: Integer;

    // Sliders
    FVolume: TJvBreatheVolume;
    FPosition: TJvBreathePosition;

    // Images
    FBack: TBitmap;
    FDeact: TBitmap;
    FInfo: TBItmap;
    FMask: TBitmap;
    FOver: TBitmap;
    FPosRuler: TBitmap;
    FPosThumb: TBitmap;
    FVolRuler: TBitmap;
    FVolThumb: TBitmap;
    FOnMoveDown: TMouseEvent;
    FOnMoveMove: TMouseMoveEvent;
    FButtons: TJvBreatheButtons;
    FOnSkinLoaded: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;

    // Methods
    procedure LoadDefault;
    procedure InitBtn(Value: TJvImage);
    procedure SetSkin(const Value: TFileName);
    procedure LoadSkin(Value: string);
    procedure InitAllBtn;
    procedure DecoupeOver(Value: TJvImage);
    procedure DecoupeOverAll;
    procedure LoadBtn(Ini: TIniFile; Value: TJvImage; Prefix: string);
    procedure LoadSlider(Ini: TIniFile; Value: TJvSlider; Prefix: string);
    procedure LoadLabel(Ini: TIniFile; Value: TJvBreatheLabel; Prefix: string); overload;
    procedure LoadLabel(Ini: TIniFile; Value: TJvBreatheScrollLabel; Prefix: string); overload;
    procedure OptionsChanged(Sender: TObject);
    function GetOnExit: TNotifyEvent;
    function GetOnForward: TNotifyEvent;
    function GetOnID3: TNotifyEvent;
    function GetOnMinimize: TNotifyEvent;
    function GetOnMove: TNotifyEvent;
    function GetOnNext: TNotifyEvent;
    function GetOnOpen: TNotifyEvent;
    function GetOnOptions: TNotifyEvent;
    function GetOnPause: TNotifyEvent;
    function GetOnPlay: TNotifyEvent;
    function GetOnPlaylist: TNotifyEvent;
    function GetOnPosition: TNotifyEvent;
    function GetOnPrevious: TNotifyEvent;
    function GetOnRewind: TNotifyEvent;
    function GetOnStop: TNotifyEvent;
    function GetOnVolume: TNotifyEvent;
    procedure SetOnExit(const Value: TNotifyEvent);
    procedure SetOnForward(const Value: TNotifyEvent);
    procedure SetOnID3(const Value: TNotifyEvent);
    procedure SetOnMinimize(const Value: TNotifyEvent);
    procedure SetOnMove(const Value: TNotifyEvent);
    procedure SetOnNext(const Value: TNotifyEvent);
    procedure SetOnOpen(const Value: TNotifyEvent);
    procedure SetOnOptions(const Value: TNotifyEvent);
    procedure SetOnPause(const Value: TNotifyEvent);
    procedure SetOnPlay(const Value: TNotifyEvent);
    procedure SetOnPlaylist(const Value: TNotifyEvent);
    procedure SetOnPosition(const Value: TNotifyEvent);
    procedure SetOnPrevious(const Value: TNotifyEvent);
    procedure SetOnRewind(const Value: TNotifyEvent);
    procedure SetOnStop(const Value: TNotifyEvent);
    procedure SetOnVolume(const Value: TNotifyEvent);
    function GetOnPosChanging: TNotifyEvent;
    function GetOnVolChanging: TNotifyEvent;
    procedure SetOnPosChanging(const Value: TNotifyEvent);
    procedure SetOnVolChanging(const Value: TNotifyEvent);
    function GetCurrentTime: TNotifyEvent;
    function GetOnBitRate: TNotifyEvent;
    function GetOnFrequency: TNotifyEvent;
    function GetOnLayer: TNotifyEvent;
    function GetOnSongName: TNotifyEvent;
    function GetOnStatus: TNotifyEvent;
    function GetOnTotalInfo: TNotifyEvent;
    function GetOnTotalTime: TNotifyEvent;
    function GetOnVersion: TNotifyEvent;
    procedure SetCurrentTime(const Value: TNotifyEvent);
    procedure SetOnBitRate(const Value: TNotifyEvent);
    procedure SetOnFrequency(const Value: TNotifyEvent);
    procedure SetOnLayer(const Value: TNotifyEvent);
    procedure SetOnSongName(const Value: TNotifyEvent);
    procedure SetOnStatus(const Value: TNotifyEvent);
    procedure SetOnTotalInfo(const Value: TNotifyEvent);
    procedure SetOnTotalTime(const Value: TNotifyEvent);
    procedure SetOnVersion(const Value: TNotifyEvent);
    procedure MoveFormDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MoveFormMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EasyFormDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EasyFormMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property SkinFile: TFileName read FSkin write SetSkin;
    property Options: TJvBreatheOption read FBOptions write FBOptions;
    property Labels: TJvBreatheLabels read FLabels write FLabels;
    property Volume: TJvBreatheVolume read FVolume write FVolume;
    property Buttons: TJvBreatheButtons read FButtons write FButtons;
    property Position: TJvBreathePosition read FPosition write FPosition;
    property OnPlayClick: TNotifyEvent read GetOnPlay write SetOnPlay;
    property OnStopClick: TNotifyEvent read GetOnStop write SetOnStop;
    property OnPauseClick: TNotifyEvent read GetOnPause write SetOnPause;
    property OnNextClick: TNotifyEvent read GetOnNext write SetOnNext;
    property OnPreviousClick: TNotifyEvent read GetOnPrevious write SetOnPrevious;
    property OnForwardClick: TNotifyEvent read GetOnForward write SetOnForward;
    property OnRewindClick: TNotifyEvent read GetOnRewind write SetOnRewind;
    property OnOptionsClick: TNotifyEvent read GetOnOptions write SetOnOptions;
    property OnID3Click: TNotifyEvent read GetOnID3 write SetOnID3;
    property OnPlaylistClick: TNotifyEvent read GetOnPlaylist write SetOnPlaylist;
    property OnMinimizeClick: TNotifyEvent read GetOnMinimize write SetOnMinimize;
    property OnExitClick: TNotifyEvent read GetOnExit write SetOnExit;
    property OnMoveClick: TNotifyEvent read GetOnMove write SetOnMove;
    property OnMoveDown: TMouseEvent read FOnMoveDown write FOnMoveDown;
    property OnMoveMove: TMouseMoveEvent read FOnMoveMove write FOnMoveMove;
    property OnOpenClick: TNotifyEvent read GetOnOpen write SetOnOpen;
    property OnVolumeChanged: TNotifyEvent read GetOnVolume write SetOnVolume;
    property OnPositionChanged: TNotifyEvent read GetOnPosition write SetOnPosition;
    property OnVolumeChanging: TNotifyEvent read GetOnVolChanging write SetOnVolChanging;
    property OnPositionChanging: TNotifyEvent read GetOnPosChanging write SetOnPosChanging;
    property OnStatusClick: TNotifyEvent read GetOnStatus write SetOnStatus;
    property OnCurrenTTimeClick: TNotifyEvent read GetCurrentTime write SetCurrentTime;
    property OnLayerClick: TNotifyEvent read GetOnLayer write SetOnLayer;
    property OnBitRateClick: TNotifyEvent read GetOnBitRate write SetOnBitRate;
    property OnFrequencyClick: TNotifyEvent read GetOnFrequency write SetOnFrequency;
    property OnVersionClick: TNotifyEvent read GetOnVersion write SetOnVersion;
    property OnSongNameClick: TNotifyEvent read GetOnSongName write SetOnSongName;
    property OnTotalTimeClick: TNotifyEvent read GetOnTotalTime write SetOnTotalTime;
    property OnTotalInfoClick: TNotifyEvent read GetOnTotalInfo write SetOnTotalInfo;
    procedure ShowAbout;
    procedure Activate;
    procedure Deactivate;
    property PopupMenu;
    property OnSkinLoaded: TNotifyEvent read FOnSkinLoaded write FOnSkinLoaded;
  end;

implementation

uses
  JvFunctions;

{$R JvBreatheSkin.res}

resourcestring
  RC_AboutCaption = 'About';

  RC_PrefixSongtitle = 'songtitle';
  RC_PrefixTotalInfo = 'totalinfo';
  RC_PrefixTotalTime = 'totaltime';
  RC_PrefixTime = 'time';
  RC_PrefixInfo = 'info';
  RC_PrefixLayer = 'layer';
  RC_PrefixBitRate = 'BitRate';
  RC_PrefixFrequency = 'frequency';
  RC_PrefixVersion = 'versiontext';
  RC_PrefixVolbar = 'volbar';
  RC_PrefixTbbar = 'tbbar';
  RC_PrefixExit = 'exit';
  RC_PrefixMove = 'move';
  RC_PrefixMinimize = 'mini';
  RC_PrefixOption = 'option';
  RC_PrefixOpen = 'open';
  RC_PrefixId3 = 'id3';
  RC_PrefixPlaylist = 'plist';
  RC_PrefixNext = 'next';
  RC_PrefixForward = 'fwd';
  RC_PrefixPlay = 'play';
  RC_PrefixPause = 'pause';
  RC_PrefixStop = 'stop';
  RC_PrefixRewind = 'rev';
  RC_PrefixPrevious = 'prev';

  RC_Section = 'main';

  RC_SufixTransp = 'transparent';
  RC_SufixTop = 'Top';
  RC_SufixLeft = 'Left';
  RC_SufixBKColor = 'bkcolor';
  RC_SufixColor = 'color';
  RC_SufixFontSize = 'fontsize';
  RC_SufixFont = 'font';
  RC_SufixHeight = 'Height';
  RC_SufixWidth = 'Width';
  RC_SufixVertical = 'vertical';

  RC_InfoFile = 'info.txt';

  RC_BmpThumb = 'volthumb.bmp';
  RC_BmpVolRuler = 'volruler.bmp';
  RC_BmpPosThumb = 'posthumb.bmp';
  RC_BmpPosRuler = 'posruler.bmp';
  RC_BmpOver = 'over.bmp';
  RC_BmpMask = 'mask.bmp';
  RC_BmpInfo = 'info.bmp';
  RC_BmpDeact = 'deact.bmp';
  RC_BmpBack = 'back.bmp';

  RC_DefTotalInfo = 'Info';
  RC_DefTotalTime = '00:00';
  RC_DefSongName = 'none';
  RC_DefVersion = '1.0';
  RC_DefNothing = '-';
  RC_DefStatus = 'Status';

//=== TJvBreatheSkin =========================================================

constructor TJvBreatheSkin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkin := '';
  FBOptions := TJvBreatheOption.Create(Self);
  FBOptions.OnChange := OptionsChanged;
  FTAbout := TStringList.Create;

  // enable painting ;)

  Parent := TWinControl(AOwner);

  FImg := TImage.Create(Self);
  FImg.AutoSize := True;
  FImg.Parent := Self;
  FImg.Left := 0;
  FImg.Top := 0;
  FImg.OnMouseDown := EasyFormDown;
  FImg.OnMouseMove := EasyFormMove;

  FLabels := TJvBreatheLabels.Create(Self);
  FButtons := TJvBreatheButtons.Create(Self);
  FButtons.Move.OnMouseMove := MoveFormMove;
  FButtons.Move.OnMouseDown := MoveFormDown;

  // Sliders
  FVolume := TJvBreatheVolume.Create(Self);
  FVolume.FSlider.Left := 0;
  FVolume.FSlider.Top := 0;
  FVolume.FSlider.Width := 0;
  FVolume.FSlider.Height := 0;
  FVolume.Maximum := 100;
  FVolume.Position := 100;

  FPosition := TJvBreathePosition.Create(Self);
  FPosition.FSlider.Left := 0;
  FPosition.FSlider.Top := 0;
  FPosition.FSlider.Width := 0;
  FPosition.FSlider.Height := 0;
  FPosition.FSlider.Parent := Self;

  // Images
  FBack := TBitmap.Create;
  FDeact := TBitmap.Create;
  FInfo := TBitmap.Create;
  FMask := TBitmap.Create;
  FOver := TBitmap.Create;
  FPosRuler := TBitmap.Create;
  FPosThumb := TBitmap.Create;
  FVolRuler := TBitmap.Create;
  FVolThumb := TBitmap.Create;

  // Labels
  FLabels.Status.Caption := RC_DefStatus;
  FLabels.CurrentTime.Caption := RC_DefTotalTime;
  FLabels.Layer.Caption := RC_DefNothing;
  FLabels.BitRate.Caption := RC_DefNothing;
  FLabels.Frequency.Caption := RC_DefNothing;
  FLabels.Version.Caption := RC_DefVersion;
  FLabels.SongName.Caption := RC_DefSongName;
  FLabels.TotalTime.Caption := RC_DefTotalTime;
  FLabels.TotalInfo.Caption := RC_DefTotalInfo;

  // Load default design
  InitAllBtn;
  LoadDefault;
end;

destructor TJvBreatheSkin.Destroy;
var
  h: THandle;
begin
  try
    // Reset the region of the window if not destroying
    if not (csDestroying in GetParentForm(TControl(Self)).ComponentState) then
    begin
      // Enable Caption
      h := GetParentForm(TControl(Self)).Handle;
      SetWindowLong(h, GWL_STYLE, GetWindowLong(h, GWL_STYLE) or WS_CAPTION);

      // Use form region
      SetWindowRgn(h, 0, True);
    end;
  except
  end;

  FBack.Free;
  FDeact.Free;
  FInfo.Free;
  FMask.Free;
  FOver.Free;
  FPosRuler.Free;
  FPosThumb.Free;
  FVolRuler.Free;
  FVolThumb.Free;

  FImg.Free;
  FVolume.Free;
  FPosition.Free;
  FBOptions.Free;
  FLabels.Free;
  FTAbout.Free;
  FButtons.Free;

  inherited Destroy;
end;

procedure TJvBreatheSkin.LoadDefault;
var
  res: TResourceStream;
  buf: array [0..255] of Char;
  st: string;

  procedure LoadFromRes(bmp: TBitmap; Value: string);
  var
    jpeg: TJpegImage;
    r: TResourceStream;
  begin
    r := TResourceStream.Create(hInstance, Value, RT_RCDATA);
    jpeg := TJPegImage.Create;
    jpeg.LoadFromStream(r);

    bmp.Assign(jpeg);

    r.Free;
    jpeg.Free;
  end;

  procedure LoadFromResPcx(bmp: TBitmap; Value: string);
  var
    pcx: TJvPcx;
    r: TResourceStream;
  begin
    r := TResourceStream.Create(hInstance, Value, RT_RCDATA);
    pcx := TJvPcx.Create;
    pcx.LoadFromStream(r);

    bmp.Assign(pcx);

    r.Free;
    pcx.Free;
  end;

begin
  // Load About text
  res := TResourceStream.Create(hInstance, 'infot', RT_RCDATA);
  FTAbout.LoadFromStream(res);
  res.Free;

  LoadFromRes(FBack, 'back');
  LoadFromRes(FDeact, 'deact');
  LoadFromRes(FInfo, 'info');
  LoadFromRes(FPosRuler, 'posruler');
  LoadFromRes(FPosThumb, 'posthumb');
  LoadFromRes(FVolRuler, 'volruler');
  LoadFromRes(FVolThumb, 'volthumb');

  LoadFromResPcx(FOver, 'over');
  LoadFromResPcx(FMask, 'mask');

  // assign back picture
  FImg.Picture.Assign(FBack);
  Width := FImg.Width;
  Height := FImg.Height;

  // load default skin
  GetTempPath(255, buf);
  st := buf;
  if st[Length(st)] <> '\' then
    st := st + '\';

  res := TResourceStream.Create(hInstance, 'main', RT_RCDATA);
  res.SaveToFile(st + '1.Ini');
  LoadSkin(st + '1.Ini');
  DeleteFile(st + '1.Ini');
end;

procedure TJvBreatheSkin.InitBtn(Value: TJvImage);
begin
  Value.Left := 0;
  Value.Top := 0;
  Value.Width := 0;
  Value.Height := 0;
end;

procedure TJvBreatheSkin.InitAllBtn;
begin
  InitBtn(FButtons.FPlay.FButton);
  InitBtn(FButtons.FStop.FButton);
  InitBtn(FButtons.FPause.FButton);
  InitBtn(FButtons.FNext.FButton);
  InitBtn(FButtons.FPrev.FButton);
  InitBtn(FButtons.FOpen.FButton);
  InitBtn(FButtons.FExit.FButton);
  InitBtn(FButtons.FMove.FButton);
  InitBtn(FButtons.FMinimize.FButton);
  InitBtn(FButtons.FId3.FButton);
  InitBtn(FButtons.FForward.FButton);
  InitBtn(FButtons.FRewind.FButton);
  InitBtn(FButtons.FOptions.FButton);
  InitBtn(FButtons.FPlaylist.FButton);
end;

procedure TJvBreatheSkin.SetSkin(const Value: TFileName);
var
  Path: string;

  procedure LoadFromRes(bmp: TBitmap; Value: string);
  var
    jpeg: TJpegImage;
    r: TResourceStream;
  begin
    r := TResourceStream.Create(HInstance, Value, RT_RCDATA);
    jpeg := TJPegImage.Create;
    jpeg.LoadFromStream(r);

    bmp.Assign(jpeg);

    r.Free;
    jpeg.Free;
  end;

begin
  FSkin := Value;
  if FSkin <> '' then
  begin
    // Load images
    Path := ExtractFilePath(Value);
    if FileExists(Path + RC_BmpBack) then
      FBack.LoadFromFile(Path + RC_BmpBack)
    else
      FBack.Assign(nil);
    if FileExists(Path + RC_BmpDeact) then
      FDeact.LoadFromFile(Path + RC_BmpDeact)
    else
      FDeact.Assign(nil);
    if FileExists(Path + RC_BmpInfo) then
      FInfo.LoadFromFile(Path + RC_BmpInfo)
    else
      FInfo.Assign(nil);
    if FileExists(Path + RC_BmpMask) then
      FMask.LoadFromFile(Path + RC_BmpMask)
    else
      FMask.Assign(nil);
    if FileExists(Path + RC_BmpOver) then
      FOver.LoadFromFile(Path + RC_BmpOver)
    else
      FOver.Assign(nil);

    if FileExists(Path + RC_BmpPosRuler) then
      FPosRuler.LoadFromFile(Path + RC_BmpPosRuler)
    else
      LoadFromRes(FPosRuler, 'posruler');
    if FileExists(Path + RC_BmpPosThumb) then
      FPosThumb.LoadFromFile(Path + RC_BmpPosThumb)
    else
      LoadFromRes(FPosThumb, 'posthumb');
    if FileExists(Path + RC_BmpVolRuler) then
      FVolRuler.LoadFromFile(Path + RC_BmpVolRuler)
    else
      LoadFromRes(FVolRuler, 'volruler');
    if FileExists(Path + RC_BmpThumb) then
      FVolThumb.LoadFromFile(Path + RC_BmpThumb)
    else
      LoadFromRes(FVolThumb, 'volthumb');

    // Assign background
    FImg.Picture.Bitmap.Assign(FBack);

    if FileExists(Path + RC_InfoFile) then
      FTAbout.LoadFromFile(Path + RC_InfoFile)
    else
      FTAbout.Clear;

    // if AutoSize
    if FBOptions.AutoSize then
    begin
      Self.Width := FImg.Width;
      Self.Height := FImg.Height;
    end;

    // Load Ini file
    LoadSkin(FSkin);
  end
  else
    LoadDefault;
end;

procedure TJvBreatheSkin.DecoupeOver(Value: TJvImage);
var
  src: TRect;
  bmp: TBitmap;
begin
  src.Left := Value.Left;
  src.Top := Value.Top;
  src.Right := Value.Left + Value.Width;
  src.Bottom := Value.Top + Value.Height;

  bmp := TBItmap.Create;
  bmp.Width := Value.Width;
  bmp.Height := Value.Height;
  bmp.Canvas.CopyRect(Rect(0, 0, Value.Width, Value.Height), FOver.Canvas, src);

  Value.Picture.Bitmap.Assign(bmp);

  bmp.Free;
end;

procedure TJvBreatheSkin.DecoupeOverAll;
begin
  DecoupeOver(FButtons.FPlay.FButton);
  DecoupeOver(FButtons.FStop.FButton);
  DecoupeOver(FButtons.FPause.FButton);
  DecoupeOver(FButtons.FNext.FButton);
  DecoupeOver(FButtons.FPrev.FButton);
  DecoupeOver(FButtons.FOpen.FButton);
  DecoupeOver(FButtons.FExit.FButton);
  DecoupeOver(FButtons.FMove.FButton);
  DecoupeOver(FButtons.FMinimize.FButton);
  DecoupeOver(FButtons.FId3.FButton);
  DecoupeOver(FButtons.FForward.FButton);
  DecoupeOver(FButtons.FRewind.FButton);
  DecoupeOver(FButtons.FOptions.FButton);
  DecoupeOver(FButtons.FPlaylist.FButton);
end;

procedure TJvBreatheSkin.LoadBtn(Ini: TIniFile; Value: TJvImage; Prefix: string);
begin
  Value.Top := Ini.ReadInteger(RC_Section, Prefix + RC_SufixTop, 0);
  Value.Left := Ini.ReadInteger(RC_Section, Prefix + RC_SufixLeft, 0);
  Value.Width := Ini.ReadInteger(RC_Section, Prefix + RC_SufixWidth, 0);
  Value.Height := Ini.ReadInteger(RC_Section, Prefix + RC_SufixHeight, 0);
end;

procedure TJvBreatheSkin.LoadSlider(Ini: TIniFile; Value: TJvSlider; Prefix: string);
begin
  Value.Horizontal := not (Ini.ReadBool(RC_Section, Prefix + RC_SufixVertical, False));
  Value.Top := Ini.ReadInteger(RC_Section, Prefix + RC_SufixTop, 0);
  Value.Left := Ini.ReadInteger(RC_Section, Prefix + RC_SufixLeft, 0);
  Value.Width := Ini.ReadInteger(RC_Section, Prefix + RC_SufixWidth, 0);
  Value.Height := Ini.ReadInteger(RC_Section, Prefix + RC_SufixHeight, 0);
end;

procedure TJvBreatheSkin.LoadLabel(Ini: TIniFile; Value: TJvBreatheLabel; Prefix: string);
var
  i: Integer;
begin
  Value.Transparent := Ini.ReadBool(RC_Section, Prefix + RC_SufixTransp, False);
  Value.Top := Ini.ReadInteger(RC_Section, Prefix + RC_SufixTop, 0);
  Value.Left := Ini.ReadInteger(RC_Section, Prefix + RC_SufixLeft, 0);
  Value.Width := Ini.ReadInteger(RC_Section, Prefix + RC_SufixWidth, 0);
  Value.Height := Ini.ReadInteger(RC_Section, Prefix + RC_SufixHeight, 0);
  Value.Font.Name := Ini.ReadString(RC_Section, Prefix + RC_SufixFont, 'arial');

  i := Ini.ReadInteger(RC_Section, Prefix + RC_SufixFontSize, 8);
  with TControlCanvas.Create do
  begin
    Control := Value.FLabel;
    Font.Name := Value.Font.Name;
    Font.Size := i;
    if Value.Height > 0 then
      while TextHeight('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/|') > Value.Height do
      begin
        Dec(i);
        Font.Size := i;
      end;
    Free;
  end;
  Value.Font.Size := i;
  Value.Font.Color := Ini.ReadInteger(RC_Section, Prefix + RC_SufixColor, clBlack);
  Value.Color := Ini.ReadInteger(RC_Section, Prefix + RC_SufixBKColor, clBlack);
end;

procedure TJvBreatheSkin.LoadLabel(Ini: TIniFile; Value: TJvBreatheScrollLabel; Prefix: string);
var
  i: Integer;
begin
  Value.Transparent := Ini.ReadBool(RC_Section, Prefix + RC_SufixTransp, False);
  Value.Top := Ini.ReadInteger(RC_Section, Prefix + RC_SufixTop, 0);
  Value.Left := Ini.ReadInteger(RC_Section, Prefix + RC_SufixLeft, 0);
  Value.Width := Ini.ReadInteger(RC_Section, Prefix + RC_SufixWidth, 0);
  Value.Height := Ini.ReadInteger(RC_Section, Prefix + RC_SufixHeight, 0);
  Value.Font.Name := Ini.ReadString(RC_Section, Prefix + RC_SufixFont, 'arial');

  i := Ini.ReadInteger(RC_Section, Prefix + RC_SufixFontSize, 8);
  with TControlCanvas.Create do
  begin
    Control := Value.FLabel;
    Font.Name := Value.Font.Name;
    Font.Size := i;
    if Value.Height > 0 then
      while TextHeight('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/|') > Value.Height do
      begin
        Dec(i);
        Font.Size := i;
      end;
    Free;
  end;
  Value.Font.Size := i;
  Value.Font.Color := Ini.ReadInteger(RC_Section, Prefix + RC_SufixColor, clBlack);
  Value.Color := Ini.ReadInteger(RC_Section, Prefix + RC_SufixBKColor, clBlack);
end;

procedure TJvBreatheSkin.LoadSkin(Value: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Value);

  InitAllBtn;

  LoadBtn(Ini, FButtons.FPrev.FButton, RC_PrefixPrevious);
  LoadBtn(Ini, FButtons.FRewind.FButton, RC_PrefixRewind);
  LoadBtn(Ini, FButtons.FStop.FButton, RC_PrefixStop);
  LoadBtn(Ini, FButtons.FPause.FButton, RC_PrefixPause);
  LoadBtn(Ini, FButtons.FPlay.FButton, RC_PrefixPlay);
  LoadBtn(Ini, FButtons.FForward.FButton, RC_PrefixForward);
  LoadBtn(Ini, FButtons.FNext.FButton, RC_PrefixNext);
  LoadBtn(Ini, FButtons.FPlaylist.FButton, RC_PrefixPlaylist);
  LoadBtn(Ini, FButtons.FId3.FButton, RC_PrefixId3);
  LoadBtn(Ini, FButtons.FOpen.FButton, RC_PrefixOpen);
  LoadBtn(Ini, FButtons.FOptions.FButton, RC_PrefixOption);
  LoadBtn(Ini, FButtons.FMinimize.FButton, RC_PrefixMinimize);
  LoadBtn(Ini, FButtons.FMove.FButton, RC_PrefixMove);
  LoadBtn(Ini, FButtons.FExit.FButton, RC_PrefixExit);

  FPosition.FSlider.ImageRuler.Assign(FPosRuler);
  FPosition.FSlider.ImageThumb.Assign(FPosThumb);
  FVolume.FSlider.ImageRuler.Assign(FVolRuler);
  FVolume.FSlider.ImageThumb.Assign(FVolThumb);
  LoadSlider(Ini, FPosition.FSlider, RC_PrefixTbbar);
  LoadSlider(Ini, FVolume.FSlider, RC_PrefixVolbar);

  LoadLabel(Ini, FLabels.Version, RC_PrefixVersion);
  LoadLabel(Ini, FLabels.Frequency, RC_PrefixFrequency);
  LoadLabel(Ini, FLabels.BitRate, RC_PrefixBitRate);
  LoadLabel(Ini, FLabels.Layer, RC_PrefixLayer);
  LoadLabel(Ini, FLabels.Status, RC_PrefixInfo);
  LoadLabel(Ini, FLabels.CurrentTime, RC_PrefixTime);
  LoadLabel(Ini, FLabels.TotalTime, RC_PrefixTotalTime);
  LoadLabel(Ini, FLabels.TotalInfo, RC_PrefixTotalInfo);
  LoadLabel(Ini, FLabels.SongName, RC_PrefixSongtitle);

  if not FOver.Empty then
    DecoupeOverAll;

  Ini.Free;

  OptionsChanged(Self);
  if Assigned(FOnSkinLoaded) then
    FOnSkinLoaded(Self);
end;

procedure TJvBreatheSkin.OptionsChanged(Sender: TObject);
var
  h: THandle;
begin
  if FBOptions.AutoSize then
  begin
    Width := FBack.Width;
    Height := FBack.Height;
  end;
  if FBOptions.AutoRegion then
  begin
    Left := -3;
    Top := -3;

    // Remove Caption
    h := GetParentForm(TControl(Self)).Handle;
    SetWindowLong(h, GWL_STYLE, GetWindowLong(h, GWL_STYLE) and not WS_CAPTION);
    GetParentForm(Self).ClientWidth := Width;
    GetParentForm(Self).ClientHeight := Height;
    GetParentForm(Self).Invalidate;

    // Use region
    SetWindowRgn(h, RegionFromBitmap(FMask), True);
  end
  else
  begin
    // Enable Caption
    h := GetParentForm(TControl(Self)).Handle;
    SetWindowLong(h, GWL_STYLE, GetWindowLong(h, GWL_STYLE) or WS_CAPTION);
    GetParentForm(Self).Invalidate;

    // Use form region
    SetWindowRgn(h, 0, True);
  end;
end;

procedure TJvBreatheSkin.ShowAbout;
var
  im: TJvImage;
  t: TForm;
  m: TMemo;
begin
  if not (FInfo.Empty) or (FTAbout.Count > 0) then
  begin
    t := TForm.Create(Application);
    with t do
    begin
      Caption := RC_AboutCaption;
      if FTAbout.Count > 0 then
        Width := 500
      else
        ClientWidth := 137;
      ClientHeight := 157;
      BorderStyle := bsDialog;
      Position := poScreenCenter;

      im := TJvImage.Create(t);
      if not FInfo.Empty then
      begin
        im.Parent := t;
        im.Top := 0;
        im.Left := 0;
        im.Width := 137;
        im.AutoSize := False;
        im.Stretch := True;
        im.Align := alLeft;
        im.Picture.Bitmap.Assign(FInfo);
        im.Pictures.PicEnter.Bitmap.Assign(FInfo);
      end;

      m := TMemo.Create(t);
      if FTAbout.Count > 0 then
      begin
        m.Parent := t;
        m.Align := alClient;
        m.ReadOnly := True;
        m.ScrollBars := ssBoth;
        m.Text := FTAbout.Text;
      end;

      ShowModal;

      m.Free;
      im.Free;

      Free;
    end;
  end;
end;

procedure TJvBreatheSkin.MoveFormDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastX := x;
  FLastY := y;

  if Assigned(FOnMoveDown) then
    FOnMoveDown(Sender, Button, Shift, X, Y);
end;

procedure TJvBreatheSkin.MoveFormMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FBOptions.AutoMove and (HiWord(GetAsyncKeystate(VK_LBUTTON)) > 0) then
  begin
    GetParentForm(Self).Left := GetParentForm(Self).Left + x - FLastX;
    GetParentForm(Self).Top := GetParentForm(Self).Top + y - FLastY;
  end;

  if Assigned(FOnMoveMove) then
    FOnMoveMove(Sender, Shift, X, Y);
end;

procedure TJvBreatheSkin.EasyFormDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastX := X;
  FLastY := Y;
end;

procedure TJvBreatheSkin.EasyFormMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FBOptions.AutoMove and (HiWord(GetAsyncKeystate(VK_LBUTTON)) > 0) then
  begin
    GetParentForm(Self).Left := GetParentForm(Self).Left + X - FLastX;
    GetParentForm(Self).Top := GetParentForm(Self).Top + Y - FLastY;
  end;
end;

procedure TJvBreatheSkin.Activate;
begin
  FImg.Picture.Bitmap.Assign(FBack);
end;

procedure TJvBreatheSkin.Deactivate;
begin
  if not FDeact.Empty then
    FImg.Picture.Bitmap.Assign(FDeact);
end;

function TJvBreatheSkin.GetOnExit: TNotifyEvent;
begin
  Result := FButtons.FExit.OnClick;
end;

function TJvBreatheSkin.GetOnForward: TNotifyEvent;
begin
  Result := FButtons.FForward.OnClick;
end;

function TJvBreatheSkin.GetOnID3: TNotifyEvent;
begin
  Result := FButtons.FID3.OnClick;
end;

function TJvBreatheSkin.GetOnMinimize: TNotifyEvent;
begin
  Result := FButtons.FMinimize.OnClick;
end;

function TJvBreatheSkin.GetOnMove: TNotifyEvent;
begin
  Result := FButtons.FMove.OnClick;
end;

function TJvBreatheSkin.GetOnNext: TNotifyEvent;
begin
  Result := FButtons.FNext.OnClick;
end;

function TJvBreatheSkin.GetOnOpen: TNotifyEvent;
begin
  Result := FButtons.FOpen.OnClick;
end;

function TJvBreatheSkin.GetOnOptions: TNotifyEvent;
begin
  Result := FButtons.FOptions.OnClick;
end;

function TJvBreatheSkin.GetOnPause: TNotifyEvent;
begin
  Result := FButtons.FPause.OnClick;
end;

function TJvBreatheSkin.GetOnPlay: TNotifyEvent;
begin
  Result := FButtons.FPlay.OnClick;
end;

function TJvBreatheSkin.GetOnPlaylist: TNotifyEvent;
begin
  Result := FButtons.FPlaylist.OnClick;
end;

function TJvBreatheSkin.GetOnPosition: TNotifyEvent;
begin
  Result := FPosition.FSlider.OnStopChanged;
end;

function TJvBreatheSkin.GetOnPrevious: TNotifyEvent;
begin
  Result := FButtons.FPrev.OnClick;
end;

function TJvBreatheSkin.GetOnRewind: TNotifyEvent;
begin
  Result := FButtons.FRewind.OnClick;
end;

function TJvBreatheSkin.GetOnStop: TNotifyEvent;
begin
  Result := FButtons.FStop.OnClick;
end;

function TJvBreatheSkin.GetOnVolume: TNotifyEvent;
begin
  Result := FVolume.FSlider.OnStopChanged;
end;

procedure TJvBreatheSkin.SetOnExit(const Value: TNotifyEvent);
begin
  FButtons.FExit.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnForward(const Value: TNotifyEvent);
begin
  FButtons.FForward.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnID3(const Value: TNotifyEvent);
begin
  FButtons.FID3.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnMinimize(const Value: TNotifyEvent);
begin
  FButtons.FMinimize.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnMove(const Value: TNotifyEvent);
begin
  FButtons.FMove.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnNext(const Value: TNotifyEvent);
begin
  FButtons.FNext.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnOpen(const Value: TNotifyEvent);
begin
  FButtons.FOpen.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnOptions(const Value: TNotifyEvent);
begin
  FButtons.FOptions.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnPause(const Value: TNotifyEvent);
begin
  FButtons.FPause.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnPlay(const Value: TNotifyEvent);
begin
  FButtons.FPlay.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnPlaylist(const Value: TNotifyEvent);
begin
  FButtons.FPlayList.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnPosition(const Value: TNotifyEvent);
begin
  FPosition.FSlider.OnStopChanged := Value;
end;

procedure TJvBreatheSkin.SetOnPrevious(const Value: TNotifyEvent);
begin
  FButtons.FPrev.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnRewind(const Value: TNotifyEvent);
begin
  FButtons.FRewind.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnStop(const Value: TNotifyEvent);
begin
  FButtons.FStop.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnVolume(const Value: TNotifyEvent);
begin
  FVolume.FSlider.OnStopChanged := Value;
end;

function TJvBreatheSkin.GetOnPosChanging: TNotifyEvent;
begin
  Result := FPosition.FSlider.OnChanged;
end;

function TJvBreatheSkin.GetOnVolChanging: TNotifyEvent;
begin
  Result := FVolume.FSlider.OnChanged;
end;

procedure TJvBreatheSkin.SetOnPosChanging(const Value: TNotifyEvent);
begin
  FPosition.FSlider.OnChanged := Value;
end;

procedure TJvBreatheSkin.SetOnVolChanging(const Value: TNotifyEvent);
begin
  FVolume.FSlider.OnChanged := Value;
end;

function TJvBreatheSkin.GetCurrentTime: TNotifyEvent;
begin
  Result := FLabels.FTime.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnBitRate: TNotifyEvent;
begin
  Result := FLabels.FBitRate.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnFrequency: TNotifyEvent;
begin
  Result := FLabels.FFrequency.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnLayer: TNotifyEvent;
begin
  Result := FLabels.FLayer.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnSongName: TNotifyEvent;
begin
  Result := FLabels.FSongName.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnStatus: TNotifyEvent;
begin
  Result := FLabels.FStatus.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnTotalInfo: TNotifyEvent;
begin
  Result := FLabels.FTotalInfo.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnTotalTime: TNotifyEvent;
begin
  Result := FLabels.FTotalTime.FLabel.OnClick;
end;

function TJvBreatheSkin.GetOnVersion: TNotifyEvent;
begin
  Result := FLabels.FVersion.FLabel.OnClick;
end;

procedure TJvBreatheSkin.SetCurrentTime(const Value: TNotifyEvent);
begin
  FLabels.FTime.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnBitRate(const Value: TNotifyEvent);
begin
  FLabels.FBitRate.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnFrequency(const Value: TNotifyEvent);
begin
  FLabels.FFrequency.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnLayer(const Value: TNotifyEvent);
begin
  FLabels.FLayer.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnSongName(const Value: TNotifyEvent);
begin
  FLabels.FSongName.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnStatus(const Value: TNotifyEvent);
begin
  FLabels.FStatus.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnTotalInfo(const Value: TNotifyEvent);
begin
  FLabels.FTotalInfo.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnTotalTime(const Value: TNotifyEvent);
begin
  FLabels.FTotalTime.FLabel.OnClick := Value;
end;

procedure TJvBreatheSkin.SetOnVersion(const Value: TNotifyEvent);
begin
  FLabels.FVersion.FLabel.OnClick := Value;
end;

//=== TJvBreatheOption =======================================================

constructor TJvBreatheOption.Create(AOwner: TComponent);
begin
  inherited Create;
  FAutoSize := True;
  FAutoMove := True;
  FAutoRegion := False;
  FEasyMove := False;
end;

procedure TJvBreatheOption.SetAutoMove(const Value: Boolean);
begin
  FAutoMove := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvBreatheOption.SetAutoRegion(const Value: Boolean);
begin
  FAutoRegion := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvBreatheOption.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvBreatheOption.SetEasyMove(const Value: Boolean);
begin
  FEasyMove := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//=== TJvBreatheLabels =======================================================

constructor TJvBreatheLabels.Create(AOwner: TComponent);
begin
  inherited Create;
  FStatus := TJvBreatheLabel.Create(AOwner);
  FBitRate := TJvBreatheLabel.Create(AOwner);
  FTime := TJvBreatheLabel.Create(AOwner);
  FFrequency := TJvBreatheLabel.Create(AOwner);
  FSongName := TJvBreatheScrollLabel.Create(AOwner);
  FTotalInfo := TJvBreatheLabel.Create(AOwner);
  FTotalTime := TJvBreatheLabel.Create(AOwner);
  FLayer := TJvBreatheLabel.Create(AOwner);
  FVersion := TJvBreatheLabel.Create(AOwner);
end;

destructor TJvBreatheLabels.Destroy;
begin
  FStatus.Free;
  FBitRate.Free;
  FTime.Free;
  FFrequency.Free;
  FSongName.Free;
  FTotalInfo.Free;
  FTotalTime.Free;
  FLayer.Free;
  FVersion.Free;
  inherited Destroy;
end;

//=== TJvBreatheLabel ========================================================

constructor TJvBreatheLabel.Create(AOwner: TComponent);
begin
  inherited Create;
  FLabel := TJvLabel.Create(AOwner);
  FLabel.Parent := TWinControl(AOwner);
  FLabel.AutoSize := False;
  FLabel.HotTrack := False;
  FLabel.ParentColor := False;
  FLabel.ParentFont := False;
  FLabel.ParentShowHint := False;
end;

function TJvBreatheLabel.GetColor: TColor;
begin
  Result := FLabel.Color;
end;

function TJvBreatheLabel.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

function TJvBreatheLabel.GetHint: string;
begin
  Result := FLabel.Hint;
end;

function TJvBreatheLabel.GetPopupMenu: TPopupMenu;
begin
  Result := FLabel.PopupMenu;
end;

function TJvBreatheLabel.GetShowHint: Boolean;
begin
  Result := FLabel.ShowHint;
end;

function TJvBreatheLabel.GetTransparent: Boolean;
begin
  Result := FLabel.Transparent;
end;

procedure TJvBreatheLabel.SetCaption(const Value: string);
begin
  FCaption := Value;
  FLabel.Caption := Value;
end;

procedure TJvBreatheLabel.SetColor(const Value: TColor);
begin
  FLabel.Color := Value;
end;

procedure TJvBreatheLabel.SetFont(const Value: TFont);
begin
  FLabel.Font.Assign(Value);
end;

procedure TJvBreatheLabel.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  FLabel.Height := Value;
end;

procedure TJvBreatheLabel.SetHint(const Value: string);
begin
  FLabel.Hint := Value;
end;

procedure TJvBreatheLabel.SetLeft(const Value: Integer);
begin
  FLeft := Value;
  FLabel.Left := Value;
end;

procedure TJvBreatheLabel.SetPopupMenu(const Value: TPopupMenu);
begin
  FLabel.PopupMenu := Value;
end;

procedure TJvBreatheLabel.SetShowHint(const Value: Boolean);
begin
  FLabel.ShowHint := Value;
end;

procedure TJvBreatheLabel.SetTop(const Value: Integer);
begin
  FTop := Value;
  FLabel.Top := Value;
end;

procedure TJvBreatheLabel.SetTransparent(const Value: Boolean);
begin
  FLabel.Transparent := Value;
end;

procedure TJvBreatheLabel.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  FLabel.Width := Value;
end;

//=== TJvBreatheVolume =======================================================

constructor TJvBreatheVolume.Create(AOwner: TComponent);
begin
  inherited Create;
  FSlider := TJvSlider.Create(AOwner);
  FSlider.Parent := TWinControl(AOwner);
end;

destructor TJvBreatheVolume.Destroy;
begin
  FSlider.Free;
  inherited Destroy;
end;

function TJvBreatheVolume.GetHint: string;
begin
  Result := FSlider.Hint;
end;

function TJvBreatheVolume.GetMaximum: Integer;
begin
  Result := FSlider.Maximum;
end;

function TJvBreatheVolume.GetPosition: Integer;
begin
  Result := FSlider.Position;
end;

function TJvBreatheVolume.GetShow: Boolean;
begin
  Result := FSlider.ShowHint;
end;

procedure TJvBreatheVolume.SetHint(const Value: string);
begin
  FSlider.Hint := Value;
end;

procedure TJvBreatheVolume.SetMaximum(const Value: Integer);
begin
  FSlider.Maximum := Value;
end;

procedure TJvBreatheVolume.SetPosition(const Value: Integer);
begin
  FSlider.Position := Value;
end;

procedure TJvBreatheVolume.SetShow(const Value: Boolean);
begin
  FSlider.ShowHint := Value;
end;

//=== TJvBreathePosition =====================================================

constructor TJvBreathePosition.Create(AOwner: TComponent);
begin
  inherited Create;
  FSlider := TJvSlider.Create(AOwner);
  FSlider.Parent := TWinControl(AOwner);
end;

destructor TJvBreathePosition.Destroy;
begin
  FSlider.Free;
  inherited Destroy;
end;

function TJvBreathePosition.GetHint: string;
begin
  Result := FSlider.Hint;
end;

function TJvBreathePosition.GetMaximum: Integer;
begin
  Result := FSlider.Maximum;
end;

function TJvBreathePosition.GetPosition: Integer;
begin
  Result := FSlider.Position;
end;

function TJvBreathePosition.GetShow: Boolean;
begin
  Result := FSlider.ShowHint;
end;

procedure TJvBreathePosition.SetHint(const Value: string);
begin
  FSlider.Hint := Value;
end;

procedure TJvBreathePosition.SetMaximum(const Value: Integer);
begin
  FSlider.Maximum := Value;
end;

procedure TJvBreathePosition.SetPosition(const Value: Integer);
begin
  FSlider.Position := Value;
end;

procedure TJvBreathePosition.SetShow(const Value: Boolean);
begin
  FSlider.ShowHint := Value;
end;

//=== TJvBreatheButtons ======================================================

constructor TJvBreatheButtons.Create(AOwner: TComponent);
begin
  inherited Create;
  FPlaylist := TJvBreatheButton.Create(AOwner);
  FPrev := TJvBreatheButton.Create(AOwner);
  FMove := TJvBreatheButton.Create(AOwner);
  FPlay := TJvBreatheButton.Create(AOwner);
  FNext := TJvBreatheButton.Create(AOwner);
  FExit := TJvBreatheButton.Create(AOwner);
  FOptions := TJvBreatheButton.Create(AOwner);
  FOpen := TJvBreatheButton.Create(AOwner);
  FPause := TJvBreatheButton.Create(AOwner);
  FId3 := TJvBreatheButton.Create(AOwner);
  FForward := TJvBreatheButton.Create(AOwner);
  FRewind := TJvBreatheButton.Create(AOwner);
  FStop := TJvBreatheButton.Create(AOwner);
  FMinimize := TJvBreatheButton.Create(AOwner);
end;

destructor TJvBreatheButtons.Destroy;
begin
  FPlaylist.Free;
  FPrev.Free;
  FMove.Free;
  FPlay.Free;
  FNext.Free;
  FExit.Free;
  FOptions.Free;
  FOpen.Free;
  FPause.Free;
  FId3.Free;
  FForward.Free;
  FRewind.Free;
  FStop.Free;
  FMinimize.Free;
  inherited Destroy;
end;

//=== TJvBreatheButton =======================================================

constructor TJvBreatheButton.Create(AOwner: TComponent);
begin
  inherited Create;
  FButton := TJvImage.Create(AOwner);
  FButton.Parent := TWinControl(AOwner);
end;

function TJvBreatheButton.GetEnabled: Boolean;
begin
  Result := FButton.Enabled;
end;

function TJvBreatheButton.GetHint: string;
begin
  Result := FButton.Hint;
end;

function TJvBreatheButton.GetMouseDown: TMouseEvent;
begin
  Result := FButton.OnMouseDown;
end;

function TJvBreatheButton.GetMouseMove: TMouseMoveEvent;
begin
  Result := FButton.OnMouseMove;
end;

function TJvBreatheButton.GetMouseUp: TMouseEvent;
begin
  Result := FButton.OnMouseUp;
end;

function TJvBreatheButton.GetOnClick: TNotifyEvent;
begin
  Result := FButton.OnClick;
end;

function TJvBreatheButton.GetOnDblClick: TNotifyEvent;
begin
  Result := FButton.OnDblClick;
end;

function TJvBreatheButton.GetPopupMenu: TPopupMenu;
begin
  Result := FButton.PopupMenu;
end;

function TJvBreatheButton.GetShowHint: Boolean;
begin
  Result := FButton.ShowHint;
end;

function TJvBreatheButton.GetVisible: Boolean;
begin
  Result := FButton.Visible;
end;

procedure TJvBreatheButton.SetEnabled(const Value: Boolean);
begin
  FButton.Enabled := Value;
end;

procedure TJvBreatheButton.SetHint(const Value: string);
begin
  FButton.Hint := Value;
end;

procedure TJvBreatheButton.SetMouseDown(const Value: TMouseEvent);
begin
  FButton.OnMouseDown := Value;
end;

procedure TJvBreatheButton.SetMouseUp(const Value: TMouseEvent);
begin
  FButton.OnMouseUp := Value;
end;

procedure TJvBreatheButton.SetMouveMove(const Value: TMouseMoveEvent);
begin
  FButton.OnMouseMove := Value;
end;

procedure TJvBreatheButton.SetOnClick(const Value: TNotifyEvent);
begin
  FButton.OnClick := Value;
end;

procedure TJvBreatheButton.SetOnDblClick(const Value: TNotifyEvent);
begin
  FButton.OnDblClick := Value;
end;

procedure TJvBreatheButton.SetPopupMenu(const Value: TPopupMenu);
begin
  FButton.PopupMenu := Value;
end;

procedure TJvBreatheButton.SetShowHint(const Value: Boolean);
begin
  FButton.ShowHint := Value;
end;

procedure TJvBreatheButton.SetVisible(const Value: Boolean);
begin
  FButton.Visible := Value;
end;

//=== TJvBreatheScrollLabel ==================================================

constructor TJvBreatheScrollLabel.Create(AOwner: TComponent);
begin
  inherited Create;
  FLabel := TJvBehaviorLabel.Create(AOwner);
  FLabel.Parent := TWinControl(AOwner);
  FLabel.AutoSize := False;
  FLabel.ParentColor := False;
  FLabel.ParentFont := False;
  FLabel.ParentShowHint := False;
  FLabel.Behavior := 'Scrolling';
end;

function TJvBreatheScrollLabel.GetColor: TColor;
begin
  Result := FLabel.Color;
end;

function TJvBreatheScrollLabel.GetDirection: TJvLabelScrollDirection;
begin
  Result := TJvLabelScroll(FLabel.BehaviorOptions).Direction;
end;

function TJvBreatheScrollLabel.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

function TJvBreatheScrollLabel.GetHint: string;
begin
  Result := FLabel.Hint;
end;

function TJvBreatheScrollLabel.GetInterval: Cardinal;
begin
  Result := TJvLabelScroll(FLabel.BehaviorOptions).Interval;
end;

function TJvBreatheScrollLabel.GetScrolling: Boolean;
begin
  Result := TJvLabelScroll(FLabel.BehaviorOptions).Active;
end;

function TJvBreatheScrollLabel.GetShowHint: Boolean;
begin
  Result := FLabel.ShowHint;
end;

function TJvBreatheScrollLabel.GetTransparent: Boolean;
begin
  Result := FLabel.Transparent;
end;

procedure TJvBreatheScrollLabel.SetCaption(const Value: string);
begin
  FCaption := Value;
  FLabel.Caption := Value;
end;

procedure TJvBreatheScrollLabel.SetColor(const Value: TColor);
begin
  FLabel.Color := Value;
end;

procedure TJvBreatheScrollLabel.SetDirection(const Value: TJvLabelScrollDirection);
begin
  TJvLabelScroll(FLabel.BehaviorOptions).Direction := Value;
end;

procedure TJvBreatheScrollLabel.SetFont(const Value: TFont);
begin
  FLabel.Font := Value;
end;

procedure TJvBreatheScrollLabel.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  FLabel.Height := Value;
end;

procedure TJvBreatheScrollLabel.SetHint(const Value: string);
begin
  FLabel.Hint := Value;
end;

procedure TJvBreatheScrollLabel.SetInterval(const Value: Cardinal);
begin
  TJvLabelScroll(FLabel.BehaviorOptions).Interval := Value;
end;

procedure TJvBreatheScrollLabel.SetLeft(const Value: Integer);
begin
  FLeft := Value;
  FLabel.Left := Value;
end;

procedure TJvBreatheScrollLabel.SetScrolling(const Value: Boolean);
begin
  TJvLabelScroll(FLabel.BehaviorOptions).Active := Value;
end;

procedure TJvBreatheScrollLabel.SetShowHint(const Value: Boolean);
begin
  FLabel.ShowHint := Value;
end;

procedure TJvBreatheScrollLabel.SetTop(const Value: Integer);
begin
  FTop := Value;
  FLabel.Top := FTop;
end;

procedure TJvBreatheScrollLabel.SetTransparent(const Value: Boolean);
begin
  FLabel.Transparent := Value;
end;

procedure TJvBreatheScrollLabel.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  FLabel.Width := Value;
end;

end.

