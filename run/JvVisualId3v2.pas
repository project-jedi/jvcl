{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvVisualId3v2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvVisualId3v2;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  Dialogs, ShellAPI, ComCtrls, StdCtrls, Menus, ExtDlgs,
  JvId3v2, JvID3v2Base, JVCLVer;

type
  TJvPInformation = class(TPersistent)
  private
    function GetExperimental: string;
    function GetExtended: string;
    function GetFileName: string;
    function GetPresent: string;
    function GetSize: string;
    function GetSynchro: string;
    procedure SetExperimental(const Value: string);
    procedure SetExtended(const Value: string);
    procedure SetFileName(const Value: string);
    procedure SetPresent(const Value: string);
    procedure SetSize(const Value: string);
    procedure SetSynchro(const Value: string);
    function GetTagVersion: string;
    procedure SetTagVersion(const Value: string);
  protected
    FParent: TObject;
  published
    property FileName: string read GetFileName write SetFileName;
    property TagPresent: string read GetPresent write SetPresent;
    property TagSize: string read GetSize write SetSize;
    property Unsynchro: string read GetSynchro write SetSynchro;
    property Experimental: string read GetExperimental write SetExperimental;
    property Extended: string read GetExtended write SetExtended;
    property TagVersion: string read GetTagVersion write SetTagVersion;
  end;

  TJvPMisc = class(TPersistent)
  private
    function GetCounter: string;
    function GetInvolved: TStringList;
    function GetPrice: string;
    function GetPurchased: string;
    function GetRating: string;
    function GetSeller: string;
    function GetUserEmail: string;
    procedure SetCounter(const Value: string);
    procedure SetInvolved(const Value: TStringList);
    procedure SetPrice(const Value: string);
    procedure SetPurchased(const Value: string);
    procedure SetRating(const Value: string);
    procedure SetSeller(const Value: string);
    procedure SetUserEmail(const Value: string);
  public
    FParent: TObject;
  published
    property InvolvedPersons: TStringList read GetInvolved write SetInvolved;
    property Seller: string read GetSeller write SetSeller;
    property Price: string read GetPrice write SetPrice;
    property PurchasedThe: string read GetPurchased write SetPurchased;
    property Counter: string read GetCounter write SetCounter;
    property Rating: string read GetRating write SetRating;
    property UserEmail: string read GetUserEmail write SetUserEmail;
  end;

  TJvPText1 = class(TPersistent)
  private
    function GetArtists: TStringList;
    function GetBand: string;
    function GetConductor: string;
    function GetCreation: string;
    function GetLanguages: TStringList;
    function GetLyricists: TStringList;
    function GetRemixed: string;
    function GetSubTitle: string;
    function GetTitle: string;
    procedure SetArtists(const Value: TStringList);
    procedure SetBand(const Value: string);
    procedure SetConductor(const Value: string);
    procedure SetCreation(const Value: string);
    procedure SetLanguages(const Value: TStringList);
    procedure SetLyricists(const Value: TStringList);
    procedure SetRemixed(const Value: string);
    procedure SetSubTitle(const Value: string);
    procedure SetTitle(const Value: string);
    function GetComposers: TStringList;
    procedure SetComposers(const Value: TStringList);
  public
    FParent: TObject;
  published
    property Creation: string read GetCreation write SetCreation;
    property Title: string read GetTitle write SetTitle;
    property SubTitle: string read GetSubTitle write SetSubTitle;
    property Artists: TStringList read GetArtists write SetArtists;
    property Band: string read GetBand write SetBand;
    property Conductor: string read GetConductor write SetConductor;
    property RemixedBy: string read GetRemixed write SetRemixed;
    property Composers: TStringList read GetComposers write SetComposers;
    property Lyricists: TStringList read GetLyricists write SetLyricists;
    property Languages: TStringList read GetLanguages write SetLanguages;
  end;

  TJvPText2 = class(TPersistent)
  private
    function GetAlbum: string;
    function GetContentType: string;
    function GetDate: string;
    function GetISRC: string;
    function GetMediaType: string;
    function GetPart: string;
    function GetRecorded: string;
    function GetTime: string;
    function GetTrack: string;
    function GetYear: string;
    procedure SetAlbum(const Value: string);
    procedure SetContentType(const Value: string);
    procedure SetDate(const Value: string);
    procedure SetISRC(const Value: string);
    procedure SetMediaType(const Value: string);
    procedure SetPart(const Value: string);
    procedure SetRecorded(const Value: string);
    procedure SetTime(const Value: string);
    procedure SetYear(const Value: string);
    procedure SetTrack(const Value: string);
  public
    FParent: TObject;
  published
    property ContentType: string read GetContentType write SetContentType;
    property Album: string read GetAlbum write SetAlbum;
    property Part: string read GetPart write SetPart;
    property Track: string read GetTrack write SetTrack;
    property ISRC: string read GetISRC write SetISRC;
    property Year: string read GetYear write SetYear;
    property Date: string read GetDate write SetDate;
    property Time: string read GetTime write SetTime;
    property recordedAt: string read GetRecorded write SetRecorded;
    property MediaType: string read GetMediaType write SetMediaType;
  end;

  TJvPText3 = class(TPersistent)
  private
    function GetBPM: string;
    function GetCopyright: string;
    function GetDelay: string;
    function GetEncodedBy: string;
    function GetEncoder: string;
    function GetInitialKey: string;
    function GetLength: string;
    function GetOFileName: string;
    function GetPublisher: string;
    function GetSize: string;
    procedure SetBPM(const Value: string);
    procedure SetCopyright(const Value: string);
    procedure SetDelay(const Value: string);
    procedure SetEncodedBy(const Value: string);
    procedure SetEncoder(const Value: string);
    procedure SetInitialKey(const Value: string);
    procedure SetLength(const Value: string);
    procedure SetOFileName(const Value: string);
    procedure SetPublisher(const Value: string);
    procedure SetSize(const Value: string);
  public
    FParent: TObject;
  published
    property BPM: string read GetBPM write SetBPM;
    property Copyright: string read GetCopyright write SetCopyright;
    property Publisher: string read GetPublisher write SetPublisher;
    property EncodedBy: string read GetEncodedBy write SetEncodedBy;
    property Encoder: string read GetEncoder write SetEncoder;
    property OriginalFileName: string read GetOFileName write SetOFileName;
    property Length: string read GetLength write SetLength;
    property Size: string read GetSize write SetSize;
    property Delay: string read GetDelay write SetDelay;
    property InitialKey: string read GetInitialKey write SetInitialKey;
  end;

  TJvPText4 = class(TPersistent)
  private
    function GetFileOwner: string;
    function GetFileType: string;
    function GetOAlbum: string;
    function GetOArtists: TStringList;
    function GetOLyricists: TStringList;
    function GetRadioName: string;
    function GetRadioOwner: string;
    function GetUserDefined: TStringList;
    function GetYear: string;
    procedure SetFileOwner(const Value: string);
    procedure SetFileType(const Value: string);
    procedure SetOAlbum(const Value: string);
    procedure SetOArtists(const Value: TStringList);
    procedure SetOLyricists(const Value: TStringList);
    procedure SetRadioName(const Value: string);
    procedure SetRadioOwner(const Value: string);
    procedure SetUserDefined(const Value: TStringList);
    procedure SetYear(const Value: string);
  public
    FParent: TObject;
  published
    property OriginalAlbum: string read GetOAlbum write SetOAlbum;
    property OriginalArtists: TStringList read GetOArtists write SetOArtists;
    property OriginalLyricists: TStringList read GetOLyricists write SetOLyricists;
    property OriginalYear: string read GetYear write SetYear;
    property FileOwner: string read GetFileOwner write SetFileOwner;
    property FileType: string read GetFileType write SetFileType;
    property RadioName: string read GetRadioName write SetRadioName;
    property RadioOwner: string read GetRadioOwner write SetRadioOwner;
    property UserDefined: TStringList read GetUserDefined write SetUserDefined;
  end;

  TJvPTexts = class(TPersistent)
  private
    FJvText1: TJvPText1;
    FJvText2: TJvPText2;
    FJvText3: TJvPText3;
    FJvText4: TJvPText4;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Text1: TJvPText1 read FJvText1 write FJvText1;
    property Text2: TJvPText2 read FJvText2 write FJvText2;
    property Text3: TJvPText3 read FJvText3 write FJvText3;
    property Text4: TJvPText4 read FJvText4 write FJvText4;
  end;

  TJvPUrls = class(TPersistent)
  private
    function GetArtistPage: string;
    function GetAudioFile: string;
    function GetAudioSource: string;
    function GetCommercial: string;
    function GetCopyright: string;
    function GetPayment: string;
    function GetPublishers: string;
    function GetRadioStation: string;
    function GetUserDefined: TStringList;
    procedure SetArtistPage(const Value: string);
    procedure SetAudioFile(const Value: string);
    procedure SetAudioSource(const Value: string);
    procedure SetCommercial(const Value: string);
    procedure SetCopyright(const Value: string);
    procedure SetPayment(const Value: string);
    procedure SetPublishers(const Value: string);
    procedure SetRadioStation(const Value: string);
    procedure SetUserDefined(const Value: TStringList);
  protected
    FParent: TObject;
  published
    property UserDefined: TStringList read GetUserDefined write SetUserDefined;
    property Commercial: string read GetCommercial write SetCommercial;
    property Copyright: string read GetCopyright write SetCopyright;
    property AudioFile: string read GetAudioFile write SetAudioFile;
    property ArtistPage: string read GetArtistPage write SetArtistPage;
    property AudioSource: string read GetAudioSource write SetAudioSource;
    property RadioStation: string read GetRadioStation write SetRadioStation;
    property Payment: string read GetPayment write SetPayment;
    property Publishers: string read GetPublishers write SetPublishers;
  end;

  TJvPId3v2Editor = class(TPersistent)
  private
    FInformations: TJvPInformation;
    FMisc: TJvPMisc;
    FTexts: TJvPTexts;
    FUrls: TJvPUrls;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Informations: TJvPInformation read FInformations write FInformations;
    property Misc: TJvPMisc read FMisc write FMisc;
    property Texts: TJvPTexts read FTexts write FTexts;
    property Urls: TJvPUrls read FUrls write FUrls;
  end;

  TJvPId3v2TreeView = class(TPersistent)
  private
    function GetHideSelection: Boolean;
    function GetHotTrack: Boolean;
    function GetReadOnly: Boolean;
    function GetWidth: Integer;
    procedure SetHideSelection(const Value: Boolean);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  public
    FParent: TObject;
  published
    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection;
    property Width: Integer read GetWidth write SetWidth;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  TJvVisualId3v2 = class(TWinControl)
  private
    FAboutJVCL: TJVCLAboutInfo;

    FId3v2: TJvId3v2;
    FPhotosTypes: TStringList;
    FDatas: TJvPId3v2Editor;
    FTreeEdit: TJvPId3v2TreeView;

    //Controls
    FTreeView: TTreeView;
    FPageControl: TPageControl;
    FImagePopup: TPopupMenu;
    FPhotoImg: TImage;
    FPhotoTypes: TComboBox;
    FUserD: TComboBox;
    FInvolved: TComboBox;
    FArtists: TComboBox;
    FLyricists: TComboBox;
    FLanguages: TComboBox;
    FUserTexts: TComboBox;
    FOArtists: TComboBox;
    FOLyricists: TComboBox;
    FPublishersLst: TComboBox;

    //Labels
    FFileName: TLabel;
    FTagSize: TLabel;
    FTagVersion: TLabel;
    FUnsynchro: TLabel;
    FExperimental: TLabel;
    FExtended: TLabel;
    FPresent: TLabel;

    FCommercial: TLabel;
    FCopyright: TLabel;
    FAudioFile: TLabel;
    FArtistPage: TLabel;
    FAudioSource: TLabel;
    FRadioStation: TLabel;
    FPayment: TLabel;
    FPublishers: TLabel;
    FSeller: TLabel;
    FPrice: TLabel;
    FDatePur: TLabel;

    FCreation: TLabel;
    FTitle: TLabel;
    FSubTitle: TLabel;
    FBand: TLabel;
    FConductor: TLabel;
    FRemixed: TLabel;

    FPhotoDesc: TStaticText;

    FContent: TLabel;
    FAlbum: TLabel;
    FPart: TLabel;
    FTrack: TLabel;
    FISRC: TLabel;
    FYear: TLabel;
    FDate: TLabel;
    FTime: TLabel;
    FRecordedAt: TLabel;
    FMediaType: TLabel;

    FBPM: TLabel;
    FCopyrightTxt: TLabel;
    FPublisher: TLabel;
    FEncoded: TLabel;
    FEncoder: TLabel;
    FOFileName: TLabel;
    FLength: TLabel;
    FSize: TLabel;
    FDelay: TLabel;
    FInitialKey: TLabel;

    FOAlbum: TLabel;
    FOYear: TLabel;
    FFileOwner: TLabel;
    FFileType: TLabel;
    FRadioName: TLabel;
    FRadioOwner: TLabel;

    FCounter: TLabel;
    FRating: TLabel;
    FUserMail: TLabel;

    //TabSheets
    FInfoSheet: TTabSheet;
    FUrlSheet: TTabSheet;
    FPhotoSheet: TTabSheet;
    FMiscSheet: TTabSheet;
    FText1Sheet: TTabSheet;
    FText2Sheet: TTabSheet;
    FText3Sheet: TTabSheet;
    FText4Sheet: TTabSheet;

    procedure CreateControls;
    procedure CreateMenu;
    procedure CreatePages;
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    procedure SaveImage(Sender: TObject);
    procedure StretchImage(Sender: TObject);
    procedure PhotoChange(Sender: TObject);
    procedure UrlClick(Sender: TObject);
    procedure UrlCombo(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property FileName: TFileName read GetFileName write SetFileName;
    property Datas: TJvPId3v2Editor read FDatas write FDatas;
    property TreeView: TJvPId3v2TreeView read FTreeEdit write FTreeEdit;
    property Align;
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUnDock;
    property TabStop;
    property TabOrder;
    property Enabled;
    property ShowHint;
    property Visible;
  end;

implementation

uses
  JvID3v2Types, JvxRConst;

//=== TJvVisualId3v2 =========================================================

constructor TJvVisualId3v2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 430;
  Height := 300;
  Parent := TWinControl(AOwner);

  FId3v2 := TJvId3v2.Create(Self);

  CreateControls;
  CreateMenu;
  CreatePages;

  FPhotosTypes := TStringList.Create;

  FDatas := TJvPId3v2Editor.Create;
  FDatas.FInformations.FParent := Self;
  FDatas.FMisc.FParent := Self;
  FDatas.FUrls.FParent := Self;
  FDatas.FTexts.FJvText1.FParent := Self;
  FDatas.FTexts.FJvText2.FParent := Self;
  FDatas.FTexts.FJvText3.FParent := Self;
  FDatas.FTexts.FJvText4.FParent := Self;

  FTreeEdit := TJvPId3v2TreeView.Create;
  FTreeEdit.FParent := Self;
end;

destructor TJvVisualId3v2.Destroy;
begin
  FTreeView.Free;
  FPageControl.Free;
  FId3v2.Free;
  FPhotosTypes.Free;
  FDatas.Free;
  FTreeEdit.Free;

  inherited Destroy;
end;

procedure TJvVisualId3v2.CreateControls;
var
  It: TMenuItem;
begin
  FTreeView := TTreeView.Create(Self);
  FTreeView.Parent := Self;
  FTreeView.Align := alLeft;
  FTreeView.Width := 120;
  FTreeView.ReadOnly := True;
  FTreeView.HotTrack := True;
  FTreeView.HideSelection := False;

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align := alClient;

  FImagePopup := TPopupMenu.Create(Self);

  It := TMenuItem.Create(FImagePopup);
  It.Caption := '&Save...';
  It.OnClick := SaveImage;
  FImagePopup.Items.Add(It);

  It := TMenuItem.Create(FImagePopup);
  It.Caption := 'Stretch';
  It.OnClick := StretchImage;
  FImagePopup.Items.Add(It);
end;

procedure TJvVisualId3v2.CreateMenu;
var
  I: Integer;
  Node: TTreeNode;
begin
  Node := FTreeView.Items.AddChild(nil, 'Informations');
  Node.Selected := True;
  FTreeView.Items.AddChild(nil, 'Misc');
  FTreeView.Items.AddChild(nil, 'Photos');
  Node := FTreeView.Items.AddChild(nil, 'Texts');
  for I := 1 to 4 do
    FTreeView.Items.AddChild(Node, Format('Part %d', [I]));
  FTreeView.Items.AddChild(nil, 'Urls');
  FTreeView.OnChange := TreeChange;
end;

procedure TJvVisualId3v2.CreatePages;

  procedure CreateGeneralSheet(var Sheet: TTabSheet; Caption: string);
  begin
    Sheet := TTabSheet.Create(FPageControl);
    Sheet.Parent := FPageControl;
    Sheet.PageControl := FPageControl;
    Sheet.Caption := Caption;
    Sheet.TabVisible := False;
  end;

  procedure CreateInfoSheet;
  var
    FPanel: TPanel;

    procedure CreateLabel1(ATop: Integer; ACaption: string);
    begin
      with TLabel.Create(FInfoSheet) do
      begin
        Parent := FInfoSheet;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateLabel2(ATop: Integer; ACaption: string): TLabel;
    begin
      Result := TLabel.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

  begin
    CreateGeneralSheet(FInfoSheet, 'Informations');

    FPanel := TPanel.Create(FInfoSheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 165;
      Parent := FInfoSheet;
      BevelOuter := bvRaised;
    end;

    CreateLabel1(15, 'FileName');
    CreateLabel1(39, 'Tag Present');
    CreateLabel1(63, 'Tag Size');
    CreateLabel1(87, 'Tag Version');
    CreateLabel1(111, 'Unsynchro');
    CreateLabel1(135, 'Experimental');
    CreateLabel1(159, 'Extended');

    FFileName := CreateLabel2(4, '-');
    FPresent := CreateLabel2(28, '-');
    FTagSize := CreateLabel2(52, '-');
    FTagVersion := CreateLabel2(76, '-');
    FUnsynchro := CreateLabel2(100, '-');
    FExperimental := CreateLabel2(124, '-');
    FExtended := CreateLabel2(149, '-');
  end;

  procedure CreateUrlSheet;
  var
    FPanel: TPanel;

    procedure CreateLabel1(ATop: Integer; ACaption: string);
    begin
      with TLabel.Create(FUrlSheet) do
      begin
        Parent := FUrlSheet;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateLabel2(ATop: Integer; ACaption: string): TLabel;
    begin
      Result := TLabel.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

  begin
    CreateGeneralSheet(FUrlSheet, 'Urls');

    FPanel := TPanel.Create(FUrlSheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 225;
      Parent := FUrlSheet;
      BevelOuter := bvRaised;
    end;

    CreateLabel1(15, 'Commercial');
    CreateLabel1(39, 'Copyright');
    CreateLabel1(63, 'Audio File');
    CreateLabel1(87, 'Artist Page');
    CreateLabel1(111, 'Audio Source');
    CreateLabel1(135, 'Radio Station');
    CreateLabel1(159, 'Payment');
    CreateLabel1(183, 'Publishers');
    CreateLabel1(207, 'User Defined');

    FCommercial := CreateLabel2(4, '-');
    FCopyright := CreateLabel2(28, '-');
    FAudioFile := CreateLabel2(52, '-');
    FArtistPage := CreateLabel2(76, '-');
    FAudioSource := CreateLabel2(100, '-');
    FRadioStation := CreateLabel2(124, '-');
    FPayment := CreateLabel2(149, '-');
    FPublishers := CreateLabel2(173, '-');

    FUserD := TComboBox.Create(FPanel);
    with FUserD do
    begin
      Parent := FPanel;
      Width := FPanel.Width - Left - 10;
      Left := 6;
      Top := 194;
      Style := csDropDownList;
      OnChange := UrlCombo;
    end;
  end;

  procedure CreatePhotoSheet;
  var
    FPanel: TPanel;
  begin
    FPhotoSheet := TTabSheet.Create(FPageControl);
    FPhotoSheet.Parent := FPageControl;
    FPhotoSheet.PageControl := FPageControl;
    FPhotoSheet.Caption := 'Photos';
    FPhotoSheet.TabVisible := False;

    with TLabel.Create(FPhotoSheet) do
    begin
      Parent := FPhotoSheet;
      AutoSize := True;
      Left := 6;
      Top := 15;
      Caption := 'Types';
    end;

    with TLabel.Create(FPhotoSheet) do
    begin
      Parent := FPhotoSheet;
      AutoSize := True;
      Left := 6;
      Top := 39;
      Caption := 'Description';
    end;

    FPanel := TPanel.Create(FPhotoSheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 60;
      Parent := FPhotoSheet;
      BevelOuter := bvRaised;
    end;

    FPhotoTypes := TComboBox.Create(FPanel);
    with FPhotoTypes do
    begin
      Parent := FPanel;
      Width := FPanel.Width - Left - 10;
      Left := 6;
      Top := 4;
      Style := csDropDownList;
      OnChange := PhotoChange;
      OnClick := PhotoChange;
      OnDropDown := PhotoChange;
    end;

    FPhotoDesc := TStaticText.Create(FPanel);
    with FPhotoDesc do
    begin
      Parent := FPanel;
      AutoSize := False;
      Left := 6;
      Top := 28;
      Width := FPanel.Width - Left - 10;
      Height := 28;
      Caption := '-';
    end;

    FPhotoImg := TImage.Create(FPhotoSheet);
    with FPhotoImg do
    begin
      Parent := FPhotoSheet;
      AutoSize := False;
      Left := 10;
      Top := 85;
      Width := FPhotoSheet.Width - 20;
      Height := FPhotoSheet.Height - 95;
      Stretch := False;
      Center := True;
      PopupMenu := FImagePopup;
    end;
  end;

  procedure CreateMiscSheet;
  var
    FPanel: TPanel;

    procedure CreateLabel1(ATop: Integer; ACaption: string);
    begin
      with TLabel.Create(FMiscSheet) do
      begin
        Parent := FMiscSheet;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateLabel2(ATop: Integer; ACaption: string): TLabel;
    begin
      Result := TLabel.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

  begin
    FMiscSheet := TTabSheet.Create(FPageControl);
    FMiscSheet.Parent := FPageControl;
    FMiscSheet.PageControl := FPageControl;
    FMiscSheet.Caption := 'Misc';
    FMiscSheet.TabVisible := False;

    FPanel := TPanel.Create(FMiscSheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 100;
      Parent := FMiscSheet;
      BevelOuter := bvRaised;
    end;

    CreateLabel1(15, 'Involved Pers');
    CreateLabel1(39, 'Seller');
    CreateLabel1(63, 'Price');
    CreateLabel1(87, 'Purchased the');

    FInvolved := TComboBox.Create(FPanel);
    with FInvolved do
    begin
      Parent := FPanel;
      Width := FPanel.Width - Left - 10;
      Left := 6;
      Top := 4;
      Style := csDropDownList;
    end;

    FSeller := CreateLabel2(28, '-');
    FPrice := CreateLabel2(52, '-');
    FDatePur := CreateLabel2(76, '-');

    //Popularimeter, play counter
    CreateLabel1(121, 'Counter');
    CreateLabel1(145, 'Rating');
    CreateLabel1(169, 'User Email');

    FPanel := TPanel.Create(FMiscSheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 115;
      Width := 210;
      Height := 75;
      Parent := FMiscSheet;
      BevelOuter := bvRaised;
    end;

    FCounter := CreateLabel2(6, '-');
    FRating := CreateLabel2(30, '-');
    FUserMail := CreateLabel2(54, '-');
  end;

  procedure CreateText1Sheet;
  var
    FPanel: TPanel;

    procedure CreateLabel1(ATop: Integer; ACaption: string);
    begin
      with TLabel.Create(FText1Sheet) do
      begin
        Parent := FText1Sheet;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateLabel2(ATop: Integer; ACaption: string): TLabel;
    begin
      Result := TLabel.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateCombo2(ATop: Integer): TComboBox;
    begin
      Result := TComboBox.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        Left := 6;
        Top := ATop;
        Width := FPanel.Width - 12;
        Style := csDropDownList;
      end;
    end;

  begin
    FText1Sheet := TTabSheet.Create(FPageControl);
    FText1Sheet.Parent := FPageControl;
    FText1Sheet.PageControl := FPageControl;
    FText1Sheet.Caption := 'Part 1';
    FText1Sheet.TabVisible := False;

    FPanel := TPanel.Create(FText1Sheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 250;
      Parent := FText1Sheet;
      BevelOuter := bvRaised;
    end;

    CreateLabel1(15, 'Creation');
    CreateLabel1(39, 'Title');
    CreateLabel1(63, 'SubTitle');
    CreateLabel1(87, 'Artist(s)');
    CreateLabel1(111, 'Band');
    CreateLabel1(135, 'Conductor');
    CreateLabel1(159, 'Remixed by');
    CreateLabel1(183, 'Composer(s)');
    CreateLabel1(207, 'Lyricist(s)');
    CreateLabel1(231, 'Langague(s)');

    FCreation := CreateLabel2(4, '-');
    FTitle := CreateLabel2(28, '-');
    FSubTitle := CreateLabel2(52, '-');
    FArtists := CreateCombo2(74);
    FBand := CreateLabel2(100, '-');
    FConductor := CreateLabel2(124, '-');
    FRemixed := CreateLabel2(149, '-');
    FPublishersLst := CreateCombo2(171);
    FLyricists := CreateCombo2(195);
    FLanguages := CreateCombo2(220);
  end;

  procedure CreateText2Sheet;
  var
    FPanel: TPanel;

    procedure CreateLabel1(ATop: Integer; ACaption: string);
    begin
      with TLabel.Create(FText2Sheet) do
      begin
        Parent := FText2Sheet;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateLabel2(ATop: Integer; ACaption: string): TLabel;
    begin
      Result := TLabel.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateCombo2(ATop: Integer): TComboBox;
    begin
      Result := TComboBox.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        Left := 6;
        Top := ATop;
        Width := FPanel.Width - 12;
        Style := csDropDownList;
      end;
    end;

  begin
    FText2Sheet := TTabSheet.Create(FPageControl);
    FText2Sheet.Parent := FPageControl;
    FText2Sheet.PageControl := FPageControl;
    FText2Sheet.Caption := 'Part 2';
    FText2Sheet.TabVisible := False;

    FPanel := TPanel.Create(FText2Sheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 250;
      Parent := FText2Sheet;
      BevelOuter := bvRaised;
    end;

    CreateLabel1(15, 'Content Type');
    CreateLabel1(39, 'Album');
    CreateLabel1(63, 'Part');
    CreateLabel1(87, 'Track');
    CreateLabel1(111, 'ISRC');
    CreateLabel1(135, 'Year');
    CreateLabel1(159, 'Date');
    CreateLabel1(183, 'Time');
    CreateLabel1(207, 'Recorded at');
    CreateLabel1(231, 'Media Type');

    FContent := CreateLabel2(4, '-');
    FAlbum := CreateLabel2(28, '-');
    FPart := CreateLabel2(52, '-');
    FTrack := CreateLabel2(76, '-');
    FISRC := CreateLabel2(100, '-');
    FYear := CreateLabel2(124, '-');
    FDate := CreateLabel2(149, '-');
    FTime := CreateLabel2(173, '-');
    FRecordedAt := CreateLabel2(197, '-');
    FMediaType := CreateLabel2(221, '-');
  end;

  procedure CreateText3Sheet;
  var
    FPanel: TPanel;

    procedure CreateLabel1(ATop: Integer; ACaption: string);
    begin
      with TLabel.Create(FText3Sheet) do
      begin
        Parent := FText3Sheet;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateLabel2(ATop: Integer; ACaption: string): TLabel;
    begin
      Result := TLabel.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateCombo2(ATop: Integer): TComboBox;
    begin
      Result := TComboBox.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        Left := 6;
        Top := ATop;
        Width := FPanel.Width - 12;
        Style := csDropDownList;
      end;
    end;

  begin
    FText3Sheet := TTabSheet.Create(FPageControl);
    FText3Sheet.Parent := FPageControl;
    FText3Sheet.PageControl := FPageControl;
    FText3Sheet.Caption := 'Part 3';
    FText3Sheet.TabVisible := False;

    FPanel := TPanel.Create(FText3Sheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 250;
      Parent := FText3Sheet;
      BevelOuter := bvRaised;
    end;

    CreateLabel1(15, 'BPM');
    CreateLabel1(39, 'Copyright');
    CreateLabel1(63, 'Publisher');
    CreateLabel1(87, 'Encoded By');
    CreateLabel1(111, 'Encoder');
    CreateLabel1(135, 'O. FileName');
    CreateLabel1(159, 'Length');
    CreateLabel1(183, 'Size');
    CreateLabel1(207, 'Delay');
    CreateLabel1(231, 'Initial Key');

    FBPM := CreateLabel2(4, '-');
    FCopyrightTxt := CreateLabel2(28, '-');
    FPublisher := CreateLabel2(52, '-');
    FEncoded := CreateLabel2(76, '-');
    FEncoder := CreateLabel2(100, '-');
    FOFileName := CreateLabel2(124, '-');
    FLength := CreateLabel2(149, '-');
    FSize := CreateLabel2(173, '-');
    FDelay := CreateLabel2(197, '-');
    FInitialKey := CreateLabel2(221, '-');
  end;

  procedure CreateText4Sheet;
  var
    FPanel: TPanel;

    procedure CreateLabel1(ATop: Integer; ACaption: string);
    begin
      with TLabel.Create(FText4Sheet) do
      begin
        Parent := FText4Sheet;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateLabel2(ATop: Integer; ACaption: string): TLabel;
    begin
      Result := TLabel.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        AutoSize := True;
        Left := 6;
        Top := ATop;
        Caption := ACaption;
      end;
    end;

    function CreateCombo2(ATop: Integer): TComboBox;
    begin
      Result := TComboBox.Create(FPanel);
      with Result do
      begin
        Parent := FPanel;
        Left := 6;
        Top := ATop;
        Width := FPanel.Width - 12;
        Style := csDropDownList;
      end;
    end;

  begin
    FText4Sheet := TTabSheet.Create(FPageControl);
    FText4Sheet.Parent := FPageControl;
    FText4Sheet.PageControl := FPageControl;
    FText4Sheet.Caption := 'Part 4';
    FText4Sheet.TabVisible := False;

    FPanel := TPanel.Create(FText4Sheet);
    with FPanel do
    begin
      Caption := '';
      Left := 80;
      Top := 10;
      Width := 210;
      Height := 225;
      Parent := FText4Sheet;
      BevelOuter := bvRaised;
    end;

    CreateLabel1(15, 'O. Album');
    CreateLabel1(39, 'O. Artist(s)');
    CreateLabel1(63, 'O. Lyricist(s)');
    CreateLabel1(87, 'O. Year');
    CreateLabel1(111, 'File Owner');
    CreateLabel1(135, 'File Type');
    CreateLabel1(159, 'Radio Name');
    CreateLabel1(183, 'Radio Owner');
    CreateLabel1(207, 'User Defined');

    FOAlbum := CreateLabel2(4, '-');
    FOArtists := CreateCombo2(26);
    FOLyricists := CreateCombo2(50);
    FOYear := CreateLabel2(76, '-');
    FFileOwner := CreateLabel2(100, '-');
    FFileType := CreateLabel2(124, '-');
    FRadioName := CreateLabel2(149, '-');
    FRadioOwner := CreateLabel2(173, '-');
    FUserTexts := CreateCombo2(194);
  end;

begin
  CreateInfoSheet;
  CreateUrlSheet;
  CreatePhotoSheet;
  CreateMiscSheet;
  CreateText1Sheet;
  CreateText2Sheet;
  CreateText3Sheet;
  CreateText4Sheet;

  FPageControl.ActivePage := FInfoSheet;
end;

function TJvVisualId3v2.GetFileName: TFileName;
begin
  Result := FId3v2.FileName;
end;

procedure TJvVisualId3v2.PhotoChange(Sender: TObject);
var
  I: Integer;

  procedure SetIt(Value: TPicture; Text: string);
  begin
    FPhotoImg.Picture.Assign(Value);
    FPhotoDesc.Caption := Text;
  end;

begin
  if FPhotoTypes.ItemIndex = -1 then
    SetIt(nil, '')
  else
  begin
    I := FPhotosTypes.IndexOf(FPhotoTypes.Items[FPhotoTypes.ItemIndex]);
    if FPhotoTypes.Tag = I then
      Exit;
    FPhotoTypes.Tag := I;
    with FId3v2.Images do
      case I of
        0:
          SetIt(Pictures.Artist, Infos.Artist);
        1:
          SetIt(Pictures.Band, Infos.Band);
        2:
          SetIt(Pictures.BandLogotype, Infos.BandLogotype);
        3:
          SetIt(Pictures.BrightColouredFish, Infos.BrightColouredFish);
        4:
          SetIt(Pictures.Composer, Infos.Composer);
        5:
          SetIt(Pictures.Conductor, Infos.Conductor);
        6:
          SetIt(Pictures.CoverBack, Infos.CoverBack);
        7:
          SetIt(Pictures.CoverFront, Infos.CoverFront);
        8:
          SetIt(Pictures.DuringPerformance, Infos.DuringPerformance);
        9:
          SetIt(Pictures.DuringRecording, Infos.DuringRecording);
        10:
          SetIt(Pictures.FileIcon, Infos.FileIcon);
        11:
          SetIt(Pictures.Illustration, Infos.Illustration);
        12:
          SetIt(Pictures.LeadArtist, Infos.LeadArtist);
        13:
          SetIt(Pictures.LeafletPage, Infos.LeafletPage);
        14:
          SetIt(Pictures.Lyricist, Infos.Lyricist);
        15:
          SetIt(Pictures.Media, Infos.Media);
        16:
          SetIt(Pictures.MovieVideoScreenCapture, Infos.MovieVideoScreenCapture);
        17:
          SetIt(Pictures.Other, Infos.Other);
        18:
          SetIt(Pictures.OtherFileIcon, Infos.OtherFileIcon);
        19:
          SetIt(Pictures.PublisherLogotype, Infos.PublisherLogotype);
        20:
          SetIt(Pictures.RecordingLocation, Infos.RecordingLocation);
      end;
  end;
end;

procedure TJvVisualId3v2.SaveImage(Sender: TObject);
begin
  if (FImagePopup.PopupComponent as TImage).Width = 0 then
  begin
    Beep;
    Exit;
  end;

  with TSavePictureDialog.Create(Self) do
  try
    if Execute then
      with FImagePopup.PopupComponent as TImage do
        Picture.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TJvVisualId3v2.SetFileName(const Value: TFileName);
var
  I: Integer;

  (*function MediaTypeToStr(Value: TJvID3MediaType): string;
  begin
    case Value of
      mtUNKNOWN:
        Result := 'Unknown';
      mtDIG:
        Result := 'Other digital media';
      mtDIG_ANALOG_TRANSFER:
        Result := 'Other digital media';
      mtANA:
        Result := 'Other analog media';
      mtANA_WAX_CYLINDER:
        Result := 'Wax cylinder';
      mtANA_8_TRACK_TAPE:
        Result := '8-track tape cassette';
      mtCD:
        Result := 'CD';
      mtCD_ANALOG:
        Result := 'CD';
      mtCD_DDD:
        Result := 'CD-DDD';
      mtCD_ADD:
        Result := 'CD-ADD';
      mtCD_AAD:
        Result := 'CD-AAD';
      mtLASERDISC:
        Result := 'Laserdisc';
      mtLASERDISC_ANALOG_TRANSFER:
        Result := 'Laserdisc';
      mtTURNTABLE:
        Result := 'Turntable records';
      mtTURNTABLE_33:
        Result := 'Turntable records - 33.33 rpm';
      mtTURNTABLE_45:
        Result := 'Turntable records - 45 rpm';
      mtTURNTABLE_71:
        Result := 'Turntable records - 71.29 rpm';
      mtTURNTABLE_76:
        Result := 'Turntable records - 76.59 rpm';
      mtTURNTABLE_78:
        Result := 'Turntable records - 78.26 rpm';
      mtTURNTABLE_80:
        Result := 'Turntable records - 80 rpm';
      mtMINIDISC:
        Result := 'MiniDisc';
      mtMINIDISC_ANALOG_TRANSFER:
        Result := 'MiniDisc';
      mtDAT:
        Result := 'DAT';
      mtDAT_ANALOG_TRANSFER:
        Result := 'DAT';
      mtDAT_48KHZ_16B:
        Result := 'DAT - standard, 48 kHz/16 bits, linear';
      mtDAT_32KHZ_16B:
        Result := 'DAT - mode 2, 32 kHz/16 bits, linear';
      mtDAT_32KHZ_12B:
        Result := 'DAT - mode 3, 32 kHz/12 bits, nonlinear, low speed';
      mtDAT_44KHZ_16B:
        Result := 'DAT - mode 5, 44.1 kHz/16 bits, linear';
      mtDAT_44KHZ_16B_WIDE:
        Result := 'DAT - mode 6, 44.1 kHz/16 bits, ''wide track'' play';
      mtDCC:
        Result := 'DCC';
      mtDCC_ANALOG_TRANSFER:
        Result := 'DCC';
      mtDVD:
        Result := 'DVD';
      mtDVD_ANALOG_TRANSFER:
        Result := 'DVD';
      mtTV:
        Result := 'Television';
      mtTV_PAL:
        Result := 'Television - Pal';
      mtTV_NTSC:
        Result := 'Television - NTSC';
      mtTV_SECAM:
        Result := 'Television - Secam';
      mtVID:
        Result := 'Video';
      mtVID_PAL:
        Result := 'Video - Pal';
      mtVID_NTSC:
        Result := 'Video - NTSC';
      mtVID_SECAM:
        Result := 'Video - Secam';
      mtVID_VHS:
        Result := 'Video - VHS';
      mtVID_SVHS:
        Result := 'Video - S-VHS';
      mtVID_BETA:
        Result := 'Video - BetaMax';
      mtRAD:
        Result := 'Radio';
      mtRAD_FM:
        Result := 'Radio - FM';
      mtRAD_AM:
        Result := 'Radio - AM';
      mtRAD_LW:
        Result := 'Radio - LW';
      mtRAD_MW:
        Result := 'Radio - MW';
      mtTEL:
        Result := 'Telephone';
      mtTEL_ISDN:
        Result := 'Telephone - ISDN';
      mtMC:
        Result := 'MC';
      mtMC_4:
        Result := 'MC - 4.75 cm/s (normal speed for a two sided cassette)';
      mtMC_9:
        Result := 'MC - 9.5 cm/s';
      mtMC_I:
        Result := 'MC - Type I cassette (ferric/normal)';
      mtMC_II:
        Result := 'MC - Type II cassette (chrome)';
      mtMC_III:
        Result := 'MC - Type III cassette (ferric chrome)';
      mtMC_IV:
        Result := 'MC - Type IV cassette (metal)';
      mtREE:
        Result := 'Reel';
      mtREE_9:
        Result := 'Reel - 9.5 cm/s';
      mtREE_19:
        Result := 'Reel - 19 cm/s';
      mtREE_38:
        Result := 'Reel - 38 cm/s';
      mtREE_76:
        Result := 'Reel - 76 cm/s';
      mtREE_I:
        Result := 'Reel - Type I cassette (ferric/normal)';
      mtREE_II:
        Result := 'Reel - Type II cassette (chrome)';
      mtREE_III:
        Result := 'Reel - Type III cassette (ferric chrome)';
      mtREE_IV:
        Result := 'Reel - Type IV cassette (metal)';
      mtDAT_32KHZ_12B_4CH:
        Result := 'DAT - mode 4, 32 kHz/12 bits, 4 channels';
    end;
  end;

  function FileTypeToStr(Value: TJvID3FileType): string;
  begin
    case Value of
      ftUNKNOWN:
        Result := 'Unknown';
      ftMPG:
        Result := 'MPEG Audio';
      ftMPG1:
        Result := 'MPEG 1/2 layer I';
      ftMPG2:
        Result := 'MPEG 1/2 layer II';
      ftMPG3:
        Result := 'MPEG 1/2 layer III';
      ftMPG2_5:
        Result := 'MPEG 2.5';
      ftMPG_AAC:
        Result := 'Advanced audio compression';
      ftVQF:
        Result := 'VQF';
      ftPCM:
        Result := 'Pulse Code Modulated audio';
    end;
  end;*)

  procedure SetBoolean(FLabel: TLabel; Value: Boolean);
  begin
    if Value then
      FLabel.Caption := 'True'
    else
      FLabel.Caption := 'False';
  end;

  procedure AddPhoto(Value: TPicture; Text: string);
  begin
    FPhotosTypes.Add(Text);
    if Value.Width <> 0 then
      FPhotoTypes.Items.Add(Text);
  end;

  procedure SetUrl(FLabel: TLabel; Value: string);
  begin
    if Value = '' then
    begin
      FLabel.Caption := '';
      FLabel.OnClick := nil;
      FLabel.Cursor := crDefault
    end
    else
    begin
      FLabel.Caption := Value;
      FLabel.OnClick := UrlClick;
      FLabel.Cursor := crHandPoint;
    end;
  end;

begin
  //Update
  FId3v2.FileName := Value;

  //General page
  FFileName.Caption := ExtractFileName(Value);
  FTagSize.Caption := IntToStr(FId3v2.TagSize) + ' Byte(s)';
  case FID3v2.Version of
    ive2_2AndLower:
      FTagVersion.Caption := '<2.3';
    ive2_3:
      FTagVersion.Caption := '2.3';
    ive2_4:
      FTagVersion.Caption := '2.4';
    ive2_5AndHigher:
      FTagVersion.Caption := '>2.4';
  else
    ID3Error(SID3UnknownVersion, Self);
  end;

  SetBoolean(FUnsynchro, hfUnsynchronisation in FId3v2.Header.Flags);
  SetBoolean(FExperimental, hfExtendedHeader in FId3v2.Header.Flags);
  SetBoolean(FExtended, hfExperimentalIndicator in FId3v2.Header.Flags);
  SetBoolean(FPresent, FId3v2.Header.HasTag);

  //Photos
  FPhotoTypes.Items.Clear;
  FPhotoTypes.OnChange(nil);
  FPhotosTypes.Clear;
  with FId3v2.Images.Pictures do
  begin
    AddPhoto(Artist, 'Artist');
    AddPhoto(Band, 'Band');
    AddPhoto(BandLogotype, 'Band Logo');
    AddPhoto(BrightColouredFish, 'Coloured Fish');
    AddPhoto(Composer, 'Composer');
    AddPhoto(Conductor, 'Conductor');
    AddPhoto(CoverBack, 'Cover Back');
    AddPhoto(CoverFront, 'Cover Front');
    AddPhoto(DuringPerformance, 'During Performance');
    AddPhoto(DuringRecording, 'During recording');
    AddPhoto(FileIcon, 'File Icon');
    AddPhoto(Illustration, 'Illustration');
    AddPhoto(LeadArtist, 'Lead Artist');
    AddPhoto(LeafletPage, 'Leaflet');
    AddPhoto(Lyricist, 'Lyricist');
    AddPhoto(Media, 'Media');
    AddPhoto(MovieVideoScreenCapture, 'Movie Capture');
    AddPhoto(Other, 'Other');
    AddPhoto(OtherFileIcon, 'Other Icon');
    AddPhoto(PublisherLogotype, 'Publisher Logo');
    AddPhoto(RecordingLocation, 'Recording Location');
  end;
  if FPhototypes.Items.Count > 0 then
  begin
    FPhotoTypes.Tag := -1;
    FPhototypes.ItemIndex := 0;
    FPhotoTypes.OnChange(nil);
  end;

  //Urls
  SetUrl(FCommercial, FId3v2.Web.CommercialInfo);
  SetUrl(FCopyright, FId3v2.Web.Copyright);
  SetUrl(FAudioFile, FId3v2.Web.AudioFile);
  SetUrl(FArtistPage, FId3v2.Web.Artist);
  SetUrl(FAudioSource, FId3v2.Web.AudioSource);
  SetUrl(FRadioStation, FId3v2.Web.RadioPage);
  SetUrl(FPayment, FId3v2.Web.Payment);
  SetUrl(FPublishers, FId3v2.Web.Publisher);
  FUserD.Items.Clear;
  for I := 0 to FId3v2.UserDefinedWeb.ItemCount - 1 do
  begin
    FId3v2.UserDefinedWeb.ItemIndex := I;
    FUserD.Items.Add(FId3v2.UserDefinedWeb.URL);
  end;
  if FUserD.Items.Count > 0 then
    FUserD.ItemIndex := 0;

  //Involved
  FInvolved.Items.Clear;
  for I := 0 to FId3v2.InvolvedPeople.ItemCount - 1 do
  begin
    FId3v2.InvolvedPeople.ItemIndex := I;
    FInvolved.Items.Add(FId3v2.InvolvedPeople.Person + ' (' + FId3v2.InvolvedPeople.Job + ')');
  end;
  if FInvolved.Items.Count > 0 then
    FInvolved.ItemIndex := 0;
  FSeller.Caption := FId3v2.Owner.Seller;
  FPrice.Caption := FId3v2.Owner.Price;
  FDatePur.Caption := DateToStr(FId3v2.Owner.DatePurchased);

  //Text part 1
  FCreation.Caption := FId3v2.Texts.Date;
  FTitle.Caption := FId3v2.Texts.Album;
  FSubTitle.Caption := FId3v2.Texts.SubTitle;
  FBand.Caption := FId3v2.Texts.Band;
  FConductor.Caption := FId3v2.Texts.Conductor;
  FRemixed.Caption := FId3v2.Texts.MixArtist;
  FArtists.Text := FId3v2.Texts.OrigArtist.Text;
  if FArtists.Items.Count > 0 then
    FArtists.ItemIndex := 0;
  FPublishersLst.Text := FId3v2.Texts.Composer.Text;
  if FPublishersLst.Items.Count > 0 then
    FPublishersLst.ItemIndex := 0;
  FLanguages.Text := FId3v2.Texts.Language.Text;
  if FLanguages.Items.Count > 0 then
    FLanguages.ItemIndex := 0;
  FLyricists.Text := FId3v2.Texts.Lyricist.Text;
  if FLyricists.Items.Count > 0 then
    FLyricists.ItemIndex := 0;

  //Text part 2
  FContent.Caption := FId3v2.Texts.ContentType;
  FAlbum.Caption := FId3v2.Texts.Album;
  FPart.Caption := FId3v2.Texts.PartInSet;
  FTrack.Caption := FId3v2.Texts.TrackNum;
  FISRC.Caption := FId3v2.Texts.ISRC;
  FYear.Caption := IntToStr(FId3v2.Texts.Year);
  FDate.Caption := FId3v2.Texts.Date;
  FTime.Caption := FId3v2.Texts.Time;
  FRecordedAt.Caption := FId3v2.Texts.RecordingDates;
  FMediaType.Caption := FId3v2.Texts.MediaType;

  //Text part 3
  FBPM.Caption := IntToStr(FId3v2.Texts.BPM);
  FCopyrightTxt.Caption := FId3v2.Texts.Copyright;
  FPublisher.Caption := FId3v2.Texts.Publisher;
  FEncoded.Caption := FId3v2.Texts.EncodedBy;
  FEncoder.Caption := FId3v2.Texts.EncoderSettings;
  FOFileName.Caption := FId3v2.Texts.OrigFileName;
  FLength.Caption := IntToStr(FId3v2.Texts.SongLen);
  FSize.Caption := IntToStr(FId3v2.Texts.Size) + ' Byte(s)';
  FDelay.Caption := IntToStr(FId3v2.Texts.PlaylistDelay) + ' millisecond(s)';
  FInitialKey.Caption := FId3v2.Texts.InitialKey;

  //Text part 4
  FOAlbum.Caption := FId3v2.Texts.OrigAlbum;
  FOArtists.Text := FId3v2.Texts.OrigArtist.Text;
  if FOArtists.Items.Count > 0 then
    FOArtists.ItemIndex := 0;
  FOLyricists.Text := FId3v2.Texts.OrigLyricist.Text;
  if FOLyricists.Items.Count > 0 then
    FOLyricists.ItemIndex := 0;
  FOYear.Caption := IntToStr(FId3v2.Texts.OrigYear);
  FFileOwner.Caption := FId3v2.Texts.FileOwner;
  FFileType.Caption := FId3v2.Texts.FileType;
  FRadioName.Caption := FId3v2.Texts.NetRadioStation;
  FRadioOwner.Caption := FId3v2.Texts.NetRadioOwner;

  FUserTexts.Clear;
  for I := 0 to FId3v2.UserDefinedText.ItemCount - 1 do
  begin
    FId3v2.UserDefinedText.ItemIndex := I;
    FUserTexts.Items.Add(FId3v2.UserDefinedText.Value + ' (' + FId3v2.UserDefinedText.Description + ')');
  end;
  if FUserTexts.Items.Count > 0 then
    FUserTexts.ItemIndex := 0;

  //Popularimeter
  FCounter.Caption := IntToStr(FId3v2.Popularimeter.Counter);
  FRating.Caption := IntToStr(FId3v2.Popularimeter.Rating div 255) + '%';
  FUserMail.Caption := FId3v2.Popularimeter.EmailAddress;
end;

procedure TJvVisualId3v2.StretchImage(Sender: TObject);
begin
  (FImagePopup.PopupComponent as TImage).Stretch := not (FImagePopup.PopupComponent as TImage).Stretch;
end;

procedure TJvVisualId3v2.TreeChange(Sender: TObject; Node: TTreeNode);
var
  I: Integer;
begin
  for I := 0 to FPageControl.PageCount - 1 do
    if FPageControl.Pages[I].Caption = Node.Text then
      FPageControl.ActivePage := FPageControl.Pages[I];
end;

procedure SurfTo(Url: string);
begin
  ShellExecute(0, nil, PChar(Url), nil, nil, SW_NORMAL);
end;

procedure TJvVisualId3v2.UrlClick(Sender: TObject);
begin
  with (Sender as TLabel) do
    SurfTo(Caption);
end;

procedure TJvVisualId3v2.UrlCombo(Sender: TObject);
begin
  with (Sender as TComboBox) do
    if ItemIndex <> -1 then
      SurfTo(Items[ItemIndex]);
end;

//=== TJvPId3v2Editor ========================================================

constructor TJvPId3v2Editor.Create;
begin
  inherited Create;
  FInformations := TJvPInformation.Create;
  FMisc := TJvPMisc.Create;
  FTexts := TJvPTexts.Create;
  FUrls := TJvPUrls.Create;
end;

destructor TJvPId3v2Editor.Destroy;
begin
  FInformations.Free;
  FMisc.Free;
  FTexts.Free;
  FUrls.Free;
  inherited Destroy;
end;

//=== TJvPInformation ========================================================

function TJvPInformation.GetExperimental: string;
begin
  Result := (FParent as TJvVisualId3v2).FExperimental.Caption;
end;

function TJvPInformation.GetExtended: string;
begin
  Result := (FParent as TJvVisualId3v2).FExtended.Caption;
end;

function TJvPInformation.GetFileName: string;
begin
  Result := (FParent as TJvVisualId3v2).FFileName.Caption;
end;

function TJvPInformation.GetPresent: string;
begin
  Result := (FParent as TJvVisualId3v2).FPresent.Caption;
end;

function TJvPInformation.GetSize: string;
begin
  Result := (FParent as TJvVisualId3v2).FTagSize.Caption;
end;

function TJvPInformation.GetSynchro: string;
begin
  Result := (FParent as TJvVisualId3v2).FUnsynchro.Caption;
end;

function TJvPInformation.GetTagVersion: string;
begin
  Result := (FParent as TJvVisualId3v2).FTagVersion.Caption;
end;

procedure TJvPInformation.SetExperimental(const Value: string);
begin
  (FParent as TJvVisualId3v2).FExperimental.Caption := Value;
end;

procedure TJvPInformation.SetExtended(const Value: string);
begin
  (FParent as TJvVisualId3v2).FExtended.Caption := Value;
end;

procedure TJvPInformation.SetFileName(const Value: string);
begin
  (FParent as TJvVisualId3v2).FFileName.Caption := Value;
end;

procedure TJvPInformation.SetPresent(const Value: string);
begin
  (FParent as TJvVisualId3v2).FPresent.Caption := Value;
end;

procedure TJvPInformation.SetSize(const Value: string);
begin
  (FParent as TJvVisualId3v2).FTagSize.Caption := Value;
end;

procedure TJvPInformation.SetSynchro(const Value: string);
begin
  (FParent as TJvVisualId3v2).FUnsynchro.Caption := Value;
end;

procedure TJvPInformation.SetTagVersion(const Value: string);
begin
  (FParent as TJvVisualId3v2).FTagVersion.Caption := Value;
end;

//=== TJvPUrls ===============================================================

function TJvPUrls.GetArtistPage: string;
begin
  Result := (FParent as TJvVisualId3v2).FArtistPage.Caption;
end;

function TJvPUrls.GetAudioFile: string;
begin
  Result := (FParent as TJvVisualId3v2).FAudioFile.Caption;
end;

function TJvPUrls.GetAudioSource: string;
begin
  Result := (FParent as TJvVisualId3v2).FAudioSource.Caption;
end;

function TJvPUrls.GetCommercial: string;
begin
  Result := (FParent as TJvVisualId3v2).FCommercial.Caption;
end;

function TJvPUrls.GetCopyright: string;
begin
  Result := (FParent as TJvVisualId3v2).FCommercial.Caption;
end;

function TJvPUrls.GetPayment: string;
begin
  Result := (FParent as TJvVisualId3v2).FPayment.Caption;
end;

function TJvPUrls.GetPublishers: string;
begin
  Result := (FParent as TJvVisualId3v2).FPublishers.Caption;
end;

function TJvPUrls.GetRadioStation: string;
begin
  Result := (FParent as TJvVisualId3v2).FRadioStation.Caption;
end;

function TJvPUrls.GetUserDefined: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FUserD.Items.Text;
end;

procedure TJvPUrls.SetArtistPage(const Value: string);
begin
  (FParent as TJvVisualId3v2).FArtistPage.Caption := Value;
end;

procedure TJvPUrls.SetAudioFile(const Value: string);
begin
  (FParent as TJvVisualId3v2).FAudioFile.Caption := Value;
end;

procedure TJvPUrls.SetAudioSource(const Value: string);
begin
  (FParent as TJvVisualId3v2).FAudioSource.Caption := Value;
end;

procedure TJvPUrls.SetCommercial(const Value: string);
begin
  (FParent as TJvVisualId3v2).FCommercial.Caption := Value;
end;

procedure TJvPUrls.SetCopyright(const Value: string);
begin
  (FParent as TJvVisualId3v2).FCopyright.Caption := Value;
end;

procedure TJvPUrls.SetPayment(const Value: string);
begin
  (FParent as TJvVisualId3v2).FPayment.Caption := Value;
end;

procedure TJvPUrls.SetPublishers(const Value: string);
begin
  (FParent as TJvVisualId3v2).FPublishers.Caption := Value;
end;

procedure TJvPUrls.SetRadioStation(const Value: string);
begin
  (FParent as TJvVisualId3v2).FRadioStation.Caption := Value;
end;

procedure TJvPUrls.SetUserDefined(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FUserD.Items.Text := Value.Text;
end;

function TJvPMisc.GetCounter: string;
begin
  Result := (FParent as TJvVisualId3v2).FCounter.Caption;
end;

//=== TJvPMisc ===============================================================

// (rom) not a good idea to create s TStringList

function TJvPMisc.GetInvolved: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FInvolved.Text;
end;

function TJvPMisc.GetPrice: string;
begin
  Result := (FParent as TJvVisualId3v2).FPrice.Caption;
end;

function TJvPMisc.GetPurchased: string;
begin
  Result := (FParent as TJvVisualId3v2).FDatePur.Caption;
end;

function TJvPMisc.GetRating: string;
begin
  Result := (FParent as TJvVisualId3v2).FRating.Caption;
end;

function TJvPMisc.GetSeller: string;
begin
  Result := (FParent as TJvVisualId3v2).FSeller.Caption;
end;

function TJvPMisc.GetUserEmail: string;
begin
  Result := (FParent as TJvVisualId3v2).FUserMail.Caption;
end;

procedure TJvPMisc.SetCounter(const Value: string);
begin
  (FParent as TJvVisualId3v2).FCounter.Caption := Value;
end;

procedure TJvPMisc.SetInvolved(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FInvolved.Text := Value.Text;
end;

procedure TJvPMisc.SetPrice(const Value: string);
begin
  (FParent as TJvVisualId3v2).FPrice.Caption := Value;
end;

procedure TJvPMisc.SetPurchased(const Value: string);
begin
  (FParent as TJvVisualId3v2).FDatePur.Caption := Value;
end;

procedure TJvPMisc.SetRating(const Value: string);
begin
  (FParent as TJvVisualId3v2).FRating.Caption := Value;
end;

procedure TJvPMisc.SetSeller(const Value: string);
begin
  (FParent as TJvVisualId3v2).FSeller.Caption := Value;
end;

procedure TJvPMisc.SetUserEmail(const Value: string);
begin
  (FParent as TJvVisualId3v2).FUserMail.Caption := Value;
end;

//=== TJvPTexts ==============================================================

constructor TJvPTexts.Create;
begin
  inherited Create;
  FJvText1 := TJvPText1.Create;
  FJvText2 := TJvPText2.Create;
  FJvText3 := TJvPText3.Create;
  FJvText4 := TJvPText4.Create;
end;

destructor TJvPTexts.Destroy;
begin
  FJvText1.Free;
  FJvText2.Free;
  FJvText3.Free;
  FJvText4.Free;
  inherited Destroy;
end;

//=== TJvPText1 ==============================================================

function TJvPText1.GetArtists: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FArtists.Text;
end;

function TJvPText1.GetBand: string;
begin
  Result := (FParent as TJvVisualId3v2).FBand.Caption;
end;

function TJvPText1.GetComposers: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FPublishersLst.Text;
end;

function TJvPText1.GetConductor: string;
begin
  Result := (FParent as TJvVisualId3v2).FConductor.Caption;
end;

function TJvPText1.GetCreation: string;
begin
  Result := (FParent as TJvVisualId3v2).FCreation.Caption;
end;

function TJvPText1.GetLanguages: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FLanguages.Text;
end;

function TJvPText1.GetLyricists: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FLyricists.Text;
end;

function TJvPText1.GetRemixed: string;
begin
  Result := (FParent as TJvVisualId3v2).FRemixed.Caption;
end;

function TJvPText1.GetSubTitle: string;
begin
  Result := (FParent as TJvVisualId3v2).FSubTitle.Caption;
end;

function TJvPText1.GetTitle: string;
begin
  Result := (FParent as TJvVisualId3v2).FTitle.Caption;
end;

procedure TJvPText1.SetArtists(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FArtists.Text := Value.Text;
end;

procedure TJvPText1.SetBand(const Value: string);
begin
  (FParent as TJvVisualId3v2).FBand.Caption := Value;
end;

procedure TJvPText1.SetComposers(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FPublishersLst.Text := Value.Text;
end;

procedure TJvPText1.SetConductor(const Value: string);
begin
  (FParent as TJvVisualId3v2).FConductor.Caption := Value;
end;

procedure TJvPText1.SetCreation(const Value: string);
begin
  (FParent as TJvVisualId3v2).FCreation.Caption := Value;
end;

procedure TJvPText1.SetLanguages(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FLanguages.Text := Value.Text;
end;

procedure TJvPText1.SetLyricists(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FLyricists.Text := Value.Text;
end;

procedure TJvPText1.SetRemixed(const Value: string);
begin
  (FParent as TJvVisualId3v2).FRemixed.Caption := Value;
end;

procedure TJvPText1.SetSubTitle(const Value: string);
begin
  (FParent as TJvVisualId3v2).FSubTitle.Caption := Value;
end;

procedure TJvPText1.SetTitle(const Value: string);
begin
  (FParent as TJvVisualId3v2).FTitle.Caption := Value;
end;

//=== TJvPText2 ==============================================================

function TJvPText2.GetAlbum: string;
begin
  Result := (FParent as TJvVisualId3v2).FAlbum.Caption;
end;

function TJvPText2.GetContentType: string;
begin
  Result := (FParent as TJvVisualId3v2).FContent.Caption;
end;

function TJvPText2.GetDate: string;
begin
  Result := (FParent as TJvVisualId3v2).FDate.Caption;
end;

function TJvPText2.GetISRC: string;
begin
  Result := (FParent as TJvVisualId3v2).FISRC.Caption;
end;

function TJvPText2.GetMediaType: string;
begin
  Result := (FParent as TJvVisualId3v2).FMediaType.Caption;
end;

function TJvPText2.GetPart: string;
begin
  Result := (FParent as TJvVisualId3v2).FPart.Caption;
end;

function TJvPText2.GetRecorded: string;
begin
  Result := (FParent as TJvVisualId3v2).FRecordedAt.Caption;
end;

function TJvPText2.GetTime: string;
begin
  Result := (FParent as TJvVisualId3v2).FTime.Caption;
end;

function TJvPText2.GetTrack: string;
begin
  Result := (FParent as TJvVisualId3v2).FTrack.Caption;
end;

function TJvPText2.GetYear: string;
begin
  Result := (FParent as TJvVisualId3v2).FYear.Caption;
end;

procedure TJvPText2.SetAlbum(const Value: string);
begin
  (FParent as TJvVisualId3v2).FAlbum.Caption := Value;
end;

procedure TJvPText2.SetContentType(const Value: string);
begin
  (FParent as TJvVisualId3v2).FContent.Caption := Value;
end;

procedure TJvPText2.SetDate(const Value: string);
begin
  (FParent as TJvVisualId3v2).FDate.Caption := Value;
end;

procedure TJvPText2.SetISRC(const Value: string);
begin
  (FParent as TJvVisualId3v2).FISRC.Caption := Value;
end;

procedure TJvPText2.SetMediaType(const Value: string);
begin
  (FParent as TJvVisualId3v2).FMediaType.Caption := Value;
end;

procedure TJvPText2.SetPart(const Value: string);
begin
  (FParent as TJvVisualId3v2).FPart.Caption := Value;
end;

procedure TJvPText2.SetRecorded(const Value: string);
begin
  (FParent as TJvVisualId3v2).FRecordedAt.Caption := Value;
end;

procedure TJvPText2.SetTime(const Value: string);
begin
  (FParent as TJvVisualId3v2).FTime.Caption := Value;
end;

procedure TJvPText2.SetTrack(const Value: string);
begin
  (FParent as TJvVisualId3v2).FTrack.Caption := Value;
end;

procedure TJvPText2.SetYear(const Value: string);
begin
  (FParent as TJvVisualId3v2).FYear.Caption := Value;
end;

//=== TJvPText3 ==============================================================

function TJvPText3.GetBPM: string;
begin
  Result := (FParent as TJvVisualId3v2).FBPM.Caption;
end;

function TJvPText3.GetCopyright: string;
begin
  Result := (FParent as TJvVisualId3v2).FCopyright.Caption;
end;

function TJvPText3.GetDelay: string;
begin
  Result := (FParent as TJvVisualId3v2).FDelay.Caption;
end;

function TJvPText3.GetEncodedBy: string;
begin
  Result := (FParent as TJvVisualId3v2).FEncoded.Caption;
end;

function TJvPText3.GetEncoder: string;
begin
  Result := (FParent as TJvVisualId3v2).FEncoder.Caption;
end;

function TJvPText3.GetInitialKey: string;
begin
  Result := (FParent as TJvVisualId3v2).FInitialKey.Caption;
end;

function TJvPText3.GetLength: string;
begin
  Result := (FParent as TJvVisualId3v2).FLength.Caption;
end;

function TJvPText3.GetOFileName: string;
begin
  Result := (FParent as TJvVisualId3v2).FOFileName.Caption;
end;

function TJvPText3.GetPublisher: string;
begin
  Result := (FParent as TJvVisualId3v2).FPublisher.Caption;
end;

function TJvPText3.GetSize: string;
begin
  Result := (FParent as TJvVisualId3v2).FSize.Caption;
end;

procedure TJvPText3.SetBPM(const Value: string);
begin
  (FParent as TJvVisualId3v2).FBPM.Caption := Value;
end;

procedure TJvPText3.SetCopyright(const Value: string);
begin
  (FParent as TJvVisualId3v2).FCopyright.Caption := Value;
end;

procedure TJvPText3.SetDelay(const Value: string);
begin
  (FParent as TJvVisualId3v2).FDelay.Caption := Value;
end;

procedure TJvPText3.SetEncodedBy(const Value: string);
begin
  (FParent as TJvVisualId3v2).FEncoded.Caption := Value;
end;

procedure TJvPText3.SetEncoder(const Value: string);
begin
  (FParent as TJvVisualId3v2).FEncoder.Caption := Value;
end;

procedure TJvPText3.SetInitialKey(const Value: string);
begin
  (FParent as TJvVisualId3v2).FInitialKey.Caption := Value;
end;

procedure TJvPText3.SetLength(const Value: string);
begin
  (FParent as TJvVisualId3v2).FLength.Caption := Value;
end;

procedure TJvPText3.SetOFileName(const Value: string);
begin
  (FParent as TJvVisualId3v2).FOFileName.Caption := Value;
end;

procedure TJvPText3.SetPublisher(const Value: string);
begin
  (FParent as TJvVisualId3v2).FPublisher.Caption := Value;
end;

procedure TJvPText3.SetSize(const Value: string);
begin
  (FParent as TJvVisualId3v2).FSize.Caption := Value;
end;

//=== TJvPText4 ==============================================================

function TJvPText4.GetFileOwner: string;
begin
  Result := (FParent as TJvVisualId3v2).FFileOwner.Caption;
end;

function TJvPText4.GetFileType: string;
begin
  Result := (FParent as TJvVisualId3v2).FFileType.Caption;
end;

function TJvPText4.GetOAlbum: string;
begin
  Result := (FParent as TJvVisualId3v2).FOAlbum.Caption;
end;

function TJvPText4.GetOArtists: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FOArtists.Text;
end;

function TJvPText4.GetOLyricists: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FOLyricists.Text;
end;

function TJvPText4.GetRadioName: string;
begin
  Result := (FParent as TJvVisualId3v2).FRadioName.Caption;
end;

function TJvPText4.GetRadioOwner: string;
begin
  Result := (FParent as TJvVisualId3v2).FRadioOwner.Caption;
end;

function TJvPText4.GetUserDefined: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := (FParent as TJvVisualId3v2).FUserTexts.Text;
end;

function TJvPText4.GetYear: string;
begin
  Result := (FParent as TJvVisualId3v2).FOYear.Caption;
end;

procedure TJvPText4.SetFileOwner(const Value: string);
begin
  (FParent as TJvVisualId3v2).FFileOwner.Caption := Value;
end;

procedure TJvPText4.SetFileType(const Value: string);
begin
  (FParent as TJvVisualId3v2).FFileType.Caption := Value;
end;

procedure TJvPText4.SetOAlbum(const Value: string);
begin
  (FParent as TJvVisualId3v2).FOAlbum.Caption := Value;
end;

procedure TJvPText4.SetOArtists(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FOArtists.Text := Value.Text;
end;

procedure TJvPText4.SetOLyricists(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FOLyricists.Text := Value.Text;
end;

procedure TJvPText4.SetRadioName(const Value: string);
begin
  (FParent as TJvVisualId3v2).FRadioName.Caption := Value;
end;

procedure TJvPText4.SetRadioOwner(const Value: string);
begin
  (FParent as TJvVisualId3v2).FRadioOwner.Caption := Value;
end;

procedure TJvPText4.SetUserDefined(const Value: TStringList);
begin
  (FParent as TJvVisualId3v2).FUserTexts.Text := Value.Text;
end;

procedure TJvPText4.SetYear(const Value: string);
begin
  (FParent as TJvVisualId3v2).FOYear.Caption := Value;
end;

//=== TJvPId3v2TreeView ======================================================

function TJvPId3v2TreeView.GetHideSelection: Boolean;
begin
  Result := (FParent as TJvVisualId3v2).FTreeView.HideSelection;
end;

function TJvPId3v2TreeView.GetHotTrack: Boolean;
begin
  Result := (FParent as TJvVisualId3v2).FTreeView.HotTrack;
end;

function TJvPId3v2TreeView.GetReadOnly: Boolean;
begin
  Result := (FParent as TJvVisualId3v2).FTreeView.ReadOnly;
end;

function TJvPId3v2TreeView.GetWidth: Integer;
begin
  Result := (FParent as TJvVisualId3v2).FTreeView.Width;
end;

procedure TJvPId3v2TreeView.SetHideSelection(const Value: Boolean);
begin
  (FParent as TJvVisualId3v2).FTreeView.HideSelection := Value;
end;

procedure TJvPId3v2TreeView.SetHotTrack(const Value: Boolean);
begin
  (FParent as TJvVisualId3v2).FTreeView.HotTrack := Value;
end;

procedure TJvPId3v2TreeView.SetReadOnly(const Value: Boolean);
begin
  (FParent as TJvVisualId3v2).FTreeView.ReadOnly := Value;
end;

procedure TJvPId3v2TreeView.SetWidth(const Value: Integer);
begin
  (FParent as TJvVisualId3v2).FTreeView.Width := Value;
end;

end.

