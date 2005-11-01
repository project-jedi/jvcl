{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{$I jvcl.inc}
unit MainFrm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, QExtCtrls, JvQExExtCtrls, JvQComponent, JvQRollOut, QImgList,
  QStdCtrls, QActnList, QStdActns, QMenus, QTypes, JvQControlComponent,
  JvQEventFilter, JvQDoubleBuffering;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    ImageList1: TImageList;
    Panel4: TPanel;
    chkShowFocus: TCheckBox;
    chkTabStop: TCheckBox;
    chkToggleAnywhere: TCheckBox;
    Panel3: TPanel;
    pnlRightAlign: TPanel;
    RO40: TJvRollOut;
    RO39: TJvRollOut;
    RO38: TJvRollOut;
    RO37: TJvRollOut;
    RO36: TJvRollOut;
    RO35: TJvRollOut;
    RO34: TJvRollOut;
    RO33: TJvRollOut;
    RO32: TJvRollOut;
    RO31: TJvRollOut;
    pnlLeftAlign: TPanel;
    RO30: TJvRollOut;
    RO29: TJvRollOut;
    RO28: TJvRollOut;
    RO27: TJvRollOut;
    RO26: TJvRollOut;
    RO25: TJvRollOut;
    RO24: TJvRollOut;
    RO23: TJvRollOut;
    RO22: TJvRollOut;
    RO21: TJvRollOut;
    Panel1: TPanel;
    pnlTopAlign: TPanel;
    RO1: TJvRollOut;
    RO2: TJvRollOut;
    RO3: TJvRollOut;
    RO4: TJvRollOut;
    RO5: TJvRollOut;
    RO6: TJvRollOut;
    RO7: TJvRollOut;
    RO8: TJvRollOut;
    RO9: TJvRollOut;
    RO10: TJvRollOut;
    pnlBottomAlign: TPanel;
    RO20: TJvRollOut;
    RO19: TJvRollOut;
    RO18: TJvRollOut;
    RO17: TJvRollOut;
    RO16: TJvRollOut;
    RO15: TJvRollOut;
    RO14: TJvRollOut;
    RO13: TJvRollOut;
    RO12: TJvRollOut;
    RO11: TJvRollOut;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    chkGroupIndex: TCheckBox;
    chkHideButton: TCheckBox;
    chkHideFrame: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    chkImages: TCheckBox;
    ActionList1: TActionList;
    JvRollOutAction1: TJvRollOutAction;
    JvRollOutAction2: TJvRollOutAction;
    MainMenu1: TMainMenu;
    Actions1: TMenuItem;
    Action1Ctrl11: TMenuItem;
    Action2Ctrl21: TMenuItem;
    JvDoubleBuffering1: TJvDoubleBuffering;
    procedure chkShowFocusClick(Sender: TObject);
    procedure chkTabStopClick(Sender: TObject);
    procedure chkToggleAnywhereClick(Sender: TObject);
    procedure chkGroupIndexClick(Sender: TObject);
    procedure chkHideButtonClick(Sender: TObject);
    procedure chkHideFrameClick(Sender: TObject);
    procedure chkImagesClick(Sender: TObject);
    procedure JvRollOutAction1Execute(Sender: TObject);
    procedure JvRollOutAction2Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.chkShowFocusClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).ShowFocus := chkShowFocus.Checked;
end;

procedure TForm1.chkTabStopClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).TabStop   := chkTabStop.Checked;
end;

procedure TForm1.chkToggleAnywhereClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).ToggleAnywhere := chkToggleAnywhere.Checked;
end;

procedure TForm1.chkGroupIndexClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
      TJvRollOut(Components[i]).GroupIndex := Ord(chkGroupIndex.Checked);
end;

procedure TForm1.chkHideButtonClick(Sender: TObject);
const
  cTopColor:array[boolean] of TColor = (clBtnHighlight, clNone);
  cBtmColor:array[boolean] of TColor = (clBtnShadow, clNone);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
    begin
      TJvRollOut(Components[i]).Colors.ButtonTop := cTopColor[chkHideButton.Checked];
      TJvRollOut(Components[i]).Colors.ButtonBottom := cBtmColor[chkHideButton.Checked];
    end;
end;

procedure TForm1.chkHideFrameClick(Sender: TObject);
const
  cTopColor:array[boolean] of TColor = (clBtnShadow, clNone);
  cBtmColor:array[boolean] of TColor = (clBtnHighlight, clNone);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
    begin
      TJvRollOut(Components[i]).Colors.FrameTop := cTopColor[chkHideFrame.Checked];
      TJvRollOut(Components[i]).Colors.FrameBottom := cBtmColor[chkHideFrame.Checked];
    end;
end;

procedure TForm1.chkImagesClick(Sender: TObject);
var i:integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJvRollOut then
    begin
      if chkImages.Checked then
        TJvRollOut(Components[i]).ImageOptions.Images := ImageList1
      else
        TJvRollOut(Components[i]).ImageOptions.Images := nil;
    end;
end;

procedure TForm1.JvRollOutAction1Execute(Sender: TObject);
begin
  Caption := 'Action 1 executed!';
end;

procedure TForm1.JvRollOutAction2Execute(Sender: TObject);
begin
  Caption := 'Action 2 executed!'; 
end;

procedure TForm1.FormCreate(Sender: TObject);
begin 
//  JvRollOutAction2.AutoCheck := true; 
end;

end.
